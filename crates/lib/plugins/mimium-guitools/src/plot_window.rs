use std::{
    collections::BTreeMap,
    ops::RangeInclusive,
    sync::{Arc, Mutex, atomic::Ordering},
};

use crate::plot_ui::{self, PlotUi};
use eframe;

use atomic_float::AtomicF64;
use egui::{Color32, RichText, TextStyle};
use egui_plot::{CoordinatesFormatter, Corner, Legend, Plot};
use ringbuf::HeapCons;

pub struct FloatParameter {
    value: AtomicF64,
    name: String,
    range: RangeInclusive<AtomicF64>,
}
impl FloatParameter {
    fn new(name: String, init: f64, min: f64, max: f64) -> Self {
        Self {
            value: AtomicF64::new(init),
            name,
            range: AtomicF64::new(min)..=AtomicF64::new(max),
        }
    }
    pub fn get(&self) -> f64 {
        self.value.load(Ordering::Relaxed)
    }
    pub fn set(&self, v: f64) {
        self.value.store(v, Ordering::Relaxed)
    }
    pub fn set_range(&self, min: f64, max: f64) {
        self.range.start().store(min, Ordering::Relaxed);
        self.range.end().store(max, Ordering::Relaxed);
    }
    pub(crate) fn name(&self) -> &str {
        self.name.as_str()
    }
}

#[derive(Default)]
pub struct PlotApp {
    plot: Vec<plot_ui::PlotUi>,
    pub(crate) sliders: Vec<Arc<FloatParameter>>,
    #[cfg(feature = "osc")]
    osc_receiver: Option<crate::osc::OscSliderReceiver>,
    #[cfg(feature = "osc")]
    osc_init_attempted: bool,
    hue: f32,
    autoscale: bool,
}

impl PlotApp {
    pub fn new_test() -> Self {
        let plot = vec![PlotUi::new_test("test")];
        Self {
            plot,
            sliders: Vec::new(),
            #[cfg(feature = "osc")]
            osc_receiver: None,
            #[cfg(feature = "osc")]
            osc_init_attempted: false,
            hue: 0.0,
            autoscale: false,
        }
    }
    const HUE_MARGIN: f32 = 1.0 / 8.0 + 0.3;
    pub fn add_plot(&mut self, label: &str, buf: HeapCons<f64>) {
        let [r, g, b] = egui::ecolor::Hsva::new(self.hue, 0.7, 0.7, 1.0).to_srgb();
        self.hue += Self::HUE_MARGIN;
        self.plot.push(PlotUi::new(
            label,
            buf,
            Color32::from_rgba_premultiplied(r, g, b, 200),
        ))
    }
    pub fn add_slider(
        &mut self,
        name: &str,
        init: f64,
        min: f64,
        max: f64,
    ) -> (Arc<FloatParameter>, usize) {
        let param = FloatParameter::new(name.to_string(), init, min, max);
        let p = Arc::new(param);
        self.sliders.push(p.clone());
        (p, self.sliders.len() - 1)
    }
    pub fn is_empty(&self) -> bool {
        self.plot.is_empty()
    }

    pub fn suggested_viewport_size(&self) -> [f32; 2] {
        let base_width = 420.0;
        let top_panel_height = 36.0;
        let plot_height = if self.plot.is_empty() { 120.0 } else { 220.0 };
        let slider_header = if self.sliders.is_empty() { 0.0 } else { 24.0 };
        let slider_rows = self.sliders.len() as f32;
        let slider_height = slider_rows * 28.0;
        let margin = 28.0;

        let total_height = top_panel_height + plot_height + slider_header + slider_height + margin;

        [base_width, total_height.clamp(260.0, 960.0)]
    }
}

impl eframe::App for PlotApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if self.plot.is_empty() && self.sliders.is_empty() {
            return;
        }
        #[cfg(feature = "osc")]
        {
            if !self.osc_init_attempted {
                self.osc_receiver = crate::osc::OscSliderReceiver::from_env();
                self.osc_init_attempted = true;
            }
            if let Some(receiver) = self.osc_receiver.as_mut() {
                receiver.poll_and_apply(&self.sliders);
            }
        }
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            // The top panel is often a good place for a menu bar:

            egui::menu::bar(ui, |ui| {
                egui::widgets::global_theme_preference_buttons(ui);
                ui.add_space(16.0);
                use egui::special_emojis::GITHUB;
                ui.hyperlink_to(
                    format!("{GITHUB} mimium-rs on GitHub"),
                    "https://github.com/tomoyanonymous/mimium-rs",
                );
                ui.checkbox(&mut self.autoscale, "Auto Scale")
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            let plot = Plot::new("lines_demo")
                .legend(Legend::default())
                .show_axes(true)
                .show_grid(true)
                .allow_scroll(false)
                .auto_bounds([true, self.autoscale].into())
                .coordinates_formatter(Corner::LeftBottom, CoordinatesFormatter::default());

            plot.show(ui, |plot_ui| {
                self.plot.iter_mut().for_each(|line| {
                    // Drain the ring buffer even for hidden channels so
                    // the producer never blocks.
                    let (_req_repaint, drawn) = line.draw_line();
                    // Only display channels that have received data.
                    if line.has_data() {
                        plot_ui.line(drawn);
                    }
                })
            });

            ui.ctx().request_repaint();
        });
        egui::TopBottomPanel::bottom("parameters")
            .resizable(true)
            .default_height(220.0)
            .min_height(120.0)
            .show(ctx, |ui| {
                if !self.sliders.is_empty() {
                    ui.label("Parameters");
                }

                let grouped_sliders = self.sliders.iter().fold(
                    BTreeMap::<String, Vec<Arc<FloatParameter>>>::new(),
                    |mut groups, slider| {
                        let group_name = slider
                            .name
                            .rsplit_once('.')
                            .map(|(group, _)| group.to_string())
                            .unwrap_or_default();
                        groups.entry(group_name).or_default().push(slider.clone());
                        groups
                    },
                );

                egui::ScrollArea::vertical().show(ui, |ui| {
                    grouped_sliders.iter().for_each(|(group_name, sliders)| {
                        let draw_group = |ui: &mut egui::Ui| {
                            const LABEL_WIDTH: f32 = 80.0;
                            const MINMAX_WIDTH: f32 = 60.0;
                            const CURRENT_WIDTH: f32 = 60.0;

                            sliders.iter().for_each(|slider| {
                                let mut value = slider.get();
                                let mut min = slider.range.start().load(Ordering::Relaxed);
                                let mut max = slider.range.end().load(Ordering::Relaxed);
                                let min_id = egui::Id::new(("slider_min", slider.name.as_str()));
                                let max_id = egui::Id::new(("slider_max", slider.name.as_str()));

                                if let Some(saved_min) =
                                    ui.ctx().data_mut(|d| d.get_persisted::<f64>(min_id))
                                {
                                    min = saved_min;
                                }
                                if let Some(saved_max) =
                                    ui.ctx().data_mut(|d| d.get_persisted::<f64>(max_id))
                                {
                                    max = saved_max;
                                }

                                let label = slider
                                    .name
                                    .rsplit_once('.')
                                    .map(|(_, leaf)| leaf)
                                    .unwrap_or(slider.name.as_str());

                                let mut min_edited = false;
                                let mut max_edited = false;
                                let slider_changed = ui
                                    .horizontal(|ui| {
                                        ui.add_sized([LABEL_WIDTH, 0.0], egui::Label::new(label));

                                        min_edited = ui
                                            .scope(|ui| {
                                                ui.style_mut().override_text_style =
                                                    Some(TextStyle::Small);
                                                ui.add_sized(
                                                    [MINMAX_WIDTH, 0.0],
                                                    egui::DragValue::new(&mut min)
                                                        .speed(0.1)
                                                        .max_decimals(6),
                                                )
                                                .changed()
                                            })
                                            .inner;

                                        let is_fine_adjust =
                                            ui.input(|input| input.modifiers.shift);
                                        let base_range = (max - min).abs();
                                        let fine_step = (base_range / 10_000.0).max(1e-9);
                                        let slider_widget = if is_fine_adjust {
                                            egui::Slider::new(&mut value, min..=max)
                                                .clamping(egui::SliderClamping::Always)
                                                .show_value(false)
                                                .smart_aim(false)
                                                .step_by(fine_step)
                                        } else {
                                            egui::Slider::new(&mut value, min..=max)
                                                .clamping(egui::SliderClamping::Always)
                                                .show_value(false)
                                        };

                                        let slider_changed = ui.add(slider_widget).changed();

                                        max_edited = ui
                                            .scope(|ui| {
                                                ui.style_mut().override_text_style =
                                                    Some(TextStyle::Small);
                                                ui.add_sized(
                                                    [MINMAX_WIDTH, 0.0],
                                                    egui::DragValue::new(&mut max)
                                                        .speed(0.1)
                                                        .max_decimals(6),
                                                )
                                                .changed()
                                            })
                                            .inner;

                                        ui.add_sized(
                                            [CURRENT_WIDTH, 0.0],
                                            egui::Label::new(
                                                RichText::new(format!("{value:.4}"))
                                                    .text_style(TextStyle::Small),
                                            ),
                                        );

                                        slider_changed
                                    })
                                    .inner;

                                if min_edited || max_edited {
                                    if min > max {
                                        if min_edited {
                                            max = min;
                                        } else {
                                            min = max;
                                        }
                                    }
                                    slider.set_range(min, max);
                                }

                                ui.ctx().data_mut(|d| {
                                    d.insert_persisted(min_id, min);
                                    d.insert_persisted(max_id, max);
                                });

                                if slider_changed {
                                    slider.set(value);
                                }
                            });
                        };

                        if group_name.is_empty() {
                            draw_group(ui);
                        } else {
                            ui.collapsing(group_name, draw_group);
                        }
                    });
                });
            });
    }
}

pub struct AsyncPlotApp {
    pub window: Arc<Mutex<PlotApp>>,
}

impl eframe::App for AsyncPlotApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if let Ok(mut window) = self.window.lock() {
            window.update(ctx, _frame);
        }
    }
}
