use std::{
    ops::RangeInclusive,
    sync::{Arc, Mutex, atomic::Ordering},
};

use crate::plot_ui::{self, PlotUi};
use eframe;

use atomic_float::AtomicF64;
use egui::Color32;
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
}

#[derive(Default)]
pub struct PlotApp {
    plot: Vec<plot_ui::PlotUi>,
    pub(crate) sliders: Vec<Arc<FloatParameter>>,
    hue: f32,
    autoscale: bool,
}

impl PlotApp {
    pub fn new_test() -> Self {
        let plot = vec![PlotUi::new_test("test")];
        Self {
            plot,
            sliders: Vec::new(),
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
}

impl eframe::App for PlotApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if self.plot.is_empty() && self.sliders.is_empty() {
            return;
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
                .auto_bounds([true, self.autoscale].into())
                .coordinates_formatter(Corner::LeftBottom, CoordinatesFormatter::default());

            plot.show(ui, |plot_ui| {
                self.plot.iter_mut().for_each(|line| {
                    let (_req_repaint, line) = line.draw_line();
                    plot_ui.line(line);
                })
            });

            ui.ctx().request_repaint();
        });
        egui::TopBottomPanel::bottom("parameters").show(ctx, |ui| {
            if !self.sliders.is_empty() {
                ui.label("Parameters");
            }
            egui::ScrollArea::vertical().show(ui, |ui| {
                for p in &self.sliders {
                    let mut v = p.get();
                    if ui
                        .add(
                            egui::Slider::new(
                                &mut v,
                                p.range.start().load(Ordering::Relaxed)
                                    ..=p.range.end().load(Ordering::Relaxed),
                            )
                            .text(&p.name)
                            .clamping(egui::SliderClamping::Always),
                        )
                        .changed()
                    {
                        p.set(v);
                    }
                }
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
