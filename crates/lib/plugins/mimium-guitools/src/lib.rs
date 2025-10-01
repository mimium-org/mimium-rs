use std::sync::Arc;

use mimium_lang::{
    ast::{Expr, Literal},
    code, function,
    interner::{ToSymbol, TypeNodeId},
    interpreter::Value,
    log, numeric,
    pattern::TypedId,
    plugin::{SysPluginSignature, SystemPlugin, SystemPluginFnType, SystemPluginMacroType},
    runtime::vm::{Machine, ReturnCode},
    string_t,
    types::{PType, Type},
};
use plot_window::PlotApp;
use ringbuf::{
    HeapProd, HeapRb,
    traits::{Producer, Split},
};

use crate::plot_window::FloatParameter;
pub(crate) mod plot_ui;
pub mod plot_window;

pub struct GuiToolPlugin {
    window: Option<PlotApp>,
    slider_instances: Vec<Arc<FloatParameter>>,
    probe_instances: Vec<HeapProd<f64>>,
}

impl Default for GuiToolPlugin {
    fn default() -> Self {
        Self {
            window: Some(PlotApp::default()),
            slider_instances: Vec::new(),
            probe_instances: Vec::new(),
        }
    }
}

impl GuiToolPlugin {
    fn get_closure_type() -> TypeNodeId {
        function!(vec![numeric!()], numeric!())
    }
    const GET_SLIDER: &'static str = "__get_slider";
    const PROBE_INTERCEPT: &'static str = "__probe_intercept";

    pub fn make_slider(&mut self, v: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(v.len(), 4);
        let (name, init, min, max, window) = match (
            v[0].0.clone(),
            v[1].0.clone(),
            v[2].0.clone(),
            v[3].0.clone(),
            self.window.as_mut(),
        ) {
            (
                Value::String(name),
                Value::Number(init),
                Value::Number(min),
                Value::Number(max),
                Some(window),
            ) => (name, init, min, max, window),
            _ => {
                log::error!("invalid argument");
                return Value::Number(0.0);
            }
        };
        let (p, idx) = window.add_slider(name.as_str(), init, min, max);
        self.slider_instances.push(p);
        Value::Code(
            Expr::Apply(
                Expr::Var(Self::GET_SLIDER.to_symbol()).into_id_without_span(),
                vec![
                    Expr::Literal(Literal::Float(idx.to_string().to_symbol()))
                        .into_id_without_span(),
                ],
            )
            .into_id_without_span(),
        )
    }

    pub fn make_probe_macro(&mut self, v: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(v.len(), 1);
        let (name, window) = match (v[0].0.clone(), self.window.as_mut()) {
            (Value::String(name), Some(window)) => (name, window),
            _ => {
                log::error!(
                    "invalid argument for Probe macro type:{},{:#?}",
                    v[0].1,
                    self.window.is_some()
                );
                return Value::Code(
                    Expr::Lambda(
                        vec![TypedId::new(
                            "x".to_symbol(),
                            Type::Primitive(PType::Numeric).into_id(),
                        )],
                        None,
                        Expr::Var("x".to_symbol()).into_id_without_span(),
                    )
                    .into_id_without_span(),
                );
            }
        };

        let (prod, cons) = HeapRb::<f64>::new(4096).split();
        window.add_plot(name.as_str(), cons);
        let idx = self.probe_instances.len();
        self.probe_instances.push(prod);

        // Generate a lambda that calls probe_intercept with the fixed ID
        Value::Code(
            Expr::Lambda(
                vec![TypedId::new(
                    "x".to_symbol(),
                    Type::Primitive(PType::Numeric).into_id(),
                )],
                None,
                Expr::Apply(
                    Expr::Var(Self::PROBE_INTERCEPT.to_symbol()).into_id_without_span(),
                    vec![
                        Expr::Var("x".to_symbol()).into_id_without_span(),
                        Expr::Literal(Literal::Float(idx.to_string().to_symbol()))
                            .into_id_without_span(),
                    ],
                )
                .into_id_without_span(),
            )
            .into_id_without_span(),
        )
    }
    pub fn get_slider(&mut self, vm: &mut Machine) -> ReturnCode {
        let slider_idx = Machine::get_as::<f64>(vm.get_stack(0)) as usize;

        match self.slider_instances.get(slider_idx) {
            Some(s) => {
                vm.set_stack(0, Machine::to_value(s.get()));
            }
            None => {
                log::error!("invalid slider index");
                return 0;
            }
        };

        1
    }

    pub fn probe_intercept(&mut self, vm: &mut Machine) -> ReturnCode {
        let value = Machine::get_as::<f64>(vm.get_stack(0));
        let probe_idx = Machine::get_as::<f64>(vm.get_stack(1)) as usize;

        match self.probe_instances.get_mut(probe_idx) {
            Some(prod) => {
                let _ = prod.try_push(value);
                // Do not modify any stack because we are returning the head of argument as is
            }
            None => {
                log::error!("invalid probe index: {probe_idx}");
            }
        }

        1
    }
}
impl SystemPlugin for GuiToolPlugin {
    fn try_get_main_loop(&mut self) -> Option<Box<dyn FnOnce()>> {
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(window) = self.window.take() {
            Some(Box::new(move || {
                let native_options = eframe::NativeOptions {
                    viewport: egui::ViewportBuilder::default()
                        .with_inner_size([400.0, 300.0])
                        .with_min_inner_size([300.0, 220.0])
                        .with_icon(
                            // NOTE: Adding an icon is optional
                            eframe::icon_data::from_png_bytes(
                                &include_bytes!("../assets/mimium_logo_256.png")[..],
                            )
                            .expect("Failed to load icon"),
                        ),
                    ..Default::default()
                };
                let _ = eframe::run_native(
                    "mimium guitools",
                    native_options,
                    Box::new(|_cc| Ok(Box::new(window))),
                )
                .inspect_err(|e| log::error!("{e}"));
            }))
        } else {
            None
        }
        #[cfg(target_arch = "wasm32")]
        None
    }
    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        let sliderf: SystemPluginMacroType<Self> = Self::make_slider;
        let make_slider = SysPluginSignature::new_macro(
            "Slider",
            sliderf,
            function!(
                vec![string_t!(), numeric!(), numeric!(), numeric!()],
                code!(numeric!())
            ),
        );

        let getsliderf: SystemPluginFnType<Self> = Self::get_slider;
        let get_slider = SysPluginSignature::new(
            Self::GET_SLIDER,
            getsliderf,
            function!(vec![numeric!()], numeric!()),
        );

        // Replace make_probe function with Probe macro
        let probe_macrof: SystemPluginMacroType<Self> = Self::make_probe_macro;
        let probe_macro = SysPluginSignature::new_macro(
            "Probe",
            probe_macrof,
            function!(
                vec![string_t!()],
                Type::Code(function!(vec![numeric!()], numeric!())).into_id()
            ),
        );
        let probe_interceptf: SystemPluginFnType<Self> = Self::probe_intercept;
        let probe_intercept = SysPluginSignature::new(
            Self::PROBE_INTERCEPT,
            probe_interceptf,
            function!(vec![numeric!(), numeric!()], numeric!()),
        );

        vec![probe_macro, make_slider, get_slider, probe_intercept]
    }
}
