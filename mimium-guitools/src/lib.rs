use std::{cell::RefCell, rc::Rc};

use mimium_lang::{
    ast::{Expr, Literal},
    function,
    interner::{ToSymbol, TypeNodeId},
    interpreter::Value,
    log, numeric,
    plugin::{
        ExtClsInfo, SysPluginSignature, SystemPlugin, SystemPluginFnType, SystemPluginMacroType,
    },
    runtime::vm::{Machine, ReturnCode},
    string_t,
    types::{PType, Type},
};
use plot_window::PlotApp;
use ringbuf::{
    HeapRb,
    traits::{Producer, Split},
};
pub(crate) mod plot_ui;
pub mod plot_window;

pub struct GuiToolPlugin {
    window: Option<PlotApp>,
}

impl Default for GuiToolPlugin {
    fn default() -> Self {
        Self {
            window: Some(PlotApp::default()),
        }
    }
}

impl GuiToolPlugin {
    fn get_closure_type() -> TypeNodeId {
        function!(vec![numeric!()], numeric!())
    }
    const GET_SLIDER: &'static str = "__get_slider";

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
        let idx = window.add_slider(name.as_str(), init, min, max);

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
    pub fn get_slider(&mut self, vm: &mut Machine) -> ReturnCode {
        if let Some(window) = self.window.as_mut() {
            let slider_idx = Machine::get_as::<f64>(vm.get_stack(0)) as usize;
            match window.sliders.get(slider_idx) {
                Some(s) => {
                    vm.set_stack(0, Machine::to_value(s.get()));
                }
                None => {
                    log::error!("invalid slider index");
                    return 0;
                }
            };
        }
        1
    }
    /// This method is exposed as "make_probe(label:String)->(float)->float".
    pub fn make_probe(&mut self, vm: &mut Machine) -> ReturnCode {
        if let Some(app) = self.window.as_mut() {
            let idx = vm.get_stack(0);
            let probename = vm.prog.strings[idx as usize].as_str();

            let (mut prod, cons) = HeapRb::<f64>::new(4096).split();
            app.add_plot(probename, cons);
            let cb = move |vm: &mut Machine| -> ReturnCode {
                let v = Machine::get_as::<f64>(vm.get_stack(0));
                let _ = prod.try_push(v);
                //do not modify any stack values
                1
            };
            let info = ExtClsInfo::new(
                "probegetter".to_symbol(),
                Self::get_closure_type(),
                Rc::new(RefCell::new(cb)),
            );
            let cls = vm.wrap_extern_cls(info);
            vm.set_stack(0, Machine::to_value(cls));
        } else {
            log::warn!("make_probe called other than global context.");
        }
        1
    }
}
impl SystemPlugin for GuiToolPlugin {
    fn try_get_main_loop(&mut self) -> Option<Box<dyn FnOnce()>> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let make_window = self.window.as_ref().is_some_and(|w| !w.is_empty());
            make_window
                .then(|| {
                    self.window.take().map(|window| -> Box<dyn FnOnce()> {
                        Box::new(move || {
                            let native_options = eframe::NativeOptions {
                                viewport: egui::ViewportBuilder::default()
                                    .with_inner_size([400.0, 300.0])
                                    .with_min_inner_size([300.0, 220.0]), // .with_icon(
                                //     // NOTE: Adding an icon is optional
                                //     eframe::icon_data::from_png_bytes(&include_bytes!("../assets/icon-256.png")[..])
                                //         .expect("Failed to load icon"),)
                                ..Default::default()
                            };
                            let _ = eframe::run_native(
                                "mimium guitools",
                                native_options,
                                Box::new(|_cc| Ok(Box::new(window))),
                            )
                            .inspect_err(|e| log::error!("{e}"));
                        })
                    })
                })
                .flatten()
        }

        #[cfg(target_arch = "wasm32")]
        None
    }
    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        let ty = function!(vec![string_t!()], Self::get_closure_type());
        let probef: SystemPluginFnType<Self> = Self::make_probe;
        let make_probe = SysPluginSignature::new("make_probe", probef, ty);
        let sliderf: SystemPluginMacroType<Self> = Self::make_slider;
        let make_slider = SysPluginSignature::new_macro(
            "Slider",
            sliderf,
            function!(
                vec![string_t!(), numeric!(), numeric!()],
                Type::Code(Type::Primitive(PType::Numeric).into_id()).into_id()
            ),
        );
        let getsliderf: SystemPluginFnType<Self> = Self::get_slider;
        let get_slider = SysPluginSignature::new(
            Self::GET_SLIDER,
            getsliderf,
            function!(vec![numeric!()], numeric!()),
        );
        vec![make_probe, make_slider, get_slider]
    }
}
