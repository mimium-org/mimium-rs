use std::{cell::RefCell, rc::Rc};

use mimium_lang::{
    function,
    interner::{ToSymbol, TypeNodeId},
    log, numeric,
    plugin::{ExtClsInfo, SysPluginSignature, SystemPlugin, SystemPluginFnType},
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
        let fptr: SystemPluginFnType<Self> = Self::make_probe;
        let make_probe = SysPluginSignature::new("make_probe", fptr, ty);
        vec![make_probe]
    }
}
