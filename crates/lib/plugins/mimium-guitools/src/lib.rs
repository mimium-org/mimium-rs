use std::sync::{Arc, Mutex};

use egui::ahash::HashMap;
use mimium_lang::{
    ast::{Expr, Literal},
    code, function,
    interner::{ToSymbol, TypeNodeId},
    interpreter::Value,
    log, numeric,
    pattern::TypedId,
    plugin::{SysPluginSignature, SystemPlugin, SystemPluginFnType, SystemPluginMacroType},
    string_t,
    types::{PType, Type},
};
use mimium_plugin_macros::{mimium_export_plugin, mimium_plugin_fn};
use plot_window::PlotApp;
use ringbuf::{
    HeapProd, HeapRb,
    traits::{Producer, Split},
};

use crate::plot_window::FloatParameter;
pub(crate) mod plot_ui;
pub mod plot_window;

/// Lock-free audio handle extracted from GuiToolPlugin after compilation.
///
/// This struct owns the producer ends of probe ring buffers and slider
/// parameter handles. It is moved to the audio thread where all access
/// is lock-free (AtomicF64 reads for sliders, SPSC push for probes).
pub struct GuiAudioHandle {
    sliders: Box<[Arc<FloatParameter>]>,
    probes: Box<[HeapProd<f64>]>,
}

unsafe impl Send for GuiAudioHandle {}

impl GuiAudioHandle {
    /// Read the current slider value (lock-free AtomicF64 load).
    pub fn get_slider(&self, idx: usize) -> f64 {
        self.sliders.get(idx).map(|s| s.get()).unwrap_or(0.0)
    }

    /// Push a probe sample into the ring buffer (lock-free SPSC push)
    /// and return the value unchanged (passthrough).
    pub fn probe_intercept(&mut self, value: f64, idx: usize) -> f64 {
        if let Some(prod) = self.probes.get_mut(idx) {
            let _ = prod.try_push(value);
        }
        value
    }
}

pub struct GuiToolPlugin {
    window: Arc<Mutex<PlotApp>>,
    slider_instances: Vec<Arc<FloatParameter>>,
    slider_namemap: HashMap<String, usize>,
    probe_instances: Vec<HeapProd<f64>>,
    probe_namemap: HashMap<String, usize>,
}

impl Default for GuiToolPlugin {
    fn default() -> Self {
        Self {
            window: Arc::new(Mutex::new(PlotApp::default())),
            slider_instances: Vec::new(),
            slider_namemap: HashMap::default(),
            probe_instances: Vec::new(),
            probe_namemap: HashMap::default(),
        }
    }
}

impl GuiToolPlugin {
    fn get_closure_type() -> TypeNodeId {
        function!(vec![numeric!()], numeric!())
    }
    pub const GET_SLIDER: &'static str = "__get_slider";
    pub const PROBE_INTERCEPT: &'static str = "__probe_intercept";

    pub fn make_slider(&mut self, v: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(v.len(), 4);
        let (name, init, min, max, mut window) = match (
            v[0].0.clone(),
            v[1].0.clone(),
            v[2].0.clone(),
            v[3].0.clone(),
            self.window.lock(),
        ) {
            (
                Value::String(name),
                Value::Number(init),
                Value::Number(min),
                Value::Number(max),
                Ok(window),
            ) => (name, init, min, max, window),
            _ => {
                log::error!("invalid argument");
                return Value::Number(0.0);
            }
        };
        let idx = if let Some(idx) = self.slider_namemap.get(name.as_str()).cloned() {
            let p = self.slider_instances.get_mut(idx).unwrap();
            p.set_range(min, max);
            idx
        } else {
            let (p, idx) = window.add_slider(name.as_str(), init, min, max);
            self.slider_instances.push(p);
            self.slider_namemap.insert(name.to_string(), idx);
            idx
        };
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
        let (name, mut window) = match (v[0].0.clone(), self.window.lock()) {
            (Value::String(name), Ok(window)) => (name, window),
            _ => {
                log::error!("invalid argument for Probe macro type {}", v[0].1);
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
        let probeid = self
            .probe_namemap
            .get(name.as_str())
            .cloned()
            .unwrap_or_else(|| {
                let (prod, cons) = HeapRb::<f64>::new(4096).split();
                window.add_plot(name.as_str(), cons);
                let idx = self.probe_instances.len();
                self.probe_instances.push(prod);
                self.probe_namemap.insert(name.to_string(), idx);
                log::info!("Created Probe '{}' with index {}", name, idx);
                idx
            });

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
                        Expr::Literal(Literal::Float(probeid.to_string().to_symbol()))
                            .into_id_without_span(),
                    ],
                )
                .into_id_without_span(),
            )
            .into_id_without_span(),
        )
    }
    #[mimium_plugin_fn]
    pub fn get_slider(&mut self, slider_idx: f64) -> f64 {
        let idx = slider_idx as usize;
        self.slider_instances
            .get(idx)
            .map(|s| s.get())
            .unwrap_or_else(|| {
                log::error!("invalid slider index: {idx}");
                0.0
            })
    }

    #[mimium_plugin_fn]
    pub fn probe_intercept(&mut self, value: f64, probe_idx: f64) -> f64 {
        let idx = probe_idx as usize;
        match self.probe_instances.get_mut(idx) {
            Some(prod) => {
                let _ = prod.try_push(value);
                log::trace!("Probe {} pushed value: {}", idx, value);
            }
            None => {
                log::error!("invalid probe index: {idx}");
            }
        }
        value // passthrough
    }

    /// Freeze the setup-phase data into a lock-free `GuiAudioHandle`.
    ///
    /// After calling this, slider/probe instances are drained from this
    /// plugin and moved into the returned handle. The handle should be
    /// sent to the audio thread.
    pub fn freeze(&mut self) -> GuiAudioHandle {
        GuiAudioHandle {
            sliders: self
                .slider_instances
                .drain(..)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            probes: self
                .probe_instances
                .drain(..)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl mimium_lang::runtime::wasm::WasmPluginCallable for GuiAudioHandle {
    fn call_method(&mut self, method: &str, args: &[f64]) -> Option<f64> {
        log::debug!(
            "GuiAudioHandle::call_method: {} with args: {:?}",
            method,
            args
        );
        match method {
            "__get_slider" => {
                if !args.is_empty() {
                    Some(self.get_slider(args[0] as usize))
                } else {
                    None
                }
            }
            "__probe_intercept" => {
                if args.len() >= 2 {
                    Some(self.probe_intercept(args[0], args[1] as usize))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl SystemPlugin for GuiToolPlugin {
    fn try_get_main_loop(&mut self) -> Option<Box<dyn FnOnce()>> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            use crate::plot_window::AsyncPlotApp;
            let app = Box::new(AsyncPlotApp {
                window: self.window.clone(),
            });
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
                let _ =
                    eframe::run_native("mimium guitools", native_options, Box::new(|_cc| Ok(app)))
                        .inspect_err(|e| log::error!("{e}"));
            }))
        }
        #[cfg(target_arch = "wasm32")]
        None
    }

    fn freeze_audio_handle(&mut self) -> Option<Box<dyn std::any::Any + Send>> {
        // Only produce a handle if there are any sliders or probes registered
        if self.slider_instances.is_empty() && self.probe_instances.is_empty() {
            return None;
        }
        Some(Box::new(self.freeze()))
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

        // Runtime functions (Stage 1) — accessed on the audio thread.
        // When registered here, they share the same plugin instance as the
        // macros above (via DynSystemPlugin's UnsafeCell), ensuring the
        // slider/probe data populated by macros is visible to these functions.
        let get_sliderf: SystemPluginFnType<Self> = Self::get_slider;
        let get_slider = SysPluginSignature::new(
            Self::GET_SLIDER,
            get_sliderf,
            function!(vec![numeric!()], numeric!()),
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

// -------------------------------------------------------------------------
// Signature helpers for FFI type info export
// -------------------------------------------------------------------------

impl GuiToolPlugin {
    /// Returns the signature for the `Slider!` macro.
    pub fn slider_signature() -> SysPluginSignature {
        let sliderf: SystemPluginMacroType<Self> = Self::make_slider;
        SysPluginSignature::new_macro(
            "Slider",
            sliderf,
            function!(
                vec![string_t!(), numeric!(), numeric!(), numeric!()],
                code!(numeric!())
            ),
        )
    }

    /// Returns the signature for the `Probe!` macro.
    pub fn probe_signature() -> SysPluginSignature {
        let probe_macrof: SystemPluginMacroType<Self> = Self::make_probe_macro;
        SysPluginSignature::new_macro(
            "Probe",
            probe_macrof,
            function!(
                vec![string_t!()],
                Type::Code(function!(vec![numeric!()], numeric!())).into_id()
            ),
        )
    }
}

// -------------------------------------------------------------------------
// Dynamic Plugin ABI (generated by mimium_export_plugin!)
// -------------------------------------------------------------------------

mimium_export_plugin! {
    plugin_type: GuiToolPlugin,
    plugin_name: "mimium-guitools",
    plugin_author: "mimium-org",
    capabilities: {
        has_audio_worker: false,
        has_macros: true,
        has_runtime_functions: true,
    },
    runtime_functions: [
        ("__get_slider", get_slider),
        ("__probe_intercept", probe_intercept),
    ],
    macro_functions: [
        ("Slider", make_slider),
        ("Probe", make_probe_macro),
    ],
    type_infos: [
        { name: "Slider", sig: GuiToolPlugin::slider_signature(), stage: 0 },
        { name: "Probe", sig: GuiToolPlugin::probe_signature(), stage: 0 },
        { name: "__get_slider", ty_expr: function!(vec![numeric!()], numeric!()), stage: 1 },
        { name: "__probe_intercept", ty_expr: function!(vec![numeric!(), numeric!()], numeric!()), stage: 1 },
    ],
}
