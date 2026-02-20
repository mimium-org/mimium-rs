use std::sync::{Arc, Mutex};

use egui::ahash::HashMap;
use mimium_lang::{
    ast::{Expr, Literal, RecordField},
    code, function,
    interner::{ToSymbol, TypeNodeId},
    interpreter::Value,
    log, numeric,
    pattern::TypedId,
    plugin::{SysPluginSignature, SystemPlugin, SystemPluginFnType, SystemPluginMacroType},
    string_t,
    types::{PType, Type, TypeSchemeId},
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

/// Generate `probe_intercept_arityN` methods (with `#[mimium_plugin_fn]`)
/// and a dispatch helper for `GuiToolPlugin`.
///
/// Each generated method calls `probe_intercept_multi` with all value
/// arguments, then returns them as a tuple.  The dispatch helper
/// `probe_intercept_fn_for_arity` maps a runtime arity number to the
/// corresponding method's function pointer.
///
/// Return types must be listed explicitly because proc-macro attributes
/// receive raw tokens and cannot see through inner declarative-macro
/// invocations.
macro_rules! define_probe_intercept_arities {
    ($(
        $name:ident, $arity:expr, ($($arg:ident),+) -> ($($ret:tt),+)
    );+ $(;)?) => {
        $(
            #[mimium_plugin_fn]
            pub fn $name(&mut self, $($arg: f64,)+ probe_idx: f64) -> ($($ret),+) {
                self.probe_intercept_multi(probe_idx, &[$($arg),+]);
                ($($arg),+)
            }
        )+

        /// Look up the runtime function pointer for a given tuple arity.
        fn probe_intercept_fn_for_arity(arity: usize) -> SystemPluginFnType<Self> {
            match arity {
                $($arity => Self::$name,)+
                _ => Self::probe_intercept,
            }
        }
    };
}

/// Generate plain `probe_intercept_arityN` methods for `GuiAudioHandle`.
///
/// These are lightweight versions without `#[mimium_plugin_fn]` wrapping.
/// Each method passes values through `probe_intercept_multi` and returns
/// the first value (passthrough for the host trampoline).
macro_rules! define_probe_intercept_arity_plain {
    ($( $name:ident($($arg:ident),+) ),+ $(,)?) => {
        $(
            pub fn $name(&mut self, $($arg: f64,)+ probe_idx: f64) -> f64 {
                self.probe_intercept_multi(probe_idx, &[$($arg),+])
            }
        )+
    };
}

/// Lock-free audio handle extracted from GuiToolPlugin after compilation.
///
/// This struct owns the producer ends of probe ring buffers and slider
/// parameter handles. It is moved to the audio thread where all access
/// is lock-free (AtomicF64 reads for sliders, SPSC push for probes).
pub struct GuiAudioHandle {
    sliders: Box<[Arc<FloatParameter>]>,
    probes: Box<[HeapProd<f64>]>,
    probe_groups: HashMap<usize, Vec<usize>>,
}

unsafe impl Send for GuiAudioHandle {}

impl GuiAudioHandle {
    const PROBE_VALUE_MAX_ARITY: usize = 16;

    fn resolve_probe_index(&self, base_idx: usize, channel: usize) -> usize {
        self.probe_groups
            .get(&base_idx)
            .and_then(|group| group.get(channel).copied())
            .unwrap_or(base_idx + channel)
    }

    fn push_probe_value(&mut self, base_idx: usize, channel: usize, value: f64) {
        let idx = self.resolve_probe_index(base_idx, channel);
        if let Some(prod) = self.probes.get_mut(idx) {
            let _ = prod.try_push(value);
        }
    }

    fn probe_intercept_multi(&mut self, probe_idx: f64, values: &[f64]) -> f64 {
        let base_idx = probe_idx as usize;
        values
            .iter()
            .enumerate()
            .for_each(|(channel, value)| self.push_probe_value(base_idx, channel, *value));
        values.first().copied().unwrap_or(0.0)
    }

    /// Read the current slider value (lock-free AtomicF64 load).
    ///
    /// All arguments use `f64` to match the WASM trampoline calling
    /// convention where every value is passed as `f64`.
    pub fn get_slider(&self, slider_idx: f64) -> f64 {
        self.sliders
            .get(slider_idx as usize)
            .map(|s| s.get())
            .unwrap_or(0.0)
    }

    /// Push a probe sample into the ring buffer (lock-free SPSC push)
    /// and return the value unchanged (passthrough).
    ///
    /// All arguments use `f64` to match the WASM trampoline calling
    /// convention where every value is passed as `f64`.
    pub fn probe_intercept(&mut self, value: f64, probe_idx: f64) -> f64 {
        self.probe_intercept_multi(probe_idx, &[value])
    }

    /// Probe interception for 2-word values (e.g. `(float, float)` tuples).
    ///
    /// Returns the first value so the host trampoline can preserve passthrough
    /// for remaining words.
    define_probe_intercept_arity_plain!(
        probe_intercept_arity2(v0, v1),
        probe_intercept_arity3(v0, v1, v2),
        probe_intercept_arity4(v0, v1, v2, v3),
        probe_intercept_arity5(v0, v1, v2, v3, v4),
        probe_intercept_arity6(v0, v1, v2, v3, v4, v5),
        probe_intercept_arity7(v0, v1, v2, v3, v4, v5, v6),
        probe_intercept_arity8(v0, v1, v2, v3, v4, v5, v6, v7),
        probe_intercept_arity9(v0, v1, v2, v3, v4, v5, v6, v7, v8),
        probe_intercept_arity10(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9),
        probe_intercept_arity11(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10),
        probe_intercept_arity12(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11),
        probe_intercept_arity13(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12),
        probe_intercept_arity14(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13),
        probe_intercept_arity15(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14),
        probe_intercept_arity16(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15),
    );
}

pub struct GuiToolPlugin {
    window: Arc<Mutex<PlotApp>>,
    slider_instances: Vec<Arc<FloatParameter>>,
    slider_namemap: HashMap<String, usize>,
    probe_instances: Vec<HeapProd<f64>>,
    probe_namemap: HashMap<String, usize>,
    probe_groups: HashMap<usize, Vec<usize>>,
    enable_mainloop: bool,
    /// Cached WASM plugin function map, reused across hot-swaps.
    #[cfg(not(target_arch = "wasm32"))]
    wasm_plugin_fns: Option<mimium_lang::runtime::wasm::WasmPluginFnMap>,
}

impl Default for GuiToolPlugin {
    fn default() -> Self {
        Self {
            window: Arc::new(Mutex::new(PlotApp::default())),
            slider_instances: Vec::new(),
            slider_namemap: HashMap::default(),
            probe_instances: Vec::new(),
            probe_namemap: HashMap::default(),
            probe_groups: HashMap::default(),
            enable_mainloop: true,
            #[cfg(not(target_arch = "wasm32"))]
            wasm_plugin_fns: None,
        }
    }
}

impl GuiToolPlugin {
    pub fn headless() -> Self {
        Self {
            enable_mainloop: false,
            ..Self::default()
        }
    }

    fn get_closure_type() -> TypeNodeId {
        function!(vec![numeric!()], numeric!())
    }
    pub const GET_SLIDER: &'static str = "__get_slider";
    pub const PROBE_INTERCEPT: &'static str = "__probe_intercept";
    pub const PROBE_INTERCEPT_ARITY2: &'static str = "__probe_intercept$arity2";
    pub const PROBE_VALUE_INTERCEPT: &'static str = "__probe_value_intercept";
    pub const PROBE_VALUE_INTERCEPT_ARITY1: &'static str = "__probe_value_intercept$arity1";
    pub const PROBE_VALUE_INTERCEPT_ARITY2: &'static str = "__probe_value_intercept$arity2";

    fn probe_value_intercept_arity_name(arity: usize) -> String {
        format!("__probe_value_intercept$arity{arity}")
    }

    fn collect_probe_labels_rec(value: &Value, current_path: &str, out: &mut Vec<String>) {
        match value {
            Value::Record(fields) => fields.iter().for_each(|(name, field_value)| {
                let child_path = Self::join_slider_path(current_path, name.as_str());
                Self::collect_probe_labels_rec(field_value, &child_path, out);
            }),
            Value::Tuple(elements) => elements.iter().enumerate().for_each(|(index, element)| {
                let child_path = Self::join_slider_path(current_path, &index.to_string());
                Self::collect_probe_labels_rec(element, &child_path, out);
            }),
            _ => out.push(current_path.to_string()),
        }
    }

    fn ensure_probe_group_with_labels(&mut self, labels: &[String]) -> usize {
        let indices = labels
            .iter()
            .map(|label| self.ensure_probe_id(label).unwrap_or(0))
            .collect::<Vec<_>>();
        let base = indices.first().copied().unwrap_or(0);
        self.probe_groups.insert(base, indices);
        base
    }

    fn ensure_probe_group_default(&mut self, root_name: &str) -> usize {
        let labels = (0..GuiAudioHandle::PROBE_VALUE_MAX_ARITY)
            .map(|i| format!("{root_name}.{i}"))
            .collect::<Vec<_>>();
        self.ensure_probe_group_with_labels(&labels)
    }

    fn resolve_probe_index(&self, base_idx: usize, channel: usize) -> usize {
        self.probe_groups
            .get(&base_idx)
            .and_then(|group| group.get(channel).copied())
            .unwrap_or(base_idx + channel)
    }

    fn push_probe_value(&mut self, base_idx: usize, channel: usize, value: f64) {
        let idx = self.resolve_probe_index(base_idx, channel);
        match self.probe_instances.get_mut(idx) {
            Some(prod) => {
                let _ = prod.try_push(value);
                log::trace!("Probe {idx} pushed value: {value}");
            }
            None => {
                log::error!("invalid probe index: {idx}");
            }
        }
    }

    fn probe_intercept_multi(&mut self, probe_idx: f64, values: &[f64]) -> f64 {
        let base_idx = probe_idx as usize;
        values
            .iter()
            .enumerate()
            .for_each(|(channel, value)| self.push_probe_value(base_idx, channel, *value));
        values.first().copied().unwrap_or(0.0)
    }

    fn ensure_probe_id(&mut self, name: &str) -> Option<usize> {
        if let Some(&existing_idx) = self.probe_namemap.get(name) {
            return Some(existing_idx);
        }

        let Ok(mut window) = self.window.lock() else {
            log::error!("failed to lock GUI window while creating probe '{name}'");
            return None;
        };

        let (prod, cons) = HeapRb::<f64>::new(4096).split();
        window.add_plot(name, cons);
        let idx = self.probe_instances.len();
        self.probe_instances.push(prod);
        self.probe_namemap.insert(name.to_string(), idx);
        log::debug!("Created Probe '{name}' with index {idx}");
        Some(idx)
    }

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
        let idx = if let Some(&existing_idx) = self.slider_namemap.get(name.as_str()) {
            // Slider with this name already exists, reuse the index
            if let Some(p) = self.slider_instances.get_mut(existing_idx) {
                // Instance still available, update range
                p.set_range(min, max);
            }
            // Return existing index whether instance is available or not
            // (if drained, the frozen handle will still have it)
            existing_idx
        } else {
            // New slider - add to window and register
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

    fn build_slider_code_from_name(&mut self, name: &str, init: f64, min: f64, max: f64) -> Value {
        let idx = if let Some(&existing_idx) = self.slider_namemap.get(name) {
            if let Some(parameter) = self.slider_instances.get_mut(existing_idx) {
                parameter.set_range(min, max);
            }
            existing_idx
        } else {
            let Ok(mut window) = self.window.lock() else {
                log::error!("failed to lock GUI window while creating slider '{name}'");
                return Value::Number(init);
            };
            let (parameter, idx) = window.add_slider(name, init, min, max);
            self.slider_instances.push(parameter);
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

    fn default_slider_range(init: f64) -> (f64, f64) {
        let span = init.abs().max(1.0);
        (init - span, init + span)
    }

    fn join_slider_path(base_path: &str, key: &str) -> String {
        if base_path.is_empty() {
            key.to_string()
        } else {
            format!("{base_path}.{key}")
        }
    }

    fn make_slider_generic_rec(&mut self, value: &Value, current_path: &str) -> Value {
        match value {
            Value::Number(init) => {
                let label = current_path.to_string();
                let (min, max) = Self::default_slider_range(*init);
                self.build_slider_code_from_name(&label, *init, min, max)
            }
            Value::Record(fields) => {
                let record_fields = fields
                    .iter()
                    .filter_map(|(name, field_value)| {
                        let child_path = Self::join_slider_path(current_path, name.as_str());
                        let child_value = self.make_slider_generic_rec(field_value, &child_path);
                        let child_expr = match child_value {
                            Value::Code(expr) => Some(expr),
                            other => other.try_into().ok(),
                        };

                        child_expr.map(|expr| RecordField { name: *name, expr })
                    })
                    .collect::<Vec<_>>();

                Value::Code(Expr::RecordLiteral(record_fields).into_id_without_span())
            }
            Value::Tuple(elements) => {
                let tuple_elements = elements
                    .iter()
                    .enumerate()
                    .filter_map(|(index, element)| {
                        let field_name = format!("{index}");
                        let child_path = Self::join_slider_path(current_path, &field_name);
                        let child_value = self.make_slider_generic_rec(element, &child_path);
                        match child_value {
                            Value::Code(expr) => Some(expr),
                            other => other.try_into().ok(),
                        }
                    })
                    .collect::<Vec<_>>();
                Value::Code(Expr::Tuple(tuple_elements).into_id_without_span())
            }
            other => match other.clone().try_into() {
                Ok(expr) => Value::Code(expr),
                Err(_) => {
                    log::warn!("SliderValue does not support value kind: {other:?}");
                    Value::Code(Expr::Block(None).into_id_without_span())
                }
            },
        }
    }

    pub fn make_slider_generic(&mut self, values: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(values.len(), 2);
        let root_name = match &values[0].0 {
            Value::String(name) => name.as_str().to_string(),
            other => {
                log::error!("SliderValue first argument must be String, got: {other:?}");
                return Value::Code(Expr::Block(None).into_id_without_span());
            }
        };
        let value = &values[1].0;
        self.make_slider_generic_rec(value, &root_name)
    }

    pub fn make_probe_macro(&mut self, v: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(v.len(), 1);
        let name = match v[0].0.clone() {
            Value::String(name) => name,
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
        let probeid = self.ensure_probe_id(name.as_str()).unwrap_or(0);

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

    pub fn make_probe_generic(&mut self, values: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(values.len(), 1);
        let root_name = match &values[0].0 {
            Value::String(name) => name.as_str(),
            other => {
                log::error!("ProbeValue first argument must be String, got: {other:?}");
                return Value::Code(Expr::Block(None).into_id_without_span());
            }
        };

        let probeid = self.ensure_probe_group_default(root_name);
        let t = Type::Unknown.into_id();
        Value::Code(
            Expr::Lambda(
                vec![TypedId::new("x".to_symbol(), t)],
                None,
                Expr::Apply(
                    Expr::Var(Self::PROBE_VALUE_INTERCEPT.to_symbol()).into_id_without_span(),
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

    pub fn make_probe_generic_with_shape(&mut self, values: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(values.len(), 2);
        let root_name = match &values[0].0 {
            Value::String(name) => name.as_str(),
            other => {
                log::error!("ProbeValueWith first argument must be String, got: {other:?}");
                return Value::Code(Expr::Block(None).into_id_without_span());
            }
        };

        let mut labels = Vec::new();
        Self::collect_probe_labels_rec(&values[1].0, root_name, &mut labels);
        if labels.is_empty() {
            labels.push(format!("{root_name}.0"));
        }
        let probeid = self.ensure_probe_group_with_labels(&labels);
        let t = Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id();
        Value::Code(
            Expr::Lambda(
                vec![TypedId::new("x".to_symbol(), t)],
                None,
                Expr::Apply(
                    Expr::Var(Self::PROBE_VALUE_INTERCEPT.to_symbol()).into_id_without_span(),
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
        self.probe_intercept_multi(probe_idx, &[value])
    }

    define_probe_intercept_arities! {
        probe_intercept_arity2,   2, (v0, v1) -> (f64, f64);
        probe_intercept_arity3,   3, (v0, v1, v2) -> (f64, f64, f64);
        probe_intercept_arity4,   4, (v0, v1, v2, v3) -> (f64, f64, f64, f64);
        probe_intercept_arity5,   5, (v0, v1, v2, v3, v4) -> (f64, f64, f64, f64, f64);
        probe_intercept_arity6,   6, (v0, v1, v2, v3, v4, v5) -> (f64, f64, f64, f64, f64, f64);
        probe_intercept_arity7,   7, (v0, v1, v2, v3, v4, v5, v6) -> (f64, f64, f64, f64, f64, f64, f64);
        probe_intercept_arity8,   8, (v0, v1, v2, v3, v4, v5, v6, v7) -> (f64, f64, f64, f64, f64, f64, f64, f64);
        probe_intercept_arity9,   9, (v0, v1, v2, v3, v4, v5, v6, v7, v8) -> (f64, f64, f64, f64, f64, f64, f64, f64, f64);
        probe_intercept_arity10, 10, (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9) -> (f64, f64, f64, f64, f64, f64, f64, f64, f64, f64);
        probe_intercept_arity11, 11, (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) -> (f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64);
        probe_intercept_arity12, 12, (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) -> (f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64);
        probe_intercept_arity13, 13, (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) -> (f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64);
        probe_intercept_arity14, 14, (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) -> (f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64);
        probe_intercept_arity15, 15, (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) -> (f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64);
        probe_intercept_arity16, 16, (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) -> (f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64, f64);
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
            probe_groups: std::mem::take(&mut self.probe_groups),
        }
    }
}

// `into_wasm_plugin_fn_map` is now auto-generated by `mimium_export_plugin!`
// via the `wasm_audio_handle` section.

impl SystemPlugin for GuiToolPlugin {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn try_get_main_loop(&mut self) -> Option<Box<dyn FnOnce()>> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            if !self.enable_mainloop {
                return None;
            }
            use crate::plot_window::AsyncPlotApp;
            let initial_size = self
                .window
                .lock()
                .map(|window| window.suggested_viewport_size())
                .unwrap_or([400.0, 300.0]);
            let app = Box::new(AsyncPlotApp {
                window: self.window.clone(),
            });
            Some(Box::new(move || {
                let native_options = eframe::NativeOptions {
                    viewport: egui::ViewportBuilder::default()
                        .with_inner_size(initial_size)
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

    #[cfg(not(target_arch = "wasm32"))]
    fn freeze_for_wasm(&mut self) -> Option<mimium_lang::runtime::wasm::WasmPluginFnMap> {
        // Return cached map if already created (for hot-swap reuse)
        if let Some(ref cached) = self.wasm_plugin_fns {
            return Some(cached.clone());
        }

        if self.slider_instances.is_empty() && self.probe_instances.is_empty() {
            return None;
        }

        let map = self.freeze().into_wasm_plugin_fn_map();
        self.wasm_plugin_fns = Some(map.clone());
        Some(map)
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

        let slider_genericf: SystemPluginMacroType<Self> = Self::make_slider_generic;
        let make_slider_generic = SysPluginSignature::new_macro(
            "SliderValue",
            slider_genericf,
            function!(
                vec![string_t!(), Type::TypeScheme(TypeSchemeId(10)).into_id()],
                code!(Type::TypeScheme(TypeSchemeId(10)).into_id())
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

        let probe_genericf: SystemPluginMacroType<Self> = Self::make_probe_generic;
        let probe_value_elem_ty = Type::TypeScheme(TypeSchemeId(9998)).into_id();
        let probe_value_fn_ty = Type::Function {
            arg: probe_value_elem_ty,
            ret: probe_value_elem_ty,
        }
        .into_id();
        let probe_generic = SysPluginSignature::new_macro(
            "ProbeValue",
            probe_genericf,
            function!(vec![string_t!()], Type::Code(probe_value_fn_ty).into_id()),
        );

        let probe_generic_with_shapef: SystemPluginMacroType<Self> =
            Self::make_probe_generic_with_shape;
        let probe_schema_ty = Type::TypeScheme(TypeSchemeId(9999)).into_id();
        let probe_with_shape_fn_ty = Type::Function {
            arg: probe_schema_ty,
            ret: probe_schema_ty,
        }
        .into_id();
        let probe_generic_with_shape = SysPluginSignature::new_macro(
            "ProbeValueWith",
            probe_generic_with_shapef,
            function!(
                vec![string_t!(), probe_schema_ty],
                Type::Code(probe_with_shape_fn_ty).into_id()
            ),
        );

        // Runtime functions (Stage 1) — accessed on the audio thread.
        // When registered here, they share the same plugin instance as the
        // macros above (via DynSystemPlugin's RefCell), ensuring the
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

        let probe_intercept_arity2f: SystemPluginFnType<Self> = Self::probe_intercept_arity2;
        let probe_intercept_arity2 = SysPluginSignature::new(
            Self::PROBE_INTERCEPT_ARITY2,
            probe_intercept_arity2f,
            function!(
                vec![numeric!(), numeric!(), numeric!()],
                Type::Tuple(vec![numeric!(), numeric!()]).into_id()
            ),
        );

        let probe_value_interceptf: SystemPluginFnType<Self> = Self::probe_intercept;
        let probe_value_intercept = SysPluginSignature::new(
            Self::PROBE_VALUE_INTERCEPT,
            probe_value_interceptf,
            function!(
                vec![
                    Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id(),
                    numeric!()
                ],
                Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id()
            ),
        );

        let probe_value_intercept_arity1f: SystemPluginFnType<Self> = Self::probe_intercept;
        let probe_value_intercept_arity1 = SysPluginSignature::new(
            Self::PROBE_VALUE_INTERCEPT_ARITY1,
            probe_value_intercept_arity1f,
            function!(
                vec![
                    Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id(),
                    numeric!()
                ],
                Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id()
            ),
        );

        let mut interfaces = vec![
            probe_macro,
            probe_generic,
            probe_generic_with_shape,
            make_slider,
            make_slider_generic,
            get_slider,
            probe_intercept,
            probe_intercept_arity2,
            probe_value_intercept,
            probe_value_intercept_arity1,
        ];

        for arity in 2..=GuiAudioHandle::PROBE_VALUE_MAX_ARITY {
            let fn_ptr = Self::probe_intercept_fn_for_arity(arity);

            let arg_types = std::iter::repeat_n(numeric!(), arity + 1).collect::<Vec<_>>();
            let ret_ty = Type::Tuple(std::iter::repeat_n(numeric!(), arity).collect()).into_id();
            let ty = Type::Function {
                arg: Type::Tuple(arg_types).into_id(),
                ret: ret_ty,
            }
            .into_id();
            let leaked_name: &'static str =
                Box::leak(Self::probe_value_intercept_arity_name(arity).into_boxed_str());
            interfaces.push(SysPluginSignature::new(leaked_name, fn_ptr, ty));
        }

        interfaces
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

    /// Returns the signature for the generic `SliderValue!` macro.
    pub fn slider_generic_signature() -> SysPluginSignature {
        let sliderf: SystemPluginMacroType<Self> = Self::make_slider_generic;
        let generic_type = Type::TypeScheme(TypeSchemeId(10)).into_id();
        SysPluginSignature::new_macro(
            "SliderValue",
            sliderf,
            function!(vec![string_t!(), generic_type], code!(generic_type)),
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

    /// Returns the signature for the generic `ProbeValue!` macro.
    pub fn probe_generic_signature() -> SysPluginSignature {
        let probe_macrof: SystemPluginMacroType<Self> = Self::make_probe_generic;
        let probe_value_elem_ty = Type::TypeScheme(TypeSchemeId(9998)).into_id();
        let probe_value_fn_ty = Type::Function {
            arg: probe_value_elem_ty,
            ret: probe_value_elem_ty,
        }
        .into_id();
        SysPluginSignature::new_macro(
            "ProbeValue",
            probe_macrof,
            function!(vec![string_t!()], Type::Code(probe_value_fn_ty).into_id()),
        )
    }

    /// Returns the signature for the schema-aware `ProbeValueWith!` macro.
    pub fn probe_generic_with_shape_signature() -> SysPluginSignature {
        let probe_macrof: SystemPluginMacroType<Self> = Self::make_probe_generic_with_shape;
        let probe_schema_ty = Type::TypeScheme(TypeSchemeId(9999)).into_id();
        let probe_value_fn_ty = Type::Function {
            arg: probe_schema_ty,
            ret: probe_schema_ty,
        }
        .into_id();
        SysPluginSignature::new_macro(
            "ProbeValueWith",
            probe_macrof,
            function!(
                vec![string_t!(), probe_schema_ty],
                Type::Code(probe_value_fn_ty).into_id()
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
        ("__probe_intercept$arity2", probe_intercept_arity2),
        ("__probe_value_intercept", probe_intercept),
        ("__probe_value_intercept$arity1", probe_intercept),
        ("__probe_value_intercept$arity2", probe_intercept_arity2),
        ("__probe_value_intercept$arity3", probe_intercept_arity3),
        ("__probe_value_intercept$arity4", probe_intercept_arity4),
        ("__probe_value_intercept$arity5", probe_intercept_arity5),
        ("__probe_value_intercept$arity6", probe_intercept_arity6),
        ("__probe_value_intercept$arity7", probe_intercept_arity7),
        ("__probe_value_intercept$arity8", probe_intercept_arity8),
        ("__probe_value_intercept$arity9", probe_intercept_arity9),
        ("__probe_value_intercept$arity10", probe_intercept_arity10),
        ("__probe_value_intercept$arity11", probe_intercept_arity11),
        ("__probe_value_intercept$arity12", probe_intercept_arity12),
        ("__probe_value_intercept$arity13", probe_intercept_arity13),
        ("__probe_value_intercept$arity14", probe_intercept_arity14),
        ("__probe_value_intercept$arity15", probe_intercept_arity15),
        ("__probe_value_intercept$arity16", probe_intercept_arity16),
    ],
    macro_functions: [
        ("Slider", make_slider),
        ("SliderValue", make_slider_generic),
        ("Probe", make_probe_macro),
        ("ProbeValue", make_probe_generic),
        ("ProbeValueWith", make_probe_generic_with_shape),
    ],
    type_infos: [
        { name: "Slider", sig: GuiToolPlugin::slider_signature(), stage: 0 },
        { name: "SliderValue", sig: GuiToolPlugin::slider_generic_signature(), stage: 0 },
        { name: "Probe", sig: GuiToolPlugin::probe_signature(), stage: 0 },
        { name: "ProbeValue", sig: GuiToolPlugin::probe_generic_signature(), stage: 0 },
        { name: "ProbeValueWith", sig: GuiToolPlugin::probe_generic_with_shape_signature(), stage: 0 },
        { name: "__get_slider", ty_expr: function!(vec![numeric!()], numeric!()), stage: 1 },
        { name: "__probe_intercept", ty_expr: function!(vec![numeric!(), numeric!()], numeric!()), stage: 1 },
        { name: "__probe_intercept$arity2", ty_expr: function!(vec![numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept", ty_expr: function!(vec![Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id(), numeric!()], Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity1", ty_expr: function!(vec![Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id(), numeric!()], Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity2", ty_expr: function!(vec![numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity3", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity4", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity5", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity6", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity7", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity8", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity9", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity10", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity11", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity12", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity13", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity14", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity15", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
        { name: "__probe_value_intercept$arity16", ty_expr: function!(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()], Type::Tuple(vec![numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!(), numeric!()]).into_id()), stage: 1 },
    ],
    wasm_audio_handle: {
        handle_type: GuiAudioHandle,
        functions: [
            ("__get_slider", get_slider, 1),
            ("__probe_intercept", probe_intercept, 2),
            ("__probe_intercept$arity2", probe_intercept_arity2, 3),
            ("__probe_value_intercept", probe_intercept, 2),
            ("__probe_value_intercept$arity1", probe_intercept, 2),
            ("__probe_value_intercept$arity2", probe_intercept_arity2, 3),
            ("__probe_value_intercept$arity3", probe_intercept_arity3, 4),
            ("__probe_value_intercept$arity4", probe_intercept_arity4, 5),
            ("__probe_value_intercept$arity5", probe_intercept_arity5, 6),
            ("__probe_value_intercept$arity6", probe_intercept_arity6, 7),
            ("__probe_value_intercept$arity7", probe_intercept_arity7, 8),
            ("__probe_value_intercept$arity8", probe_intercept_arity8, 9),
            ("__probe_value_intercept$arity9", probe_intercept_arity9, 10),
            ("__probe_value_intercept$arity10", probe_intercept_arity10, 11),
            ("__probe_value_intercept$arity11", probe_intercept_arity11, 12),
            ("__probe_value_intercept$arity12", probe_intercept_arity12, 13),
            ("__probe_value_intercept$arity13", probe_intercept_arity13, 14),
            ("__probe_value_intercept$arity14", probe_intercept_arity14, 15),
            ("__probe_value_intercept$arity15", probe_intercept_arity15, 16),
            ("__probe_value_intercept$arity16", probe_intercept_arity16, 17),
        ],
    },
}
