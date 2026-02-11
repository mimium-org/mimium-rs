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
    runtime::vm::{Machine, ReturnCode},
    string_t,
    types::{PType, Type},
};
use mimium_plugin_macros::mimium_plugin_fn;
use plot_window::PlotApp;
use ringbuf::{
    HeapProd, HeapRb,
    traits::{Producer, Split},
};

use crate::plot_window::FloatParameter;
pub(crate) mod plot_ui;
pub mod plot_window;

pub struct GuiToolPlugin {
    window: Arc<Mutex<PlotApp>>,
    slider_instances: Arc<Mutex<Vec<Arc<FloatParameter>>>>,
    slider_namemap: Arc<Mutex<HashMap<String, usize>>>,

    probe_instances: Arc<Mutex<Vec<HeapProd<f64>>>>,
    probe_namemap: Arc<Mutex<HashMap<String, usize>>>,
}

impl Clone for GuiToolPlugin {
    fn clone(&self) -> Self {
        Self {
            window: self.window.clone(),
            slider_instances: self.slider_instances.clone(),
            slider_namemap: self.slider_namemap.clone(),
            probe_instances: self.probe_instances.clone(),
            probe_namemap: self.probe_namemap.clone(),
        }
    }
}

impl Default for GuiToolPlugin {
    fn default() -> Self {
        Self {
            window: Arc::new(Mutex::new(PlotApp::default())),
            slider_instances: Arc::new(Mutex::new(Vec::new())),
            slider_namemap: Arc::new(Mutex::new(HashMap::default())),
            probe_instances: Arc::new(Mutex::new(Vec::new())),
            probe_namemap: Arc::new(Mutex::new(HashMap::default())),
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
        let mut slider_namemap = self.slider_namemap.lock().unwrap();
        let mut slider_instances = self.slider_instances.lock().unwrap();
        let idx = if let Some(idx) = slider_namemap.get(name.as_str()).cloned() {
            let p = slider_instances.get_mut(idx).unwrap();
            p.set_range(min, max);
            idx
        } else {
            let (p, idx) = window.add_slider(name.as_str(), init, min, max);
            slider_instances.push(p);
            slider_namemap.insert(name.to_string(), idx);
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
        let mut probe_namemap = self.probe_namemap.lock().unwrap();
        let mut probe_instances = self.probe_instances.lock().unwrap();
        let probeid = probe_namemap
            .get(name.as_str())
            .cloned()
            .unwrap_or_else(|| {
                let (prod, cons) = HeapRb::<f64>::new(4096).split();
                window.add_plot(name.as_str(), cons);
                let idx = probe_instances.len();
                probe_instances.push(prod);
                probe_namemap.insert(name.to_string(), idx);
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
        let slider_instances = self.slider_instances.lock().unwrap();
        match slider_instances.get(idx) {
            Some(s) => s.get(),
            None => {
                log::error!("invalid slider index: {idx}");
                0.0
            }
        }
    }

    #[mimium_plugin_fn]
    pub fn probe_intercept(&mut self, value: f64, probe_idx: f64) -> f64 {
        let idx = probe_idx as usize;
        let mut probe_instances = self.probe_instances.lock().unwrap();
        match probe_instances.get_mut(idx) {
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
}

#[cfg(not(target_arch = "wasm32"))]
impl mimium_lang::runtime::wasm::WasmPluginCallable for GuiToolPlugin {
    fn call_method(&mut self, method: &str, args: &[f64]) -> Option<f64> {
        log::debug!("WasmPluginCallable::call_method: {} with args: {:?}", method, args);
        match method {
            "__get_slider" => {
                if args.len() >= 1 {
                    let idx = args[0] as usize;
                    let slider_instances = self.slider_instances.lock().unwrap();
                    Some(
                        slider_instances
                            .get(idx)
                            .map(|s| s.get())
                            .unwrap_or(0.0),
                    )
                } else {
                    None
                }
            }
            "__probe_intercept" => {
                if args.len() >= 2 {
                    let value = args[0];
                    let idx = args[1] as usize;
                    let mut probe_instances = self.probe_instances.lock().unwrap();
                    log::debug!("probe_intercept: idx={}, probe_instances.len()={}", idx, probe_instances.len());
                    if let Some(prod) = probe_instances.get_mut(idx) {
                        let _ = prod.try_push(value);
                    } else {
                        log::error!("invalid probe index: {}, available probes: {}", idx, probe_instances.len());
                    }
                    Some(value) // passthrough
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

        // Note: get_slider and probe_intercept runtime functions are provided
        // by the dynamic plugin system via #[mimium_plugin_fn] macro
        vec![probe_macro, make_slider]
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
// Dynamic Plugin ABI
// -------------------------------------------------------------------------

#[cfg(not(target_arch = "wasm32"))]
use std::ffi::{CString, c_char};

#[cfg(not(target_arch = "wasm32"))]
use mimium_lang::plugin::loader::{PluginCapabilities, PluginInstance, PluginMetadata};

#[cfg(not(target_arch = "wasm32"))]
static PLUGIN_NAME: &str = "mimium-guitools\0";
#[cfg(not(target_arch = "wasm32"))]
static PLUGIN_VERSION: &str = env!("CARGO_PKG_VERSION");
#[cfg(not(target_arch = "wasm32"))]
static PLUGIN_AUTHOR: &str = "mimium-org\0";

#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub extern "C" fn mimium_plugin_metadata() -> *const PluginMetadata {
    use std::sync::OnceLock;

    static METADATA: OnceLock<(CString, PluginMetadata)> = OnceLock::new();

    let (_version_cstr, metadata) = METADATA.get_or_init(|| {
        let version_cstr = CString::new(PLUGIN_VERSION).expect("Version string is valid");
        let metadata = PluginMetadata {
            name: PLUGIN_NAME.as_ptr() as *const c_char,
            version: version_cstr.as_ptr(),
            author: PLUGIN_AUTHOR.as_ptr() as *const c_char,
            capabilities: PluginCapabilities {
                has_audio_worker: false,
                has_macros: true,
                has_runtime_functions: true,
            },
        };
        (version_cstr, metadata)
    });

    metadata
}

#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub extern "C" fn mimium_plugin_create() -> *mut PluginInstance {
    let plugin = Box::new(GuiToolPlugin::default());
    Box::into_raw(plugin) as *mut PluginInstance
}

/// Share the host process's interner with this plugin.
///
/// Must be called before any other function that uses the interner
/// (e.g. `mimium_plugin_get_type_infos`, macro invocations).
///
/// # Safety
///
/// `globals_ptr` must point to a valid `Mutex<SessionGlobals>` that
/// outlives the plugin.
#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mimium_plugin_set_interner(globals_ptr: *const std::ffi::c_void) {
    unsafe { mimium_lang::interner::set_external_session_globals(globals_ptr) };
}

#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub extern "C" fn mimium_plugin_destroy(instance: *mut PluginInstance) {
    if !instance.is_null() {
        unsafe {
            let _ = Box::from_raw(instance as *mut GuiToolPlugin);
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
use mimium_lang::plugin::loader::PluginFunctionFn;

/// Get a plugin function by name.
///
/// # Safety
///
/// - `name` must be a valid pointer to a null-terminated C string
/// - The returned function pointer, if any, must only be called with valid arguments
#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mimium_plugin_get_function(
    name: *const c_char,
) -> Option<PluginFunctionFn> {
    use std::ffi::CStr;

    if name.is_null() {
        return None;
    }

    let name_str = unsafe { CStr::from_ptr(name) }.to_str().ok()?;

    match name_str {
        "__get_slider" => Some(ffi_get_slider),
        "__probe_intercept" => Some(ffi_probe_intercept),
        _ => None,
    }
}

#[cfg(not(target_arch = "wasm32"))]
use mimium_lang::plugin::loader::PluginMacroFn;

/// Get a plugin macro function by name.
///
/// # Safety
///
/// - `name` must be a valid pointer to a null-terminated C string
/// - The returned function pointer, if any, must only be called with valid arguments
#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mimium_plugin_get_macro(name: *const c_char) -> Option<PluginMacroFn> {
    use std::ffi::CStr;

    if name.is_null() {
        return None;
    }

    let name_str = unsafe { CStr::from_ptr(name) }.to_str().ok()?;

    match name_str {
        "Slider" | "make_slider" => Some(ffi_make_slider),
        "Probe" | "make_probe_macro" => Some(ffi_make_probe_macro),
        _ => None,
    }
}

/// FFI wrapper for `__get_slider`.
#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" fn ffi_get_slider(
    instance: *mut PluginInstance,
    runtime: *mut std::ffi::c_void,
) -> i64 {
    if instance.is_null() || runtime.is_null() {
        return 0;
    }
    let machine = unsafe { &mut *(runtime as *mut Machine) };
    let plugin = unsafe { &mut *(instance as *mut GuiToolPlugin) };
    plugin.get_slider(machine)
}

/// FFI wrapper for `__probe_intercept`.
#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" fn ffi_probe_intercept(
    instance: *mut PluginInstance,
    runtime: *mut std::ffi::c_void,
) -> i64 {
    if instance.is_null() || runtime.is_null() {
        return 0;
    }
    let machine = unsafe { &mut *(runtime as *mut Machine) };
    let plugin = unsafe { &mut *(instance as *mut GuiToolPlugin) };
    plugin.probe_intercept(machine)
}

/// FFI bridge for `Slider!` macro.
///
/// # Safety
///
/// - `instance` must be a valid pointer to a GuiToolPlugin instance
/// - `args_ptr` and `args_len` must describe a valid byte buffer containing serialized arguments
/// - `out_ptr` and `out_len` must be valid pointers to write the result
#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ffi_make_slider(
    instance: *mut std::ffi::c_void,
    args_ptr: *const u8,
    args_len: usize,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    use mimium_lang::runtime::ffi_serde::{deserialize_macro_args, serialize_value};

    if instance.is_null() || args_ptr.is_null() || out_ptr.is_null() || out_len.is_null() {
        log::error!("ffi_make_slider: Null pointer detected");
        return -3;
    }

    unsafe {
        let plugin = &mut *(instance as *mut GuiToolPlugin);
        let args_bytes = std::slice::from_raw_parts(args_ptr, args_len);
        let args = match deserialize_macro_args(args_bytes) {
            Ok(a) => a,
            Err(e) => {
                log::error!("Failed to deserialize macro arguments for Slider: {e}");
                return -1;
            }
        };

        let result = plugin.make_slider(&args);

        let result_bytes = match serialize_value(&result) {
            Ok(b) => b,
            Err(e) => {
                log::error!("Failed to serialize macro result for Slider: {e}");
                return -2;
            }
        };

        let boxed = result_bytes.into_boxed_slice();
        *out_len = boxed.len();
        *out_ptr = Box::into_raw(boxed) as *mut u8;
        0
    }
}

/// FFI bridge for `Probe!` macro.
///
/// # Safety
///
/// - `instance` must be a valid pointer to a GuiToolPlugin instance
/// - `args_ptr` and `args_len` must describe a valid byte buffer containing serialized arguments
/// - `out_ptr` and `out_len` must be valid pointers to write the result
#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ffi_make_probe_macro(
    instance: *mut std::ffi::c_void,
    args_ptr: *const u8,
    args_len: usize,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    use mimium_lang::runtime::ffi_serde::{deserialize_macro_args, serialize_value};

    if instance.is_null() || args_ptr.is_null() || out_ptr.is_null() || out_len.is_null() {
        log::error!("ffi_make_probe_macro: Null pointer detected");
        return -3;
    }

    unsafe {
        let plugin = &mut *(instance as *mut GuiToolPlugin);
        let args_bytes = std::slice::from_raw_parts(args_ptr, args_len);
        let args = match deserialize_macro_args(args_bytes) {
            Ok(a) => a,
            Err(e) => {
                log::error!("Failed to deserialize macro arguments for Probe: {e}");
                return -1;
            }
        };

        let result = plugin.make_probe_macro(&args);

        let result_bytes = match serialize_value(&result) {
            Ok(b) => b,
            Err(e) => {
                log::error!("Failed to serialize macro result for Probe: {e}");
                return -2;
            }
        };

        let boxed = result_bytes.into_boxed_slice();
        *out_len = boxed.len();
        *out_ptr = Box::into_raw(boxed) as *mut u8;
        0
    }
}

// -------------------------------------------------------------------------
// Type information export
// -------------------------------------------------------------------------

#[cfg(not(target_arch = "wasm32"))]
mod ffi_exports {
    use super::*;

    static mut TYPE_INFO_STORAGE: Option<Vec<(std::ffi::CString, Vec<u8>)>> = None;
    static mut FFI_TYPE_INFOS: Option<Vec<mimium_lang::plugin::loader::FfiTypeInfo>> = None;

    /// Export type information for the plugin.
    ///
    /// Returns an array of FfiTypeInfo structures that describe the types of
    /// all macros provided by the plugin.
    #[unsafe(no_mangle)]
    pub extern "C" fn mimium_plugin_get_type_infos(
        out_len: *mut usize,
    ) -> *const mimium_lang::plugin::loader::FfiTypeInfo {
        use mimium_lang::plugin::loader::FfiTypeInfo;
        if out_len.is_null() {
            return std::ptr::null();
        }

        unsafe {
            if (*std::ptr::addr_of!(TYPE_INFO_STORAGE)).is_none() {
                let mut storage = Vec::new();
                let mut infos = Vec::new();

                let add_info = |name_str: &str,
                                ty: mimium_lang::interner::TypeNodeId,
                                stage: u8,
                                storage: &mut Vec<(std::ffi::CString, Vec<u8>)>,
                                infos: &mut Vec<FfiTypeInfo>| {
                    let name_cstr = std::ffi::CString::new(name_str).ok()?;
                    let type_bytes = bincode::serialize(&ty).ok()?;
                    let name_ptr = name_cstr.as_ptr();
                    let type_ptr = type_bytes.as_ptr();
                    let type_len = type_bytes.len();
                    storage.push((name_cstr, type_bytes));
                    infos.push(FfiTypeInfo {
                        name: name_ptr,
                        type_data: type_ptr,
                        type_len,
                        stage,
                    });
                    Some(())
                };

                // Slider! macro (stage 0)
                let slider_sig = GuiToolPlugin::slider_signature();
                add_info(
                    slider_sig.get_name(),
                    slider_sig.get_type(),
                    0,
                    &mut storage,
                    &mut infos,
                );

                // Probe! macro (stage 0)
                let probe_sig = GuiToolPlugin::probe_signature();
                add_info(
                    probe_sig.get_name(),
                    probe_sig.get_type(),
                    0,
                    &mut storage,
                    &mut infos,
                );

                // __get_slider runtime function (stage 1)
                {
                    use mimium_lang::{function, numeric};
                    let ty = function!(vec![numeric!()], numeric!());
                    add_info(GuiToolPlugin::GET_SLIDER, ty, 1, &mut storage, &mut infos);
                }

                // __probe_intercept runtime function (stage 1)
                {
                    use mimium_lang::{function, numeric};
                    let ty = function!(vec![numeric!(), numeric!()], numeric!());
                    add_info(
                        GuiToolPlugin::PROBE_INTERCEPT,
                        ty,
                        1,
                        &mut storage,
                        &mut infos,
                    );
                }

                TYPE_INFO_STORAGE = Some(storage);
                FFI_TYPE_INFOS = Some(infos);
            }

            let infos = (*std::ptr::addr_of!(FFI_TYPE_INFOS)).as_ref().unwrap();
            *out_len = infos.len();
            infos.as_ptr()
        }
    }
}
