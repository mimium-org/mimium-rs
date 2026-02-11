//! Dynamic plugin loader for native targets.
//!
//! This module provides functionality to load mimium plugins from shared
//! libraries (DLL/SO/DYLIB) at runtime. Plugins must export a standard set of
//! C-compatible functions that allow the runtime to discover and register them.
//!
//! # Plugin ABI
//!
//! A plugin library must export the following functions:
//!
//! - `mimium_plugin_metadata() -> *const PluginMetadata`
//!   Returns plugin name, version, and capabilities.
//!
//! - `mimium_plugin_create() -> *mut PluginInstance`
//!   Creates a new instance of the plugin.
//!
//! - `mimium_plugin_destroy(instance: *mut PluginInstance)`
//!   Destroys a plugin instance.
//!
//! # Safety
//!
//! Loading plugins is inherently unsafe as it involves executing arbitrary code
//! from dynamic libraries. The loader performs basic validation but cannot
//! guarantee plugin correctness or safety.

#[cfg(not(target_arch = "wasm32"))]
use std::ffi::{CStr, c_char, c_void};
#[cfg(not(target_arch = "wasm32"))]
use std::path::{Path, PathBuf};

#[cfg(not(target_arch = "wasm32"))]
use libloading::{Library, Symbol};

use super::system_plugin::SystemPlugin;

// -------------------------------------------------------------------------
// Plugin interface types
// -------------------------------------------------------------------------

/// Metadata describing a plugin.
///
/// This struct is returned by the plugin's `mimium_plugin_metadata()` function
/// and provides basic information about the plugin.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct PluginMetadata {
    /// Plugin name (null-terminated UTF-8).
    pub name: *const c_char,
    /// Plugin version string (null-terminated UTF-8).
    pub version: *const c_char,
    /// Author name (null-terminated UTF-8).
    pub author: *const c_char,
    /// Plugin capabilities flags.
    pub capabilities: PluginCapabilities,
}

// SAFETY: PluginMetadata contains only pointers to static strings that are
// guaranteed to outlive the program, so it is safe to share across threads.
#[cfg(not(target_arch = "wasm32"))]
unsafe impl Sync for PluginMetadata {}

// SAFETY: PluginMetadata contains only pointers to static strings that are
// guaranteed to outlive the program, so it is safe to send across threads.
#[cfg(not(target_arch = "wasm32"))]
unsafe impl Send for PluginMetadata {}

/// Flags describing plugin capabilities.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct PluginCapabilities {
    /// Plugin provides audio processing (has `on_sample` callback).
    pub has_audio_worker: bool,
    /// Plugin provides compile-time macros.
    pub has_macros: bool,
    /// Plugin provides runtime functions.
    pub has_runtime_functions: bool,
}

/// Opaque handle to a plugin instance.
///
/// The actual structure is defined by the plugin library; the runtime only
/// manipulates it through the plugin's exported functions.
#[repr(C)]
pub struct PluginInstance {
    _private: [u8; 0],
}

/// Type signature for plugin functions that use RuntimeHandle.
///
/// Plugin functions receive a mutable reference to the plugin instance and
/// a RuntimeHandle for accessing arguments and setting return values.
pub type PluginFunctionFn = unsafe extern "C" fn(
    instance: *mut PluginInstance,
    runtime: *mut c_void, // RuntimeHandle as opaque pointer
) -> i64; // ReturnCode

/// Type signature for plugin macro functions.
///
/// Macro functions are compile-time transformations that take serialized
/// arguments and return a serialized result. The signature is:
///
/// - `instance`: Mutable pointer to the plugin instance
/// - `args_ptr`: Pointer to serialized arguments (bincode-encoded `Vec<(Value, TypeNodeId)>`)
/// - `args_len`: Length of the serialized arguments buffer
/// - `out_ptr`: Output pointer for the serialized result buffer
/// - `out_len`: Output length of the serialized result buffer
/// - Returns: Status code (0 = success, negative = error)
pub type PluginMacroFn = unsafe extern "C" fn(
    instance: *mut c_void,
    args_ptr: *const u8,
    args_len: usize,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32;

// -------------------------------------------------------------------------
// Plugin function signatures
// -------------------------------------------------------------------------

/// Type of the `mimium_plugin_metadata` export.
type PluginMetadataFn = unsafe extern "C" fn() -> *const PluginMetadata;

/// Type of the `mimium_plugin_create` export.
type PluginCreateFn = unsafe extern "C" fn() -> *mut PluginInstance;

/// Type of the `mimium_plugin_destroy` export.
type PluginDestroyFn = unsafe extern "C" fn(instance: *mut PluginInstance);

/// Type of the `mimium_plugin_set_interner` export.
///
/// Shares the host's session globals with the plugin so that interned IDs
/// (TypeNodeId, ExprNodeId, Symbol) are valid across the DLL boundary.
type PluginSetInternerFn = unsafe extern "C" fn(globals_ptr: *const std::ffi::c_void);

/// Type of the `mimium_plugin_get_function` export.
///
/// Returns a function pointer for the named plugin function, or null if not found.
type PluginGetFunctionFn = unsafe extern "C" fn(name: *const c_char) -> Option<PluginFunctionFn>;

/// Type of the `mimium_plugin_get_macro` export.
///
/// Returns a function pointer for the named macro function, or null if not found.
type PluginGetMacroFn = unsafe extern "C" fn(name: *const c_char) -> Option<PluginMacroFn>;

/// FFI-safe representation of type information.
///
/// Used to pass type information from plugins to the host.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct FfiTypeInfo {
    /// Function/macro name (null-terminated UTF-8).
    pub name: *const c_char,
    /// Serialized type (bincode-encoded TypeNodeId).
    pub type_data: *const u8,
    /// Length of serialized type data.
    pub type_len: usize,
    /// Stage where this function is available (0=Macro, 1=Machine, 2=Persistent).
    pub stage: u8,
}

// SAFETY: FfiTypeInfo contains only pointers to data that outlives the plugin,
// so it is safe to share across threads.
#[cfg(not(target_arch = "wasm32"))]
unsafe impl Sync for FfiTypeInfo {}

#[cfg(not(target_arch = "wasm32"))]
unsafe impl Send for FfiTypeInfo {}

/// Type of the `mimium_plugin_get_type_infos` export.
///
/// Returns an array of type information structures.
/// `out_len` is set to the number of elements in the returned array.
type PluginGetTypeInfosFn = unsafe extern "C" fn(out_len: *mut usize) -> *const FfiTypeInfo;

// -------------------------------------------------------------------------
// Loaded plugin handle
// -------------------------------------------------------------------------

/// A loaded plugin with its library and exported functions.
#[cfg(not(target_arch = "wasm32"))]
pub struct LoadedPlugin {
    /// The dynamic library handle (kept alive for the lifetime of the plugin).
    _library: Library,
    /// Plugin metadata.
    metadata: PluginMetadata,
    /// Plugin instance pointer.
    instance: *mut PluginInstance,
    /// Destroy function pointer (called on drop).
    destroy_fn: PluginDestroyFn,
    /// Function lookup function pointer (optional).
    get_function_fn: Option<PluginGetFunctionFn>,
    /// Macro function lookup function pointer (optional).
    get_macro_fn: Option<PluginGetMacroFn>,
    /// Type information lookup function pointer (optional).
    get_type_infos_fn: Option<PluginGetTypeInfosFn>,
}

#[cfg(not(target_arch = "wasm32"))]
impl LoadedPlugin {
    /// Get plugin metadata.
    pub fn metadata(&self) -> &PluginMetadata {
        &self.metadata
    }

    /// Get the plugin name as a Rust string.
    pub fn name(&self) -> String {
        unsafe { CStr::from_ptr(self.metadata.name) }
            .to_string_lossy()
            .into_owned()
    }

    /// Get the plugin version as a Rust string.
    pub fn version(&self) -> String {
        unsafe { CStr::from_ptr(self.metadata.version) }
            .to_string_lossy()
            .into_owned()
    }

    /// Get a plugin function by name.
    ///
    /// Returns `None` if the function is not found or if the plugin doesn't
    /// support function lookup.
    pub fn get_function(&self, name: &str) -> Option<PluginFunctionFn> {
        let get_fn = self.get_function_fn?;
        let name_cstr = std::ffi::CString::new(name).ok()?;
        unsafe { get_fn(name_cstr.as_ptr()) }
    }

    /// Get a plugin macro function by name.
    ///
    /// Returns `None` if the macro is not found or if the plugin doesn't
    /// support macro function lookup.
    pub fn get_macro(&self, name: &str) -> Option<PluginMacroFn> {
        let get_fn = self.get_macro_fn?;
        let name_cstr = std::ffi::CString::new(name).ok()?;
        unsafe { get_fn(name_cstr.as_ptr()) }
    }

    /// Get type information from the plugin.
    ///
    /// Returns a vector of type information if the plugin supports it.
    pub fn get_type_infos(&self) -> Option<Vec<crate::plugin::ExtFunTypeInfo>> {
        use crate::interner::{ToSymbol, TypeNodeId};
        use crate::plugin::{EvalStage, ExtFunTypeInfo};

        let get_fn = self.get_type_infos_fn?;
        let mut len: usize = 0;
        let array_ptr = unsafe { get_fn(&mut len as *mut usize) };

        if array_ptr.is_null() || len == 0 {
            crate::log::debug!("Plugin {} has no type info or returned null", self.name());
            return None;
        }

        crate::log::debug!("Plugin {} provided {} type info entries", self.name(), len);
        let mut result = Vec::with_capacity(len);
        for i in 0..len {
            let info = unsafe { &*array_ptr.add(i) };

            // Convert C string to Rust string
            let name_str = unsafe { CStr::from_ptr(info.name) }
                .to_string_lossy()
                .into_owned();
            let name = name_str.to_symbol();

            // Deserialize type data
            let type_slice = unsafe { std::slice::from_raw_parts(info.type_data, info.type_len) };
            let ty: TypeNodeId = match bincode::deserialize(type_slice) {
                Ok(t) => t,
                Err(e) => {
                    crate::log::warn!("Failed to deserialize type for {name_str}: {e:?}");
                    continue;
                }
            };

            // Convert stage number to EvalStage
            let stage = match info.stage {
                0 => EvalStage::Stage(0),   // Macro stage (compile-time)
                1 => EvalStage::Stage(1),   // Machine stage (runtime)
                2 => EvalStage::Persistent, // Persistent stage
                _ => {
                    crate::log::warn!("Unknown stage {} for {}", info.stage, name_str);
                    continue;
                }
            };

            result.push(ExtFunTypeInfo::new(name, ty, stage));
        }

        Some(result)
    }

    /// Get the plugin instance pointer (for advanced use).
    ///
    /// # Safety
    ///
    /// The returned pointer is valid for the lifetime of this LoadedPlugin.
    pub unsafe fn instance_ptr(&self) -> *mut PluginInstance {
        self.instance
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl Drop for LoadedPlugin {
    fn drop(&mut self) {
        // SAFETY: instance is valid and was created by this plugin's create function.
        unsafe {
            (self.destroy_fn)(self.instance);
        }
    }
}

// -------------------------------------------------------------------------
// Dynamic plugin macro wrapper
// -------------------------------------------------------------------------

/// Wrapper for dynamically loaded plugin macro functions.
///
/// This implements the `MacroFunction` trait by calling the FFI bridge
/// and serializing/deserializing arguments and results.
#[cfg(not(target_arch = "wasm32"))]
pub struct DynPluginMacroInfo {
    name: crate::interner::Symbol,
    ty: crate::interner::TypeNodeId,
    /// Plugin instance pointer
    instance: *mut PluginInstance,
    /// Macro function pointer
    macro_fn: PluginMacroFn,
}

#[cfg(not(target_arch = "wasm32"))]
impl DynPluginMacroInfo {
    /// Create a new dynamic plugin macro wrapper.
    ///
    /// # Safety
    ///
    /// - `instance` must be a valid pointer to the plugin instance
    /// - `macro_fn` must be a valid function pointer for the macro
    /// - Both must remain valid for the lifetime of this struct
    pub unsafe fn new(
        name: crate::interner::Symbol,
        ty: crate::interner::TypeNodeId,
        instance: *mut PluginInstance,
        macro_fn: PluginMacroFn,
    ) -> Self {
        Self {
            name,
            ty,
            instance,
            macro_fn,
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl crate::plugin::MacroFunction for DynPluginMacroInfo {
    fn get_name(&self) -> crate::interner::Symbol {
        self.name
    }

    fn get_type(&self) -> crate::interner::TypeNodeId {
        self.ty
    }

    fn get_fn(&self) -> crate::plugin::MacroFunType {
        use std::cell::RefCell;
        use std::rc::Rc;

        let instance = self.instance;
        let macro_fn = self.macro_fn;

        Rc::new(RefCell::new(
            move |args: &[(crate::interpreter::Value, crate::interner::TypeNodeId)]| {
                use crate::runtime::ffi_serde::{deserialize_value, serialize_macro_args};

                // Serialize arguments
                let args_bytes = match serialize_macro_args(args) {
                    Ok(b) => b,
                    Err(e) => {
                        crate::log::error!("Failed to serialize macro arguments: {e}");
                        let err_expr = crate::ast::Expr::Error
                            .into_id(crate::utils::metadata::Location::internal());
                        return crate::interpreter::Value::ErrorV(err_expr);
                    }
                };

                // Prepare output buffers
                let mut out_ptr: *mut u8 = std::ptr::null_mut();
                let mut out_len: usize = 0;

                // Call FFI function
                let result_code = unsafe {
                    macro_fn(
                        instance as *mut c_void,
                        args_bytes.as_ptr(),
                        args_bytes.len(),
                        &mut out_ptr,
                        &mut out_len,
                    )
                };

                if result_code != 0 {
                    crate::log::error!(
                        "Dynamic plugin macro function returned error code: {result_code}"
                    );
                    let err_expr = crate::ast::Expr::Error
                        .into_id(crate::utils::metadata::Location::internal());
                    return crate::interpreter::Value::ErrorV(err_expr);
                }

                if out_ptr.is_null() || out_len == 0 {
                    crate::log::error!("Dynamic plugin macro function returned null/empty result");
                    let err_expr = crate::ast::Expr::Error
                        .into_id(crate::utils::metadata::Location::internal());
                    return crate::interpreter::Value::ErrorV(err_expr);
                }

                // Deserialize result
                let out_bytes = unsafe { std::slice::from_raw_parts(out_ptr, out_len) };
                let result = match deserialize_value(out_bytes) {
                    Ok(v) => v,
                    Err(e) => {
                        crate::log::error!("Failed to deserialize macro result: {e}");
                        let err_expr = crate::ast::Expr::Error
                            .into_id(crate::utils::metadata::Location::internal());
                        crate::interpreter::Value::ErrorV(err_expr)
                    }
                };

                // Clean up allocated output buffer
                unsafe {
                    let _ = Box::from_raw(std::slice::from_raw_parts_mut(out_ptr, out_len));
                }

                result
            },
        ))
    }
}

// SAFETY: DynPluginMacroInfo doesn't implement Send/Sync by default due to raw pointers,
// but in our use case:
// - The plugin instance is guaranteed to be valid for the macro's lifetime
// - Macro functions are only called from the compiler thread, never concurrently
// - The LoadedPlugin that owns the instance is kept alive by PluginLoader
#[cfg(not(target_arch = "wasm32"))]
unsafe impl Send for DynPluginMacroInfo {}
#[cfg(not(target_arch = "wasm32"))]
unsafe impl Sync for DynPluginMacroInfo {}

// -------------------------------------------------------------------------
// Dynamic plugin runtime function wrapper
// -------------------------------------------------------------------------

/// Wrapper for dynamically loaded plugin runtime functions.
///
/// This implements the `MachineFunction` trait by calling the FFI bridge.
#[cfg(not(target_arch = "wasm32"))]
pub struct DynPluginFunctionInfo {
    name: crate::interner::Symbol,
    instance: *mut PluginInstance,
    function_fn: PluginFunctionFn,
}

#[cfg(not(target_arch = "wasm32"))]
impl DynPluginFunctionInfo {
    /// Create a new dynamic plugin function wrapper.
    ///
    /// # Safety
    ///
    /// - `instance` must be a valid pointer to the plugin instance
    /// - `function_fn` must be a valid function pointer
    /// - Both must remain valid for the lifetime of this struct
    pub unsafe fn new(
        name: crate::interner::Symbol,
        instance: *mut PluginInstance,
        function_fn: PluginFunctionFn,
    ) -> Self {
        Self {
            name,
            instance,
            function_fn,
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl crate::plugin::MachineFunction for DynPluginFunctionInfo {
    fn get_name(&self) -> crate::interner::Symbol {
        self.name
    }

    fn get_fn(&self) -> crate::plugin::ExtClsType {
        use std::cell::RefCell;
        use std::rc::Rc;

        let instance = self.instance;
        let function_fn = self.function_fn;

        Rc::new(RefCell::new(move |machine: &mut crate::runtime::vm::Machine| {
            let ret = unsafe {
                function_fn(instance, machine as *mut crate::runtime::vm::Machine as *mut c_void)
            };
            ret as crate::runtime::vm::ReturnCode
        }))
    }
}

#[cfg(not(target_arch = "wasm32"))]
unsafe impl Send for DynPluginFunctionInfo {}
#[cfg(not(target_arch = "wasm32"))]
unsafe impl Sync for DynPluginFunctionInfo {}

// -------------------------------------------------------------------------
// Plugin loader
// -------------------------------------------------------------------------

/// Dynamic plugin loader.
///
/// Manages loading and unloading of plugin libraries. The loader searches
/// for plugins in standard directories and validates them before loading.
#[cfg(not(target_arch = "wasm32"))]
pub struct PluginLoader {
    /// All loaded plugins.
    plugins: Vec<LoadedPlugin>,
}

#[cfg(not(target_arch = "wasm32"))]
impl PluginLoader {
    /// Create a new plugin loader.
    pub fn new() -> Self {
        Self {
            plugins: Vec::new(),
        }
    }

    /// Load a plugin from the specified path.
    ///
    /// The path should point to a shared library without extension
    /// (e.g., "path/to/plugin" will load "path/to/plugin.dll" on Windows).
    pub fn load_plugin<P: AsRef<Path>>(&mut self, path: P) -> Result<(), PluginLoaderError> {
        let base_path = path.as_ref();
        let lib_path = get_library_path(base_path)?;

        // Load library
        // SAFETY: Loading arbitrary code is inherently unsafe.
        let library = unsafe { Library::new(&lib_path) }
            .map_err(|e| PluginLoaderError::LoadFailed(lib_path.clone(), e.to_string()))?;

        // Load required symbols
        let metadata_fn: Symbol<PluginMetadataFn> = unsafe {
            library
                .get(b"mimium_plugin_metadata\0")
                .map_err(|_| PluginLoaderError::MissingSymbol("mimium_plugin_metadata"))?
        };

        let create_fn: Symbol<PluginCreateFn> = unsafe {
            library
                .get(b"mimium_plugin_create\0")
                .map_err(|_| PluginLoaderError::MissingSymbol("mimium_plugin_create"))?
        };

        let destroy_fn: Symbol<PluginDestroyFn> = unsafe {
            library
                .get(b"mimium_plugin_destroy\0")
                .map_err(|_| PluginLoaderError::MissingSymbol("mimium_plugin_destroy"))?
        };

        // Try to load the optional get_function symbol
        let get_function_fn: Option<Symbol<PluginGetFunctionFn>> =
            unsafe { library.get(b"mimium_plugin_get_function\0").ok() };

        // Try to load the optional get_macro symbol
        let get_macro_fn: Option<Symbol<PluginGetMacroFn>> =
            unsafe { library.get(b"mimium_plugin_get_macro\0").ok() };

        // Try to load the optional get_type_infos symbol
        let get_type_infos_fn: Option<Symbol<PluginGetTypeInfosFn>> =
            unsafe { library.get(b"mimium_plugin_get_type_infos\0").ok() };

        // Get metadata
        let metadata_ptr = unsafe { metadata_fn() };
        if metadata_ptr.is_null() {
            return Err(PluginLoaderError::InvalidMetadata);
        }
        let metadata = unsafe { (*metadata_ptr).clone() };

        // Share the host's interner with the plugin.
        // This must happen before create_fn or any other call that may touch
        // the interner (type construction, symbol interning, etc.).
        let set_interner_fn: Option<Symbol<PluginSetInternerFn>> =
            unsafe { library.get(b"mimium_plugin_set_interner\0").ok() };
        if let Some(set_interner) = &set_interner_fn {
            let host_globals = crate::interner::get_session_globals_ptr();
            unsafe { set_interner(host_globals) };
            crate::log::info!("Shared host interner with plugin");
        } else {
            crate::log::warn!(
                "Plugin does not export mimium_plugin_set_interner; \
                 interned IDs may be invalid across the DLL boundary"
            );
        }

        // Create instance
        let instance = unsafe { create_fn() };
        if instance.is_null() {
            return Err(PluginLoaderError::CreateFailed);
        }

        // Copy the destroy function pointer before moving library
        let destroy_fn_ptr = *destroy_fn;
        let get_function_fn_ptr = get_function_fn.as_ref().map(|f| **f);
        let get_macro_fn_ptr = get_macro_fn.as_ref().map(|f| **f);
        let get_type_infos_fn_ptr = get_type_infos_fn.as_ref().map(|f| **f);

        // Store the loaded plugin
        let plugin = LoadedPlugin {
            _library: library,
            metadata,
            instance,
            destroy_fn: destroy_fn_ptr,
            get_function_fn: get_function_fn_ptr,
            get_macro_fn: get_macro_fn_ptr,
            get_type_infos_fn: get_type_infos_fn_ptr,
        };

        crate::log::info!("Loaded plugin: {} v{}", plugin.name(), plugin.version());
        self.plugins.push(plugin);

        Ok(())
    }

    /// Load all plugins from the standard plugin directory.
    pub fn load_builtin_plugins(&mut self) -> Result<(), PluginLoaderError> {
        let plugin_dir = get_plugin_directory()?;

        if !plugin_dir.exists() {
            crate::log::warn!("Plugin directory not found: {}", plugin_dir.display());
            return Ok(());
        }

        for entry in std::fs::read_dir(&plugin_dir)
            .map_err(|e| PluginLoaderError::DirectoryReadFailed(plugin_dir.clone(), e))?
        {
            let entry =
                entry.map_err(|e| PluginLoaderError::DirectoryReadFailed(plugin_dir.clone(), e))?;
            let path = entry.path();

            if is_library_file(&path) {
                // Remove extension for load_plugin
                let stem = path.with_extension("");
                match self.load_plugin(&stem) {
                    Ok(_) => {}
                    Err(e) => {
                        crate::log::warn!("Failed to load plugin {}: {:?}", path.display(), e);
                    }
                }
            }
        }

        Ok(())
    }

    /// Get a list of all loaded plugins.
    pub fn loaded_plugins(&self) -> &[LoadedPlugin] {
        &self.plugins
    }

    /// Get type information from all loaded plugins.
    pub fn get_type_infos(&self) -> Vec<crate::plugin::ExtFunTypeInfo> {
        self.plugins
            .iter()
            .filter_map(|plugin| plugin.get_type_infos())
            .flatten()
            .collect()
    }

    /// Get all macro functions from loaded plugins with their type information.
    ///
    /// Discovers macros automatically by iterating type info entries with
    /// stage 0 (macro/compile-time) and looking up the corresponding FFI
    /// function from `mimium_plugin_get_macro`.
    pub fn get_macro_functions(
        &self,
    ) -> Vec<(
        crate::interner::Symbol,
        crate::interner::TypeNodeId,
        Box<dyn crate::plugin::MacroFunction>,
    )> {
        use crate::interner::ToSymbol;

        let mut result = Vec::new();

        for plugin in &self.plugins {
            if !plugin.metadata.capabilities.has_macros {
                continue;
            }

            let get_macro_fn = match plugin.get_macro_fn {
                Some(f) => f,
                None => continue,
            };

            // Discover macros from type info (stage 0 = macro/compile-time)
            let type_infos = match plugin.get_type_infos() {
                Some(infos) => infos,
                None => {
                    crate::log::debug!(
                        "Plugin {} has no type info, skipping macro discovery",
                        plugin.name()
                    );
                    continue;
                }
            };

            let macro_infos: Vec<_> = type_infos
                .into_iter()
                .filter(|info| matches!(info.stage, crate::plugin::EvalStage::Stage(0)))
                .collect();

            for info in macro_infos {
                let name_str = info.name.as_str();
                let name_cstr = match std::ffi::CString::new(name_str) {
                    Ok(s) => s,
                    Err(_) => continue,
                };

                let macro_fn = unsafe { get_macro_fn(name_cstr.as_ptr()) };
                if let Some(macro_fn) = macro_fn {
                    let ty = info.ty;
                    let wrapper = unsafe {
                        DynPluginMacroInfo::new(
                            name_str.to_symbol(),
                            ty,
                            plugin.instance,
                            macro_fn,
                        )
                    };

                    crate::log::info!("Registered dynamic macro: {name_str}");
                    result.push((
                        name_str.to_symbol(),
                        ty,
                        Box::new(wrapper) as Box<dyn crate::plugin::MacroFunction>,
                    ));
                }
            }
        }

        result
    }

    /// Get all runtime functions from loaded plugins.
    ///
    /// Discovers runtime functions by iterating type info entries with
    /// stage 1 (machine/runtime) and looking up the corresponding FFI
    /// function from `mimium_plugin_get_function`.
    pub fn get_runtime_functions(&self) -> Vec<Box<dyn crate::plugin::MachineFunction>> {
        use crate::interner::ToSymbol;

        let mut result = Vec::new();

        for plugin in &self.plugins {
            if !plugin.metadata.capabilities.has_runtime_functions {
                continue;
            }

            let get_function_fn = match plugin.get_function_fn {
                Some(f) => f,
                None => continue,
            };

            // Discover runtime functions from type info (stage 1)
            let type_infos = match plugin.get_type_infos() {
                Some(infos) => infos,
                None => continue,
            };

            let runtime_infos: Vec<_> = type_infos
                .into_iter()
                .filter(|info| matches!(info.stage, crate::plugin::EvalStage::Stage(1)))
                .collect();

            for info in runtime_infos {
                let name_str = info.name.as_str();
                let name_cstr = match std::ffi::CString::new(name_str) {
                    Ok(s) => s,
                    Err(_) => continue,
                };

                let func = unsafe { get_function_fn(name_cstr.as_ptr()) };
                if let Some(func) = func {
                    let wrapper = unsafe {
                        DynPluginFunctionInfo::new(
                            name_str.to_symbol(),
                            plugin.instance,
                            func,
                        )
                    };

                    crate::log::info!("Registered dynamic runtime function: {name_str}");
                    result.push(
                        Box::new(wrapper) as Box<dyn crate::plugin::MachineFunction>,
                    );
                }
            }
        }

        result
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl Default for PluginLoader {
    fn default() -> Self {
        Self::new()
    }
}

// -------------------------------------------------------------------------
// Error types
// -------------------------------------------------------------------------

/// Errors that can occur during plugin loading.
#[derive(Debug)]
pub enum PluginLoaderError {
    /// Failed to load the library file.
    LoadFailed(PathBuf, String),
    /// Required symbol not found in the library.
    MissingSymbol(&'static str),
    /// Plugin metadata is invalid or null.
    InvalidMetadata,
    /// Plugin instance creation failed (returned null).
    CreateFailed,
    /// Failed to determine plugin directory.
    PluginDirectoryNotFound,
    /// Failed to read plugin directory.
    DirectoryReadFailed(PathBuf, std::io::Error),
    /// Invalid library path.
    InvalidPath,
}

impl std::fmt::Display for PluginLoaderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LoadFailed(path, msg) => {
                write!(f, "Failed to load plugin from {}: {}", path.display(), msg)
            }
            Self::MissingSymbol(name) => write!(f, "Missing required symbol: {}", name),
            Self::InvalidMetadata => write!(f, "Invalid or null plugin metadata"),
            Self::CreateFailed => write!(f, "Plugin instance creation failed"),
            Self::PluginDirectoryNotFound => write!(f, "Plugin directory not found"),
            Self::DirectoryReadFailed(path, err) => {
                write!(f, "Failed to read directory {}: {}", path.display(), err)
            }
            Self::InvalidPath => write!(f, "Invalid plugin path"),
        }
    }
}

impl std::error::Error for PluginLoaderError {}

// -------------------------------------------------------------------------
// Helper functions
// -------------------------------------------------------------------------

/// Get the standard plugin directory.
///
/// Resolution order:
/// 1. `MIMIUM_PLUGIN_DIR` environment variable (if set)
/// 2. `$HOME/.mimium/plugins` (cross-platform default)
#[cfg(not(target_arch = "wasm32"))]
fn get_plugin_directory() -> Result<PathBuf, PluginLoaderError> {
    // Try environment variable first
    if let Ok(dir) = std::env::var("MIMIUM_PLUGIN_DIR") {
        return Ok(PathBuf::from(dir));
    }

    // Cross-platform default: $HOME/.mimium/plugins
    #[cfg(target_os = "windows")]
    let home = std::env::var("USERPROFILE").ok();
    #[cfg(not(target_os = "windows"))]
    let home = std::env::var("HOME").ok();

    home.map(|h| PathBuf::from(h).join(".mimium").join("plugins"))
        .ok_or(PluginLoaderError::PluginDirectoryNotFound)
}

/// Determine the correct library path with platform-specific extension.
#[cfg(not(target_arch = "wasm32"))]
fn get_library_path(base_path: &Path) -> Result<PathBuf, PluginLoaderError> {
    #[cfg(target_os = "windows")]
    let ext = "dll";

    #[cfg(target_os = "linux")]
    let ext = "so";

    #[cfg(target_os = "macos")]
    let ext = "dylib";

    Ok(base_path.with_extension(ext))
}

/// Check if a path is a library file.
#[cfg(not(target_arch = "wasm32"))]
fn is_library_file(path: &Path) -> bool {
    if let Some(ext) = path.extension() {
        #[cfg(target_os = "windows")]
        return ext == "dll";

        #[cfg(target_os = "linux")]
        return ext == "so";

        #[cfg(target_os = "macos")]
        return ext == "dylib";
    }
    false
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn test_loader_creation() {
        let loader = PluginLoader::new();
        assert_eq!(loader.loaded_plugins().len(), 0);
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn test_library_path() {
        let base = Path::new("/test/plugin");
        let lib_path = get_library_path(base).unwrap();

        #[cfg(target_os = "windows")]
        assert_eq!(lib_path, Path::new("/test/plugin.dll"));

        #[cfg(target_os = "linux")]
        assert_eq!(lib_path, Path::new("/test/plugin.so"));

        #[cfg(target_os = "macos")]
        assert_eq!(lib_path, Path::new("/test/plugin.dylib"));
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn test_is_library_file() {
        #[cfg(target_os = "windows")]
        {
            assert!(is_library_file(Path::new("plugin.dll")));
            assert!(!is_library_file(Path::new("plugin.so")));
        }

        #[cfg(target_os = "linux")]
        {
            assert!(is_library_file(Path::new("plugin.so")));
            assert!(!is_library_file(Path::new("plugin.dll")));
        }

        #[cfg(target_os = "macos")]
        {
            assert!(is_library_file(Path::new("plugin.dylib")));
            assert!(!is_library_file(Path::new("plugin.so")));
        }
    }
}
