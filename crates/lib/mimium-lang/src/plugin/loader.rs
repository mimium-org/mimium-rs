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

// -------------------------------------------------------------------------
// Plugin function signatures
// -------------------------------------------------------------------------

/// Type of the `mimium_plugin_metadata` export.
type PluginMetadataFn = unsafe extern "C" fn() -> *const PluginMetadata;

/// Type of the `mimium_plugin_create` export.
type PluginCreateFn = unsafe extern "C" fn() -> *mut PluginInstance;

/// Type of the `mimium_plugin_destroy` export.
type PluginDestroyFn = unsafe extern "C" fn(instance: *mut PluginInstance);

/// Type of the `mimium_plugin_get_function` export.
///
/// Returns a function pointer for the named plugin function, or null if not found.
type PluginGetFunctionFn = unsafe extern "C" fn(name: *const c_char) -> Option<PluginFunctionFn>;

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
    /// # Arguments
    ///
    /// * `path` - Path to the plugin library (without extension).
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The library cannot be loaded
    /// - Required symbols are missing
    /// - Plugin metadata is invalid
    pub fn load_plugin<P: AsRef<Path>>(&mut self, path: P) -> Result<(), PluginLoaderError> {
        let path = path.as_ref();

        // Determine the platform-specific library extension
        let lib_path = get_library_path(path)?;

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
        let get_function_fn: Option<Symbol<PluginGetFunctionFn>> = unsafe {
            library.get(b"mimium_plugin_get_function\0").ok()
        };

        // Get metadata
        let metadata_ptr = unsafe { metadata_fn() };
        if metadata_ptr.is_null() {
            return Err(PluginLoaderError::InvalidMetadata);
        }
        let metadata = unsafe { (*metadata_ptr).clone() };

        // Create instance
        let instance = unsafe { create_fn() };
        if instance.is_null() {
            return Err(PluginLoaderError::CreateFailed);
        }

        // Copy the destroy function pointer before moving library
        let destroy_fn_ptr = *destroy_fn;
        let get_function_fn_ptr = get_function_fn.as_ref().map(|f| **f);

        // Store the loaded plugin
        let plugin = LoadedPlugin {
            _library: library,
            metadata,
            instance,
            destroy_fn: destroy_fn_ptr,
            get_function_fn: get_function_fn_ptr,
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
            let entry = entry
                .map_err(|e| PluginLoaderError::DirectoryReadFailed(plugin_dir.clone(), e))?;
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
#[cfg(not(target_arch = "wasm32"))]
fn get_plugin_directory() -> Result<PathBuf, PluginLoaderError> {
    // Try environment variable first
    if let Ok(dir) = std::env::var("MIMIUM_PLUGIN_DIR") {
        return Ok(PathBuf::from(dir));
    }

    // Use platform-specific defaults
    #[cfg(target_os = "windows")]
    {
        if let Ok(appdata) = std::env::var("APPDATA") {
            return Ok(PathBuf::from(appdata).join("mimium").join("plugins"));
        }
    }

    #[cfg(target_os = "linux")]
    {
        if let Ok(home) = std::env::var("HOME") {
            return Ok(PathBuf::from(home).join(".local").join("share").join("mimium").join("plugins"));
        }
    }

    #[cfg(target_os = "macos")]
    {
        if let Ok(home) = std::env::var("HOME") {
            return Ok(PathBuf::from(home)
                .join("Library")
                .join("Application Support")
                .join("mimium")
                .join("plugins"));
        }
    }

    Err(PluginLoaderError::PluginDirectoryNotFound)
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
