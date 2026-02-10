//! C-compatible FFI layer for plugin communication.
//!
//! This module defines the stable ABI that plugins use to interact with the
//! runtime.  Instead of depending on [`vm::Machine`] directly, plugins receive
//! a [`RuntimeHandle`] whose function-pointer table ([`RuntimeVTable`]) is
//! backed by either the native VM or the WASM engine.
//!
//! # Design rationale
//!
//! *   **`RuntimeHandle`** wraps an opaque pointer together with a static
//!     vtable.  The pointer addresses the concrete backend (e.g. `Machine` or
//!     `WasmModule`) while the vtable provides generic accessors.
//!
//! *   **`RuntimeVTable`** contains only *generic* operations that every plugin
//!     may require — argument extraction, return-value writing, and time
//!     queries.  Plugin-specific actions (e.g. scheduling closures) live in the
//!     plugin's own API.
//!
//! *   The `extern "C"` calling convention and `#[repr(C)]` layout guarantee
//!     ABI stability across dynamic libraries and across languages.

use std::ffi::{c_char, c_void};

// -------------------------------------------------------------------------
// Core FFI types
// -------------------------------------------------------------------------

/// Opaque, C-compatible handle passed to every plugin function.
///
/// The handle bundles a raw pointer to the actual runtime with a vtable of
/// accessor functions.  Plugins never dereference `runtime_ptr` directly;
/// instead they call through the [`RuntimeVTable`].
#[repr(C)]
pub struct RuntimeHandle {
    /// Opaque pointer to the concrete runtime (VM `Machine`, WASM store, …).
    pub(crate) runtime_ptr: *mut c_void,
    /// Static function table.
    pub(crate) vtable: &'static RuntimeVTable,
}

/// Function-pointer table for generic runtime operations.
///
/// Every entry uses the `extern "C"` ABI so that the vtable can be shared
/// across FFI boundaries (dynamic libraries, WASM host imports, etc.).
///
/// Note: Time and sample rate are not included here as they are managed at
/// the driver level and passed through `on_sample` callbacks when needed.
#[repr(C)]
pub struct RuntimeVTable {
    /// Read a float argument at `idx` (0-based, relative to the current
    /// call frame).
    pub get_arg_f64: unsafe extern "C" fn(rt: *mut c_void, idx: u32) -> f64,

    /// Write a float return value at `idx`.
    pub set_return_f64: unsafe extern "C" fn(rt: *mut c_void, idx: u32, value: f64),

    /// Read a string argument at `idx`.
    ///
    /// Returns a pointer to a null-terminated UTF-8 string that is valid for
    /// the duration of the current plugin call.  Returns null when the index
    /// is out of bounds.
    pub get_arg_string: unsafe extern "C" fn(rt: *mut c_void, idx: u32) -> *const c_char,
}

// SAFETY: RuntimeHandle is only accessed on a single thread at a time (the
// audio thread or the main thread) and every raw pointer it contains is
// thread-confined in practice.
unsafe impl Send for RuntimeHandle {}

// -------------------------------------------------------------------------
// Safe Rust API over the raw handle
// -------------------------------------------------------------------------

impl RuntimeHandle {
    /// Create a new handle from a raw pointer and a vtable.
    ///
    /// # Safety
    ///
    /// `runtime_ptr` must point to a valid object whose lifetime exceeds that
    /// of the returned handle.  The vtable functions must be consistent with
    /// the runtime type behind the pointer.
    pub unsafe fn new(runtime_ptr: *mut c_void, vtable: &'static RuntimeVTable) -> Self {
        Self {
            runtime_ptr,
            vtable,
        }
    }

    /// Read a `f64` argument at position `idx`.
    #[inline]
    pub fn get_arg_f64(&self, idx: u32) -> f64 {
        unsafe { (self.vtable.get_arg_f64)(self.runtime_ptr, idx) }
    }

    /// Write a `f64` return value at position `idx`.
    #[inline]
    pub fn set_return_f64(&mut self, idx: u32, value: f64) {
        unsafe { (self.vtable.set_return_f64)(self.runtime_ptr, idx, value) }
    }

    /// Read a string argument at position `idx`.
    ///
    /// Returns `None` when the vtable function returns a null pointer.
    pub fn get_arg_string(&self, idx: u32) -> Option<String> {
        let ptr = unsafe { (self.vtable.get_arg_string)(self.runtime_ptr, idx) };
        if ptr.is_null() {
            None
        } else {
            Some(
                unsafe { std::ffi::CStr::from_ptr(ptr) }
                    .to_string_lossy()
                    .into_owned(),
            )
        }
    }
}
