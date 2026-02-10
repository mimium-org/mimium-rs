//! VM-backed implementation of [`RuntimeVTable`].
//!
//! This module provides the vtable that bridges the generic
//! [`RuntimeHandle`](super::ffi::RuntimeHandle) API to the native bytecode VM
//! [`Machine`](super::vm::Machine).
//!
//! The vtable is a `static` value; each `extern "C"` function casts the opaque
//! `*mut c_void` back to `&mut Machine` and delegates to the existing Machine
//! API.

use std::ffi::{CString, c_char, c_void};

use super::ffi::{RuntimeHandle, RuntimeVTable};
use super::vm::Machine;

// -------------------------------------------------------------------------
// VTable implementation for the native VM
// -------------------------------------------------------------------------

unsafe extern "C" fn vm_get_arg_f64(rt: *mut c_void, idx: u32) -> f64 {
    let machine = unsafe { &*(rt as *const Machine) };
    Machine::get_as::<f64>(machine.get_stack(idx as i64))
}

unsafe extern "C" fn vm_set_return_f64(rt: *mut c_void, idx: u32, value: f64) {
    let machine = unsafe { &mut *(rt as *mut Machine) };
    machine.set_stack(idx as i64, Machine::to_value(value));
}

unsafe extern "C" fn vm_get_arg_string(rt: *mut c_void, idx: u32) -> *const c_char {
    let machine = unsafe { &*(rt as *const Machine) };
    let raw = machine.get_stack(idx as i64);
    let string_idx = raw as usize;
    match machine.prog.strings.get(string_idx) {
        Some(s) => {
            // The returned pointer must outlive the current plugin call.
            // We leak a CString here; in a production build the caller or a
            // per-call arena should free it.  For now this is acceptable
            // because string arguments are rare and short-lived.
            match CString::new(s.as_str()) {
                Ok(cs) => cs.into_raw() as *const c_char,
                Err(_) => std::ptr::null(),
            }
        }
        None => std::ptr::null(),
    }
}

/// Static vtable for the native VM.
pub static VM_RUNTIME_VTABLE: RuntimeVTable = RuntimeVTable {
    get_arg_f64: vm_get_arg_f64,
    set_return_f64: vm_set_return_f64,
    get_arg_string: vm_get_arg_string,
};

/// Create a [`RuntimeHandle`] that wraps a mutable reference to a [`Machine`].
///
/// # Safety
///
/// The caller must ensure that `machine` outlives the returned handle and that
/// no other mutable alias exists while the handle is in use.
pub unsafe fn runtime_handle_from_machine(machine: &mut Machine) -> RuntimeHandle {
    unsafe {
        RuntimeHandle::new(
            machine as *mut Machine as *mut c_void,
            &VM_RUNTIME_VTABLE,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vtable_has_all_entries() {
        // Compile-time check: the static vtable must initialize every field.
        let _vt: &RuntimeVTable = &VM_RUNTIME_VTABLE;
    }
}
