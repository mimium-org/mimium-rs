use crate::{
    compiler::IoChannelInfo,
    utils::{error::ReportableError, metadata::Location},
};
use thiserror::Error;

pub mod ffi;
pub mod primitives;
pub mod vm;
pub mod vm_ffi;
pub mod wasm;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time(pub u64);

/// Return code from DSP execution (alias for vm::ReturnCode).
pub type ReturnCode = i64;

/// Type-safe representation of programs that can be hot-swapped into a runtime.
///
/// This replaces the previous `Box<dyn Any + Send>` approach with an explicit
/// enum, providing compile-time type safety and eliminating runtime downcasts.
pub enum ProgramPayload {
    /// Native bytecode VM program.
    VmProgram(vm::Program),
    /// WASM module bytes.
    WasmModule(Vec<u8>),
}

/// Abstraction over per-sample DSP execution backends.
///
/// This trait decouples audio drivers from a concrete runtime implementation,
/// allowing both the native bytecode VM and the WASM backend to be used
/// interchangeably with the same `Driver` infrastructure (cpal, CSV, etc.).
pub trait DspRuntime {
    /// Execute the DSP function for one sample tick.
    fn run_dsp(&mut self, time: Time) -> ReturnCode;

    /// Read the output produced by the last `run_dsp` call.
    ///
    /// The returned slice should contain at least `n_channels` elements.
    fn get_output(&self, n_channels: usize) -> &[f64];

    /// Write input samples that will be available during the next `run_dsp` call.
    fn set_input(&mut self, input: &[f64]);

    /// I/O channel configuration (None if the dsp function was not found).
    fn io_channels(&self) -> Option<IoChannelInfo>;

    /// Attempt to hot-swap the running program.
    ///
    /// Takes a type-safe `ProgramPayload` which can be either a native VM
    /// program or WASM module bytes. Returns `true` on success.
    fn try_hot_swap(&mut self, new_program: ProgramPayload) -> bool {
        let _ = new_program;
        false
    }

    /// Upcast to `Any` so callers can downcast to a concrete runtime type.
    fn as_any(&self) -> &dyn std::any::Any;
    /// Mutable upcast to `Any`.
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any;
}

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error("Unknown Error")]
    Unknown,
}

#[derive(Debug, Error)]
#[error("Runtime Error: {0}")]
pub struct RuntimeError(pub ErrorKind, pub Location);

impl ReportableError for RuntimeError {
    fn get_labels(&self) -> Vec<(crate::utils::metadata::Location, String)> {
        vec![(self.1.clone(), self.0.to_string())]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Compile-time assertion that `ProgramPayload` implements `Send`.
    #[test]
    fn ensure_payload_is_send() {
        fn assert_send<T: Send>() {}
        assert_send::<ProgramPayload>();
    }
}
