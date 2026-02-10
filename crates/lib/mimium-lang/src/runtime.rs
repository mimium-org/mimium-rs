use crate::{
    compiler::IoChannelInfo,
    utils::{error::ReportableError, metadata::Location},
};
use thiserror::Error;

pub mod primitives;
pub mod vm;
pub mod wasm;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time(pub u64);

/// Return code from DSP execution (alias for vm::ReturnCode).
pub type ReturnCode = i64;

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
    /// The concrete type inside `new_program` depends on the runtime: `vm::Program`
    /// for the native VM, `Vec<u8>` (WASM bytes) for the WASM backend, etc.
    /// Returns `true` on success.
    fn try_hot_swap(&mut self, new_program: Box<dyn std::any::Any + Send>) -> bool {
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
