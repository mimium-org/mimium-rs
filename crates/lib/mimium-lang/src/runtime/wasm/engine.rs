// Wasmtime execution engine utilities
//
// This module provides higher-level execution utilities for the WASM runtime.

use super::{WasmModule, WasmRuntime};
use crate::compiler::IoChannelInfo;
use crate::runtime::primitives::Word;
use crate::runtime::{DspRuntime, ProgramPayload, ReturnCode, Time};

/// High-level WASM execution engine
pub struct WasmEngine {
    runtime: WasmRuntime,
    current_module: Option<WasmModule>,
    /// Cached dsp function for fast per-sample execution
    dsp_func: Option<wasmtime::Func>,
}

impl WasmEngine {
    /// Create a new WASM execution engine
    pub fn new() -> Result<Self, String> {
        let runtime = WasmRuntime::new()?;
        Ok(Self {
            runtime,
            current_module: None,
            dsp_func: None,
        })
    }

    /// Load a WASM module for execution
    pub fn load_module(&mut self, wasm_bytes: &[u8]) -> Result<(), String> {
        let mut module = self.runtime.load_module(wasm_bytes)?;

        // Cache the dsp function for fast per-sample execution
        self.dsp_func = module.get_or_cache_function("dsp").ok();

        self.current_module = Some(module);
        Ok(())
    }

    /// Execute the 'dsp' function (main audio processing function)
    pub fn execute_dsp(&mut self, inputs: &[Word]) -> Result<Vec<Word>, String> {
        let module = self
            .current_module
            .as_mut()
            .ok_or("No WASM module loaded")?;

        // Use cached dsp function if available, otherwise fall back to lookup
        if let Some(func) = &self.dsp_func {
            module.call_func_direct(func, inputs)
        } else {
            module.call_function("dsp", inputs)
        }
    }

    /// Execute a named function
    pub fn execute_function(&mut self, name: &str, args: &[Word]) -> Result<Vec<Word>, String> {
        let module = self
            .current_module
            .as_mut()
            .ok_or("No WASM module loaded")?;

        module.call_function(name, args)
    }

    /// Get mutable access to the current module (if loaded).
    pub fn current_module_mut(&mut self) -> Option<&mut super::WasmModule> {
        self.current_module.as_mut()
    }

    /// Read an f64 value from the WASM module's linear memory at the given byte offset.
    pub fn read_memory_f64(&mut self, offset: usize) -> Result<f64, String> {
        self.current_module
            .as_mut()
            .ok_or_else(|| "No WASM module loaded".to_string())
            .and_then(|m| m.read_memory_f64(offset))
    }
}

impl Default for WasmEngine {
    fn default() -> Self {
        Self::new().expect("Failed to create WASM engine")
    }
}

/// [`DspRuntime`] implementation backed by a compiled WASM module.
///
/// This wraps a [`WasmEngine`] and exposes the same per-sample DSP interface
/// that the native VM runtime provides, so that audio drivers (cpal, CSV, etc.)
/// can work with either backend transparently.
pub struct WasmDspRuntime {
    engine: WasmEngine,
    io_channels: Option<IoChannelInfo>,
    /// Cached output buffer filled after each `run_dsp` call.
    output_cache: Vec<f64>,
    /// Input buffer passed to the DSP function on the next tick.
    input_cache: Vec<f64>,
    sample_rate: f64,
}

impl WasmDspRuntime {
    /// Create a new WASM DSP runtime from a loaded engine and I/O info.
    ///
    /// `engine` must already have a WASM module loaded via
    /// [`WasmEngine::load_module`].
    pub fn new(engine: WasmEngine, io_channels: Option<IoChannelInfo>) -> Self {
        let ochannels = io_channels.map_or(0, |io| io.output as usize);
        let ichannels = io_channels.map_or(0, |io| io.input as usize);
        Self {
            engine,
            io_channels,
            output_cache: vec![0.0; ochannels],
            input_cache: vec![0.0; ichannels],
            sample_rate: 48000.0,
        }
    }

    /// Set the sample rate used by the runtime.
    pub fn set_sample_rate(&mut self, sr: f64) {
        self.sample_rate = sr;
        if let Some(module) = self.engine.current_module_mut() {
            if let Some(state) = module.get_runtime_state_mut() {
                state.sample_rate = sr;
            }
        }
    }

    /// Run the `mimium_main` (or global init) function if exported.
    pub fn run_main(&mut self) -> Result<(), String> {
        // Try "main" first (global initializer), then fall back to "mimium_main"
        match self.engine.execute_function("main", &[]) {
            Ok(_) => Ok(()),
            Err(e) if e.contains("not found") => {
                // Try old name for compatibility
                match self.engine.execute_function("mimium_main", &[]) {
                    Ok(_) => Ok(()),
                    Err(e) if e.contains("not found") => Ok(()), // no main — that's fine
                    Err(e) => Err(e),
                }
            }
            Err(e) => Err(e),
        }
    }
}

impl DspRuntime for WasmDspRuntime {
    fn run_dsp(&mut self, time: Time) -> ReturnCode {
        // Update current_time in the WASM runtime state.
        if let Some(module) = self.engine.current_module_mut() {
            if let Some(state) = module.get_runtime_state_mut() {
                state.current_time = time.0;
            }
        }

        // Convert input samples to Words (bit-cast f64 → u64).
        let args: Vec<Word> = self.input_cache.iter().map(|v| v.to_bits()).collect();

        let out_channels = self.io_channels.map_or(1, |io| io.output as usize);

        match self.engine.execute_dsp(&args) {
            Ok(result) => {
                self.output_cache.clear();
                if out_channels > 1 {
                    // Multi-channel (stereo, etc.): dsp() returns an i64 pointer
                    // to a tuple in linear memory. Dereference each element.
                    if let Some(&ptr_word) = result.first() {
                        let ptr = ptr_word as usize;
                        for ch in 0..out_channels {
                            let val = self
                                .engine
                                .read_memory_f64(ptr + ch * 8)
                                .unwrap_or(0.0);
                            self.output_cache.push(val);
                        }
                    }
                } else {
                    // Mono: dsp() returns a single f64 directly.
                    self.output_cache
                        .extend(result.iter().map(|&w| f64::from_bits(w)));
                }
                0 // success
            }
            Err(e) => {
                log::error!("WASM DSP execution error: {e}");
                -1
            }
        }
    }

    fn get_output(&self, n_channels: usize) -> &[f64] {
        &self.output_cache[..n_channels.min(self.output_cache.len())]
    }

    fn set_input(&mut self, input: &[f64]) {
        let copy_len = input.len().min(self.input_cache.len());
        self.input_cache[..copy_len].copy_from_slice(&input[..copy_len]);
    }

    fn io_channels(&self) -> Option<IoChannelInfo> {
        self.io_channels
    }

    fn try_hot_swap(&mut self, new_program: ProgramPayload) -> bool {
        if let ProgramPayload::WasmModule(wasm_bytes) = new_program {
            match self.engine.load_module(&wasm_bytes) {
                Ok(()) => {
                    // Re-run main after hot-swap.
                    let _ = self.run_main();
                    true
                }
                Err(e) => {
                    log::error!("WASM hot-swap failed: {e}");
                    false
                }
            }
        } else {
            false
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wasm_engine_create() {
        let engine = WasmEngine::new();
        assert!(engine.is_ok(), "Should create WASM engine");
    }

    #[test]
    fn test_wasm_engine_load_and_call() {
        let mut engine = WasmEngine::new().unwrap();

        // Simple WASM module with an add function
        let wasm_bytes = wat::parse_str(
            r#"
            (module
                (memory (export "memory") 1)
                (func (export "add") (param i64 i64) (result i64)
                    local.get 0
                    local.get 1
                    i64.add
                )
            )
            "#,
        )
        .expect("Failed to parse WAT");

        engine.load_module(&wasm_bytes).expect("Should load module");

        let result = engine.execute_function("add", &[10, 20]);
        assert!(result.is_ok(), "Should execute add function");
        assert_eq!(result.unwrap(), vec![30], "Should compute 10 + 20 = 30");
    }
}
