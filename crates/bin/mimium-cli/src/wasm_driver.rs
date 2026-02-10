//! WASM-based audio driver implementation
//! 
//! This module provides a Driver implementation that executes WASM modules
//! for audio processing, bridging the gap between WASM execution and the
//! mimium audio driver infrastructure.

use mimium_audiodriver::driver::{Driver, RuntimeData, SampleRate};
use mimium_lang::{
    compiler::IoChannelInfo,
    plugin::{ExtClsInfo, InstantPlugin},
    runtime::{wasm::engine::WasmEngine, vm, Time},
};
use std::sync::mpsc;

/// A Driver implementation that executes WASM modules for audio processing
pub struct WasmDriver {
    wasm_engine: Option<WasmEngine>,
    dsp_function_name: String,
    sample_rate: u32,
    current_sample: Time,
    is_playing: bool,
    /// Channel for VM-related communication (currently unused for WASM)
    vm_channel: Option<mpsc::Sender<vm::Program>>,
    /// Output buffer for testing
    output_buffer: Vec<f64>,
}

impl WasmDriver {
    /// Create a new WasmDriver
    pub fn new() -> Self {
        Self {
            wasm_engine: None,
            dsp_function_name: "dsp".to_string(),
            sample_rate: 48000,
            current_sample: Time(0),
            is_playing: false,
            vm_channel: None,
            output_buffer: Vec::new(),
        }
    }

    /// Set the WASM engine for this driver
    pub fn set_wasm_engine(&mut self, engine: WasmEngine) {
        self.wasm_engine = Some(engine);
    }

    /// Process audio samples using the WASM module
    /// This is called by the audio callback to generate samples
    pub fn process_sample(&mut self) -> Result<f64, String> {
        if let Some(engine) = &mut self.wasm_engine {
            // Call the dsp function with no arguments
            // The dsp function should return a single f64 sample
            let result = engine.execute_dsp(&[])?;
            
            // Increment sample counter
            self.current_sample.0 += 1;
            
            // Extract the f64 result
            if result.len() == 1 {
                let sample = f64::from_bits(result[0]);
                self.output_buffer.push(sample);
                Ok(sample)
            } else {
                Err(format!("DSP function returned {} values, expected 1", result.len()))
            }
        } else {
            Ok(0.0) // Return silence if no WASM module is loaded
        }
    }

    /// Get the output buffer for testing
    pub fn get_output_buffer(&self) -> &[f64] {
        &self.output_buffer
    }
}

impl Driver for WasmDriver {
    type Sample = f64;

    fn get_runtimefn_infos(&self) -> Vec<ExtClsInfo> {
        // WASM modules have their own import system, so we return empty
        Vec::new()
    }

    fn init(
        &mut self,
        _runtimedata: RuntimeData,
        manual_samplerate: Option<SampleRate>,
    ) -> Option<IoChannelInfo> {
        // Extract sample rate
        if let Some(sr) = manual_samplerate {
            self.sample_rate = sr.get();
        }

        // Note: We don't use RuntimeData for WASM since it's designed for the native VM
        // The WASM runtime manages its own state through the store

        // Return None since WASM driver doesn't use traditional I/O channels
        None
    }

    fn play(&mut self) -> bool {
        self.is_playing = true;
        true
    }

    fn pause(&mut self) -> bool {
        self.is_playing = false;
        true
    }

    fn renew_program(&mut self, _new_program: Box<dyn std::any::Any + Send>) {
        // WASM driver doesn't use the native VM
        // This method is a no-op for WASM
    }

    fn get_program_channel(&self) -> Option<mpsc::Sender<Box<dyn std::any::Any + Send>>> {
        None
    }

    fn get_samplerate(&self) -> u32 {
        self.sample_rate
    }

    fn get_current_sample(&self) -> Time {
        self.current_sample
    }

    fn is_playing(&self) -> bool {
        self.is_playing
    }
}
