// Wasmtime execution engine utilities
//
// This module provides higher-level execution utilities for the WASM runtime.

use super::{WasmModule, WasmRuntime};
use crate::runtime::primitives::Word;

/// High-level WASM execution engine
pub struct WasmEngine {
    runtime: WasmRuntime,
    current_module: Option<WasmModule>,
}

impl WasmEngine {
    /// Create a new WASM execution engine
    pub fn new() -> Result<Self, String> {
        let runtime = WasmRuntime::new()?;
        Ok(Self {
            runtime,
            current_module: None,
        })
    }

    /// Load a WASM module for execution
    pub fn load_module(&mut self, wasm_bytes: &[u8]) -> Result<(), String> {
        let module = self.runtime.load_module(wasm_bytes)?;
        self.current_module = Some(module);
        Ok(())
    }

    /// Execute the 'dsp' function (main audio processing function)
    pub fn execute_dsp(&mut self, inputs: &[Word]) -> Result<Vec<Word>, String> {
        let module = self
            .current_module
            .as_mut()
            .ok_or("No WASM module loaded")?;

        module.call_function("dsp", inputs)
    }

    /// Execute a named function
    pub fn execute_function(&mut self, name: &str, args: &[Word]) -> Result<Vec<Word>, String> {
        let module = self
            .current_module
            .as_mut()
            .ok_or("No WASM module loaded")?;

        module.call_function(name, args)
    }
}

impl Default for WasmEngine {
    fn default() -> Self {
        Self::new().expect("Failed to create WASM engine")
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
