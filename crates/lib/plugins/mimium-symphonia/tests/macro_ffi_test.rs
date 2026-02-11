//! Test that the macro FFI functions work correctly.
//!
//! **IMPORTANT:** These tests must be run sequentially with `--test-threads=1`
//! due to SESSION_GLOBALS mutex contention when running in parallel.
//!
//! Run with: `cargo test --test macro_ffi_test -- --test-threads=1`

use mimium_lang::runtime::ffi_serde::serialize_macro_args;

#[test]
fn test_basic_serialization() {
    // Just test that we can serialize an empty args vector
    let args: Vec<(
        mimium_lang::interpreter::Value,
        mimium_lang::interner::TypeNodeId,
    )> = vec![];
    let bytes = serialize_macro_args(&args).unwrap();
    assert!(!bytes.is_empty() || args.is_empty());
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn test_make_sampler_mono_direct() {
    // Direct call (statically linked)
    use mimium_lang::ast::Expr;
    use mimium_lang::interner::ToSymbol;
    use mimium_lang::interpreter::Value;
    use mimium_lang::types::{PType, Type};
    use mimium_symphonia::SamplerPlugin;

    let mut plugin = SamplerPlugin::default();

    // Prepare arguments: a file path as a string
    let args = vec![(
        Value::String("test.wav".to_symbol()),
        Type::Primitive(PType::String).into_id(),
    )];

    // Call the macro function directly
    let result = plugin.make_sampler_mono(&args);

    // Result should be Value::Code containing a Lambda expression
    match result {
        Value::Code(expr_id) => {
            let expr = expr_id.to_expr();
            assert!(matches!(expr, Expr::Lambda(..)));
        }
        other => panic!("Expected Value::Code, got {other:?}"),
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn test_make_sampler_mono_serialization() {
    // Test that serialization/deserialization works
    use mimium_lang::interner::ToSymbol;
    use mimium_lang::interpreter::Value;
    use mimium_lang::runtime::ffi_serde::{deserialize_macro_args, serialize_macro_args};
    use mimium_lang::types::{PType, Type};

    let args = vec![(
        Value::String("test.wav".to_symbol()),
        Type::Primitive(PType::String).into_id(),
    )];

    // Serialize
    let bytes = serialize_macro_args(&args).unwrap();
    assert!(!bytes.is_empty());

    // Deserialize
    let decoded_args = deserialize_macro_args(&bytes).unwrap();
    assert_eq!(decoded_args.len(), 1);

    match &decoded_args[0].0 {
        Value::String(s) => {
            assert_eq!(s.to_string(), "test.wav");
        }
        _ => panic!("Expected String value"),
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn test_ffi_make_sampler_mono() {
    // Test the FFI function directly
    use mimium_lang::interner::ToSymbol;
    use mimium_lang::interpreter::Value;
    use mimium_lang::runtime::ffi_serde::{deserialize_value, serialize_macro_args};
    use mimium_lang::types::{PType, Type};
    use mimium_symphonia::{SamplerPlugin, ffi_make_sampler_mono};

    let mut plugin = SamplerPlugin::default();
    let args = vec![(
        Value::String("test.wav".to_symbol()),
        Type::Primitive(PType::String).into_id(),
    )];

    // Serialize arguments
    let args_bytes = serialize_macro_args(&args).expect("Failed to serialize args");

    // Prepare output buffers
    let mut out_ptr: *mut u8 = std::ptr::null_mut();
    let mut out_len: usize = 0;

    // Call FFI function
    let result_code = unsafe {
        ffi_make_sampler_mono(
            &mut plugin as *mut _ as *mut std::ffi::c_void,
            args_bytes.as_ptr(),
            args_bytes.len(),
            &mut out_ptr,
            &mut out_len,
        )
    };

    assert_eq!(result_code, 0, "FFI call should succeed");
    assert!(!out_ptr.is_null(), "Output pointer should not be null");
    assert!(out_len > 0, "Output length should be positive");

    // Deserialize the result
    let out_bytes = unsafe { std::slice::from_raw_parts(out_ptr, out_len) };
    let result_value = deserialize_value(out_bytes).expect("Failed to deserialize result");

    // Clean up allocated output buffer
    unsafe {
        let _boxed = Box::from_raw(std::slice::from_raw_parts_mut(out_ptr, out_len));
    }

    // Should return a Code value
    assert!(matches!(result_value, Value::Code(_)));
}
