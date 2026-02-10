//! Test to verify that SessionGlobals operations don't cause deadlocks
//! during serialization.

use mimium_lang::interner::ToSymbol;
use mimium_lang::interpreter::Value;
use mimium_lang::types::{PType, Type};
use mimium_lang::runtime::ffi_serde::{serialize_macro_args, deserialize_macro_args};

#[test]
fn test_01_empty_serialization() {
    println!("[TEST 1] Empty serialization");
    let args: Vec<(Value, mimium_lang::interner::TypeNodeId)> = vec![];
    let bytes = serialize_macro_args(&args).unwrap();
    println!("[TEST 1] Success: {} bytes", bytes.len());
}

#[test]
fn test_02_symbol_creation() {
    println!("[TEST 2] Creating symbol");
    let sym = "test".to_symbol();
    println!("[TEST 2] Symbol created: {:?}", sym);
}

#[test]
fn test_03_type_into_id() {
    println!("[TEST 3] Type into_id");
    let ty_id = Type::Primitive(PType::String).into_id();
    println!("[TEST 3] TypeNodeId created: {:?}", ty_id);
}

#[test]
fn test_04_value_with_symbol() {
    println!("[TEST 4] Creating Value with Symbol");
    let sym = "test".to_symbol();
    println!("[TEST 4] Symbol created");
    let value = Value::String(sym);
    println!("[TEST 4] Value created: {:?}", value);
}

#[test]
fn test_05_serialize_value_with_symbol() {
    println!("[TEST 5] Serializing Value with Symbol");
    
    println!("[TEST 5] Step 1: Creating symbol");
    let sym = "test".to_symbol();
    
    println!("[TEST 5] Step 2: Creating type ID");
    let ty_id = Type::Primitive(PType::String).into_id();
    
    println!("[TEST 5] Step 3: Creating value");
    let value = Value::String(sym);
    
    println!("[TEST 5] Step 4: Creating args vector");
    let args = vec![(value, ty_id)];
    
    println!("[TEST 5] Step 5: Calling serialize_macro_args");
    let bytes = serialize_macro_args(&args).unwrap();
    
    println!("[TEST 5] Success: {} bytes", bytes.len());
}

#[test]
fn test_06_deserialize_value_with_symbol() {
    println!("[TEST 6] Deserializing Value with Symbol");
    
    // Serialize first
    let sym = "test".to_symbol();
    let ty_id = Type::Primitive(PType::String).into_id();
    let value = Value::String(sym);
    let args = vec![(value, ty_id)];
    let bytes = serialize_macro_args(&args).unwrap();
    
    println!("[TEST 6] Serialized, now deserializing");
    let decoded = deserialize_macro_args(&bytes).unwrap();
    
    println!("[TEST 6] Success: {} args decoded", decoded.len());
}
