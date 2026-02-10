//! FFI helpers for serializing macro function arguments across plugin boundaries.
//!
//! **IMPORTANT**: Dynamic plugins run in separate address spaces and cannot access
//! the parent process's SessionGlobals (symbol interner). Therefore, we must convert
//! Symbol references to actual String data when crossing FFI boundaries.
//!
//! FfiValue is an FFI-safe representation of Value that uses actual Strings
//! instead of Symbol IDs.

use crate::interner::{ToSymbol, TypeNodeId};
use crate::interpreter::Value;
use serde::{Deserialize, Serialize};

/// FFI-safe Value representation that uses actual Strings instead of Symbol IDs.
///
/// This type is used for communication with dynamic plugins, which don't share
/// the same symbol interner as the host process.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FfiValue {
    ErrorV, // Can't serialize ExprNodeId across FFI, so just mark as error
    Unit,
    Number(f64),
    String(String), // Real String, not Symbol!
    Array(Vec<FfiValue>),
    Tuple(Vec<FfiValue>),
    Record(Vec<(String, FfiValue)>), // String key instead of Symbol
    Code,                            // Can't serialize ExprNodeId, just a marker
    TaggedUnion(u64, Box<FfiValue>),
    // Note: Closures, Fixpoints, ExternalFn, Store, and ConstructorFn
    // cannot be safely serialized across FFI boundaries
}

impl FfiValue {
    /// Convert FfiValue back to Value by interning strings into Symbols.
    pub fn to_value(self) -> Value {
        match self {
            FfiValue::ErrorV => Value::Unit, // Best effort
            FfiValue::Unit => Value::Unit,
            FfiValue::Number(n) => Value::Number(n),
            FfiValue::String(s) => Value::String(s.to_symbol()),
            FfiValue::Array(arr) => Value::Array(arr.into_iter().map(|v| v.to_value()).collect()),
            FfiValue::Tuple(t) => Value::Tuple(t.into_iter().map(|v| v.to_value()).collect()),
            FfiValue::Record(fields) => Value::Record(
                fields
                    .into_iter()
                    .map(|(k, v)| (k.to_symbol(), v.to_value()))
                    .collect(),
            ),
            FfiValue::Code => Value::Unit, // Best effort - can't reconstruct ExprNodeId
            FfiValue::TaggedUnion(tag, val) => Value::TaggedUnion(tag, Box::new(val.to_value())),
        }
    }
}

impl Value {
    /// Convert Value to FFI-safe FfiValue by expanding Symbols to Strings.
    ///
    /// Returns Err if the Value contains types that cannot cross FFI boundaries
    /// (Closures, ExternalFn, Fixpoint, Store, ConstructorFn).
    pub fn to_ffi_value(&self) -> Result<FfiValue, String> {
        match self {
            Value::ErrorV(_) => Ok(FfiValue::ErrorV),
            Value::Unit => Ok(FfiValue::Unit),
            Value::Number(n) => Ok(FfiValue::Number(*n)),
            Value::String(sym) => Ok(FfiValue::String(sym.as_str().to_string())),
            Value::Array(arr) => {
                let ffi_arr: Result<Vec<_>, _> = arr.iter().map(|v| v.to_ffi_value()).collect();
                Ok(FfiValue::Array(ffi_arr?))
            }
            Value::Tuple(t) => {
                let ffi_tuple: Result<Vec<_>, _> = t.iter().map(|v| v.to_ffi_value()).collect();
                Ok(FfiValue::Tuple(ffi_tuple?))
            }
            Value::Record(fields) => {
                let ffi_fields: Result<Vec<_>, _> = fields
                    .iter()
                    .map(|(k, v)| {
                        v.to_ffi_value()
                            .map(|ffi_v| (k.as_str().to_string(), ffi_v))
                    })
                    .collect();
                Ok(FfiValue::Record(ffi_fields?))
            }
            Value::Code(_) => Ok(FfiValue::Code),
            Value::TaggedUnion(tag, val) => {
                Ok(FfiValue::TaggedUnion(*tag, Box::new(val.to_ffi_value()?)))
            }
            Value::Closure(_, _, _) => {
                Err("Closures cannot be serialized across FFI boundaries".to_string())
            }
            Value::Fixpoint(_, _) => {
                Err("Fixpoints cannot be serialized across FFI boundaries".to_string())
            }
            Value::ExternalFn(_) => {
                Err("External functions cannot be serialized across FFI boundaries".to_string())
            }
            Value::Store(_) => {
                Err("Mutable stores cannot be serialized across FFI boundaries".to_string())
            }
            Value::ConstructorFn(_, _, _) => {
                Err("Constructor functions cannot be serialized across FFI boundaries".to_string())
            }
        }
    }
}

/// Serialize macro arguments (Vec<(Value, TypeNodeId)>) for FFI transmission.
///
/// Converts Symbols to Strings before serialization to support dynamic plugins.
/// Returns Err if the arguments contain values that cannot be serialized
/// (closures, external functions, or mutable references).
pub fn serialize_macro_args(args: &[(Value, TypeNodeId)]) -> Result<Vec<u8>, String> {
    // Convert to FFI-safe representation
    let ffi_args: Result<Vec<_>, _> = args
        .iter()
        .map(|(val, ty)| val.to_ffi_value().map(|ffi_val| (ffi_val, *ty)))
        .collect();

    let ffi_args = ffi_args?;
    bincode::serialize(&ffi_args).map_err(|e| format!("Failed to serialize macro arguments: {}", e))
}

/// Deserialize macro arguments from FFI.
///
/// Converts Strings back to Symbols using the current interner.
pub fn deserialize_macro_args(data: &[u8]) -> Result<Vec<(Value, TypeNodeId)>, String> {
    let ffi_args: Vec<(FfiValue, TypeNodeId)> = bincode::deserialize(data)
        .map_err(|e| format!("Failed to deserialize macro arguments: {}", e))?;

    // Convert back to normal Value by interning strings
    Ok(ffi_args
        .into_iter()
        .map(|(ffi_val, ty)| (ffi_val.to_value(), ty))
        .collect())
}

/// Serialize a return value for FFI transmission.
///
/// Converts Symbols to Strings before serialization.
pub fn serialize_value(value: &Value) -> Result<Vec<u8>, String> {
    let ffi_value = value.to_ffi_value()?;
    bincode::serialize(&ffi_value).map_err(|e| format!("Failed to serialize return value: {e}"))
}

/// Deserialize a return value from FFI.
///
/// Converts Strings back to Symbols using the current interner.
pub fn deserialize_value(data: &[u8]) -> Result<Value, String> {
    let ffi_value: FfiValue = bincode::deserialize(data)
        .map_err(|e| format!("Failed to deserialize return value: {e}"))?;
    Ok(ffi_value.to_value())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr;
    use crate::interner::ToSymbol;
    use crate::types::{PType, Type};

    #[test]
    fn test_roundtrip_number() {
        let val = Value::Number(42.5);
        let bytes = serialize_value(&val).unwrap();
        let decoded = deserialize_value(&bytes).unwrap();
        assert!(matches!(decoded, Value::Number(n) if (n - 42.5).abs() < 1e-10));
    }

    #[test]
    fn test_roundtrip_string() {
        let val = Value::String("hello".to_symbol());
        let bytes = serialize_value(&val).unwrap();
        let decoded = deserialize_value(&bytes).unwrap();
        if let Value::String(s) = decoded {
            assert_eq!(s.as_str(), "hello");
        } else {
            panic!("Expected String value");
        }
    }

    #[test]
    fn test_roundtrip_array() {
        let val = Value::Array(vec![Value::Number(1.0), Value::Number(2.0), Value::Unit]);
        let bytes = serialize_value(&val).unwrap();
        let decoded = deserialize_value(&bytes).unwrap();
        if let Value::Array(arr) = decoded {
            assert_eq!(arr.len(), 3);
        } else {
            panic!("Expected Array value");
        }
    }

    #[test]
    fn test_roundtrip_code() {
        use crate::ast::Literal;
        let expr = Expr::Literal(Literal::Int(42)).into_id_without_span();
        let val = Value::Code(expr);
        let bytes = serialize_value(&val).unwrap();
        let decoded = deserialize_value(&bytes).unwrap();
        if let Value::Code(id) = decoded {
            assert_eq!(id.to_expr(), Expr::Literal(Literal::Int(42)));
        } else {
            panic!("Expected Code value");
        }
    }

    #[test]
    fn test_roundtrip_macro_args() {
        let args = vec![
            (
                Value::Number(1.0),
                Type::Primitive(PType::Numeric).into_id(),
            ),
            (
                Value::String("test".to_symbol()),
                Type::Primitive(PType::String).into_id(),
            ),
        ];
        let bytes = serialize_macro_args(&args).unwrap();
        let decoded = deserialize_macro_args(&bytes).unwrap();
        assert_eq!(decoded.len(), 2);
    }

    #[test]
    fn test_closure_serialization_fails() {
        use crate::utils::environment::Environment;

        let expr = Expr::Literal(crate::ast::Literal::Int(0)).into_id_without_span();
        let val = Value::Closure(expr, vec![], Environment::new());

        let result = serialize_value(&val);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Closure"));
    }
}
