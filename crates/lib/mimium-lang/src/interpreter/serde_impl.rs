// Manual Serialize/Deserialize for Value enum
// This is necessary because Closure, ExternalFn, and Store variants
// should not be serialized for macro function arguments (higher-order functions are prohibited).

use super::*;
use serde::de::{self, Deserializer, VariantAccess, Visitor};
use serde::ser::{SerializeStructVariant, Serializer};
use serde::{Deserialize, Serialize};

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Value::ErrorV(id) => {
                let mut sv = serializer.serialize_struct_variant("Value", 0, "ErrorV", 1)?;
                sv.serialize_field("0", id)?;
                sv.end()
            }
            Value::Unit => serializer.serialize_unit_variant("Value", 1, "Unit"),
            Value::Number(n) => {
                let mut sv = serializer.serialize_struct_variant("Value", 2, "Number", 1)?;
                sv.serialize_field("0", n)?;
                sv.end()
            }
            Value::String(s) => {
                let mut sv = serializer.serialize_struct_variant("Value", 3, "String", 1)?;
                sv.serialize_field("0", s)?;
                sv.end()
            }
            Value::Array(arr) => {
                let mut sv = serializer.serialize_struct_variant("Value", 4, "Array", 1)?;
                sv.serialize_field("0", arr)?;
                sv.end()
            }
            Value::Record(rec) => {
                let mut sv = serializer.serialize_struct_variant("Value", 5, "Record", 1)?;
                sv.serialize_field("0", rec)?;
                sv.end()
            }
            Value::Tuple(tuple) => {
                let mut sv = serializer.serialize_struct_variant("Value", 6, "Tuple", 1)?;
                sv.serialize_field("0", tuple)?;
                sv.end()
            }
            Value::Fixpoint(sym, id) => {
                let mut sv = serializer.serialize_struct_variant("Value", 7, "Fixpoint", 2)?;
                sv.serialize_field("0", sym)?;
                sv.serialize_field("1", id)?;
                sv.end()
            }
            Value::Code(id) => {
                let mut sv = serializer.serialize_struct_variant("Value", 8, "Code", 1)?;
                sv.serialize_field("0", id)?;
                sv.end()
            }
            Value::TaggedUnion(tag, val) => {
                let mut sv = serializer.serialize_struct_variant("Value", 9, "TaggedUnion", 2)?;
                sv.serialize_field("0", tag)?;
                sv.serialize_field("1", val)?;
                sv.end()
            }
            Value::ConstructorFn(tag, sym, ty) => {
                let mut sv =
                    serializer.serialize_struct_variant("Value", 10, "ConstructorFn", 3)?;
                sv.serialize_field("0", tag)?;
                sv.serialize_field("1", sym)?;
                sv.serialize_field("2", ty)?;
                sv.end()
            }
            Value::Closure(..) => Err(serde::ser::Error::custom(
                "Cannot serialize Value::Closure (higher-order functions prohibited in macro arguments)",
            )),
            Value::ExternalFn(_) => Err(serde::ser::Error::custom(
                "Cannot serialize Value::ExternalFn (function pointers cannot cross FFI boundary)",
            )),
            Value::Store(_) => Err(serde::ser::Error::custom(
                "Cannot serialize Value::Store (mutable references not supported in macro arguments)",
            )),
        }
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "PascalCase")]
        enum Field {
            ErrorV,
            Unit,
            Number,
            String,
            Array,
            Record,
            Tuple,
            Fixpoint,
            Code,
            TaggedUnion,
            ConstructorFn,
        }

        struct ValueVisitor;

        impl<'de> Visitor<'de> for ValueVisitor {
            type Value = Value;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("enum Value")
            }

            fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
            where
                A: de::EnumAccess<'de>,
            {
                let (variant, variant_access) = data.variant()?;
                match variant {
                    Field::ErrorV => {
                        let id: ExprNodeId = variant_access.newtype_variant()?;
                        Ok(Value::ErrorV(id))
                    }
                    Field::Unit => {
                        variant_access.unit_variant()?;
                        Ok(Value::Unit)
                    }
                    Field::Number => {
                        let n: f64 = variant_access.newtype_variant()?;
                        Ok(Value::Number(n))
                    }
                    Field::String => {
                        let s: Symbol = variant_access.newtype_variant()?;
                        Ok(Value::String(s))
                    }
                    Field::Array => {
                        let arr: Vec<Value> = variant_access.newtype_variant()?;
                        Ok(Value::Array(arr))
                    }
                    Field::Record => {
                        let rec: Vec<(Symbol, Value)> = variant_access.newtype_variant()?;
                        Ok(Value::Record(rec))
                    }
                    Field::Tuple => {
                        let tuple: Vec<Value> = variant_access.newtype_variant()?;
                        Ok(Value::Tuple(tuple))
                    }
                    Field::Fixpoint => {
                        #[derive(Deserialize)]
                        struct FixpointFields(Symbol, ExprNodeId);
                        let fields: FixpointFields = variant_access.newtype_variant()?;
                        Ok(Value::Fixpoint(fields.0, fields.1))
                    }
                    Field::Code => {
                        let id: ExprNodeId = variant_access.newtype_variant()?;
                        Ok(Value::Code(id))
                    }
                    Field::TaggedUnion => {
                        #[derive(Deserialize)]
                        struct TaggedUnionFields(u64, Box<Value>);
                        let fields: TaggedUnionFields = variant_access.newtype_variant()?;
                        Ok(Value::TaggedUnion(fields.0, fields.1))
                    }
                    Field::ConstructorFn => {
                        #[derive(Deserialize)]
                        struct ConstructorFnFields(u64, Symbol, TypeNodeId);
                        let fields: ConstructorFnFields = variant_access.newtype_variant()?;
                        Ok(Value::ConstructorFn(fields.0, fields.1, fields.2))
                    }
                }
            }
        }

        deserializer.deserialize_enum(
            "Value",
            &[
                "ErrorV",
                "Unit",
                "Number",
                "String",
                "Array",
                "Record",
                "Tuple",
                "Fixpoint",
                "Code",
                "TaggedUnion",
                "ConstructorFn",
            ],
            ValueVisitor,
        )
    }
}
