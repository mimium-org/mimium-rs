// Manual Serialize/Deserialize for Type enum
// This is necessary because Type::Intermediate and Type::TypeScheme
// should not be serialized for macro function arguments.

use super::*;
use serde::de::{self, Deserializer, Visitor};
use serde::ser::{SerializeStructVariant, Serializer};
use serde::{Deserialize, Serialize};

impl Serialize for Type {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Type::Primitive(p) => {
                let mut sv = serializer.serialize_struct_variant("Type", 0, "Primitive", 1)?;
                sv.serialize_field("0", p)?;
                sv.end()
            }
            Type::Array(t) => {
                let mut sv = serializer.serialize_struct_variant("Type", 1, "Array", 1)?;
                sv.serialize_field("0", t)?;
                sv.end()
            }
            Type::Tuple(v) => {
                let mut sv = serializer.serialize_struct_variant("Type", 2, "Tuple", 1)?;
                sv.serialize_field("0", v)?;
                sv.end()
            }
            Type::Record(v) => {
                let mut sv = serializer.serialize_struct_variant("Type", 3, "Record", 1)?;
                sv.serialize_field("0", v)?;
                sv.end()
            }
            Type::Function { arg, ret } => {
                let mut sv = serializer.serialize_struct_variant("Type", 4, "Function", 2)?;
                sv.serialize_field("arg", arg)?;
                sv.serialize_field("ret", ret)?;
                sv.end()
            }
            Type::Ref(t) => {
                let mut sv = serializer.serialize_struct_variant("Type", 5, "Ref", 1)?;
                sv.serialize_field("0", t)?;
                sv.end()
            }
            Type::Code(t) => {
                let mut sv = serializer.serialize_struct_variant("Type", 6, "Code", 1)?;
                sv.serialize_field("0", t)?;
                sv.end()
            }
            Type::Union(v) => {
                let mut sv = serializer.serialize_struct_variant("Type", 7, "Union", 1)?;
                sv.serialize_field("0", v)?;
                sv.end()
            }
            Type::UserSum { name, variants } => {
                let mut sv = serializer.serialize_struct_variant("Type", 8, "UserSum", 2)?;
                sv.serialize_field("name", name)?;
                sv.serialize_field("variants", variants)?;
                sv.end()
            }
            Type::Boxed(t) => {
                let mut sv = serializer.serialize_struct_variant("Type", 9, "Boxed", 1)?;
                sv.serialize_field("0", t)?;
                sv.end()
            }
            Type::TypeAlias(s) => {
                let mut sv = serializer.serialize_struct_variant("Type", 10, "TypeAlias", 1)?;
                sv.serialize_field("0", s)?;
                sv.end()
            }
            Type::Any => serializer.serialize_unit_variant("Type", 11, "Any"),
            Type::Failure => serializer.serialize_unit_variant("Type", 12, "Failure"),
            Type::Unknown => serializer.serialize_unit_variant("Type", 13, "Unknown"),
            Type::Intermediate(_) => Err(serde::ser::Error::custom(
                "Cannot serialize Type::Intermediate (internal compiler state)",
            )),
            Type::TypeScheme(_) => Err(serde::ser::Error::custom(
                "Cannot serialize Type::TypeScheme (internal compiler state)",
            )),
        }
    }
}

// Simplified Deserialize - won't support Intermediate/TypeScheme
impl<'de> Deserialize<'de> for Type {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Primitive,
            Array,
            Tuple,
            Record,
            Function,
            Ref,
            Code,
            Union,
            UserSum,
            Boxed,
            TypeAlias,
            Any,
            Failure,
            Unknown,
        }

        struct TypeVisitor;

        impl<'de> Visitor<'de> for TypeVisitor {
            type Value = Type;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("enum Type")
            }

            fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
            where
                A: de::EnumAccess<'de>,
            {
                use de::VariantAccess;
                let (variant, variant_access) = data.variant()?;
                match variant {
                    Field::Primitive => {
                        let p: PType = variant_access.newtype_variant()?;
                        Ok(Type::Primitive(p))
                    }
                    Field::Array => {
                        let t: TypeNodeId = variant_access.newtype_variant()?;
                        Ok(Type::Array(t))
                    }
                    Field::Tuple => {
                        let v: Vec<TypeNodeId> = variant_access.newtype_variant()?;
                        Ok(Type::Tuple(v))
                    }
                    Field::Record => {
                        let v: Vec<RecordTypeField> = variant_access.newtype_variant()?;
                        Ok(Type::Record(v))
                    }
                    Field::Function => {
                        #[derive(Deserialize)]
                        struct FunctionFields {
                            arg: TypeNodeId,
                            ret: TypeNodeId,
                        }
                        let fields: FunctionFields = variant_access.newtype_variant()?;
                        Ok(Type::Function {
                            arg: fields.arg,
                            ret: fields.ret,
                        })
                    }
                    Field::Ref => {
                        let t: TypeNodeId = variant_access.newtype_variant()?;
                        Ok(Type::Ref(t))
                    }
                    Field::Code => {
                        let t: TypeNodeId = variant_access.newtype_variant()?;
                        Ok(Type::Code(t))
                    }
                    Field::Union => {
                        let v: Vec<TypeNodeId> = variant_access.newtype_variant()?;
                        Ok(Type::Union(v))
                    }
                    Field::UserSum => {
                        #[derive(Deserialize)]
                        struct UserSumFields {
                            name: Symbol,
                            variants: Vec<(Symbol, Option<TypeNodeId>)>,
                        }
                        let fields: UserSumFields = variant_access.newtype_variant()?;
                        Ok(Type::UserSum {
                            name: fields.name,
                            variants: fields.variants,
                        })
                    }
                    Field::Boxed => {
                        let t: TypeNodeId = variant_access.newtype_variant()?;
                        Ok(Type::Boxed(t))
                    }
                    Field::TypeAlias => {
                        let s: Symbol = variant_access.newtype_variant()?;
                        Ok(Type::TypeAlias(s))
                    }
                    Field::Any => {
                        variant_access.unit_variant()?;
                        Ok(Type::Any)
                    }
                    Field::Failure => {
                        variant_access.unit_variant()?;
                        Ok(Type::Failure)
                    }
                    Field::Unknown => {
                        variant_access.unit_variant()?;
                        Ok(Type::Unknown)
                    }
                }
            }
        }

        deserializer.deserialize_enum(
            "Type",
            &[
                "Primitive",
                "Array",
                "Tuple",
                "Record",
                "Function",
                "Ref",
                "Code",
                "Union",
                "UserSum",
                "Boxed",
                "TypeAlias",
                "Any",
                "Failure",
                "Unknown",
            ],
            TypeVisitor,
        )
    }
}
