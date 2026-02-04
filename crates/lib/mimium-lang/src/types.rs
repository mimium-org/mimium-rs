use std::{
    fmt,
    sync::{Arc, RwLock},
};

use crate::{
    format_vec,
    interner::{Symbol, TypeNodeId, with_session_globals},
    pattern::TypedId,
    utils::metadata::Location,
};

/// Basic types that are not boxed.
/// They should be splitted semantically as the type of `feed x.e`cannot take function type.
#[derive(Clone, Debug, PartialEq)]
pub enum PType {
    Unit,
    Int,
    Numeric,
    String,
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntermediateId(pub u64);

#[derive(Clone, Debug, PartialEq)]
pub struct TypeBound {
    pub lower: TypeNodeId,
    pub upper: TypeNodeId,
}
impl Default for TypeBound {
    fn default() -> Self {
        Self {
            lower: Type::Failure.into_id(),
            upper: Type::Any.into_id(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeVar {
    pub parent: Option<TypeNodeId>,
    pub var: IntermediateId,
    pub level: u64,
    pub bound: TypeBound,
}
impl TypeVar {
    pub fn new(var: IntermediateId, level: u64) -> Self {
        Self {
            parent: None,
            var,
            level,
            bound: TypeBound::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordTypeField {
    pub key: Symbol,
    pub ty: TypeNodeId,
    pub has_default: bool,
}
impl RecordTypeField {
    pub fn new(key: Symbol, ty: TypeNodeId, has_default: bool) -> Self {
        Self {
            key,
            ty,
            has_default,
        }
    }
}
impl From<TypedId> for RecordTypeField {
    fn from(value: TypedId) -> Self {
        Self {
            key: value.id,
            ty: value.ty,
            has_default: value.default_value.is_some(),
        }
    }
}
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeSchemeId(pub u64);

#[derive(Clone, Debug)]
pub enum Type {
    Primitive(PType),
    //aggregate types
    Array(TypeNodeId),
    Tuple(Vec<TypeNodeId>),
    Record(Vec<RecordTypeField>),
    ///Function that takes some type and return some type
    ///If the function takes multiple arguments, the arguments becomes record type.
    Function {
        arg: TypeNodeId,
        ret: TypeNodeId,
    },
    Ref(TypeNodeId),
    //(experimental) code-type for multi-stage computation that will be evaluated on the next stage
    Code(TypeNodeId),
    Intermediate(Arc<RwLock<TypeVar>>),
    TypeScheme(TypeSchemeId),
    /// Any type is the top level, it can be unified with anything.
    Any,
    /// Failure type: it is bottom type that can be unified to any type and return bottom type.
    Failure,
    Unknown,
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Intermediate(a), Type::Intermediate(b)) => {
                let a = a.read().unwrap();
                let b = b.read().unwrap();
                a.var == b.var
            }
            (Type::Primitive(a), Type::Primitive(b)) => a == b,
            (Type::Array(a), Type::Array(b)) => a == b,
            (Type::Tuple(a), Type::Tuple(b)) => a == b,
            (Type::Record(a), Type::Record(b)) => a == b,
            (Type::Function { arg: a1, ret: a2 }, Type::Function { arg: b1, ret: b2 }) => {
                a1 == b1 && a2 == b2
            }
            (Type::Ref(a), Type::Ref(b)) => a == b,
            (Type::Code(a), Type::Code(b)) => a == b,
            (Type::TypeScheme(a), Type::TypeScheme(b)) => a == b,
            (Type::Any, Type::Any) => true,
            (Type::Failure, Type::Failure) => true,
            (Type::Unknown, Type::Unknown) => true,
            _ => false,
        }
    }
}

// currently, this refers to the number of registers
pub type TypeSize = u8;

impl Type {
    // check if contains any function type in its member.
    // if no functions are contained, it means that the value can be placed in linear memory.
    pub fn contains_function(&self) -> bool {
        match self {
            Type::Function { arg: _, ret: _ } => true,
            Type::Tuple(t) => t.iter().any(|t| t.to_type().contains_function()),
            Type::Record(t) => t
                .iter()
                .any(|RecordTypeField { ty, .. }| ty.to_type().contains_function()),
            _ => false,
        }
    }
    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function { arg: _, ret: _ })
    }
    pub fn contains_code(&self) -> bool {
        match self {
            Type::Code(_) => true,
            Type::Tuple(t) => t.iter().any(|t| t.to_type().contains_code()),
            Type::Record(t) => t
                .iter()
                .any(|RecordTypeField { ty, .. }| ty.to_type().contains_code()),
            _ => false,
        }
    }

    pub fn contains_type_scheme(&self) -> bool {
        match self {
            Type::TypeScheme(_) => true,
            Type::Array(t) => t.to_type().contains_type_scheme(),
            Type::Tuple(t) => t.iter().any(|t| t.to_type().contains_type_scheme()),
            Type::Record(t) => t
                .iter()
                .any(|RecordTypeField { ty, .. }| ty.to_type().contains_type_scheme()),
            Type::Function { arg, ret } => {
                arg.to_type().contains_type_scheme() || ret.to_type().contains_type_scheme()
            }
            Type::Ref(t) => t.to_type().contains_type_scheme(),
            Type::Code(t) => t.to_type().contains_type_scheme(),
            _ => false,
        }
    }

    pub fn is_intermediate(&self) -> Option<Arc<RwLock<TypeVar>>> {
        match self {
            Type::Intermediate(tvar) => Some(tvar.clone()),
            _ => None,
        }
    }

    pub fn get_as_tuple(&self) -> Option<Vec<TypeNodeId>> {
        match self {
            Type::Tuple(types) => Some(types.to_vec()),
            Type::Record(fields) => Some(
                fields
                    .iter()
                    .map(|RecordTypeField { ty, .. }| *ty)
                    .collect::<Vec<_>>(),
            ),
            _ => None,
        }
    }
    pub fn can_be_unpacked(&self) -> bool {
        matches!(self, Type::Tuple(_) | Type::Record(_))
    }
    pub fn get_iochannel_count(&self) -> Option<u32> {
        match self {
            Type::Tuple(ts) => {
                if ts
                    .iter()
                    .all(|t| t.to_type() == Type::Primitive(PType::Numeric))
                {
                    Some(ts.len() as _)
                } else {
                    None
                }
            }
            Type::Record(kvs) => {
                if kvs.iter().all(|RecordTypeField { ty, .. }| {
                    ty.to_type() == Type::Primitive(PType::Numeric)
                }) {
                    Some(kvs.len() as _)
                } else {
                    None
                }
            }
            Type::Primitive(PType::Numeric) => Some(1),
            Type::Primitive(PType::Unit) => Some(0),
            _ => None,
        }
    }
    pub fn into_id(self) -> TypeNodeId {
        with_session_globals(|session_globals| session_globals.store_type(self))
    }

    pub fn into_id_with_location(self, loc: Location) -> TypeNodeId {
        with_session_globals(|session_globals| session_globals.store_type_with_location(self, loc))
    }

    pub fn to_string_for_error(&self) -> String {
        match self {
            Type::Array(a) => {
                format!("[{}, ...]", a.to_type().to_string_for_error())
            }
            Type::Tuple(v) => {
                let vf = format_vec!(
                    v.iter()
                        .map(|x| x.to_type().to_string_for_error())
                        .collect::<Vec<_>>(),
                    ","
                );
                format!("({vf})")
            }
            Type::Record(v) => {
                let vf = format_vec!(
                    v.iter()
                        .map(|RecordTypeField { key, ty, .. }| format!(
                            "{}: {}",
                            key.as_str(),
                            ty.to_type().to_string_for_error()
                        ))
                        .collect::<Vec<_>>(),
                    ","
                );
                format!("({vf})")
            }
            Type::Function { arg, ret } => {
                format!(
                    "({})->{}",
                    arg.to_type().to_string_for_error(),
                    ret.to_type().to_string_for_error()
                )
            }
            Type::Ref(x) => format!("&{}", x.to_type().to_string_for_error()),
            Type::Code(c) => format!("`({})", c.to_type().to_string_for_error()),
            Type::Intermediate(_id) => "?".to_string(),
            // if no special treatment is needed, forward to the Display implementation
            x => x.to_string(),
        }
    }

    /// Generate a mangled name for monomorphization.
    /// This creates a unique string representation of the type that can be used
    /// to create specialized function names.
    pub fn to_mangled_string(&self) -> String {
        match self {
            Type::Primitive(p) => match p {
                PType::Unit => "unit".to_string(),
                PType::Int => "int".to_string(),
                PType::Numeric => "num".to_string(),
                PType::String => "str".to_string(),
            },
            Type::Array(a) => {
                format!("arr_{}", a.to_type().to_mangled_string())
            }
            Type::Tuple(v) => {
                let mangled_types = v
                    .iter()
                    .map(|x| x.to_type().to_mangled_string())
                    .collect::<Vec<_>>()
                    .join("_");
                format!("tup_{mangled_types}")
            }
            Type::Record(v) => {
                let mangled_fields = v
                    .iter()
                    .map(|RecordTypeField { key, ty, .. }| {
                        format!("{}_{}", key.as_str(), ty.to_type().to_mangled_string())
                    })
                    .collect::<Vec<_>>()
                    .join("_");
                format!("rec_{mangled_fields}")
            }
            Type::Function { arg, ret } => {
                format!(
                    "fn_{}_{}",
                    arg.to_type().to_mangled_string(),
                    ret.to_type().to_mangled_string()
                )
            }
            Type::Ref(x) => format!("ref_{}", x.to_type().to_mangled_string()),
            Type::Code(c) => format!("code_{}", c.to_type().to_mangled_string()),
            Type::Intermediate(tvar) => {
                let tv = tvar.read().unwrap();
                tv.parent
                    .map(|p| p.to_type().to_mangled_string())
                    .unwrap_or_else(|| format!("ivar_{}", tv.var.0))
            }
            Type::TypeScheme(id) => format!("scheme_{}", id.0),
            Type::Any => "any".to_string(),
            Type::Failure => "fail".to_string(),
            Type::Unknown => "unknown".to_string(),
        }
    }
}

impl TypeNodeId {
    pub fn get_root(&self) -> TypeNodeId {
        match self.to_type() {
            Type::Intermediate(cell) => {
                let tv = cell.read().unwrap();
                tv.parent.map_or(*self, |t| t.get_root())
            }
            _ => *self,
        }
    }

    /// Generate a mangled string for this type, useful for monomorphization.
    pub fn to_mangled_string(&self) -> String {
        self.to_type().to_mangled_string()
    }

    pub fn apply_fn<F>(&self, mut closure: F) -> Self
    where
        F: FnMut(Self) -> Self,
    {
        let apply_scalar = |a: Self, c: &mut F| -> Self { c(a) };
        let apply_vec = |v: &[Self], c: &mut F| -> Vec<Self> { v.iter().map(|a| c(*a)).collect() };
        let result = match self.to_type() {
            Type::Array(a) => Type::Array(apply_scalar(a, &mut closure)),
            Type::Tuple(v) => Type::Tuple(apply_vec(&v, &mut closure)),
            Type::Record(s) => Type::Record(
                s.iter()
                    .map(
                        |RecordTypeField {
                             key,
                             ty,
                             has_default,
                         }| {
                            RecordTypeField::new(
                                *key,
                                apply_scalar(*ty, &mut closure),
                                *has_default,
                            )
                        },
                    )
                    .collect(),
            ),
            Type::Function { arg, ret } => Type::Function {
                arg: apply_scalar(arg, &mut closure),
                ret: apply_scalar(ret, &mut closure),
            },
            Type::Ref(x) => Type::Ref(apply_scalar(x, &mut closure)),
            Type::Code(c) => Type::Code(apply_scalar(c, &mut closure)),
            Type::Intermediate(id) => Type::Intermediate(id.clone()),
            _ => self.to_type(),
        };

        result.into_id()
    }

    pub fn fold<F, R>(&self, _closure: F) -> R
    where
        F: Fn(Self, Self) -> R,
    {
        todo!()
    }
}

impl fmt::Display for PType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PType::Unit => write!(f, "()"),
            PType::Int => write!(f, "int"),
            PType::Numeric => write!(f, "number"),
            PType::String => write!(f, "string"),
        }
    }
}
impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "?{}[{}]{}",
            self.var.0,
            self.level,
            self.parent
                .map_or_else(|| "".to_string(), |t| format!(":{}", t.to_type()))
        )
    }
}
impl fmt::Display for RecordTypeField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let def = if self.has_default { "(default)" } else { "" };
        write!(f, "{}:{}{def}", self.key, self.ty.to_type())
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Primitive(p) => write!(f, "{p}"),
            Type::Array(a) => write!(f, "[{}]", a.to_type()),
            Type::Tuple(v) => {
                let vf = format_vec!(
                    v.iter().map(|x| x.to_type().clone()).collect::<Vec<_>>(),
                    ","
                );
                write!(f, "({vf})")
            }
            Type::Record(v) => {
                write!(f, "{{{}}}", format_vec!(v, ", "))
            }
            Type::Function { arg, ret } => {
                write!(f, "({})->{}", arg.to_type(), ret.to_type())
            }
            Type::Ref(x) => write!(f, "&{}", x.to_type()),

            Type::Code(c) => write!(f, "<{}>", c.to_type()),
            Type::Intermediate(id) => {
                write!(f, "{}", id.read().unwrap())
            }
            Type::TypeScheme(id) => {
                write!(f, "g({})", id.0)
            }
            Type::Any => write!(f, "any"),
            Type::Failure => write!(f, "!"),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

pub mod builder;

// #[cfg(test)]
// mod type_test {
//     use super::*;
// #[test]

// }
