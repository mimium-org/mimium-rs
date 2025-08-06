use std::{cell::RefCell, fmt, rc::Rc};

use crate::{
    format_vec,
    interner::{Symbol, TypeNodeId, with_session_globals},
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
pub(crate) struct IntermediateId(pub u64);

#[derive(Clone, Debug, PartialEq)]
pub struct TypeVar {
    pub parent: Option<TypeNodeId>,
    pub var: IntermediateId,
    pub level: u64,
}
impl TypeVar {
    pub fn new(var: IntermediateId, level: u64) -> Self {
        Self {
            parent: None,
            var,
            level,
        }
    }
}
/// A parameter representation for function type, with an optional label and a type.
#[derive(Clone, Debug, PartialEq)]
pub struct LabeledParam {
    pub label: Option<Symbol>,
    pub ty: TypeNodeId,
    pub has_default: bool,
}
impl LabeledParam {
    pub fn new(label: Symbol, ty: TypeNodeId, has_default: bool) -> Self {
        Self {
            label: Some(label),
            ty,
            has_default,
        }
    }
}
impl From<TypeNodeId> for LabeledParam {
    fn from(ty: TypeNodeId) -> Self {
        Self {
            label: None,
            ty,
            has_default: false,
        }
    }
}
impl From<RecordTypeField> for LabeledParam {
    fn from(value: RecordTypeField) -> Self {
        Self {
            label: Some(value.key),
            ty: value.ty,
            has_default: value.has_default,
        }
    }
}
/// A parameter representation for function type, with an optional label and a type.
#[derive(Clone, Debug, PartialEq)]
pub struct LabeledParams(Vec<LabeledParam>);
impl LabeledParams {
    pub fn new(params: Vec<LabeledParam>) -> Self {
        Self(params)
    }
    pub fn get_as_slice(&self) -> &[LabeledParam] {
        &self.0
    }
    pub fn ty_iter(&self) -> impl Iterator<Item = TypeNodeId> + Clone {
        self.0.iter().map(|p| p.ty)
    }
    pub fn ty_map(
        &self,
        mut f: impl FnMut(TypeNodeId) -> TypeNodeId,
    ) -> impl Iterator<Item = LabeledParam> {
        self.0.iter().map(move |p| LabeledParam {
            label: p.label,
            ty: f(p.ty),
            has_default: p.has_default,
        })
    }
    pub fn has_label(&self) -> bool {
        debug_assert!(
            self.0.iter().all(|p| p.label.is_some()) || self.0.iter().all(|p| p.label.is_none()),
            "parameter labels are either all set or all unset"
        );
        // if the parameters are empty,just returns false
        self.0.first().is_some_and(|p| p.label.is_some())
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
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeSchemeId(pub u64);

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Primitive(PType),
    //aggregate types
    Array(TypeNodeId),
    Tuple(Vec<TypeNodeId>),
    Record(Vec<RecordTypeField>),
    ///Function that has a vector of parameters, return type, and type for internal states.
    ///Parameters are represented as `Vec<(Option<Symbol>, TypeNodeId)>` to support parameter names
    Function(LabeledParams, TypeNodeId, Option<TypeNodeId>),
    Ref(TypeNodeId),
    //(experimental) code-type for multi-stage computation that will be evaluated on the next stage
    Code(TypeNodeId),
    Intermediate(Rc<RefCell<TypeVar>>),
    TypeScheme(TypeSchemeId),
    /// Failure type: it is bottom type that can be unified to any type and return bottom type.
    Failure,
    Unknown,
}

// currently, this refers to the number of registers
pub type TypeSize = u8;

impl Type {
    // check if contains any function type in its member.
    // if no functions are contained, it means that the value can be placed in linear memory.
    pub fn contains_function(&self) -> bool {
        match self {
            Type::Function(_, _, _) => true,
            Type::Tuple(t) => t.iter().any(|t| t.to_type().contains_function()),
            Type::Record(t) => t
                .iter()
                .any(|RecordTypeField { ty, .. }| ty.to_type().contains_function()),
            _ => false,
        }
    }
    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function(_, _, _))
    }

    // Helper method to get parameter types without their names
    pub fn get_param_types(&self) -> Option<Vec<TypeNodeId>> {
        match self {
            Type::Function(params, _, _) => Some(params.ty_iter().collect()),
            _ => None,
        }
    }
    pub fn is_intermediate(&self) -> Option<Rc<RefCell<TypeVar>>> {
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
            Type::Function(p, r, _s) => {
                let args = format_vec!(
                    p.ty_iter()
                        .map(|x| x.to_type().to_string_for_error())
                        .collect::<Vec<_>>(),
                    ","
                );
                format!("({args})->{}", r.to_type().to_string_for_error())
            }
            Type::Ref(x) => format!("&{}", x.to_type().to_string_for_error()),
            Type::Code(c) => format!("`({})", c.to_type().to_string_for_error()),
            Type::Intermediate(_id) => "?".to_string(),
            // if no special treatment is needed, forward to the Display implementation
            x => x.to_string(),
        }
    }
}

impl TypeNodeId {
    pub fn get_root(&self) -> TypeNodeId {
        match self.to_type() {
            Type::Intermediate(cell) => {
                let tv = cell.borrow_mut();
                tv.parent.map_or(*self, |t| t.get_root())
            }
            _ => *self,
        }
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
                                key.clone(),
                                apply_scalar(*ty, &mut closure),
                                *has_default,
                            )
                        },
                    )
                    .collect(),
            ),
            Type::Function(p, r, s) => {
                let at = p
                    .ty_map(|t| apply_scalar(t, &mut closure))
                    .collect::<Vec<_>>();
                let rt = apply_scalar(r, &mut closure);
                Type::Function(
                    LabeledParams::new(at),
                    rt,
                    s.map(|t| apply_scalar(t, &mut closure)),
                )
            }
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
            "?{}[{}]({})",
            self.var.0,
            self.level,
            self.parent
                .map_or_else(|| "".to_string(), |t| t.to_type().to_string())
        )
    }
}
impl fmt::Display for RecordTypeField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let def = if self.has_default { "(default)" } else { "" };
        write!(f, "{}:{}{def}", self.key, self.ty)
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
            Type::Function(p, r, _s) => {
                let args = format_vec!(
                    p.ty_iter().map(|x| x.to_type().clone()).collect::<Vec<_>>(),
                    ","
                );
                write!(f, "({args})->{}", r.to_type())
            }
            Type::Ref(x) => write!(f, "&{}", x.to_type()),

            Type::Code(c) => write!(f, "<{}>", c.to_type()),
            Type::Intermediate(id) => {
                write!(f, "{}", id.borrow())
            }
            Type::TypeScheme(id) => {
                write!(f, "g({})", id.0)
            }
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
