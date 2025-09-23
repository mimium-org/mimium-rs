use crate::interner::{ExprNodeId, Symbol, TypeNodeId};
//todo! need to replace with interned string.
use crate::types::Type;
use crate::utils::metadata::{Location, Span};
use crate::utils::miniprint::MiniPrint;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Single(Symbol),
    Tuple(Vec<Self>),
    Record(Vec<(Symbol, Self)>),
    Error,
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Single(id) => write!(f, "{id}"),
            Pattern::Tuple(vec) => {
                let s = vec
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "({s})")
            }
            Pattern::Record(items) => {
                if items.is_empty() {
                    write!(f, "{{}}")
                } else {
                    let s = items
                        .iter()
                        .map(|(k, v)| format!("{k}: {v}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{{{s}}}")
                }
            }
            Pattern::Error => {
                write!(f, "<error pattern>")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedId {
    pub id: Symbol,
    // TypeNodeId is always issued even if the expression doesn't have the type
    // specification at all. This can be used for querying for the span.
    pub ty: TypeNodeId,
    // Optional default value for function parameters
    pub default_value: Option<ExprNodeId>,
}

impl TypedId {
    // Constructor that initializes a TypedId with default values
    pub fn new(id: Symbol, ty: TypeNodeId) -> Self {
        TypedId {
            id,
            ty,
            default_value: None,
        }
    }

    // Constructor that includes a default value
    pub fn with_default(id: Symbol, ty: TypeNodeId, default_value: ExprNodeId) -> Self {
        TypedId {
            id,
            ty,
            default_value: Some(default_value),
        }
    }

    pub fn to_span(&self) -> Span {
        self.ty.to_span()
    }

    pub fn is_unknown(&self) -> bool {
        self.ty.to_type() == Type::Unknown
    }
}

impl std::fmt::Display for TypedId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.is_unknown() {
            write!(f, "{} :{}", self.id, self.ty.to_type())
        } else {
            write!(f, "{}", self.id)
        }
    }
}

impl MiniPrint for TypedId {
    fn simple_print(&self) -> String {
        if !self.is_unknown() {
            format!("(tid {} {})", self.id, self.ty.to_type())
        } else {
            self.id.to_string()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedPattern {
    pub pat: Pattern,
    pub ty: TypeNodeId,
    pub default_value: Option<ExprNodeId>,
}
impl TypedPattern {
    pub fn new(pat: Pattern, ty: TypeNodeId) -> Self {
        TypedPattern {
            pat,
            ty,
            default_value: None,
        }
    }
}

impl TypedPattern {
    pub fn to_span(&self) -> Span {
        self.ty.to_span()
    }
    pub fn to_loc(&self) -> Location {
        self.ty.to_loc()
    }

    pub fn is_unknown(&self) -> bool {
        self.ty.to_type() == Type::Unknown
    }
}

#[derive(Debug)]
pub struct ConversionError;
impl std::fmt::Display for ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Failed to convert pattern. The pattern did not matched to single identifier."
        )
    }
}
impl std::error::Error for ConversionError {}

impl From<TypedId> for TypedPattern {
    fn from(value: TypedId) -> Self {
        TypedPattern {
            pat: Pattern::Single(value.id),
            ty: value.ty,
            default_value: value.default_value, // Default value is optional
        }
    }
}
impl TryFrom<TypedPattern> for TypedId {
    type Error = ConversionError;

    fn try_from(value: TypedPattern) -> Result<Self, Self::Error> {
        match value.pat {
            Pattern::Single(id) => Ok(TypedId {
                id,
                ty: value.ty,
                default_value: value.default_value,
            }),
            _ => Err(ConversionError),
        }
    }
}

impl MiniPrint for TypedPattern {
    fn simple_print(&self) -> String {
        if !self.is_unknown() {
            format!("(tpat {} {})", self.pat, self.ty.to_type())
        } else {
            self.pat.to_string()
        }
    }
}
impl std::fmt::Display for TypedPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.is_unknown() {
            write!(f, "{} :{}", self.pat, self.ty.to_type())
        } else {
            write!(f, "{}", self.pat)
        }
    }
}
