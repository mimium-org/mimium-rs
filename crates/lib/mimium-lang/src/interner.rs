//! Storage for interning symbols, expressions and types during compilation.
//!
//! The global [`SessionGlobals`] instance keeps track of all identifiers and
//! AST nodes created while parsing.

use std::{
    borrow::Cow,
    cell::RefCell,
    collections::BTreeMap,
    fmt::{self, Display},
    hash::Hash,
};

use slotmap::SlotMap;
use string_interner::{StringInterner, backend::StringBackend};

use crate::{
    ast::Expr,
    dummy_span,
    types::{PType, RecordTypeField, Type},
    utils::metadata::{Location, Span},
};
slotmap::new_key_type! {
    pub struct ExprKey;
    pub struct TypeKey;
}

/// Global storages shared during compilation stages.
pub struct SessionGlobals {
    pub symbol_interner: StringInterner<StringBackend<usize>>,
    pub expr_storage: SlotMap<ExprKey, Expr>,
    pub type_storage: SlotMap<TypeKey, Type>,
    pub loc_storage: BTreeMap<NodeId, Location>,
}

impl SessionGlobals {
    fn store_expr(&mut self, expr: Expr) -> ExprNodeId {
        let key = self.expr_storage.insert(expr);
        ExprNodeId(key)
    }

    fn store_loc<T: ToNodeId>(&mut self, node_id: T, loc: Location) {
        self.loc_storage.insert(node_id.to_node_id(), loc);
    }

    pub fn store_type(&mut self, ty: Type) -> TypeNodeId {
        let key = self.type_storage.insert(ty);
        TypeNodeId(key)
    }

    pub fn store_expr_with_location(&mut self, expr: Expr, loc: Location) -> ExprNodeId {
        let expr_id = self.store_expr(expr);
        self.store_loc(expr_id, loc);
        expr_id
    }

    pub fn store_type_with_location(&mut self, ty: Type, loc: Location) -> TypeNodeId {
        let type_id = self.store_type(ty);
        self.store_loc(type_id, loc);
        type_id
    }

    // TODO: in theory, instead of cloning, this can return &Expr with an
    // extended lifetime by `std::mem::transmute()` just like we do in
    // Symbol::as_str(). However, we would get segfault for some reason.
    //
    // cf. https://github.com/tomoyanonymous/mimium-rs/pull/27#issuecomment-2306226748
    pub fn get_expr(&self, expr_id: ExprNodeId) -> Expr {
        unsafe { self.expr_storage.get_unchecked(expr_id.0) }.clone()
    }

    pub fn get_type(&self, type_id: TypeNodeId) -> Type {
        unsafe { self.type_storage.get_unchecked(type_id.0) }.clone()
    }

    pub fn get_span<T: ToNodeId>(&self, node_id: T) -> Option<&Location> {
        self.loc_storage.get(&node_id.to_node_id())
    }
}

thread_local!(static SESSION_GLOBALS: RefCell<SessionGlobals> =  RefCell::new(
    SessionGlobals {
        symbol_interner: StringInterner::new(),
        expr_storage: SlotMap::with_key(),
        type_storage: SlotMap::with_key(),
        loc_storage: BTreeMap::new()
    }
));

pub fn with_session_globals<R, F>(f: F) -> R
where
    F: FnOnce(&mut SessionGlobals) -> R,
{
    SESSION_GLOBALS.with_borrow_mut(f)
}

#[derive(Default, Copy, Clone, PartialEq, Hash, Eq, PartialOrd, Ord)]
pub struct Symbol(pub usize); //Symbol Trait is implemented on usize

pub trait ToSymbol {
    fn to_symbol(&self) -> Symbol;
}

impl<T: AsRef<str>> ToSymbol for T {
    fn to_symbol(&self) -> Symbol {
        Symbol(with_session_globals(|session_globals| {
            session_globals.symbol_interner.get_or_intern(self.as_ref())
        }))
    }
}
impl<'a> From<Symbol> for Cow<'a, str> {
    fn from(val: Symbol) -> Self {
        with_session_globals(|session_globals| {
            Cow::Owned(
                session_globals
                    .symbol_interner
                    .resolve(val.0)
                    .unwrap()
                    .to_string(),
            )
        })
    }
}
impl Symbol {
    pub fn as_str(&self) -> &str {
        with_session_globals(|session_globals| unsafe {
            // This transmute is needed to convince the borrow checker. Since
            // the session_global should exist until the end of the session,
            // this &str should live sufficiently long.
            std::mem::transmute::<&str, &str>(
                session_globals
                    .symbol_interner
                    .resolve(self.0)
                    .expect("invalid symbol"),
            )
        })
    }
}
impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        with_session_globals(|session_globals| unsafe {
            // This transmute is needed to convince the borrow checker. Since
            // the session_global should exist until the end of the session,
            // this &str should live sufficiently long.
            std::mem::transmute::<&str, &str>(
                session_globals
                    .symbol_interner
                    .resolve(self.0)
                    .expect("invalid symbol"),
            )
        })
    }
}

// Note: to_string() is auto-implemented by this
impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.as_str(), self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum NodeId {
    ExprArena(ExprKey),
    TypeArena(TypeKey),
}

#[derive(Clone, Copy, Default)]
pub struct ExprNodeId(pub ExprKey);

#[derive(Clone, Copy, Default)]
pub struct TypeNodeId(pub TypeKey);

// traits required for Key trait

impl PartialEq for ExprNodeId {
    fn eq(&self, other: &Self) -> bool {
        self.to_expr() == other.to_expr() && self.to_span() == other.to_span()
    }
}

impl PartialEq for TypeNodeId {
    fn eq(&self, other: &Self) -> bool {
        self.to_type() == other.to_type() && self.to_span() == other.to_span()
    }
}
impl Eq for TypeNodeId {}
impl Hash for TypeNodeId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl state_tree::tree::SizedType for TypeNodeId {
    fn word_size(&self) -> u64 {
        match self.to_type() {
            Type::Primitive(PType::Unit) => 0,
            Type::Primitive(PType::String) => 1,
            Type::Primitive(_) => 1,
            Type::Array(_) => 1, //array is represented as a pointer to the special storage
            Type::Tuple(types) => types.iter().map(|t| t.word_size()).sum(),
            Type::Record(types) => types
                .iter()
                .map(|RecordTypeField { ty, .. }| ty.word_size())
                .sum(),
            Type::Function { arg: _, ret: _ } => 1,
            Type::Ref(_) => 1,
            Type::Code(_) => todo!(),
            _ => {
                //todo: this may contain intermediate types
                1
            }
        }
    }
}

impl ExprNodeId {
    pub fn to_expr(&self) -> Expr {
        with_session_globals(|session_globals| session_globals.get_expr(*self))
    }

    pub fn to_span(&self) -> Span {
        with_session_globals(|session_globals| match session_globals.get_span(*self) {
            Some(loc) => loc.span.clone(),
            None => dummy_span!(),
        })
    }
    pub fn to_location(&self) -> Location {
        with_session_globals(|session_globals| match session_globals.get_span(*self) {
            Some(loc) => loc.clone(),
            None => Location {
                span: dummy_span!(),
                path: "".to_symbol(),
            },
        })
    }
}

impl TypeNodeId {
    pub fn to_type(&self) -> Type {
        with_session_globals(|session_globals| session_globals.get_type(*self))
    }

    pub fn to_span(&self) -> Span {
        with_session_globals(|session_globals| match session_globals.get_span(*self) {
            Some(loc) => loc.span.clone(),
            None => dummy_span!(),
        })
    }
    pub fn to_loc(&self) -> Location {
        let dummy_path = "".to_symbol();
        with_session_globals(|session_globals| match session_globals.get_span(*self) {
            Some(loc) => loc.clone(),
            None => Location {
                span: dummy_span!(),
                path: dummy_path,
            },
        })
    }
    // Flatten a composite type into a list of types so that the offset of the
    // element can be calculated easily. For example:
    //
    // original:       Tuple(float, function, Tuple(float, float))
    // flattened form: [float, function, float, float]
    pub fn flatten(&self) -> Vec<Self> {
        match self.to_type() {
            Type::Tuple(t) => t.iter().flat_map(|t| t.flatten()).collect::<Vec<_>>(),
            Type::Record(t) => t
                .iter()
                .flat_map(|RecordTypeField { ty, .. }| ty.flatten())
                .collect::<Vec<_>>(),
            _ => vec![*self],
        }
    }
}

pub trait ToNodeId {
    fn to_node_id(&self) -> NodeId;
}

impl ToNodeId for ExprNodeId {
    fn to_node_id(&self) -> NodeId {
        NodeId::ExprArena(self.0)
    }
}

impl ToNodeId for TypeNodeId {
    fn to_node_id(&self) -> NodeId {
        NodeId::TypeArena(self.0)
    }
}
impl std::fmt::Display for ExprNodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let span = self.to_span();
        write!(f, "{:?},{}..{}", self.to_expr(), span.start, span.end)
    }
}
impl std::fmt::Debug for ExprNodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let span = self.to_span();
        write!(f, "{:#?},{}..{}", self.to_expr(), span.start, span.end)
    }
}
impl std::fmt::Display for TypeNodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let span = self.to_span();

        write!(f, "{:?},{}..{}", self.to_type(), span.start, span.end)
    }
}
impl std::fmt::Debug for TypeNodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let span = self.to_span();

        write!(f, "{:#?},{}..{}", self.to_type(), span.start, span.end)
    }
}
