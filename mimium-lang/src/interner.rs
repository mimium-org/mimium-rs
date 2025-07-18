//! Storage for interning symbols, expressions and types during compilation.
//!
//! The global [`SessionGlobals`] instance keeps track of all identifiers and
//! AST nodes created while parsing.

use std::{
    cell::RefCell,
    collections::BTreeMap,
    fmt::{self, Display},
    hash::Hash,
};

use slotmap::SlotMap;
use string_interner::{StringInterner, backend::StringBackend};

use crate::{
    ast::{Expr, RecordField},
    dummy_span,
    types::Type,
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
    pub fn apply_fn(&self, f: &mut impl FnMut(Self) -> Self) -> Self {
        let mut apply_node = |e: ExprNodeId| e.apply_fn(f);

        let expr = match self.to_expr() {
            Expr::Tuple(e) => {
                let apply_vec = |vec: &Vec<_>| vec.clone().into_iter().map(apply_node).collect();
                Expr::Tuple(apply_vec(&e))
            }
            Expr::Block(e) => Expr::Block(e.map(apply_node)),
            Expr::Proj(e, idx) => Expr::Proj(apply_node(e), idx),
            Expr::ArrayAccess(e, i) => Expr::ArrayAccess(apply_node(e), apply_node(i)),
            Expr::ArrayLiteral(items) => {
                let apply_vec = |vec: &Vec<_>| vec.clone().into_iter().map(apply_node).collect();
                Expr::ArrayLiteral(apply_vec(&items))
            }
            Expr::RecordLiteral(fields) => Expr::RecordLiteral(
                fields
                    .iter()
                    .map(|f| RecordField {
                        name: f.name,
                        expr: apply_node(f.expr),
                    })
                    .collect(),
            ),
            Expr::Apply(func, args) => Expr::Apply(
                apply_node(func),
                args.clone().into_iter().map(apply_node).collect(),
            ),
            Expr::PipeApply(lhs, rhs) => Expr::PipeApply(apply_node(lhs), apply_node(rhs)),
            Expr::FieldAccess(record, field) => Expr::FieldAccess(apply_node(record), field),
            Expr::Lambda(params, ty, body) => Expr::Lambda(params.clone(), ty, apply_node(body)),
            Expr::Feed(id, body) => Expr::Feed(id, apply_node(body)),
            Expr::Let(id, body, then) => {
                Expr::Let(id.clone(), apply_node(body), then.map(apply_node))
            }
            Expr::LetRec(id, body, then) => {
                Expr::LetRec(id.clone(), apply_node(body), then.map(apply_node))
            }
            Expr::Assign(lid, rhs) => Expr::Assign(apply_node(lid), apply_node(rhs)),
            Expr::Then(first, second) => Expr::Then(apply_node(first), second.map(apply_node)),
            Expr::If(cond, then, optelse) => Expr::If(
                apply_node(cond),
                apply_node(then),
                optelse.map(apply_node),
            ),
            Expr::Bracket(e) => Expr::Bracket(apply_node(e)),
            Expr::Escape(e) => Expr::Escape(apply_node(e)),
            _ => self.to_expr(),
        };
        expr.into_id(self.to_location())
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
    // Flatten a composite type into a list of types so that the offset of the
    // element can be calculated easily. For example:
    //
    // original:       Tuple(float, function, Tuple(float, float))
    // flattened form: [float, function, float, float]
    pub fn flatten(&self) -> Vec<Self> {
        match self.to_type() {
            Type::Tuple(t) => t.iter().flat_map(|t| t.flatten()).collect::<Vec<_>>(),
            Type::Record(t) => t.iter().flat_map(|(_, t)| t.flatten()).collect::<Vec<_>>(),
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
