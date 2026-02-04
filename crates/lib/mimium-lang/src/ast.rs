pub mod builder;
pub mod operators;
pub mod program;
mod resolve_include;
pub mod statement;
use crate::ast::operators::Op;
use crate::ast::program::QualifiedPath;
use crate::interner::{ExprNodeId, Symbol, TypeNodeId, with_session_globals};
use crate::pattern::{TypedId, TypedPattern};
use crate::utils::metadata::{Location, Span};
use crate::utils::miniprint::MiniPrint;
use std::fmt::{self};
pub type Time = i64;

#[derive(Clone, Debug, PartialEq)]
pub enum StageKind {
    Persistent = -1,
    Macro = 0,
    Main,
}
impl std::fmt::Display for StageKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StageKind::Persistent => write!(f, "persistent"),
            StageKind::Macro => write!(f, "macro"),
            StageKind::Main => write!(f, "main"),
        }
    }
}
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Literal {
    String(Symbol),
    Int(i64),
    Float(Symbol),
    SelfLit,
    Now,
    SampleRate,
    PlaceHolder,
}

impl Expr {
    fn into_id_inner(self, loc: Option<Location>) -> ExprNodeId {
        let loc = loc.unwrap_or_default();
        with_session_globals(|session_globals| session_globals.store_expr_with_location(self, loc))
    }

    pub fn into_id(self, loc: Location) -> ExprNodeId {
        self.into_id_inner(Some(loc))
    }

    // For testing purposes
    pub fn into_id_without_span(self) -> ExprNodeId {
        self.into_id_inner(None)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordField {
    pub name: Symbol,
    pub expr: ExprNodeId,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Literal), // literal, or special symbols (self, now, _)
    Var(Symbol),
    QualifiedVar(QualifiedPath), // qualified name like modA::funcB
    Block(Option<ExprNodeId>),
    Tuple(Vec<ExprNodeId>),
    Proj(ExprNodeId, i64),
    ArrayAccess(ExprNodeId, ExprNodeId),
    ArrayLiteral(Vec<ExprNodeId>),   // Array literal [e1, e2, ..., en]
    RecordLiteral(Vec<RecordField>), // Complete record literal {field1 = expr1, field2 = expr2, ...}
    ImcompleteRecord(Vec<RecordField>), // Incomplete record literal with default values {field1 = expr1, ..}
    RecordUpdate(ExprNodeId, Vec<RecordField>), // Record update syntax: { record <- field1 = expr1, field2 = expr2, ... }
    FieldAccess(ExprNodeId, Symbol),            // Record field access: record.field
    Apply(ExprNodeId, Vec<ExprNodeId>),

    MacroExpand(ExprNodeId, Vec<ExprNodeId>), // syntax sugar: hoge!(a,b) => ${hoge(a,b)}
    BinOp(ExprNodeId, (Op, Span), ExprNodeId), // syntax sugar: LHS op RHS =>  OP(LHS, RHS) except for pipe operator : RHS(LHS)
    UniOp((Op, Span), ExprNodeId), // syntax sugar: LHS op RHS =>  OP(LHS, RHS) except for pipe operator : RHS(LHS)
    Paren(ExprNodeId),             // syntax sugar to preserve context for pretty printing

    Lambda(Vec<TypedId>, Option<TypeNodeId>, ExprNodeId), //lambda, maybe information for internal state is needed
    Assign(ExprNodeId, ExprNodeId),
    Then(ExprNodeId, Option<ExprNodeId>),
    Feed(Symbol, ExprNodeId), //feedback connection primitive operation. This will be shown only after self-removal stage
    Let(TypedPattern, ExprNodeId, Option<ExprNodeId>),
    LetRec(TypedId, ExprNodeId, Option<ExprNodeId>),
    If(ExprNodeId, ExprNodeId, Option<ExprNodeId>),
    //exprimental macro system using multi-stage computation
    Bracket(ExprNodeId),
    Escape(ExprNodeId),

    Error,
}

impl ExprNodeId {
    pub fn wrap_to_staged_expr(self) -> Self {
        // TODO: what if more escape is used than minimum level??

        // let min_level = self.get_min_stage_rec(0);
        // let res = if min_level < 0 {
        //     std::iter::repeat_n((), -min_level as usize).fold(self, |wrapped, _level| {
        //         Expr::Bracket(wrapped).into_id_without_span()
        //     })
        // } else {
        //     self
        // };
        //we have to wrap one more time because if there are no macro-related expression, that means stage-1(runtime) code.
        Expr::Bracket(self).into_id_without_span()
    }
    fn get_min_stage_rec(self, current_level: i32) -> i32 {
        let conv = move |e: &Self| e.get_min_stage_rec(current_level);
        let conv2 = move |e1: &Self, e2: &Self| {
            e1.get_min_stage_rec(current_level)
                .min(e2.get_min_stage_rec(current_level))
        };
        let conv_opt = move |e: &Option<Self>| {
            e.as_ref()
                .map_or(current_level, |e| e.get_min_stage_rec(current_level))
        };
        let convvec = move |es: &[Self]| es.iter().map(conv).min().unwrap_or(current_level);
        let convfields = move |fs: &[RecordField]| {
            fs.iter()
                .map(|f| f.expr.get_min_stage_rec(current_level))
                .min()
                .unwrap_or(current_level)
        };
        match self.to_expr() {
            Expr::Bracket(e) => e.get_min_stage_rec(current_level + 1),
            Expr::Escape(e) => e.get_min_stage_rec(current_level - 1),
            Expr::MacroExpand(e, args) => conv(&e).min(convvec(&args)) - 1,
            Expr::Proj(e, _)
            | Expr::FieldAccess(e, _)
            | Expr::UniOp(_, e)
            | Expr::Paren(e)
            | Expr::Lambda(_, _, e)
            | Expr::Feed(_, e) => conv(&e),
            Expr::ArrayAccess(e1, e2) | Expr::BinOp(e1, _, e2) | Expr::Assign(e1, e2) => {
                conv2(&e1, &e2)
            }
            Expr::Block(e) => conv_opt(&e),
            Expr::Tuple(es) | Expr::ArrayLiteral(es) => convvec(&es),

            Expr::RecordLiteral(fields) | Expr::ImcompleteRecord(fields) => convfields(&fields),
            Expr::RecordUpdate(e1, fields) => conv(&e1).min(convfields(&fields)),
            Expr::Apply(e, args) => conv(&e).min(convvec(&args)),
            Expr::Then(e1, e2) | Expr::Let(_, e1, e2) | Expr::LetRec(_, e1, e2) => {
                conv(&e1).min(conv_opt(&e2))
            }
            Expr::If(cond, then, orelse) => conv(&cond).min(conv(&then)).min(conv_opt(&orelse)),

            _ => current_level,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Float(n) => write!(f, "(float {n})"),
            Literal::Int(n) => write!(f, "(int {n})"),
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Now => write!(f, "now"),
            Literal::SampleRate => write!(f, "samplerate"),
            Literal::SelfLit => write!(f, "self"),
            Literal::PlaceHolder => write!(f, "_"),
        }
    }
}

impl MiniPrint for Literal {
    fn simple_print(&self) -> String {
        self.to_string()
    }
}

fn concat_vec<T: MiniPrint>(vec: &[T]) -> String {
    vec.iter()
        .map(|t| t.simple_print())
        .collect::<Vec<_>>()
        .join(" ")
}

impl MiniPrint for ExprNodeId {
    fn simple_print(&self) -> String {
        let span = self.to_span();
        format!(
            "{}:{}..{}",
            self.to_expr().simple_print(),
            span.start,
            span.end
        )
    }
}

impl MiniPrint for Option<ExprNodeId> {
    fn simple_print(&self) -> String {
        match self {
            Some(e) => e.simple_print(),
            None => "()".to_string(),
        }
    }
}

impl MiniPrint for RecordField {
    fn simple_print(&self) -> String {
        format!("{}: {}", self.name, self.expr.simple_print())
    }
}

impl MiniPrint for Expr {
    fn simple_print(&self) -> String {
        match self {
            Expr::Literal(l) => l.simple_print(),
            Expr::Var(v) => format!("{v}"),
            Expr::QualifiedVar(path) => path
                .segments
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join("::"),
            Expr::Block(e) => e.map_or("".to_string(), |eid| {
                format!("(block {})", eid.simple_print())
            }),
            Expr::Tuple(e) => {
                let e1 = e.iter().map(|e| e.to_expr().clone()).collect::<Vec<Expr>>();
                format!("(tuple ({}))", concat_vec(&e1))
            }
            Expr::Proj(e, idx) => format!("(proj {} {})", e.simple_print(), idx),
            Expr::Apply(e1, e2) => {
                format!("(app {} ({}))", e1.simple_print(), concat_vec(e2))
            }
            Expr::MacroExpand(e1, e2s) => {
                format!("(macro {} ({}))", e1.simple_print(), concat_vec(e2s))
            }
            Expr::ArrayAccess(e, i) => {
                format!("(arrayaccess {} ({}))", e.simple_print(), i.simple_print())
            }
            Expr::ArrayLiteral(items) => {
                let items_str = items
                    .iter()
                    .map(|e| e.simple_print())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("(array [{items_str}])")
            }
            Expr::RecordLiteral(fields) => {
                let fields_str = fields
                    .iter()
                    .map(|f| f.simple_print())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("(record {{{fields_str}}})")
            }
            Expr::ImcompleteRecord(fields) => {
                let fields_str = fields
                    .iter()
                    .map(|f| f.simple_print())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("(incomplete-record {{{fields_str}, ..}})")
            }
            Expr::RecordUpdate(record, fields) => {
                let fields_str = fields
                    .iter()
                    .map(|f| f.simple_print())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!(
                    "(record-update {} {{{}}})",
                    record.simple_print(),
                    fields_str
                )
            }
            Expr::FieldAccess(record, field) => {
                format!("(field-access {} {})", record.simple_print(), field)
            }
            Expr::UniOp(op, expr) => {
                format!("(unary {} {})", op.0, expr.simple_print())
            }
            Expr::BinOp(lhs, op, rhs) => {
                format!(
                    "(binop {} {} {})",
                    op.0,
                    lhs.simple_print(),
                    rhs.simple_print()
                )
            }
            Expr::Lambda(params, _, body) => {
                format!("(lambda ({}) {})", concat_vec(params), body.simple_print())
            }
            Expr::Feed(id, body) => format!("(feed {} {})", id, body.simple_print()),
            Expr::Let(id, body, then) => format!(
                "(let {} {} {})",
                id.simple_print(),
                body.simple_print(),
                then.simple_print()
            ),
            Expr::LetRec(id, body, then) => format!(
                "(letrec {} {} {})",
                &id.simple_print(),
                body.simple_print(),
                then.simple_print()
            ),
            Expr::Assign(lid, rhs) => {
                format!("(assign {} {})", lid.simple_print(), rhs.simple_print())
            }
            Expr::Then(first, second) => {
                format!("(then {} {})", first.simple_print(), second.simple_print())
            }
            Expr::If(cond, then, optelse) => format!(
                "(if {} {} {})",
                cond.simple_print(),
                then.simple_print(),
                optelse.simple_print()
            ),
            Expr::Bracket(e) => format!("(bracket {})", e.simple_print()),
            Expr::Escape(e) => format!("(escape {})", e.simple_print()),
            Expr::Error => "(error)".to_string(),
            Expr::Paren(expr_node_id) => format!("(paren {})", expr_node_id.simple_print()),
        }
    }
}
