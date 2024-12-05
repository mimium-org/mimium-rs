pub mod builder;

use crate::interner::{with_session_globals, ExprNodeId, Symbol, TypeNodeId};
use crate::pattern::{TypedId, TypedPattern};
use crate::utils::metadata::Location;
use crate::utils::miniprint::MiniPrint;
use std::fmt::{self};
pub type Time = i64;

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
pub enum Expr {
    Literal(Literal), // literal, or special symbols (self, now, _)
    Var(Symbol),
    Block(Option<ExprNodeId>),
    Tuple(Vec<ExprNodeId>),
    Proj(ExprNodeId, i64),
    ArrayAccess(ExprNodeId, ExprNodeId),
    Apply(ExprNodeId, Vec<ExprNodeId>),
    PipeApply(ExprNodeId, ExprNodeId), // LHS and RHS
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

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Float(n) => write!(f, "(float {})", n),
            Literal::Int(n) => write!(f, "(int {})", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
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

impl MiniPrint for Expr {
    fn simple_print(&self) -> String {
        match self {
            Expr::Literal(l) => l.simple_print(),
            Expr::Var(v) => format!("{v}"),
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
            Expr::ArrayAccess(e, i) => {
                format!("(arrayaccess {} ({}))", e.simple_print(), i.simple_print())
            }
            Expr::PipeApply(lhs, rhs) => {
                format!("(pipe {} {})", lhs.simple_print(), rhs.simple_print())
            }
            Expr::Lambda(params, _, body) => {
                format!("(lambda ({}) {})", concat_vec(params), body.simple_print())
            }
            Expr::Feed(id, body) => format!("(feed {} {})", id, body.simple_print()),
            Expr::Let(id, body, then) => format!(
                "(let {} {} {})",
                id,
                body.simple_print(),
                then.simple_print()
            ),
            Expr::LetRec(id, body, then) => format!(
                "(letrec {} {} {})",
                &id.id,
                body.simple_print(),
                then.simple_print()
            ),
            Expr::Assign(lid, rhs) => format!("(assign {lid} {})", rhs.simple_print()),
            Expr::Then(first, second) => {
                format!("(then {} {})", first.simple_print(), second.simple_print())
            }
            Expr::If(cond, then, optelse) => format!(
                "(if {} {} {})",
                cond.simple_print(),
                then.simple_print(),
                optelse.simple_print()
            ),
            Expr::Bracket(_) => todo!(),
            Expr::Escape(_) => todo!(),
            Expr::Error => "(error)".to_string(),
        }
    }
}
