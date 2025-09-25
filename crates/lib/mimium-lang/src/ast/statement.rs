use crate::{
    ast::Expr,
    interner::ExprNodeId,
    pattern::{TypedId, TypedPattern},
    utils::metadata::{Location, Span},
};

use super::StageKind;
// an intermediate representation used in parser.
// Note that this struct do not distinct between a global statement(allows `fn(){}`) and a local statement.
// The distinction is done in the actual parser logic.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(TypedPattern, ExprNodeId),
    LetRec(TypedId, ExprNodeId),
    Assign(ExprNodeId, ExprNodeId),
    Single(ExprNodeId),
    DeclareStage(StageKind),
    Error,
}

pub fn stmt_from_expr_top(expr: ExprNodeId) -> Vec<Statement> {
    let mut res = vec![];
    stmt_from_expr(expr, &mut res);
    res
}
fn stmt_from_expr(expr: ExprNodeId, target: &mut Vec<Statement>) {
    match expr.to_expr() {
        Expr::Let(pat, e, then_opt) => {
            target.push(Statement::Let(pat, e));
            if let Some(then) = then_opt {
                stmt_from_expr(then, target);
            }
        }
        Expr::LetRec(id, e, then_opt) => {
            target.push(Statement::LetRec(id, e));
            if let Some(then) = then_opt {
                stmt_from_expr(then, target);
            }
        }
        _ => target.push(Statement::Single(expr)),
    }
}

// A helper function to convert vector of statements to nested expression
pub(crate) fn into_then_expr(stmts: &[(Statement, Location)]) -> Option<ExprNodeId> {
    // Concat all statements into one nested expression.
    // We need to check statements from first to last to resolve macro stage, but we need to build expression from last to first.
    // So we build a closure to build expression from first to last, and then evaluate them in reverse order.
    type ClsType = Box<dyn FnOnce(Option<ExprNodeId>) -> Option<ExprNodeId>>;
    let mut last_stage = StageKind::Main;
    let mut closures = Vec::<ClsType>::new();
    for (stmt, loc) in stmts.iter() {
        let stmt = stmt.clone();
        let loc = loc.clone();
        let cls = match stmt {
            Statement::Let(typed_pattern, expr_node_id) => Box::new(move |then| {
                Some(Expr::Let(typed_pattern.clone(), expr_node_id, then).into_id(loc.clone()))
            }) as ClsType,
            Statement::LetRec(typed_id, expr_node_id) => Box::new(move |then| {
                Some(Expr::LetRec(typed_id.clone(), expr_node_id, then).into_id(loc.clone()))
            }) as ClsType,
            Statement::Assign(expr_node_id, expr_node_id1) => Box::new(move |then| {
                Some(
                    Expr::Then(
                        Expr::Assign(expr_node_id, expr_node_id1).into_id(loc.clone()),
                        then,
                    )
                    .into_id(loc.clone()),
                )
            }) as ClsType,
            Statement::Single(expr_node_id) => {
                Box::new(move |then: Option<ExprNodeId>| match then {
                    None => Some(expr_node_id),
                    Some(t) => Some(Expr::Then(expr_node_id, Some(t)).into_id(loc.clone())),
                }) as ClsType
            }
            Statement::DeclareStage(stage_kind) => {
                let res = match (&last_stage, stage_kind.clone()) {
                    (StageKind::Macro, StageKind::Main) => {
                        Box::new(|then: Option<_>| then.map(|e| Expr::Bracket(e).into_id(loc)))
                            as ClsType
                    }
                    (StageKind::Main, StageKind::Macro) => {
                        Box::new(|then: Option<_>| then.map(|e| Expr::Escape(e).into_id(loc)))
                            as ClsType
                    }
                    (StageKind::Persistent, _) => {
                        log::warn!("Persistent stage declaration is not supported yet,ignored");
                        Box::new(move |then: Option<ExprNodeId>| then) as ClsType
                    }
                    (_, _) => Box::new(move |then: Option<ExprNodeId>| then) as ClsType,
                };
                last_stage = stage_kind.clone();
                res
            }
            Statement::Error => Box::new(move |then| {
                Some(Expr::Then(Expr::Error.into_id(loc.clone()), then).into_id(loc.clone()))
            }) as ClsType,
        };
        closures.push(cls);
    }
    let e_pre = closures
        .into_iter()
        .rev()
        .fold(None, move |then, cls: ClsType| cls(then));
    // log::debug!("stmts {:?}, e_pre: {:?}", stmts, e_pre);
    e_pre
}
