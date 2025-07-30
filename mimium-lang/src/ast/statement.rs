use crate::{
    ast::Expr,
    interner::ExprNodeId,
    pattern::{TypedId, TypedPattern},
    utils::metadata::{Location, Span}
};


// an intermediate representation used in parser.
// Note that this struct do not distinct between a global statement(allows `fn(){}`) and a local statement.
// The distinction is done in the actual parser logic.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Statement {
    Let(TypedPattern, ExprNodeId),
    LetRec(TypedId, ExprNodeId),
    Assign(ExprNodeId, ExprNodeId),
    Single(ExprNodeId),
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
    let get_span = |spana: Span, spanb: Option<ExprNodeId>| match spanb {
        Some(b) => {
            let start = spana.start;
            start..b.to_span().end
        }
        None => spana,
    };
    let e_pre = stmts.iter().rev().fold(None, |then, (stmt, loc)| {
        let span = get_span(loc.span.clone(), then);
        let new_loc = Location {
            span,
            path: loc.path,
        };
        match (then, stmt) {
            (_, Statement::Let(pat, body)) => {
                Some(Expr::Let(pat.clone(), *body, then).into_id(new_loc))
            }

            (_, Statement::LetRec(id, body)) => {
                Some(Expr::LetRec(id.clone(), *body, then).into_id(new_loc))
            }
            (_, Statement::Assign(name, body)) => Some(
                Expr::Then(Expr::Assign(*name, *body).into_id(loc.clone()), then).into_id(new_loc),
            ),
            (None, Statement::Single(e)) => Some(*e),
            (t, Statement::Single(e)) => Some(Expr::Then(*e, t).into_id(new_loc)),
            (t, Statement::Error) => {
                Some(Expr::Then(Expr::Error.into_id(new_loc.clone()), t).into_id(new_loc))
            }
        }
    });
    // log::debug!("stmts {:?}, e_pre: {:?}", stmts, e_pre);
    e_pre
}
