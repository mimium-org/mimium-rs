use crate::{
    ast::{Expr, RecordField},
    interner::{ExprNodeId, TypeNodeId},
    pattern::{TypedId, TypedPattern},
    utils::metadata::Location,
};

pub fn desugar(expr_id: ExprNodeId) -> ExprNodeId {
    let expr = expr_id.to_expr();
    let loc = expr_id.to_loc();
    match expr {
        Expr::FnDef(name, args, ret_type, body) => {
            let lambda = Expr::Lambda(args, ret_type, desugar(body)).into_id(loc.clone());
            let let_rec = Expr::LetRec(name, lambda, then.map(desugar));
            let_rec.into_id(loc)
        }
        Expr::Literal(_) | Expr::Var(_) | Expr::Error => expr_id,
        Expr::Block(e) => Expr::Block(e.map(desugar)).into_id(loc),
        Expr::Tuple(es) => Expr::Tuple(es.into_iter().map(desugar).collect()).into_id(loc),
        Expr::Proj(e, i) => Expr::Proj(desugar(e), i).into_id(loc),
        Expr::ArrayAccess(e, i) => Expr::ArrayAccess(desugar(e), desugar(i)).into_id(loc),
        Expr::ArrayLiteral(es) => {
            Expr::ArrayLiteral(es.into_iter().map(desugar).collect()).into_id(loc)
        }
        Expr::RecordLiteral(fs) => Expr::RecordLiteral(
            fs.into_iter()
                .map(|f| RecordField {
                    name: f.name,
                    expr: desugar(f.expr),
                })
                .collect(),
        )
        .into_id(loc),
        Expr::FieldAccess(e, f) => Expr::FieldAccess(desugar(e), f).into_id(loc),
        Expr::Apply(e, es) => {
            Expr::Apply(desugar(e), es.into_iter().map(desugar).collect()).into_id(loc)
        }
        Expr::PipeApply(e1, e2) => Expr::PipeApply(desugar(e1), desugar(e2)).into_id(loc),
        Expr::Lambda(args, ret_type, body) => {
            Expr::Lambda(args, ret_type, desugar(body)).into_id(loc)
        }
        Expr::Assign(e1, e2) => Expr::Assign(desugar(e1), desugar(e2)).into_id(loc),
        Expr::Then(e1, e2) => Expr::Then(desugar(e1), e2.map(desugar)).into_id(loc),
        Expr::Feed(s, e) => Expr::Feed(s, desugar(e)).into_id(loc),
        Expr::Let(p, e, t) => Expr::Let(p, desugar(e), t.map(desugar)).into_id(loc),
        Expr::LetRec(id, e, t) => Expr::LetRec(id, desugar(e), t.map(desugar)).into_id(loc),
        Expr::If(c, t, e) => Expr::If(desugar(c), desugar(t), e.map(desugar)).into_id(loc),
        Expr::Bracket(e) => Expr::Bracket(desugar(e)).into_id(loc),
        Expr::Escape(e) => Expr::Escape(desugar(e)).into_id(loc),
    }
}
