use std::path::PathBuf;

///remove redundunt letrec definition and convert them to plain let
use crate::{
    ast::{Expr, RecordField},
    interner::{ExprNodeId, Symbol},
    pattern::TypedPattern,
    utils::metadata::Location,
};

fn try_find_recurse(e_s: ExprNodeId, name: Symbol) -> bool {
    match e_s.to_expr() {
        Expr::Var(n) => n == name,
        Expr::Let(_id, body, then) => {
            try_find_recurse(body, name) || then.is_some_and(|e| try_find_recurse(e, name))
        }
        Expr::LetRec(_id, _body, _then) => {
            //todo: start new search so we return false here
            false
        }
        Expr::Assign(_v, e) => try_find_recurse(e, name),
        Expr::Then(body, then_opt) => {
            try_find_recurse(body, name)
                || then_opt.is_some_and(|then| try_find_recurse(then, name))
        }
        Expr::Lambda(_ids, _opt_type, body) => try_find_recurse(body, name),
        Expr::Proj(body, _idx) => try_find_recurse(body, name),
        Expr::Block(body) => body.is_some_and(|b| try_find_recurse(b, name)),
        Expr::Apply(fun, callee) => {
            try_find_recurse(fun, name) || callee.into_iter().any(|v| try_find_recurse(v, name))
        }
        Expr::Tuple(vec) => vec.iter().any(|v| try_find_recurse(*v, name)),
        Expr::If(cond, then, opt_else) => {
            try_find_recurse(cond, name)
                || try_find_recurse(then, name)
                || opt_else.is_some_and(|e| try_find_recurse(e, name))
        }
        Expr::Bracket(e) | Expr::Escape(e) => try_find_recurse(e, name),
        Expr::Feed(_x, body) => try_find_recurse(body, name),
        Expr::ArrayAccess(e, i) => try_find_recurse(e, name) || try_find_recurse(i, name),
        Expr::ArrayLiteral(items) => items.iter().any(|e| try_find_recurse(*e, name)),
        Expr::RecordLiteral(fields) => fields.iter().any(|f| try_find_recurse(f.expr, name)),
        Expr::ImcompleteRecord(record_fields) => {
            record_fields.iter().any(|f| try_find_recurse(f.expr, name))
        }
        Expr::RecordUpdate(record, fields) => {
            try_find_recurse(record, name) || fields.iter().any(|f| try_find_recurse(f.expr, name))
        }
        Expr::FieldAccess(record, _field) => try_find_recurse(record, name),
        Expr::BinOp(_, _, _) => unreachable!(),
        Expr::UniOp(_, _) => unreachable!(),
        Expr::MacroExpand(_, _) => unreachable!(),
        Expr::Paren(inner) => try_find_recurse(inner, name),
        Expr::Match(scrutinee, arms) => {
            try_find_recurse(scrutinee, name)
                || arms.iter().any(|arm| try_find_recurse(arm.body, name))
        }
        Expr::Literal(_) | Expr::Error => false,
        // Qualified variables don't directly recurse; name resolution handles them
        Expr::QualifiedVar(_) => false,
    }
}

pub fn convert_recurse(e_s: ExprNodeId, file_path: PathBuf) -> ExprNodeId {
    let convert = |v: ExprNodeId| convert_recurse(v, file_path.clone());
    let convert_vec = |v: Vec<_>| {
        v.into_iter()
            .map(|e| convert_recurse(e, file_path.clone()))
            .collect()
    };
    let span = e_s.to_span();
    let res = match e_s.to_expr() {
        Expr::LetRec(id, body, then) => {
            if !try_find_recurse(body, id.id) {
                Expr::Let(
                    TypedPattern::from(id.clone()),
                    convert(body),
                    then.map(convert),
                )
            } else {
                Expr::LetRec(id.clone(), convert(body), then.map(convert))
            }
        }
        Expr::Let(id, body, then) => Expr::Let(id.clone(), convert(body), then.map(convert)),
        Expr::Assign(v, e) => Expr::Assign(v, convert(e)),
        Expr::Then(body, then_opt) => Expr::Then(convert(body), then_opt.map(convert)),
        Expr::Tuple(es) => Expr::Tuple(convert_vec(es)),
        Expr::Proj(t, idx) => Expr::Proj(convert(t), idx),
        Expr::Block(body) => Expr::Block(body.map(convert)),
        Expr::Apply(fun, callee) => Expr::Apply(convert(fun), convert_vec(callee)),
        Expr::If(cond, then, opt_else) => {
            Expr::If(convert(cond), convert(then), opt_else.map(convert))
        }
        Expr::Lambda(ids, opt_type, body) => Expr::Lambda(ids.clone(), opt_type, convert(body)),
        Expr::Feed(x, body) => Expr::Feed(x, convert(body)),
        Expr::Bracket(e) => Expr::Bracket(convert(e)),
        Expr::Escape(e) => Expr::Escape(convert(e)),
        Expr::ArrayAccess(e, i) => Expr::ArrayAccess(convert(e), convert(i)),
        Expr::ArrayLiteral(items) => Expr::ArrayLiteral(convert_vec(items)),
        Expr::RecordLiteral(fields) => Expr::RecordLiteral(
            fields
                .iter()
                .map(|f| RecordField {
                    name: f.name,
                    expr: convert(f.expr),
                })
                .collect(),
        ),
        Expr::ImcompleteRecord(record_fields) => Expr::ImcompleteRecord(
            record_fields
                .iter()
                .map(|f| RecordField {
                    name: f.name,
                    expr: convert(f.expr),
                })
                .collect(),
        ),
        Expr::RecordUpdate(record, fields) => Expr::RecordUpdate(
            convert(record),
            fields
                .iter()
                .map(|f| RecordField {
                    name: f.name,
                    expr: convert(f.expr),
                })
                .collect(),
        ),
        Expr::FieldAccess(record, field) => Expr::FieldAccess(convert(record), field),
        Expr::Match(scrutinee, arms) => Expr::Match(
            convert(scrutinee),
            arms.iter()
                .map(|arm| crate::ast::MatchArm {
                    pattern: arm.pattern.clone(),
                    body: convert(arm.body),
                })
                .collect(),
        ),
        Expr::BinOp(_, _, _) => unreachable!(),
        Expr::UniOp(_, _) => unreachable!(),
        Expr::MacroExpand(_, _) => unreachable!(),
        Expr::Paren(inner) => return convert_recurse(inner, file_path),
        Expr::Literal(_) | Expr::Var(_) | Expr::Error | Expr::QualifiedVar(_) => e_s.to_expr(),
    };
    let loc = Location {
        span,
        path: file_path.clone(),
    };
    res.into_id(loc)
}

#[cfg(test)]
mod test {

    use crate::{
        app,
        ast::{Expr, Literal},
        ifexpr, lambda, let_, letrec, number,
        pattern::TypedId,
        var,
    };

    use super::*;
    #[test]
    fn recurse_remove() {
        let sample = letrec!(
            "testfn",
            None,
            lambda!(
                ["count"],
                ifexpr!(
                    var!("test"),
                    app!(var!("testfn"), vec![number!("10.0")]),
                    //this letrec should be converted to plain let
                    letrec!("lettest", None, number!("12.0"), Some(number!("2.0")))
                )
            ),
            None
        );
        // top letrec should not be converted
        let ans = letrec!(
            "testfn",
            None,
            lambda!(
                ["count"],
                ifexpr!(
                    var!("test"),
                    app!(var!("testfn"), vec![number!("10.0")]),
                    // this
                    let_!("lettest", number!("12.0"), number!("2.0"))
                )
            ),
            None
        );

        assert_eq!(convert_recurse(sample, PathBuf::from("/")), ans)
    }
}
