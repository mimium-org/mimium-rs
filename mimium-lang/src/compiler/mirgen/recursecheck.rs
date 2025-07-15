///remove redundunt letrec definition and convert them to plain let
use crate::{
    ast::Expr,
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
        Expr::Feed(_x, _body) => panic!("feed should not be shown in recurse removal process"),
        _ => false,
    }
}

pub fn convert_recurse(e_s: ExprNodeId, file_path: Symbol) -> ExprNodeId {
    let convert = |v: ExprNodeId| convert_recurse(v, file_path);
    let convert_vec = |v: Vec<_>| {
        v.into_iter()
            .map(|e| convert_recurse(e, file_path))
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
        Expr::Feed(_x, _body) => panic!("feed should not be shown in recurse removal process"),
        Expr::Bracket(e) => Expr::Bracket(convert(e)),
        Expr::Escape(e) => Expr::Escape(convert(e)),
        e => e.clone(),
    };
    let loc = Location {
        span,
        path: file_path,
    };
    res.into_id(loc)
}

#[cfg(test)]
mod test {

    use crate::{
        app,
        ast::{Expr, Literal},
        ifexpr,
        interner::ToSymbol,
        lambda, let_, letrec, number,
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

        assert_eq!(convert_recurse(sample, "/".to_symbol()), ans)
    }
}
