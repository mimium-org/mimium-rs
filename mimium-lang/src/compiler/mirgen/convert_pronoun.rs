use crate::ast::{Expr, Literal};
use crate::interner::{ExprNodeId, Symbol, ToSymbol};
use crate::pattern::TypedId;
use crate::types::Type;
use crate::utils::error::SimpleError;
use crate::utils::metadata::Location;
pub type Error = SimpleError;
struct ConvertResult {
    expr: ExprNodeId,
    found_any: bool,
}

// This applies conversion() recursively. This is intended to be used in the `_`
// branch of pattern matching so that particular types of epressions can be
// caught and treated differently.
fn convert_recursively<F>(
    e_id: ExprNodeId,
    conversion: F,
    file_path: Symbol,
) -> (ConvertResult, Vec<Error>)
where
    F: Fn(ExprNodeId) -> (ConvertResult, Vec<Error>),
{
    let loc = Location {
        span: e_id.to_span().clone(),
        path: file_path,
    };

    let opt_conversion = |opt_e: Option<ExprNodeId>| -> (Option<ConvertResult>, Vec<Error>) {
        let mut err_res = vec![];
        let expr = opt_e.map(|e| {
            let (res, errs) = conversion(e);
            err_res = errs;
            res
        });
        (expr, err_res)
    };
    match e_id.to_expr().clone() {
        Expr::Tuple(v) => {
            let (res_vec, errs): (Vec<_>, Vec<_>) = v.into_iter().map(&conversion).unzip();
            let res_e = Expr::Tuple(res_vec.iter().map(|e| e.expr).collect()).into_id(loc);
            let found_any = res_vec.into_iter().any(|e| e.found_any);
            (
                ConvertResult {
                    expr: res_e,
                    found_any,
                },
                errs.concat(),
            )
        }
        Expr::Proj(e, idx) => {
            let (res, errs) = conversion(e);
            (
                ConvertResult {
                    expr: Expr::Proj(res.expr, idx).into_id(loc),
                    found_any: res.found_any,
                },
                errs,
            )
        }
        Expr::Let(id, body, then) => {
            let (bodyres, errs) = conversion(body);
            let (then_res, errs2) = opt_conversion(then);
            let found_any = bodyres.found_any | then_res.as_ref().is_some_and(|r| r.found_any);
            let expr = Expr::Let(id, bodyres.expr, then_res.map(|r| r.expr)).into_id(loc);
            (ConvertResult { expr, found_any }, [errs, errs2].concat())
        }
        Expr::LetRec(id, body, then) => {
            let (bodyres, errs) = conversion(body);
            let (then_res, errs2) = opt_conversion(then);
            let found_any = bodyres.found_any | then_res.as_ref().is_some_and(|r| r.found_any);
            let expr = Expr::LetRec(id, bodyres.expr, then_res.map(|r| r.expr)).into_id(loc);
            (ConvertResult { expr, found_any }, [errs, errs2].concat())
        }
        Expr::Lambda(params, r_type, body) => {
            // Note: params and r_type cannot be handled by conversion() because
            //       these are Type, not Expr.
            let (ConvertResult { expr, found_any }, errs) = conversion(body);
            let expr = Expr::Lambda(params, r_type, expr).into_id(loc);
            (ConvertResult { expr, found_any }, errs)
        }
        Expr::Apply(fun, callee) => {
            let (fun, errs) = conversion(fun);
            let (res_vec, errs2): (Vec<_>, Vec<_>) = callee.into_iter().map(&conversion).unzip();
            let expr = Expr::Apply(fun.expr, res_vec.iter().map(|e| e.expr).collect()).into_id(loc);
            let found_any = res_vec.into_iter().any(|e| e.found_any) | fun.found_any;
            (
                ConvertResult { expr, found_any },
                [errs, errs2.concat()].concat(),
            )
        }
        Expr::PipeApply(callee, fun) => {
            let (callee, errs) = conversion(callee);
            let (fun, errs2) = conversion(fun);
            let expr = Expr::PipeApply(callee.expr, fun.expr).into_id(loc);
            let found_any = callee.found_any | fun.found_any;
            (ConvertResult { expr, found_any }, [errs, errs2].concat())
        }
        Expr::If(cond, then, opt_else) => {
            let (cond, err) = conversion(cond);
            let (then, err2) = conversion(then);
            let (opt_else, errs3) = opt_conversion(opt_else);
            let found_any =
                cond.found_any | then.found_any | opt_else.as_ref().is_some_and(|e| e.found_any);
            let expr = Expr::If(cond.expr, then.expr, opt_else.map(|e| e.expr)).into_id(loc);
            let errs = [err, err2, errs3].concat();
            (ConvertResult { expr, found_any }, errs)
        }
        Expr::Block(body) => {
            let (body, errs) = opt_conversion(body);
            let found_any = body.as_ref().is_some_and(|b| b.found_any);
            let expr = Expr::Block(body.map(|e| e.expr)).into_id(loc);
            (ConvertResult { expr, found_any }, errs)
        }
        Expr::Escape(e) => {
            let (res, errs) = conversion(e);
            let expr = Expr::Escape(res.expr).into_id(loc);
            (
                ConvertResult {
                    expr,
                    found_any: res.found_any,
                },
                errs,
            )
        }
        Expr::Bracket(e) => {
            let (res, errs) = conversion(e);
            let expr = Expr::Bracket(res.expr).into_id(loc);
            (
                ConvertResult {
                    expr,
                    found_any: res.found_any,
                },
                errs,
            )
        }
        _ => (
            ConvertResult {
                expr: e_id,
                found_any: false,
            },
            vec![],
        ),
    }
}
fn convert_recursively_pure<F>(e_id: ExprNodeId, conversion: F, file_path: Symbol) -> ExprNodeId
where
    F: Fn(ExprNodeId) -> ExprNodeId,
{
    convert_recursively(
        e_id,
        |e| {
            let dummy_flag = false;
            let dummy_errs = vec![];
            (
                ConvertResult {
                    expr: conversion(e),
                    found_any: dummy_flag,
                },
                dummy_errs,
            )
        },
        file_path,
    )
    .0
    .expr
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FeedId {
    Global,
    Local(i64),
}
impl FeedId {
    pub(self) fn get_next_id(self) -> Self {
        match self {
            Self::Global => Self::Local(0),
            Self::Local(i) => Self::Local(i + 1),
        }
    }
    pub(self) fn get_name(&self) -> Symbol {
        match self {
            FeedId::Global => "feed_global".to_symbol(),
            FeedId::Local(i) => format!("feed_id{i}").to_symbol(),
        }
    }
}

fn convert_self(
    e_id: ExprNodeId,
    feedctx: FeedId,
    file_path: Symbol,
) -> (ConvertResult, Vec<Error>) {
    let conversion =
        |e: ExprNodeId| -> (ConvertResult, Vec<Error>) { convert_self(e, feedctx, file_path) };
    let loc = Location::new(e_id.to_span().clone(), file_path);

    match e_id.to_expr().clone() {
        Expr::Literal(Literal::SelfLit) => match feedctx {
            FeedId::Global => {
                let err = vec![SimpleError {
                    message: "self cannot be used in global context.".to_string(),
                    span: loc.clone(),
                }];
                (
                    ConvertResult {
                        expr: Expr::Error.into_id(loc),
                        found_any: false,
                    },
                    err,
                )
            }
            FeedId::Local(_) => {
                let expr = Expr::Var(feedctx.get_name()).into_id(loc.clone());
                (
                    ConvertResult {
                        expr,
                        found_any: true,
                    },
                    vec![],
                )
            }
        },
        Expr::Lambda(params, r_type, body) => {
            let nfctx = feedctx.get_next_id();
            let (res, err) = convert_self(body, nfctx, file_path);
            let nbody = if res.found_any {
                Expr::Feed(nfctx.get_name(), res.expr).into_id(loc.clone())
            } else {
                res.expr
            };
            let expr = Expr::Lambda(params, r_type, nbody).into_id(loc.clone());
            (
                ConvertResult {
                    expr,
                    found_any: false,
                },
                err,
            )
        }
        Expr::Feed(_, _) => panic!(
            "Feed should not be shown before conversion at {}..{}",
            loc.span.start, loc.span.end
        ),
        _ => convert_recursively(e_id, conversion, file_path),
    }
}

fn convert_placeholder(e_id: ExprNodeId, file_path: Symbol) -> ExprNodeId {
    let loc = Location::new(e_id.to_span().clone(), file_path);
    match e_id.to_expr() {
        // if _ is used outside of pipe, treat it as a usual variable.
        Expr::Literal(Literal::PlaceHolder) => Expr::Var("_".to_symbol()).into_id(loc),
        Expr::Apply(fun, args)
            if args
                .iter()
                .any(|e| matches!(e.to_expr(), Expr::Literal(Literal::PlaceHolder))) =>
        {
            let fun = convert_placeholder(fun, file_path);
            let (lambda_args_sparse, new_args): (Vec<_>, Vec<_>) = args
                .into_iter()
                .enumerate()
                .map(|(i, e)| {
                    if matches!(e.to_expr(), Expr::Literal(Literal::PlaceHolder)) {
                        let loc = Location::new(e_id.to_span().clone(), file_path);
                        let id = format!("__lambda_arg_{i}").to_symbol();
                        let ty = Type::Unknown.into_id_with_location(loc.clone());
                        let newid = TypedId { id, ty };
                        let e = Expr::Var(id).into_id(loc);
                        (Some(newid), e)
                    } else {
                        (None, e)
                    }
                })
                .unzip();
            let lambda_args = lambda_args_sparse.into_iter().flatten().collect();
            let body = Expr::Apply(fun, new_args).into_id(loc.clone());
            Expr::Lambda(lambda_args, None, body).into_id(loc.clone())
        }
        _ => convert_recursively_pure(e_id, |e| convert_placeholder(e, file_path), file_path),
    }
}

fn convert_pipe(e_id: ExprNodeId, file_path: Symbol) -> ExprNodeId {
    let loc = Location::new(e_id.to_span().clone(), file_path);
    match e_id.to_expr() {
        Expr::PipeApply(callee, fun) => {
            let callee = convert_pipe(callee, file_path);
            let fun = convert_pipe(fun, file_path);
            Expr::Apply(fun, vec![callee]).into_id(loc)
        }
        // because convert_pipe never fails, it propagate dummy error
        _ => convert_recursively_pure(e_id, |e| convert_pipe(e, file_path), file_path),
    }
}
fn convert_macroexpand(e_id: ExprNodeId, file_path: Symbol) -> ExprNodeId {
    let loc = Location::new(e_id.to_span().clone(), file_path);
    match e_id.to_expr() {
        Expr::MacroExpand(callee, args) => {
            let callee = convert_pipe(callee, file_path);
            let args = args
                .into_iter()
                .map(|arg| convert_pipe(arg, file_path))
                .collect();

            Expr::Escape(Expr::Apply(callee, args).into_id(loc.clone())).into_id(loc)
        }
        _ => convert_recursively_pure(e_id, |e| convert_macroexpand(e, file_path), file_path),
    }
}

pub fn convert_pronoun(expr: ExprNodeId, file_path: Symbol) -> (ExprNodeId, Vec<Error>) {
    // these 3 operations can be done in any order, and ideally, merged to one pattern matching. 
    // However, for clarity, we split them into separated operations.
    let expr = convert_placeholder(expr, file_path);
    let expr = convert_pipe(expr, file_path);
    let expr = convert_macroexpand(expr, file_path);
    let (res, errs) = convert_self(expr, FeedId::Global, file_path);
    (res.expr, errs)
}

#[cfg(test)]
mod test {
    use crate::{
        pattern::{Pattern, TypedId, TypedPattern},
        types::Type,
    };

    use super::*;

    #[test]
    pub fn test_selfconvert() {
        let loc = Location {
            span: 0..1,
            path: "/".to_symbol(),
        };
        let unknownty = Type::Unknown.into_id_with_location(loc.clone());
        let src = Expr::Let(
            TypedPattern {
                pat: Pattern::Single("lowpass".to_symbol()),
                ty: unknownty,
            },
            Expr::Lambda(
                vec![TypedId {
                    id: "input".to_symbol(),
                    ty: unknownty,
                }],
                None,
                Expr::Literal(Literal::SelfLit).into_id(loc.clone()),
            )
            .into_id(loc.clone()),
            None,
        )
        .into_id(loc.clone());
        let (res, errs) = convert_pronoun(src, "/".to_symbol());

        let ans = Expr::Let(
            TypedPattern {
                pat: Pattern::Single("lowpass".to_symbol()),
                ty: unknownty,
            },
            Expr::Lambda(
                vec![TypedId {
                    id: "input".to_symbol(),
                    ty: unknownty,
                }],
                None,
                Expr::Feed(
                    "feed_id0".to_symbol(),
                    Expr::Var("feed_id0".to_symbol()).into_id(loc.clone()),
                )
                .into_id(loc.clone()),
            )
            .into_id(loc.clone()),
            None,
        )
        .into_id(loc);
        assert!(errs.is_empty());
        assert_eq!(res, ans);
    }
}
