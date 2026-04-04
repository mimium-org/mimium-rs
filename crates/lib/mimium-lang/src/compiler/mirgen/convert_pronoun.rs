use std::path::PathBuf;

use crate::ast::operators::Op;
use crate::ast::program::QualifiedPath;
use crate::ast::{Expr, Literal, RecordField};
use crate::interner::{ExprNodeId, Symbol, ToSymbol};
use crate::pattern::TypedId;
use crate::types::Type;
use crate::utils::error::SimpleError;
use crate::utils::metadata::Location;
use crate::utils::miniprint::MiniPrint;
pub type Error = SimpleError;

const OP_INTRINSIC_MARKER_NS: &str = "__mimium_op_intrinsic";

#[derive(Clone)]
struct ConvertResult {
    expr: ExprNodeId,
    found_any: bool,
}

fn convert_lambda_param_defaults<F>(
    params: Vec<TypedId>,
    conversion: &F,
) -> (Vec<TypedId>, bool, Vec<Error>)
where
    F: Fn(ExprNodeId) -> (ConvertResult, Vec<Error>),
{
    let mut found_any = false;
    let mut all_errs = vec![];
    let converted_params = params
        .into_iter()
        .map(|mut param| {
            if let Some(default_expr) = param.default_value {
                let (res, errs) = conversion(default_expr);
                found_any |= res.found_any;
                all_errs.extend(errs);
                param.default_value = Some(res.expr);
            }
            param
        })
        .collect();
    (converted_params, found_any, all_errs)
}

// This applies conversion() recursively. This is intended to be used in the `_`
// branch of pattern matching so that particular types of epressions can be
// caught and treated differently.
fn convert_recursively<F>(
    e_id: ExprNodeId,
    conversion: F,
    file_path: PathBuf,
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
            let (params, params_found_any, params_errs) =
                convert_lambda_param_defaults(params, &conversion);
            let (ConvertResult { expr, found_any }, errs) = conversion(body);
            let expr = Expr::Lambda(params, r_type, expr).into_id(loc);
            (
                ConvertResult {
                    expr,
                    found_any: found_any | params_found_any,
                },
                [params_errs, errs].concat(),
            )
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
        Expr::BinOp(lhs, (op, opspan), rhs) => {
            let (lhs, errs) = conversion(lhs);
            let (rhs, errs2) = conversion(rhs);
            let expr = Expr::BinOp(lhs.expr, (op, opspan), rhs.expr).into_id(loc);
            let found_any = lhs.found_any | rhs.found_any;
            (ConvertResult { expr, found_any }, [errs, errs2].concat())
        }
        Expr::UniOp((op, opspan), expr) => {
            let (res, errs) = conversion(expr);
            let expr = Expr::UniOp((op, opspan), res.expr).into_id(loc);
            (
                ConvertResult {
                    expr,
                    found_any: res.found_any,
                },
                errs,
            )
        }
        Expr::MacroExpand(fun, callee) => {
            let (fun, errs) = conversion(fun);
            let (res_vec, errs2): (Vec<_>, Vec<_>) = callee.into_iter().map(&conversion).unzip();
            let expr =
                Expr::MacroExpand(fun.expr, res_vec.iter().map(|e| e.expr).collect()).into_id(loc);
            let found_any = res_vec.into_iter().any(|e| e.found_any) | fun.found_any;
            (
                ConvertResult { expr, found_any },
                [errs, errs2.concat()].concat(),
            )
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
        Expr::FieldAccess(record, field) => {
            let (res, errs) = conversion(record);
            let expr = Expr::FieldAccess(res.expr, field).into_id(loc);
            (
                ConvertResult {
                    expr,
                    found_any: res.found_any,
                },
                errs,
            )
        }
        Expr::ArrayAccess(array, index) => {
            let (array_res, errs) = conversion(array);
            let (index_res, errs2) = conversion(index);
            let expr = Expr::ArrayAccess(array_res.expr, index_res.expr).into_id(loc);
            (
                ConvertResult {
                    expr,
                    found_any: array_res.found_any | index_res.found_any,
                },
                [errs, errs2].concat(),
            )
        }
        Expr::ArrayLiteral(items) => {
            let (res_vec, errs): (Vec<_>, Vec<_>) = items.into_iter().map(&conversion).unzip();
            let expr = Expr::ArrayLiteral(res_vec.iter().map(|e| e.expr).collect()).into_id(loc);
            let found_any = res_vec.into_iter().any(|e| e.found_any);
            (ConvertResult { expr, found_any }, errs.concat())
        }
        Expr::RecordLiteral(fields) => {
            let (res_vec, errs): (Vec<_>, Vec<_>) = fields
                .into_iter()
                .map(|f| {
                    let (res, errs) = conversion(f.expr);
                    (
                        (
                            RecordField {
                                name: f.name,
                                expr: res.expr,
                            },
                            res.found_any,
                        ),
                        errs,
                    )
                })
                .unzip();
            let expr = Expr::RecordLiteral(res_vec.clone().into_iter().map(|e| e.0).collect())
                .into_id(loc);
            let found_any = res_vec.into_iter().any(|f| f.1);
            (ConvertResult { expr, found_any }, errs.concat())
        }
        Expr::RecordUpdate(_record, _fields) => {
            // RecordUpdate should not exist at this stage - it should have been expanded earlier
            unreachable!("RecordUpdate should have been expanded in convert_operators")
        }
        Expr::Assign(left, right) => {
            let (left, err) = conversion(left);
            let (right, err2) = conversion(right);
            let found_any = left.found_any | right.found_any;
            let expr = Expr::Assign(left.expr, right.expr).into_id(loc);
            let errs = [err, err2].concat();
            (ConvertResult { expr, found_any }, errs)
        }
        Expr::Then(cond, body) => {
            let (cond, err) = conversion(cond);
            let (body, err2) = opt_conversion(body);
            let found_any = cond.found_any | body.clone().is_some_and(|b| b.found_any);
            let expr = Expr::Then(cond.expr, body.map(|b| b.expr)).into_id(loc);
            let errs = [err, err2].concat();
            (ConvertResult { expr, found_any }, errs)
        }
        Expr::Match(scrutinee, arms) => {
            let (scrutinee_res, mut errs) = conversion(scrutinee);
            let new_arms: Vec<_> = arms
                .into_iter()
                .map(|arm| {
                    let (body_res, body_errs) = conversion(arm.body);
                    errs.extend(body_errs);
                    (
                        crate::ast::MatchArm {
                            pattern: arm.pattern,
                            body: body_res.expr,
                        },
                        body_res.found_any,
                    )
                })
                .collect();
            let found_any = scrutinee_res.found_any | new_arms.iter().any(|(_, found)| *found);
            let expr = Expr::Match(
                scrutinee_res.expr,
                new_arms.into_iter().map(|(arm, _)| arm).collect(),
            )
            .into_id(loc);
            (ConvertResult { expr, found_any }, errs)
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
fn convert_recursively_pure<F>(e_id: ExprNodeId, conversion: F, file_path: PathBuf) -> ExprNodeId
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
    file_path: PathBuf,
) -> (ConvertResult, Vec<Error>) {
    let conversion = |e: ExprNodeId| -> (ConvertResult, Vec<Error>) {
        convert_self(e, feedctx, file_path.clone())
    };
    let loc = Location::new(e_id.to_span().clone(), file_path.clone());

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
            let (params, params_found_any, params_errs) =
                convert_lambda_param_defaults(params, &|e| {
                    convert_self(e, feedctx, file_path.clone())
                });
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
                    found_any: params_found_any,
                },
                [params_errs, err].concat(),
            )
        }
        Expr::Feed(_, _) => panic!(
            "Feed should not be shown before conversion at {}..{}",
            loc.span.start, loc.span.end
        ),
        _ => convert_recursively(e_id, conversion, file_path.clone()),
    }
}

fn convert_placeholder(e_id: ExprNodeId, file_path: PathBuf) -> ExprNodeId {
    let loc = Location::new(e_id.to_span().clone(), file_path.clone());
    match e_id.to_expr() {
        // if _ is used outside of pipe, treat it as a usual variable.
        Expr::Literal(Literal::PlaceHolder) => Expr::Var("_".to_symbol()).into_id(loc),
        Expr::Apply(fun, args)
            if args
                .iter()
                .any(|e| matches!(e.to_expr(), Expr::Literal(Literal::PlaceHolder))) =>
        {
            let fun = convert_placeholder(fun, file_path.clone());
            let (lambda_args_sparse, new_args): (Vec<_>, Vec<_>) = args
                .into_iter()
                .enumerate()
                .map(|(i, e)| {
                    if matches!(e.to_expr(), Expr::Literal(Literal::PlaceHolder)) {
                        let loc = Location::new(e_id.to_span().clone(), file_path.clone());
                        let escaped_loc = loc.clone();
                        let id = format!("__lambda_arg_{i}").to_symbol();
                        let ty = Type::Unknown.into_id_with_location(loc.clone());
                        let newid = TypedId::new(id, ty);
                        let e = Expr::Escape(Expr::Var(id).into_id(escaped_loc.clone()))
                            .into_id(escaped_loc);
                        (Some(newid), e)
                    } else {
                        (None, convert_placeholder(e, file_path.clone()))
                    }
                })
                .unzip();
            let lambda_args = lambda_args_sparse.into_iter().flatten().collect();
            let body_loc = loc.clone();
            let body = Expr::Bracket(Expr::Apply(fun, new_args).into_id(body_loc.clone()))
                .into_id(body_loc);
            Expr::Lambda(lambda_args, None, body).into_id(loc.clone())
        }
        _ => convert_recursively_pure(
            e_id,
            |e| convert_placeholder(e, file_path.clone()),
            file_path.clone(),
        ),
    }
}

fn convert_operators(e_id: ExprNodeId, file_path: PathBuf) -> ExprNodeId {
    let loc = Location::new(e_id.to_span().clone(), file_path.clone());
    match e_id.to_expr() {
        Expr::BinOp(arg, (Op::Pipe, _opspan), fun) => {
            let arg = convert_operators(arg, file_path.clone());
            let fun = convert_operators(fun, file_path.clone());
            Expr::Apply(fun, vec![arg]).into_id(loc)
        }
        Expr::BinOp(arg, (Op::PipeMacro, _opspan), fun) => {
            let arg = convert_operators(arg, file_path.clone());
            let fun = convert_operators(fun, file_path.clone());
            Expr::BinOp(arg, (Op::PipeMacro, loc.span.clone()), fun).into_id(loc)
        }
        Expr::BinOp(lhs, (op, opspan), rhs) => {
            log::trace!(
                "convert_operators: {} {} {}",
                lhs.to_expr().simple_print(),
                op,
                rhs.to_expr().simple_print()
            );
            let lhs = convert_operators(lhs, file_path.clone());
            let rhs = convert_operators(rhs, file_path.clone());
            let op_var = op.get_associated_fn_name();
            let oploc = Location {
                span: opspan.clone(),
                path: loc.path.clone(),
            };
            let fname = Expr::QualifiedVar(QualifiedPath::new(vec![
                OP_INTRINSIC_MARKER_NS.to_symbol(),
                op_var,
            ]))
            .into_id(oploc);
            match op {
                Op::At => Expr::Apply(fname, vec![rhs, lhs]).into_id(loc), // applies in reversed order,
                _ => Expr::Apply(fname, vec![lhs, rhs]).into_id(loc),
            }
        }
        Expr::UniOp((Op::Minus, opspan), expr) => {
            let e = convert_operators(expr, file_path);
            let op_var = Op::Minus.get_associated_fn_name();
            let oploc = Location {
                span: opspan.clone(),
                path: loc.path.clone(),
            };
            let fname = Expr::QualifiedVar(QualifiedPath::new(vec![
                OP_INTRINSIC_MARKER_NS.to_symbol(),
                op_var,
            ]))
            .into_id(oploc);
            let zero = Expr::Literal(Literal::Float("0.0".to_symbol())).into_id(loc.clone());
            Expr::Apply(fname, vec![zero, e]).into_id(loc)
        }
        Expr::UniOp((op, opspan), expr) => {
            let e = convert_operators(expr, file_path);
            let op_var = op.get_associated_fn_name();
            let oploc = Location {
                span: opspan.clone(),
                path: loc.path.clone(),
            };
            let fname = Expr::QualifiedVar(QualifiedPath::new(vec![
                OP_INTRINSIC_MARKER_NS.to_symbol(),
                op_var,
            ]))
            .into_id(oploc);
            Expr::Apply(fname, vec![e]).into_id(loc.clone())
        }
        Expr::Paren(e) => convert_operators(e, file_path),
        Expr::RecordUpdate(record, fields) => {
            // Expand record update syntax using the suggested approach:
            // { record <- field1 = val1, field2 = val2 }
            // becomes:
            // {
            //   let original = record
            //   original.field1 = val1
            //   original.field2 = val2
            //   original
            // }

            let record = convert_operators(record, file_path.clone());
            let fields = fields
                .into_iter()
                .map(|field| RecordField {
                    name: field.name,
                    expr: convert_operators(field.expr, file_path.clone()),
                })
                .collect::<Vec<_>>();

            // Generate a unique temporary variable name
            let temp_var_name = "record_update_temp".to_symbol();

            // Create the initial let binding: let original = record
            let temp_pattern = crate::pattern::TypedPattern {
                pat: crate::pattern::Pattern::Single(temp_var_name),
                ty: crate::types::Type::Unknown.into_id_with_location(loc.clone()),
                default_value: None,
            };

            // Build the chain of assignments and final return
            let mk_target = || Expr::Var(temp_var_name).into_id_without_span();
            let target_expr = mk_target();

            // Process assignments in reverse order to build the Then chain correctly
            let then_chain = fields.into_iter().rev().fold(target_expr, |e, field| {
                let access = Expr::FieldAccess(mk_target(), field.name).into_id_without_span();
                let assign = Expr::Assign(access, field.expr).into_id_without_span();
                Expr::Then(assign, Some(e)).into_id_without_span()
            });

            // Create the let expression: let original = record in (assignments; original)
            let let_expr = Expr::Let(temp_pattern, record, Some(then_chain)).into_id(loc.clone());
            log::trace!("expanded record update:{}", let_expr.simple_print());
            // Wrap in a block
            Expr::Block(Some(let_expr)).into_id(loc)
        }
        // because convert_pipe never fails, it propagate dummy error
        _ => convert_recursively_pure(
            e_id,
            |e| convert_operators(e, file_path.clone()),
            file_path.clone(),
        ),
    }
}

fn substitute_macro_arg(
    e_id: ExprNodeId,
    target: Symbol,
    replacement: ExprNodeId,
    file_path: PathBuf,
) -> ExprNodeId {
    match e_id.to_expr() {
        Expr::Escape(inner) if matches!(inner.to_expr(), Expr::Var(name) if name == target) => {
            replacement
        }
        _ => {
            let recursive_path = file_path.clone();
            convert_recursively_pure(
                e_id,
                |e| substitute_macro_arg(e, target, replacement, recursive_path.clone()),
                file_path,
            )
        }
    }
}

fn convert_macro_pipe(e_id: ExprNodeId, file_path: PathBuf) -> ExprNodeId {
    let loc = Location::new(e_id.to_span().clone(), file_path.clone());
    match e_id.to_expr() {
        Expr::BinOp(arg, (Op::PipeMacro, _opspan), fun) => {
            let arg = convert_macro_pipe(arg, file_path.clone());
            let fun = convert_macro_pipe(fun, file_path.clone());
            match fun.to_expr() {
                Expr::Lambda(params, _, body) if params.len() == 1 => match body.to_expr() {
                    Expr::Bracket(inner) => {
                        substitute_macro_arg(inner, params[0].id, arg, file_path)
                    }
                    _ => Expr::Apply(fun, vec![arg]).into_id(loc),
                },
                _ => Expr::Apply(fun, vec![arg]).into_id(loc),
            }
        }
        _ => {
            let recursive_path = file_path.clone();
            convert_recursively_pure(
                e_id,
                |e| convert_macro_pipe(e, recursive_path.clone()),
                file_path,
            )
        }
    }
}
fn convert_macroexpand(e_id: ExprNodeId, file_path: PathBuf) -> ExprNodeId {
    let loc = Location::new(e_id.to_span().clone(), file_path.clone());
    match e_id.to_expr() {
        Expr::MacroExpand(callee, args) => {
            let callee = convert_macroexpand(callee, file_path.clone());
            let args = args
                .into_iter()
                .map(|arg| convert_macroexpand(arg, file_path.clone()))
                .collect();

            Expr::Escape(Expr::Apply(callee, args).into_id(loc.clone())).into_id(loc)
        }
        _ => convert_recursively_pure(
            e_id,
            |e| convert_macroexpand(e, file_path.clone()),
            file_path.clone(),
        ),
    }
}

pub fn convert_pronoun(expr: ExprNodeId, file_path: PathBuf) -> (ExprNodeId, Vec<Error>) {
    // these 3 operations can be done in any order, and ideally, merged to one pattern matching.
    // However, for clarity, we split them into separated operations.
    let expr = convert_operators(expr, file_path.clone());
    let expr = convert_placeholder(expr, file_path.clone());
    let expr = convert_macro_pipe(expr, file_path.clone());
    let expr = convert_macroexpand(expr, file_path.clone());
    let (res, errs) = convert_self(expr, FeedId::Global, file_path.clone());
    (res.expr, errs)
    // (expr, vec![])
}

use super::convert_qualified_names;
use crate::ast::program::ModuleInfo;
use crate::utils::error::ReportableError;

/// Extended version of convert_pronoun that also resolves qualified names.
/// This should be used when module information is available.
///
/// The pipeline is:
/// 1. convert_operators: Expand |>, ||>, binary ops, RecordUpdate
/// 2. convert_placeholder: _ -> macro-level lambda args
/// 3. convert_macro_pipe: `||>` + macro-level partial application -> expanded AST
/// 4. convert_macroexpand: macro!() -> Escape(Apply(...))
/// 5. convert_self: self -> Feed variables
/// 6. convert_qualified_names: QualifiedVar -> Var (with mangled names), resolve use aliases and wildcards
///
/// The `builtin_names` parameter should contain names from builtin functions and external functions.
pub fn convert_pronoun_with_module(
    expr: ExprNodeId,
    file_path: PathBuf,
    module_info: &ModuleInfo,
    builtin_names: &[Symbol],
) -> (ExprNodeId, Vec<Box<dyn ReportableError>>) {
    // Run the existing pronoun conversions
    let expr = convert_operators(expr, file_path.clone());
    let expr = convert_placeholder(expr, file_path.clone());
    let expr = convert_macro_pipe(expr, file_path.clone());
    let expr = convert_macroexpand(expr, file_path.clone());
    let (res, self_errs) = convert_self(expr, FeedId::Global, file_path.clone());

    // Now resolve qualified names (uses 2-pass approach internally)
    let (expr, name_errs) = convert_qualified_names::convert_qualified_names(
        res.expr,
        module_info,
        builtin_names,
        file_path,
    );

    // Combine errors
    let mut errors: Vec<Box<dyn ReportableError>> = self_errs
        .into_iter()
        .map(|e| Box::new(e) as Box<dyn ReportableError>)
        .collect();
    errors.extend(
        name_errs
            .into_iter()
            .map(|e| Box::new(e) as Box<dyn ReportableError>),
    );

    (expr, errors)
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
            path: PathBuf::from("/"),
        };
        let unknownty = Type::Unknown.into_id_with_location(loc.clone());
        let src = Expr::Let(
            TypedPattern {
                pat: Pattern::Single("lowpass".to_symbol()),
                ty: unknownty,
                default_value: None,
            },
            Expr::Lambda(
                vec![TypedId::new("input".to_symbol(), unknownty)],
                None,
                Expr::Literal(Literal::SelfLit).into_id(loc.clone()),
            )
            .into_id(loc.clone()),
            None,
        )
        .into_id(loc.clone());
        let (res, errs) = convert_pronoun(src, PathBuf::from("/"));

        let ans = Expr::Let(
            TypedPattern {
                pat: Pattern::Single("lowpass".to_symbol()),
                ty: unknownty,
                default_value: None,
            },
            Expr::Lambda(
                vec![TypedId::new("input".to_symbol(), unknownty)],
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

    #[test]
    pub fn test_placeholder_converts_to_macro_lambda() {
        let loc = Location {
            span: 0..1,
            path: PathBuf::from("/"),
        };
        let unknownty = Type::Unknown.into_id_with_location(loc.clone());
        let src = Expr::Apply(
            Expr::Var("lowpass".to_symbol()).into_id(loc.clone()),
            vec![
                Expr::Literal(Literal::PlaceHolder).into_id(loc.clone()),
                Expr::Literal(Literal::Float("2000".to_symbol())).into_id(loc.clone()),
                Expr::Literal(Literal::Float("2".to_symbol())).into_id(loc.clone()),
            ],
        )
        .into_id(loc.clone());

        let (res, errs) = convert_pronoun(src, PathBuf::from("/"));
        assert!(errs.is_empty());

        let expected = Expr::Lambda(
            vec![TypedId::new("__lambda_arg_0".to_symbol(), unknownty)],
            None,
            Expr::Bracket(
                Expr::Apply(
                    Expr::Var("lowpass".to_symbol()).into_id(loc.clone()),
                    vec![
                        Expr::Escape(Expr::Var("__lambda_arg_0".to_symbol()).into_id(loc.clone()))
                            .into_id(loc.clone()),
                        Expr::Literal(Literal::Float("2000".to_symbol())).into_id(loc.clone()),
                        Expr::Literal(Literal::Float("2".to_symbol())).into_id(loc.clone()),
                    ],
                )
                .into_id(loc.clone()),
            )
            .into_id(loc.clone()),
        )
        .into_id(loc);
        assert_eq!(res, expected);
    }

    #[test]
    pub fn test_macro_pipe_converts_to_escape_apply_with_bracketed_argument() {
        let loc = Location {
            span: 0..1,
            path: PathBuf::from("/"),
        };
        let src = Expr::BinOp(
            Expr::Var("input".to_symbol()).into_id(loc.clone()),
            (Op::PipeMacro, 0..1),
            Expr::Apply(
                Expr::Var("effect".to_symbol()).into_id(loc.clone()),
                vec![Expr::Literal(Literal::PlaceHolder).into_id(loc.clone())],
            )
            .into_id(loc.clone()),
        )
        .into_id(loc.clone());

        let (res, errs) = convert_pronoun(src, PathBuf::from("/"));
        assert!(errs.is_empty());
        let expected = Expr::Apply(
            Expr::Var("effect".to_symbol()).into_id(loc.clone()),
            vec![Expr::Var("input".to_symbol()).into_id(loc.clone())],
        )
        .into_id(loc);
        assert_eq!(res, expected);
    }
}
