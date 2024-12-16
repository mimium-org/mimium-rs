use std::path::PathBuf;

use crate::ast::*;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::types::{PType, Type};
use crate::utils::error::ReportableError;
use crate::utils::metadata::*;
use chumsky::{prelude::*, Parser};
// use chumsky::Parser;
mod token;
use resolve_include::resolve_include;
use token::{Op, Token};
mod error;
mod lexer;
mod resolve_include;
mod statement;
use statement::{into_then_expr, stmt_from_expr_top, Statement};

use super::intrinsics;

#[cfg(test)]
mod test;

#[derive(Clone)]
struct ParseContext {
    file_path: Symbol,
}
pub(crate) type ParseError = Simple<Token>;

fn type_parser(ctx: ParseContext) -> impl Parser<Token, TypeNodeId, Error = ParseError> + Clone {
    let path = ctx.file_path;
    recursive(move |ty| {
        let primitive = select! {
           Token::FloatType => Type::Primitive(PType::Numeric),
           Token::IntegerType => Type::Primitive(PType::Int),
           Token::StringType => Type::Primitive(PType::String)
        }
        .map_with_span(move |t, span| t.into_id_with_location(Location::new(span, path)));

        let tuple = ty
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map_with_span(move |t: Vec<TypeNodeId>, span: Span| {
                Type::Tuple(t).into_id_with_location(Location::new(span, path))
            })
            .boxed()
            .labelled("Tuple");

        // let _struct_t = todo!();
        let atom = primitive.or(tuple);
        let func = atom
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .then(just(Token::Arrow).ignore_then(ty.clone()))
            .map_with_span(move |(a, e), span| {
                Type::Function(a, e, None).into_id_with_location(Location::new(span, path))
            })
            .boxed()
            .labelled("function");

        func.or(atom).labelled("Type")
    })
}
fn ident_parser() -> impl Parser<Token, Symbol, Error = ParseError> + Clone {
    select! { Token::Ident(s) => s }.labelled("ident")
}
fn literals_parser(
    ctx: ParseContext,
) -> impl Parser<Token, ExprNodeId, Error = ParseError> + Clone {
    select! {
        //Currently Integer literals are treated as float until the integer type is introduced in type system.
        // Token::Int(x) => Literal::Int(x),
        Token::Int(x)=>Literal::Float(x.to_string().to_symbol()),
        Token::Float(x) =>Literal::Float(x.to_symbol()),
        Token::Str(s) => Literal::String(s.to_symbol()),
        Token::SelfLit => Literal::SelfLit,
        Token::Now => Literal::Now,
        Token::SampleRate => Literal::SampleRate,
        Token::PlaceHolder => Literal::PlaceHolder,
    }
    .map_with_span(move |e, span| {
        Expr::Literal(e).into_id(Location {
            span,
            path: ctx.file_path,
        })
    })
    .labelled("literal")
}
fn var_parser(ctx: ParseContext) -> impl Parser<Token, ExprNodeId, Error = ParseError> + Clone {
    ident_parser().map_with_span(move |e, span| {
        Expr::Var(e).into_id(Location {
            span,
            path: ctx.file_path,
        })
    })
}
fn with_type_annotation<P, O>(
    parser: P,
    ctx: ParseContext,
) -> impl Parser<Token, (O, Option<TypeNodeId>), Error = ParseError> + Clone
where
    P: Parser<Token, O, Error = ParseError> + Clone,
{
    parser
        .then(just(Token::Colon).ignore_then(type_parser(ctx)).or_not())
        .map(|(id, t)| (id, t))
}

fn lvar_parser_typed(ctx: ParseContext) -> impl Parser<Token, TypedId, Error = ParseError> + Clone {
    with_type_annotation(ident_parser(), ctx.clone())
        .map_with_span(move |(sym, t), span| match t {
            Some(ty) => TypedId { id: sym, ty },
            None => TypedId {
                id: sym,
                ty: Type::Unknown.into_id_with_location(Location {
                    span,
                    path: ctx.file_path,
                }),
            },
        })
        .labelled("lvar_typed")
}
fn pattern_parser(
    ctx: ParseContext,
) -> impl Parser<Token, TypedPattern, Error = ParseError> + Clone {
    let pat = recursive(|pat| {
        pat.clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map(Pattern::Tuple)
            .or(select! {
                Token::Ident(s) => Pattern::Single(s),
                // Note: _ represents an unused variable, but it is treated as
                // an ordinary symbol here.
                Token::PlaceHolder => Pattern::Single("_".to_symbol()),
            })
            .labelled("Pattern")
    });
    with_type_annotation(pat, ctx.clone()).map_with_span(move |(pat, ty), span| match ty {
        Some(ty) => TypedPattern { pat, ty },
        None => TypedPattern {
            pat,
            ty: Type::Unknown.into_id_with_location(Location {
                span,
                path: ctx.file_path,
            }),
        },
    })
}
fn binop_folder<'a, I, OP>(
    prec: I,
    op: OP,
    ctx: ParseContext,
) -> BoxedParser<'a, Token, ExprNodeId, ParseError>
where
    I: Parser<Token, ExprNodeId, Error = ParseError> + Clone + 'a,
    OP: Parser<Token, (Op, Span), Error = ParseError> + Clone + 'a,
{
    prec.clone()
        .then(
            op.then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
                .then(prec)
                .repeated(),
        )
        .foldl(move |x, ((op, opspan), y)| {
            let span = x.to_span().start..y.to_span().end;
            let loc = Location {
                span,
                path: ctx.file_path,
            };
            let arg = match op {
                Op::Pipe => return Expr::PipeApply(x, y).into_id(loc.clone()),
                // A@B is a syntactic sugar of _mimium_schedule_at(B, A)
                Op::At => vec![y, x],
                _ => vec![x, y],
            };
            Expr::Apply(
                Expr::Var(op.get_associated_fn_name()).into_id(Location {
                    span: opspan,
                    path: ctx.file_path,
                }),
                arg,
            )
            .into_id(loc)
        })
        .boxed()
}

type ExprParser<'a> = Recursive<'a, Token, ExprNodeId, ParseError>;

fn items_parser(
    expr: ExprParser<'_>,
) -> impl Parser<Token, Vec<ExprNodeId>, Error = ParseError> + Clone + '_ {
    expr.separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
}

fn op_parser<'a, I>(
    apply: I,
    ctx: ParseContext,
) -> impl Parser<Token, ExprNodeId, Error = ParseError> + Clone + 'a
where
    I: Parser<Token, ExprNodeId, Error = ParseError> + Clone + 'a,
{
    let ctx = ctx.clone();
    let unary = select! { Token::Op(Op::Minus) => {} }
        .map_with_span(|e, s| (e, s))
        .repeated()
        .then(apply.clone())
        .foldr(move |(_op, op_span), rhs| {
            let rhs_span = rhs.to_span();
            let loc = Location {
                span: op_span.start..rhs_span.start,
                path: ctx.file_path,
            };
            let neg_op = Expr::Var("neg".to_symbol()).into_id(loc);
            let loc = Location {
                span: op_span.start..rhs_span.end,
                path: ctx.file_path,
            };
            Expr::Apply(neg_op, vec![rhs]).into_id(loc)
        })
        .labelled("unary");

    let optoken = move |o: Op| {
        just(Token::Op(o))
            .try_map(|e, s| match e {
                Token::Op(o) => Ok((o, s)),
                _ => Err(Simple::custom(s, "Invalid operator used")),
            })
            .boxed()
    };
    // allow pipe opertor to absorb linebreaks so that it can be also used at
    // the head of the line.
    let pipe = just(Token::LineBreak)
        .repeated()
        .then(just(Token::Op(Op::Pipe)))
        .map_with_span(|_, s| (Op::Pipe, s))
        .boxed();
    //defining binary operators in order of precedence.
    let ops = [
        optoken(Op::Exponent),
        choice((
            optoken(Op::Product),
            optoken(Op::Divide),
            optoken(Op::Modulo),
        ))
        .boxed(),
        optoken(Op::Sum).or(optoken(Op::Minus)).boxed(),
        optoken(Op::Equal).or(optoken(Op::NotEqual)).boxed(),
        optoken(Op::And),
        optoken(Op::Or),
        choice((
            optoken(Op::LessThan),
            optoken(Op::LessEqual),
            optoken(Op::GreaterThan),
            optoken(Op::GreaterEqual),
        ))
        .boxed(),
        pipe,
        optoken(Op::At),
    ];
    ops.into_iter().fold(unary.boxed(), move |acc, x| {
        binop_folder(acc, x, ctx.clone())
    })
}
fn atom_parser<'a>(
    expr: ExprParser<'a>,
    expr_group: ExprParser<'a>,
    ctx: ParseContext,
) -> impl Parser<Token, ExprNodeId, Error = ParseError> + Clone + 'a {
    let lambda = lvar_parser_typed(ctx.clone())
        .separated_by(just(Token::Comma))
        .delimited_by(
            just(Token::LambdaArgBeginEnd),
            just(Token::LambdaArgBeginEnd),
        )
        .then(
            just(Token::Arrow)
                .ignore_then(type_parser(ctx.clone()))
                .or_not(),
        )
        .then(expr_group.clone())
        .map_with_span(move |((ids, r_type), body), span| {
            Expr::Lambda(ids, r_type, body).into_id(Location {
                span,
                path: ctx.file_path,
            })
        })
        .labelled("lambda");
    let macro_expand = select! { Token::MacroExpand(s) => Expr::Var(s) }
        .map_with_span(move |e, span| {
            e.into_id(Location {
                span,
                path: ctx.file_path,
            })
        })
        .then_ignore(just(Token::ParenBegin))
        .then(expr_group.clone())
        .then_ignore(just(Token::ParenEnd))
        .map_with_span(move |(id, then), span| {
            let loc = Location {
                span,
                path: ctx.file_path,
            };
            Expr::Escape(Expr::Apply(id, vec![then]).into_id(loc.clone())).into_id(loc)
        })
        .labelled("macroexpand");

    let tuple = items_parser(expr.clone())
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .map_with_span(move |e, span| {
            Expr::Tuple(e).into_id(Location {
                span,
                path: ctx.file_path,
            })
        })
        .labelled("tuple");
    let parenexpr = expr
        .clone()
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .labelled("paren_expr");
    //tuple must  lower precedence than parenexpr, not to parse single element tuple without trailing comma
    choice((
        literals_parser(ctx.clone()),
        var_parser(ctx.clone()),
        lambda,
        macro_expand,
        parenexpr,
        tuple,
    ))
}
fn expr_parser(expr_group: ExprParser<'_>, ctx: ParseContext) -> ExprParser<'_> {
    recursive(|expr: Recursive<Token, ExprNodeId, ParseError>| {
        enum FoldItem {
            Args(Vec<ExprNodeId>),
            ArrayIndex(ExprNodeId),
        }
        let parenitems = items_parser(expr.clone())
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map_with_span(|args, args_span| (FoldItem::Args(args), args_span));
        let angle_paren_expr = expr
            .clone()
            .delimited_by(just(Token::ArrayBegin), just(Token::ArrayEnd))
            .map_with_span(|e, s| (FoldItem::ArrayIndex(e), s));

        let folder = move |f: ExprNodeId, (item, args_span): (FoldItem, Span)| {
            let f_span = f.to_span();
            let span = f_span.start..args_span.end;
            let loc = Location {
                span,
                path: ctx.file_path,
            };
            match item {
                FoldItem::Args(args) => Expr::Apply(f, args).into_id(loc),
                FoldItem::ArrayIndex(index) => Expr::ArrayAccess(f, index).into_id(loc),
            }
        };

        let apply = atom_parser(expr.clone(), expr_group, ctx.clone())
            .then(angle_paren_expr.or(parenitems).repeated())
            .foldl(folder)
            .labelled("apply");

        op_parser(apply, ctx)
    })
}
// fn expr_statement_parser<'a>(
//     expr_group: ExprParser<'a>,
//     then: ExprParser<'a>,
// ) -> impl Parser<Token, ExprNodeId, Error = ParseError> + Clone + 'a {
//     let let_stmt = just(Token::Let)
//         .ignore_then(pattern_parser().clone())
//         .then_ignore(just(Token::Assign))
//         .then(expr_group.clone())
//         .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
//         .then(then.clone().or_not())
//         .map_with_span(|((ident, body), then), span| Expr::Let(ident, body, then).into_id(span))
//         .labelled("let_stmt");
//     let assign = placement_parser()
//         .then_ignore(just(Token::Assign))
//         .then(expr_group.clone())
//         .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
//         .then(then.or_not())
//         .map_with_span(|((ident, body), then), span| {
//             Expr::Then(Expr::Assign(ident, body).into_id(span.clone()), then).into_id(span)
//         })
//         .labelled("assign");
//     let_stmt.or(assign)
// }
fn validate_reserved_pat(id: &TypedPattern, span: Span) -> Result<(), ParseError> {
    match &id.pat {
        Pattern::Single(symbol) => validate_reserved_ident(*symbol, span),
        _ => Ok(()),
    }
}

fn validate_reserved_ident(id: Symbol, span: Span) -> Result<(), ParseError> {
    if intrinsics::BUILTIN_SYMS.with(|syms| syms.binary_search(&id).is_ok()) {
        Err(Simple::custom(
            span,
            "Builtin functions cannot be re-defined.",
        ))
    } else {
        Ok(())
    }
}

fn statement_parser(
    expr: ExprParser<'_>,
    ctx: ParseContext,
) -> impl Parser<Token, (Statement, Location), Error = ParseError> + Clone + '_ {
    let let_ = just(Token::Let)
        .ignore_then(pattern_parser(ctx.clone()).validate(|pat, span, emit| {
            if let Err(e) = validate_reserved_pat(&pat, span.clone()) {
                emit(e);
            }
            pat
        }))
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map_with_span(|(ident, body), span| (Statement::Let(ident, body), span))
        .labelled("let");
    let letrec = just(Token::LetRec)
        .ignore_then(
            lvar_parser_typed(ctx.clone()).validate(|ident, span, emit| {
                if let Err(e) = validate_reserved_ident(ident.id, span.clone()) {
                    emit(e);
                }
                ident
            }),
        )
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map_with_span(|(ident, body), span| (Statement::LetRec(ident, body), span))
        .labelled("letrec");
    let assign = var_parser(ctx.clone())
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map_with_span(|(lvar, body), span| (Statement::Assign(lvar, body), span))
        .labelled("assign");
    let single = expr.map_with_span(|e, span| (Statement::Single(e), span));
    let_.or(letrec).or(assign).or(single).map(move |(t, span)| {
        (
            t,
            Location {
                span,
                path: ctx.file_path,
            },
        )
    })
}
fn statements_parser(
    expr: ExprParser<'_>,
    ctx: ParseContext,
) -> impl Parser<Token, Option<ExprNodeId>, Error = ParseError> + Clone + '_ {
    statement_parser(expr, ctx)
        .separated_by(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
        .allow_leading()
        .allow_trailing()
        .recover_with(skip_until([Token::LineBreak, Token::SemiColon], |_| vec![]))
        .map(|stmts| into_then_expr(&stmts))
}

fn block_parser(
    expr: ExprParser<'_>,
    ctx: ParseContext,
) -> impl Parser<Token, ExprNodeId, Error = ParseError> + Clone + '_ {
    let stmts = statements_parser(expr, ctx.clone());
    stmts
        .delimited_by(just(Token::BlockBegin), just(Token::BlockEnd))
        .map_with_span(move |stmts, span| {
            Expr::Block(stmts).into_id(Location {
                span,
                path: ctx.file_path,
            })
        })
        .recover_with(nested_delimiters(
            Token::BlockBegin,
            Token::BlockEnd,
            [],
            |_| Expr::Error.into_id_without_span(),
        ))
}
// expr_group contains let statement, assignment statement, function definiton,... they cannot be placed as an argument for apply directly.
fn exprgroup_parser<'a>(ctx: ParseContext) -> ExprParser<'a> {
    recursive(move |expr_group: ExprParser<'a>| {
        let expr = expr_parser(expr_group.clone(), ctx.clone());

        let block = block_parser(expr_group.clone(), ctx.clone());
        //todo: should be recursive(to paranthes be not needed)
        let if_ = just(Token::If)
            .ignore_then(
                expr_group
                    .clone()
                    .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
            )
            .then(expr_group.clone())
            .then(just(Token::Else).ignore_then(expr_group.clone()).or_not())
            .map_with_span(move |((cond, then), opt_else), span| {
                Expr::If(cond, then, opt_else).into_id(Location {
                    span,
                    path: ctx.file_path,
                })
            })
            .labelled("if");

        block
            .or(if_)
            // .or(expr_statement_parser(expr_group.clone(), expr_group))
            .or(expr.clone())
    })
}

fn gen_unknown_function_type(
    ids: &[TypedId],
    r_type: Option<TypeNodeId>,
    loc: Location,
) -> TypeNodeId {
    let atypes = ids
        .iter()
        .map(|tid| {
            if !tid.is_unknown() {
                tid.ty
            } else {
                Type::Unknown.into_id_with_location(loc.clone())
            }
        })
        .collect::<Vec<_>>();
    Type::Function(
        atypes,
        r_type.unwrap_or_else(|| Type::Unknown.into_id_with_location(loc.clone())),
        None,
    )
    .into_id_with_location(loc)
}
fn func_parser(ctx: ParseContext) -> impl Parser<Token, ExprNodeId, Error = ParseError> + Clone {
    let exprgroup = exprgroup_parser(ctx.clone());
    let lvar = lvar_parser_typed(ctx.clone());
    let blockstart = just(Token::BlockBegin)
        .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated());
    let blockend = just(Token::LineBreak)
        .or(just(Token::SemiColon))
        .repeated()
        .ignore_then(just(Token::BlockEnd));
    let fnparams = lvar
        .clone()
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .labelled("fnparams");

    let function_s = just(Token::Function)
        .ignore_then(lvar.clone().validate(|ident, span, emit| {
            if let Err(e) = validate_reserved_ident(ident.id, span) {
                emit(e);
            }
            ident
        }))
        .then(fnparams.clone())
        .then(
            just(Token::Arrow)
                .ignore_then(type_parser(ctx.clone()))
                .or_not(),
        )
        .then(
            block_parser(exprgroup.clone(), ctx.clone()).map(|e| match e.to_expr() {
                Expr::Block(e) => e.unwrap(),
                _ => e,
            }),
        )
        .map_with_span(move |(((fname, ids), r_type), block), span| {
            let loc = Location {
                span: span.clone(),
                path: ctx.file_path,
            };
            let fname = TypedId {
                id: fname.id,
                ty: gen_unknown_function_type(&ids, r_type, loc.clone()),
            };
            (
                Statement::LetRec(fname, Expr::Lambda(ids, r_type, block).into_id(loc.clone())),
                loc,
            )
        })
        .labelled("function decl");

    let macro_s = just(Token::Macro)
        .ignore_then(lvar.clone())
        .then(fnparams.clone())
        .then(
            exprgroup
                .clone()
                .delimited_by(blockstart.clone(), blockend.clone())
                .map(Expr::Bracket),
        )
        .map_with_span(move |((fname, ids), block), span| {
            let loc = Location {
                span,
                path: ctx.file_path,
            };
            (
                Statement::MacroExpand(
                    fname,
                    Expr::Lambda(ids, None, block.into_id(loc.clone())).into_id(loc.clone()),
                ),
                loc,
            )
        })
        .labelled("macro definition");
    let global_stmt = statement_parser(exprgroup.clone(), ctx.clone());
    let stmt = function_s.or(macro_s).or(global_stmt);
    let separator = just(Token::LineBreak).or(just(Token::SemiColon)).repeated();
    let stmts = stmt
        .map(|s: (Statement, Location)| vec![s])
        .or(
            preprocess_parser(ctx.clone()).map_with_span(move |e, span| {
                stmt_from_expr_top(e)
                    .into_iter()
                    .map(|st| (st, Location::new(span.clone(), ctx.file_path)))
                    .collect()
            }),
        )
        .separated_by(separator)
        .allow_leading()
        .allow_trailing()
        .recover_with(skip_until([Token::LineBreak, Token::SemiColon], |_| vec![]))
        .flatten()
        .map(|stmt| into_then_expr(&stmt).unwrap_or(Expr::Error.into_id_without_span()));
    stmts
}

fn preprocess_parser(
    ctx: ParseContext,
) -> impl Parser<Token, ExprNodeId, Error = ParseError> + Clone {
    just(Token::Include)
        .ignore_then(
            select! {Token::Str(s) => s}
                .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
        )
        .try_map(move |filename, span: Span| {
            let cfile = ctx.file_path.as_str();
            let (c, errs) = resolve_include(cfile, &filename, span.clone());
            if errs.is_empty() {
                Ok(c)
            } else {
                let e = errs.into_iter().fold(
                    Simple::<Token>::custom(
                        span.clone(),
                        format!("failed to resolve include for {filename}"),
                    ),
                    |simple_e, reportable_e| {
                        let wrapped =
                            Simple::<Token>::custom(span.clone(), reportable_e.to_string());
                        wrapped.merge(simple_e)
                    },
                );
                Err(e)
            }
        })
}
fn parser(
    current_file: Option<PathBuf>,
) -> impl Parser<Token, ExprNodeId, Error = ParseError> + Clone {
    let ignored = just(Token::LineBreak)
        .ignored()
        .or(just(Token::SemiColon).ignored());
    let ctx = ParseContext {
        file_path: current_file.map_or("".to_symbol(), |p| p.to_string_lossy().to_symbol()),
    };
    func_parser(ctx)
        .padded_by(ignored.repeated())
        .then_ignore(end())
}

pub(crate) fn add_global_context(ast: ExprNodeId, file_path: Symbol) -> ExprNodeId {
    let span = ast.to_span();
    let loc = Location {
        span: span.clone(),
        path: file_path,
    };
    let res = Expr::Let(
        TypedPattern {
            pat: Pattern::Single(GLOBAL_LABEL.to_symbol()),
            ty: Type::Unknown.into_id_with_location(loc.clone()),
        },
        Expr::Lambda(vec![], None, ast).into_id(loc.clone()),
        None,
    );
    res.into_id(loc)
}
pub fn parse(
    src: &str,
    current_file: Option<PathBuf>,
) -> (ExprNodeId, Vec<Box<dyn ReportableError>>) {
    let len = src.chars().count();
    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src);
    let lex_errs = lex_errs.into_iter().map(|e| -> Box<dyn ReportableError> {
        Box::new(error::ParseError::<char> {
            content: e,
            file: current_file
                .clone()
                .unwrap_or_default()
                .to_string_lossy()
                .to_symbol(),
        })
    });

    if let Some(t) = tokens {
        let (ast, parse_errs) = parser(current_file.clone())
            .parse_recovery(chumsky::Stream::from_iter(len..len + 1, t.into_iter()));
        let errs = parse_errs
            .into_iter()
            .map(|e| -> Box<dyn ReportableError> {
                Box::new(error::ParseError {
                    content: e,
                    file: current_file
                        .clone()
                        .unwrap_or_default()
                        .to_string_lossy()
                        .to_symbol(),
                })
            })
            .chain(lex_errs)
            .collect::<Vec<_>>();
        (ast.unwrap_or(Expr::Error.into_id_without_span()), errs)
    } else {
        (Expr::Error.into_id_without_span(), lex_errs.collect())
    }
}
