use crate::ast::operators::Op;
use crate::ast::*;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::types::{PType, RecordTypeField, Type};
use crate::utils::error::ReportableError;
use crate::utils::{atomic, metadata::*};
use std::cell::RefCell;
use std::path::PathBuf;
use std::sync::Arc;

use chumsky::input::{Stream, ValueInput};
use chumsky::{Parser, prelude::*};
mod token;
use token::Token;
mod error;
mod lexer;
use crate::ast::program::{Program, ProgramStatement, expr_from_program};
use crate::ast::statement;
use statement::{Statement, into_then_expr};

use super::intrinsics;

#[cfg(test)]
mod test;

#[derive(Clone)]
pub(super) struct ParseContext {
    file_path: Symbol,
}
impl ParseContext {
    pub fn gen_loc(&self, span: SimpleSpan) -> Location {
        Location {
            span: span.start()..span.end(),
            path: self.file_path,
        }
    }
}
pub(crate) type ParseError<'src> = chumsky::extra::Err<Rich<'src, Token, SimpleSpan>>;
fn merge_span(a: Span, b: Span) -> Span {
    a.start..b.end
}
fn get_span<T: chumsky::span::Span<Offset = usize>>(e: T) -> Span {
    e.start()..e.end()
}

fn breakable_comma<'src, I>() -> impl Parser<'src, I, (), ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    just(Token::Comma)
        .ignore_then(just(Token::LineBreak).or_not())
        .ignored()
}
fn breakable_blockbegin<'src, I>() -> impl Parser<'src, I, (), ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    just(Token::BlockBegin)
        .then(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
        .ignored()
}
fn breakable_blockend<'src, I>() -> impl Parser<'src, I, (), ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    just(Token::LineBreak)
        .or(just(Token::SemiColon))
        .repeated()
        .then(just(Token::BlockEnd))
        .ignored()
}

fn type_primitive<'src, I>(
    ctx: ParseContext,
) -> impl Parser<'src, I, TypeNodeId, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    select! {
        Token::FloatType => PType::Numeric,
        Token::StringType => PType::String,
        Token::IntegerType => PType::Int,
    }
    .map_with(move |t, e| {
        Type::Primitive(t).into_id_with_location(Location::new(get_span(e.span()), ctx.file_path))
    })
    .labelled("primitive type")
}

fn type_parser<'src, I>(
    ctx: ParseContext,
) -> impl Parser<'src, I, TypeNodeId, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let path = ctx.file_path;
    recursive(move |ty| {
        let tuple = ty
            .clone()
            .separated_by(just(Token::Comma))
            .at_least(1)
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map_with(move |item: Vec<TypeNodeId>, e| {
                Type::Tuple(item).into_id_with_location(Location {
                    span: get_span(e.span()),
                    path,
                })
            })
            .recover_with(via_parser(nested_delimiters(
                Token::ParenBegin,
                Token::ParenEnd,
                [],
                move |_span| Type::Failure.into_id(),
            )))
            .labelled("Tuple Type");

        let record = ident_parser()
            .then_ignore(just(Token::Colon))
            .then(ty.clone())
            .map(|(key, ty)| RecordTypeField::new(key, ty, false))
            .separated_by(breakable_comma())
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::BlockBegin), just(Token::BlockEnd))
            .map_with(move |fields, e| {
                Type::Record(fields).into_id_with_location(Location {
                    span: get_span(e.span()),
                    path,
                })
            })
            .recover_with(via_parser(nested_delimiters(
                Token::BlockBegin,
                Token::BlockEnd,
                [],
                move |_span| Type::Failure.into_id(),
            )))
            .labelled("Record Type");
        // Parse array type [T]
        let array = ty
            .clone()
            .delimited_by(just(Token::ArrayBegin), just(Token::ArrayEnd))
            .map_with(move |element_type, e| {
                Type::Array(element_type).into_id_with_location(Location {
                    span: get_span(e.span()),
                    path,
                })
            })
            .boxed()
            .labelled("Array");

        // let _struct_t = todo!();
        let atom = choice((type_primitive(ctx.clone()), record, tuple, array));
        let func = atom
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .map_with(move |arg, e| (arg, e.span()))
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .then(just(Token::Arrow).ignore_then(ty.clone()))
            .map_with(move |((body, bspan), ret), e| {
                Type::Function {
                    arg: Type::Tuple(body).into_id_with_location(ctx.gen_loc(bspan)),
                    ret,
                }
                .into_id_with_location(ctx.gen_loc(e.span()))
            })
            .boxed()
            .labelled("function");

        func.or(atom).labelled("Type")
    })
}
pub(super) fn ident_parser<'src, I>() -> impl Parser<'src, I, Symbol, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    select! { Token::Ident(s) => s }.labelled("ident")
}
fn literals_parser<'src, I>(
    ctx: ParseContext,
) -> impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    select! {
        //Currently Integer literals are treated as float until the integer type is introduced in type system.
        // Token::Int(x) => Literal::Int(x),
        Token::Int(x)=>Literal::Float(Arc::new(atomic::F64::new(x as f64))),
        Token::Float(x) =>Literal::Float(Arc::new(atomic::F64::new(x.parse::<f64>().unwrap()))),
        Token::Str(s) => Literal::String(s.to_symbol()),
        Token::SelfLit => Literal::SelfLit,
        Token::Now => Literal::Now,
        Token::SampleRate => Literal::SampleRate,
        Token::PlaceHolder => Literal::PlaceHolder,
    }
    .map_with(move |lit, e| {
        Expr::Literal(lit).into_id(Location {
            span: get_span(e.span()),
            path: ctx.file_path,
        })
    })
    .labelled("literal")
}
fn var_parser<'src, I>(
    ctx: ParseContext,
) -> impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    ident_parser().map_with(move |s, e| {
        Expr::Var(s).into_id(Location {
            span: get_span(e.span()),
            path: ctx.file_path,
        })
    })
}
fn with_type_annotation<'src, I, T>(
    parser: impl Parser<'src, I, T, ParseError<'src>> + Clone,
    ctx: ParseContext,
) -> impl Parser<'src, I, (T, Option<TypeNodeId>), ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    parser.then(just(Token::Colon).ignore_then(type_parser(ctx)).or_not())
}

fn lvar_parser_typed<'src, I>(
    ctx: ParseContext,
) -> impl Parser<'src, I, TypedId, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    with_type_annotation(ident_parser(), ctx.clone())
        .map_with(move |(sym, t), e| match t {
            Some(ty) => TypedId {
                id: sym,
                ty,
                default_value: None,
            },
            None => TypedId {
                id: sym,
                ty: Type::Unknown.into_id_with_location(Location {
                    span: get_span(e.span()),
                    path: ctx.file_path,
                }),
                default_value: None,
            },
        })
        .labelled("lvar_typed")
}

// Parameter parser with support for default values
fn lvar_parser_typed_with_default<'src, I>(
    ctx: ParseContext,
    expr: impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone,
) -> impl Parser<'src, I, TypedPattern, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    lvar_parser_typed(ctx.clone())
        .then(
            // Parse optional default value: = expr
            just(Token::Assign).ignore_then(expr).or_not(),
        )
        .map(|(param, default_value)| TypedId {
            id: param.id,
            ty: param.ty,
            default_value,
        })
        .map(TypedPattern::from)
        .labelled("lvar_typed_with_default")
}
fn pattern_parser<'src, I>(
    ctx: ParseContext,
) -> impl Parser<'src, I, TypedPattern, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let single_pat = select! {
        Token::Ident(s) => Pattern::Single(s),
        // Note: _ represents an unused variable, but it is treated as
        // an ordinary symbol here.
        Token::PlaceHolder => Pattern::Single("_".to_symbol())

    }
    .labelled("single pattern");
    let pat = recursive(|pat| {
        let tup = pat
            .clone()
            .separated_by(just(Token::Comma))
            .at_least(1)
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map(Pattern::Tuple)
            .labelled("tuple pattern");
        let record = (ident_parser()
            .then_ignore(just(Token::Assign))
            .then(pat.clone()))
        .separated_by(breakable_comma())
        .at_least(1)
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(breakable_blockbegin(), breakable_blockend())
        .map(Pattern::Record)
        .recover_with(via_parser(nested_delimiters(
            Token::BlockBegin,
            Token::BlockEnd,
            [],
            |_| Pattern::Error,
        )))
        .labelled("record pattern");
        choice((single_pat, tup, record)).labelled("pattern")
    });
    with_type_annotation(pat, ctx.clone())
        .map_with(move |(pat, ty), e| match ty {
            Some(ty) => TypedPattern::new(pat, ty),
            None => TypedPattern::new(
                pat,
                Type::Unknown.into_id_with_location(Location {
                    span: get_span(e.span()),
                    path: ctx.file_path,
                }),
            ),
        })
        .boxed()
}

fn items_parser<'src, I, E>(
    expr: E,
    allow_empty: bool,
) -> impl Parser<'src, I, Vec<ExprNodeId>, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
    E: Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone,
{
    let least_repeat = if allow_empty { 0 } else { 1 };
    expr.separated_by(breakable_comma())
        .allow_trailing()
        .at_least(least_repeat)
        .collect::<Vec<_>>()
}
enum DotField {
    Index(i64),
    Ident(Symbol),
}
fn dot_field<'src, I>() -> impl Parser<'src, I, (DotField, Span), ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    select! {
        Token::Int(i) => DotField::Index(i),
        Token::Ident(s) => DotField::Ident(s),
    }
    .map_with(|field, e| (field, get_span(e.span())))
    .labelled("dot_field")
}
fn op_parser<'src, I, P>(
    apply: P,
    ctx: ParseContext,
) -> impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone
where
    P: Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone + 'src,
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let ctx = ctx.clone();
    let dot = apply // this boxing is necessary for windows CI environment
        .foldl(
            just(Token::Dot).ignore_then(dot_field()).repeated(),
            move |lhs, (rhs, rspan)| {
                let span = lhs.to_span().start..rspan.end;

                let loc = Location {
                    span,
                    path: ctx.file_path,
                };
                match rhs {
                    DotField::Ident(name) => Expr::FieldAccess(lhs, name).into_id(loc),
                    DotField::Index(idx) => Expr::Proj(lhs, idx).into_id(loc),
                }
            },
        )
        .labelled("dot");

    let unary = one_of([Token::Op(Op::Minus), Token::BackQuote, Token::Dollar])
        .map_with(|token, e| (token, get_span(e.span())))
        .repeated()
        .foldr(dot, move |(op, op_span), rhs| {
            let rhs_span = rhs.to_span();
            let loc = Location {
                span: op_span.start..rhs_span.end,
                path: ctx.file_path,
            };
            match op {
                Token::BackQuote => Expr::Bracket(rhs).into_id(loc.clone()),
                Token::Dollar => Expr::Escape(rhs).into_id(loc.clone()),
                Token::Op(Op::Minus) => Expr::UniOp((Op::Minus, op_span), rhs).into_id(loc),
                _ => unreachable!("Unexpected unary operator: {:?}", op),
            }
        })
        .labelled("unary");
    let optoken = move |target: Op| {
        select! {
            Token::Op(o) if o == target => o,
        }
        .boxed()
    };
    // allow pipe opertor to absorb linebreaks so that it can be also used at
    // the head of the line.
    let pipe = just(Token::LineBreak)
        .repeated()
        .collect::<Vec<_>>()
        .ignore_then(just(Token::Op(Op::Pipe)))
        .to(Op::Pipe)
        .boxed();
    //defining binary operators in order of precedence.
    // The order of precedence is from the lowest to the highest.
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
    ops.into_iter().fold(unary.boxed(), move |prec, op| {
        prec.clone()
            .foldl(
                op.then_ignore(just(Token::LineBreak).repeated())
                    .map_with(move |op, e| (op, get_span(e.span())))
                    .then(prec)
                    .repeated(),
                move |x, ((op, opspan), y)| {
                    let span = x.to_span().start..y.to_span().end;
                    let loc = Location {
                        span,
                        path: ctx.file_path,
                    };
                    Expr::BinOp(x, (op, opspan), y).into_id(loc)
                },
            )
            .boxed()
    })
}
fn record_fields<'src, I, P>(expr: P) -> impl Parser<'src, I, RecordField, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
    P: Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone,
{
    ident_parser()
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(move |(name, expr)| RecordField { name, expr })
}

pub(super) fn atom_parser<'src, I>(
    expr: impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone + 'src,
    expr_group: impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone + 'src,
    ctx: ParseContext,
) -> impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let lambda = lvar_parser_typed(ctx.clone())
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
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
        .map_with(move |((ids, r_type), body), e| {
            Expr::Lambda(ids, r_type, body).into_id(Location {
                span: get_span(e.span()),
                path: ctx.file_path,
            })
        })
        .labelled("lambda");
    let macro_expand = select! { Token::MacroExpand(s) => Expr::Var(s) }
        .map_with(move |v, e| {
            v.into_id(Location {
                span: get_span(e.span()),
                path: ctx.file_path,
            })
        })
        .then_ignore(just(Token::ParenBegin))
        .then(
            expr_group
                .clone()
                .separated_by(breakable_comma())
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::ParenEnd))
        .map_with(move |(id, then), e| {
            let loc = Location {
                span: get_span(e.span()),
                path: ctx.file_path,
            };
            Expr::MacroExpand(id, then).into_id(loc.clone())
        })
        .labelled("macroexpand");

    let tuple = items_parser(expr.clone(), false)
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .map_with(move |items, e| {
            Expr::Tuple(items).into_id(Location {
                span: get_span(e.span()),
                path: ctx.file_path,
            })
        })
        .labelled("tuple");

    let array_literal = items_parser(expr.clone(), true)
        .delimited_by(just(Token::ArrayBegin), just(Token::ArrayEnd))
        .map_with(move |items, e| {
            // Create a nested expression that constructs an array with the given items
            // For now, we create a special AST node type for array literals
            let loc = Location {
                span: get_span(e.span()),
                path: ctx.file_path,
            };
            Expr::ArrayLiteral(items).into_id(loc)
        })
        .labelled("array_literal");
    let record_literal = record_fields(expr.clone())
        .separated_by(breakable_comma())
        .allow_trailing()
        .collect::<Vec<_>>()
        .then(just(Token::DoubleDot).or_not())
        .delimited_by(breakable_blockbegin(), breakable_blockend())
        .map_with(move |(fields, is_imcomplete), e| {
            //fields are implicitly sorted by name.
            let mut fields = fields;
            fields.sort_by(|a, b| a.name.cmp(&b.name));
            let loc = Location {
                span: get_span(e.span()),
                path: ctx.file_path,
            };
            if is_imcomplete.is_some() {
                log::trace!("is imcomplete record literal");
                Expr::ImcompleteRecord(fields).into_id(loc)
            } else {
                Expr::RecordLiteral(fields).into_id(loc)
            }
        })
        .labelled("record_literal");
    let parenexpr = expr
        .clone()
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .map_with(move |e, e_s| {
            Expr::Paren(e).into_id(Location {
                span: get_span(e_s.span()),
                path: ctx.file_path,
            })
        })
        .labelled("paren_expr");
    //tuple must  lower precedence than parenexpr, not to parse single element tuple without trailing comma
    choice((
        literals_parser(ctx.clone()),
        var_parser(ctx.clone()),
        lambda,
        macro_expand,
        parenexpr,
        record_literal,
        array_literal,
        tuple,
    ))
    .boxed()
    .labelled("atom")
}
fn expr_parser<'src, I>(
    expr_group: impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone + 'src,
    ctx: ParseContext,
) -> impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    recursive(|expr| {
        enum FoldItem {
            Args(Vec<ExprNodeId>),
            ArrayIndex(ExprNodeId),
        }
        let parenitems = items_parser(expr.clone(), true)
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map_with(|args, e| (FoldItem::Args(args), get_span(e.span())));
        let angle_paren_expr = expr
            .clone()
            .delimited_by(just(Token::ArrayBegin), just(Token::ArrayEnd))
            .map_with(|v, e| (FoldItem::ArrayIndex(v), get_span(e.span())));

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
            .foldl(angle_paren_expr.or(parenitems).repeated(), folder)
            .labelled("apply");

        op_parser(apply, ctx).boxed()
    })
}
fn validate_reserved_pat<'src>(
    id: &TypedPattern,
    span: SimpleSpan,
) -> Result<(), Rich<'src, Token>> {
    match &id.pat {
        Pattern::Single(symbol) => validate_reserved_ident(*symbol, span),
        _ => Ok(()),
    }
}

fn validate_reserved_ident<'src>(id: Symbol, span: SimpleSpan) -> Result<(), Rich<'src, Token>> {
    if intrinsics::BUILTIN_SYMS.with(|syms| syms.binary_search(&id).is_ok()) {
        Err(Rich::custom(
            span,
            "Builtin functions cannot be re-defined.",
        ))
    } else {
        Ok(())
    }
}

fn statement_parser<'src, I>(
    expr: impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone + 'src,
    ctx: ParseContext,
) -> impl Parser<'src, I, (Statement, Location), ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let let_ = just(Token::Let)
        .ignore_then(pattern_parser(ctx.clone()).validate(|pat, e, emitter| {
            if let Err(e) = validate_reserved_pat(&pat, e.span()) {
                emitter.emit(e);
            }
            pat
        }))
        .then_ignore(just(Token::Assign).then(just(Token::LineBreak).repeated()))
        .then(expr.clone())
        .map_with(|(ident, body), e| (Statement::Let(ident, body), get_span(e.span())))
        .labelled("let");
    let letrec = just(Token::LetRec)
        .ignore_then(
            lvar_parser_typed(ctx.clone()).validate(|ident, e, emitter| {
                if let Err(e) = validate_reserved_ident(ident.id, e.span()) {
                    emitter.emit(e);
                }
                ident
            }),
        )
        .then_ignore(just(Token::Assign).then(just(Token::LineBreak).repeated()))
        .then(expr.clone())
        .map_with(|(ident, body), e| (Statement::LetRec(ident, body), get_span(e.span())))
        .labelled("letrec");
    let assign = var_parser(ctx.clone())
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map_with(|(lvar, body), e| (Statement::Assign(lvar, body), get_span(e.span())))
        .labelled("assign");
    let single = expr
        .map_with(|s, e| (Statement::Single(s), get_span(e.span())))
        .labelled("single");
    choice((let_, letrec, assign, single))
        .boxed()
        .map(move |(t, span)| {
            (
                t,
                Location {
                    span: span.start()..span.end(),
                    path: ctx.file_path,
                },
            )
        })
}
fn statements_parser<'src, I>(
    expr: impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone + 'src,
    ctx: ParseContext,
) -> impl Parser<'src, I, Option<ExprNodeId>, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    statement_parser(expr, ctx.clone())
        .separated_by(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .recover_with(skip_then_retry_until(
            any().ignored(),
            one_of([Token::LineBreak, Token::SemiColon])
                .ignored()
                .or(end()),
        ))
        .map(|stmts| into_then_expr(&stmts))
        .boxed()
}

fn block_parser<'src, I>(
    expr: impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone + 'src,
    ctx: ParseContext,
) -> impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let stmts = statements_parser(expr, ctx.clone());
    let ctx3 = ctx.clone();
    let block = stmts
        .delimited_by(breakable_blockbegin(), breakable_blockend())
        .map_with(move |stmts, e| Expr::Block(stmts).into_id(ctx.clone().gen_loc(e.span())));
    one_of([Token::BackQuote, Token::Dollar])
        .map_with(move |op, e| (op, get_span(e.span())))
        .repeated()
        .foldr(block, move |(op, op_span), rhs| {
            let rhs_span = rhs.to_span();
            let loc = Location {
                span: merge_span(op_span, rhs_span),
                path: ctx3.file_path,
            };
            match op {
                Token::BackQuote => Expr::Bracket(rhs).into_id(loc.clone()),
                Token::Dollar => Expr::Escape(rhs).into_id(loc.clone()),
                _ => unreachable!("Unexpected block operator: {:?}", op),
            }
        })
}

fn exprgroup_parser<'src, I>(
    ctx: ParseContext,
) -> impl Parser<'src, I, ExprNodeId, ParseError<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    recursive(move |expr_group: Recursive<_>| {
        let expr = expr_parser(expr_group.clone(), ctx.clone());

        let block = block_parser(expr_group.clone(), ctx.clone());
        //todo: should be recursive(to paranthes be not needed)
        let if_ = just(Token::If)
            .ignore_then(
                expr_group
                    .clone()
                    .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
            )
            .padded_by(just(Token::LineBreak).repeated())
            .then(expr_group.clone())
            .then(
                just(Token::Else)
                    .padded_by(just(Token::LineBreak).repeated())
                    .ignore_then(expr_group.clone())
                    .or_not(),
            )
            .map_with(move |((cond, then), opt_else), e| {
                Expr::If(cond, then, opt_else).into_id(Location {
                    span: get_span(e.span()),
                    path: ctx.file_path,
                })
            })
            .labelled("if");

        block
            .or(if_)
            // .or(expr_statement_parser(expr_group.clone(), expr_group))
            .or(expr.clone())
            // we have to recover from nested delimiters at exactly here, not each of record literal parser and block parser,
            // because we have to try parsing to distinguish them.
            .recover_with(via_parser(nested_delimiters(
                Token::BlockBegin,
                Token::BlockEnd,
                [(Token::ParenBegin, Token::ParenEnd)],
                move |span| Expr::Error.into_id(ctx.clone().gen_loc(span)),
            )))
    })
    .boxed()
}

fn toplevel_parser<'src, I>(
    ctx: ParseContext,
) -> impl Parser<'src, I, Program, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let exprgroup = exprgroup_parser(ctx.clone());
    let lvar =
        lvar_parser_typed_with_default(ctx.clone(), exprgroup.clone()).try_map(|ty, span| {
            TypedId::try_from(ty).map_err(|err| Rich::custom(span, err.to_string()))
        });

    let fnparams = lvar
        .clone()
        .separated_by(breakable_comma())
        .collect()
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .map_with(move |params, e| {
            let loc = Location {
                span: get_span(e.span()),
                path: ctx.file_path,
            };
            (params, loc)
        })
        .labelled("fnparams");

    let function_s = just(Token::Function)
        .ignore_then(ident_parser().clone().validate(|ident, e, emitter| {
            if let Err(e) = validate_reserved_ident(ident, e.span()) {
                emitter.emit(e);
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
            block_parser(exprgroup.clone(), ctx.clone())
                .map(|e| match e.to_expr() {
                    Expr::Block(e) => e.unwrap(),
                    _ => e,
                })
                .recover_with(via_parser(nested_delimiters(
                    Token::BlockBegin,
                    Token::BlockEnd,
                    [(Token::ParenBegin, Token::ParenEnd)],
                    move |span| {
                        Expr::Error.into_id(Location {
                            span: get_span(span),
                            path: ctx.file_path,
                        })
                    },
                ))),
        )
        .map_with(move |(((name, args), return_type), body), e| {
            let loc = Location {
                span: get_span(e.span()),
                path: ctx.file_path,
            };
            (
                ProgramStatement::FnDefinition {
                    name,
                    args,
                    return_type,
                    body,
                },
                loc,
            )
        })
        .labelled("function decl");

    let global_stmt = statement_parser(exprgroup.clone(), ctx.clone())
        .map(|(s, span)| (ProgramStatement::GlobalStatement(s), span));
    let import = just(Token::Include)
        .ignore_then(
            select! {Token::Str(s) => s.to_symbol()}
                .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
        )
        .map_with(move |path, e| {
            (
                ProgramStatement::Import(path),
                Location::new(get_span(e.span()), ctx.file_path),
            )
        });
    let stmt = choice((function_s, global_stmt, import))
        .recover_with(skip_then_retry_until(
            any().ignored(),
            one_of([Token::LineBreak, Token::SemiColon])
                .ignored()
                .or(end()),
        ))
        .labelled("toplevel statement");
    let separator = just(Token::LineBreak).or(just(Token::SemiColon)).repeated();

    stmt.separated_by(separator.clone())
        .collect()
        .padded_by(separator)
        .then_ignore(end())
        .recover_with(skip_then_retry_until(
            any().ignored(),
            one_of([Token::LineBreak, Token::SemiColon])
                .ignored()
                .or(end()),
        ))
        .map(|stmts: Vec<(ProgramStatement, Location)>| Program {
            statements: stmts
                .into_iter()
                .map(|(s, loc)| (s, loc.span))
                .collect::<Vec<_>>(),
        })
        .boxed()
}

// fn preprocess_parser(
//     ctx: ParseContext,
// ) -> impl Parser<Token, ExprNodeId, Error = ParseError> + Clone {
//     just(Token::Include)
//         .ignore_then(
//             select! {Token::Str(s) => s}
//                 .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
//         )
//         .try_map(move |filename, span: Span| {
//             let cfile = ctx.file_path.as_str();
//             let (c, errs) = resolve_include(cfile, &filename, span.clone());
//             if errs.is_empty() {
//                 Ok(c)
//             } else {
//                 let e = errs.into_iter().fold(
//                     Simple::<Token>::custom(
//                         span.clone(),
//                         format!("failed to resolve include for {filename}"),
//                     ),
//                     |simple_e, reportable_e| {
//                         let wrapped =
//                             Simple::<Token>::custom(span.clone(), reportable_e.to_string());
//                         wrapped.merge(simple_e)
//                     },
//                 );
//                 Err(e)
//             }
//         })
// }
fn parser<'src, I>(
    current_file: Option<PathBuf>,
) -> impl Parser<'src, I, Program, ParseError<'src>> + Clone
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let ctx = ParseContext {
        file_path: current_file.map_or("".to_symbol(), |p| p.to_string_lossy().to_symbol()),
    };
    toplevel_parser(ctx)
}

pub(crate) fn add_global_context(ast: ExprNodeId, file_path: Symbol) -> ExprNodeId {
    let span = ast.to_span();
    let loc = Location {
        span: span.clone(),
        path: file_path,
    };
    let res = Expr::Let(
        TypedPattern::new(
            Pattern::Single(GLOBAL_LABEL.to_symbol()),
            Type::Unknown.into_id_with_location(loc.clone()),
        ),
        Expr::Lambda(vec![], None, ast).into_id(loc.clone()),
        None,
    );
    res.into_id(loc)
}
pub fn lex(
    src: &str,
    current_file: Option<PathBuf>,
) -> (
    Option<Vec<(Token, SimpleSpan)>>,
    Vec<Box<dyn ReportableError>>,
) {
    let (tokens, lex_errs) = lexer::lexer().parse(src).into_output_errors();
    let lex_errs = lex_errs.into_iter().map(|e| -> Box<dyn ReportableError> {
        Box::new(error::ParseError::<char>::new(
            e,
            current_file
                .clone()
                .unwrap_or_default()
                .to_string_lossy()
                .to_symbol(),
        ))
    });
    (tokens, lex_errs.collect())
}
pub(super) fn convert_parse_errors<'src>(
    errs: &[Rich<'src, Token>],
) -> impl Iterator<Item = Box<dyn ReportableError>> {
    errs.iter().map(move |e| -> Box<dyn ReportableError> {
        Box::new(error::ParseError::new(e.clone(), Symbol::default()))
    })
}

pub fn parse(
    src: &'_ str,
    current_file: Option<PathBuf>,
) -> (Program, Vec<Box<dyn ReportableError>>) {
    let (tokens, lex_errs) = lex(src, current_file.clone());
    if let Some(t) = tokens {
        let tokens_comment_filtered = t
            .into_iter()
            .filter_map(move |(tkn, span)| match tkn {
                Token::Comment(token::Comment::SingleLine(_)) => Some((Token::LineBreak, span)),
                Token::Comment(token::Comment::MultiLine(_)) => None,
                _ => Some((tkn.clone(), span)),
            })
            .collect::<Vec<_>>();

        let (ast, errs) = parser(current_file.clone())
            .parse(
                Stream::from_iter(tokens_comment_filtered)
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
            )
            .into_output_errors();
        let errs = convert_parse_errors(&errs)
            .chain(lex_errs)
            .collect::<Vec<_>>();
        (ast.unwrap_or_default(), errs)
    } else {
        (Program::default(), lex_errs)
    }
}
pub fn parse_to_expr(
    src: &str,
    current_file: Option<PathBuf>,
) -> (ExprNodeId, Vec<Box<dyn ReportableError>>) {
    let (prog, mut errs) = parse(src, current_file.clone());
    if prog.statements.is_empty() {
        return (Expr::Error.into_id_without_span(), errs);
    }
    let (expr, mut new_errs) = expr_from_program(
        prog,
        current_file.map_or("".to_symbol(), |p| p.to_string_lossy().to_symbol()),
    );
    errs.append(&mut new_errs);
    (expr, errs)
}
