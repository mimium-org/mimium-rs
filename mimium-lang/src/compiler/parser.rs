use std::path::PathBuf;

use crate::ast::operators::Op;
use crate::ast::*;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::types::{LabeledParam, LabeledParams, PType, Type};
use crate::utils::error::ReportableError;
use crate::utils::metadata::*;
use chumsky::extra::ParserExtra;
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
    pub fn gen_loc(&self, span: Span) -> Location {
        Location {
            span,
            path: self.file_path,
        }
    }
}
pub(crate) type ParseError<'src> = chumsky::extra::Err<Rich<'src, Token>>;

fn merge_span(a: Span, b: Span) -> Span {
    a.start..b.end
}
macro_rules! ParserTrait {
    ($src:lifetime,$input:ty,$output:ty) => { impl Parser<$src, $input, $output, ParseError<$src>> + Clone + 'src };
}

fn breakable_comma<'src, I>() -> impl Parser<'src, I, (), ParseError<'src>>
where
    I: Input<'src, Token = Token>,
{
    just(Token::Comma)
        .ignore_then(just(Token::LineBreak).or_not())
        .ignored()
}
fn breakable_blockbegin<'src>() -> ParserTrait!('src, ()) {
    just(Token::BlockBegin)
        .then(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
        .ignored()
}
fn breakable_blockend<'src>() -> ParserTrait!('src, ()) {
    just(Token::LineBreak)
        .or(just(Token::SemiColon))
        .repeated()
        .then(just(Token::BlockEnd))
        .ignored()
}
fn get_span<T: chumsky::span::Span<Offset = usize>>(e: T) -> Span {
    e.start()..e.end()
}
fn type_primitive<'src>(ctx: ParseContext) -> ParserTrait!('src, TypeNodeId) {
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
macro_rules! recoverable_delimited {
    ($parser:expr,$delim:expr,$fallback:expr,$map:expr) => {
        $parser
            .delimited_by(just($delim.0.clone()), just($delim.1.clone()))
            .map_with($map)
            .recover_with(via_parser(nested_delimiters(
                $delim.0,
                $delim.1,
                [],
                move |_span| $fallback.clone(),
            )))
            .boxed()
    };
}
fn recoverable_delimited<'src, 'b, T, F, E>(
    parser: ParserTrait!('src,T),
    delim: (Token, Token),
    fallback: T,
    map: F,
) -> ParserTrait!('src,T)
where
    T: Clone + 'src,
    F: Fn(T, &mut E) -> T,
{
    parser
        .delimited_by(just(delim.0.clone()), just(delim.1.clone()))
        .map_with(map)
        .recover_with(via_parser(nested_delimiters(
            delim.0,
            delim.1,
            [],
            move |_span| fallback.clone(),
        )))
        .boxed()
}

fn type_parser<'src>(ctx: ParseContext) -> ParserTrait!('src, TypeNodeId) {
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
                Type::Tuple(item).into_id_with_location(Location::new(get_span(e.span()), path))
            })
            .recover_with(via_parser(nested_delimiters(
                Token::ParenBegin,
                Token::ParenEnd,
                [],
                move |_span| Type::Failure.into_id(),
            )))
            .labelled("Tuple Type");

        let record = recoverable_delimited!(
            ident_parser()
                .then_ignore(just(Token::Colon))
                .then(ty.clone())
                .separated_by(breakable_comma())
                .allow_trailing(),
            (Token::BlockBegin, Token::BlockEnd),
            Type::Failure.into_id(),
            move |fields: Vec<_>, e| {
                Type::Record(fields).into_id_with_location(Location::new(e.span(), path))
            }
        )
        .labelled("Record Type");
        // Parse array type [T]
        let array = ty
            .clone()
            .delimited_by(just(Token::ArrayBegin), just(Token::ArrayEnd))
            .map_with(move |element_type, e| {
                Type::Array(element_type)
                    .into_id_with_location(Location::new(get_span(e.span()), path))
            })
            .boxed()
            .labelled("Array");

        // let _struct_t = todo!();
        let atom = choice((type_primitive(ctx.clone()), record, tuple, array));
        let func = atom
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .then(just(Token::Arrow).ignore_then(ty.clone()))
            .map_with(move |(a, body), e| {
                Type::Function(
                    LabeledParams::new(a.into_iter().map(LabeledParam::from).collect()),
                    body,
                    None,
                )
                .into_id_with_location(Location::new(get_span(e.span()), path))
            })
            .boxed()
            .labelled("function");

        func.or(atom).labelled("Type")
    })
}
pub(super) fn ident_parser<'src>() -> ParserTrait!('src, Symbol) {
    select! { Token::Ident(s) => s }.labelled("ident")
}
fn literals_parser<'src>(ctx: ParseContext) -> ParserTrait!('src, ExprNodeId) {
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
    .map_with(move |lit, e| {
        Expr::Literal(lit).into_id(Location {
            span: get_span(e.span()),
            path: ctx.file_path,
        })
    })
    .labelled("literal")
}
fn var_parser<'src>(ctx: ParseContext) -> ParserTrait!('src, ExprNodeId) {
    ident_parser().map_with(move |s, e| {
        Expr::Var(s).into_id(Location {
            span: get_span(e.span()),
            path: ctx.file_path,
        })
    })
}
fn with_type_annotation<'src, P, O>(
    parser: ParserTrait!('src, P),
    ctx: ParseContext,
) -> ParserTrait!('src,(O, Option<TypeNodeId>)) {
    parser.then(just(Token::Colon).ignore_then(type_parser(ctx)).or_not())
}

fn lvar_parser_typed<'src>(ctx: ParseContext) -> ParserTrait!('src, TypedId) {
    with_type_annotation(ident_parser(), ctx.clone())
        .map_with(move |(sym, t), e| match t {
            Some(ty) => TypedId { id: sym, ty },
            None => TypedId {
                id: sym,
                ty: Type::Unknown.into_id_with_location(Location {
                    span: get_span(e.span()),
                    path: ctx.file_path,
                }),
            },
        })
        .labelled("lvar_typed")
}
fn pattern_parser<'src>(ctx: ParseContext) -> ParserTrait!('src, TypedPattern) {
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
    with_type_annotation(pat, ctx.clone()).map_with(move |(pat, ty), e| match ty {
        Some(ty) => TypedPattern { pat, ty },
        None => TypedPattern {
            pat,
            ty: Type::Unknown.into_id_with_location(Location {
                span: get_span(e.span()),
                path: ctx.file_path,
            }),
        },
    })
}

fn items_parser<'src, E>(expr: E, allow_empty: bool) -> ParserTrait!('src, Vec<ExprNodeId>)
where
    E: Parser<'src, &'src [Token], ExprNodeId, ParseError<'src>> + Clone + 'src,
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
fn dot_field<'src>() -> ParserTrait!('src, (DotField, Span)) {
    select! {
        Token::Int(i) => DotField::Index(i),
        Token::Ident(s) => DotField::Ident(s),
    }
    .map_with(|field, e| (field, get_span(e.span())))
    .labelled("dot_field")
}
fn op_parser<'src, I>(apply: I, ctx: ParseContext) -> ParserTrait!('src, ExprNodeId)
where
    I: Parser<'src, &'src [Token], ExprNodeId, ParseError<'src>> + Clone + 'src,
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
        .foldr(dot.clone(), move |(op, op_span), rhs| {
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
    let optoken = move |o: Op| {
        select! {
            Token::Op(o) => o,
        }
        .boxed()
    };
    // allow pipe opertor to absorb linebreaks so that it can be also used at
    // the head of the line.
    let pipe = just(Token::LineBreak)
        .repeated()
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
                op.then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
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
fn record_fields<'src>(expr: ParserTrait!('src, ExprNodeId)) -> ParserTrait!('src, RecordField) {
    ident_parser()
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(move |(name, expr)| RecordField { name, expr })
}

pub(super) fn atom_parser<'src>(
    expr: ParserTrait!('src, ExprNodeId),
    expr_group: ParserTrait!('src, ExprNodeId),
    ctx: ParseContext,
) -> ParserTrait!('src, ExprNodeId) {
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
        .at_least(1)
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(breakable_blockbegin(), breakable_blockend())
        .map_with(move |fields, e| {
            //fields are implicitly sorted by name.
            let mut fields = fields;
            fields.sort_by(|a, b| a.name.cmp(&b.name));
            Expr::RecordLiteral(fields).into_id(Location {
                span: get_span(e.span()),
                path: ctx.file_path,
            })
        })
        .recover_with(via_parser(nested_delimiters(
            Token::BlockBegin,
            Token::BlockEnd,
            [],
            move |_| {
                Expr::Error.into_id(Location {
                    span: Span::default(),
                    path: ctx.file_path,
                })
            },
        )))
        .labelled("record_literal");
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
        record_literal,
        array_literal,
        tuple,
    ))
}
fn expr_parser<'src>(
    expr_group: ParserTrait!('src,ExprNodeId),
    ctx: ParseContext,
) -> ParserTrait!('src,ExprNodeId) {
    recursive(|expr: Recursive<_>| {
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

        op_parser(apply, ctx)
    })
}
fn validate_reserved_pat<'src, S>(id: &TypedPattern, span: S) -> Result<(), Rich<'src, Token>>
where
    S: chumsky::span::Span,
{
    match &id.pat {
        Pattern::Single(symbol) => validate_reserved_ident(*symbol, span),
        _ => Ok(()),
    }
}

fn validate_reserved_ident<'src, S: chumsky::span::Span>(
    id: Symbol,
    span: S,
) -> Result<(), Rich<'src, Token>>
where
    S: chumsky::span::Span,
{
    if intrinsics::BUILTIN_SYMS.with(|syms| syms.binary_search(&id).is_ok()) {
        Err(Rich::custom(
            span,
            "Builtin functions cannot be re-defined.",
        ))
    } else {
        Ok(())
    }
}

fn statement_parser<'src>(
    expr: ParserTrait!('src, ExprNodeId),
    ctx: ParseContext,
) -> ParserTrait!('src, (Statement, Location)) {
    let let_ = just(Token::Let)
        .ignore_then(pattern_parser(ctx.clone()).validate(|pat, e, emitter| {
            if let Err(e) = validate_reserved_pat(&pat, e.span()) {
                emitter.emit(e);
            }
            pat
        }))
        .then_ignore(just(Token::Assign))
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
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map_with(|(ident, body), e| (Statement::LetRec(ident, body), get_span(e.span())))
        .labelled("letrec");
    let assign = var_parser(ctx.clone())
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map_with(|(lvar, body), e| (Statement::Assign(lvar, body), get_span(e.span())))
        .labelled("assign");
    let single = expr
        .map_with(|e, e| (Statement::Single(e), get_span(e.span())))
        .labelled("single");
    let_.or(letrec)
        .or(assign)
        .or(single)
        .map(move |(t, span)| (t, ctx.clone().gen_loc(span)))
}
fn statements_parser<'src>(
    expr: ParserTrait!('src, ExprNodeId),
    ctx: ParseContext,
) -> ParserTrait!('src, Option<ExprNodeId>) {
    statement_parser(expr, ctx.clone())
        .separated_by(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .recover_with(skip_then_retry_until(
            one_of([Token::LineBreak, Token::SemiColon]).not().ignored(),
            one_of([Token::LineBreak, Token::SemiColon]).ignored(),
        ))
        .map(|stmts| into_then_expr(&stmts))
}

fn block_parser<'src>(
    expr: ParserTrait!('src, ExprNodeId),
    ctx: ParseContext,
) -> ParserTrait!('src, ExprNodeId) {
    let stmts = statements_parser(expr, ctx.clone());
    let ctx2 = ctx.clone();
    let ctx3 = ctx.clone();
    let block = stmts
        .delimited_by(breakable_blockbegin(), breakable_blockend())
        .map_with(move |stmts, e| {
            Expr::Block(stmts).into_id(ctx.clone().gen_loc(get_span(e.span())))
        })
        .recover_with(via_parser(nested_delimiters(
            Token::BlockBegin,
            Token::BlockEnd,
            [],
            move |span| Expr::Error.into_id(ctx2.clone().gen_loc(get_span(span))),
        )));
    one_of([Token::BackQuote, Token::Dollar])
        .map_with(move |op, e| (op, get_span(e.span())))
        .repeated()
        .foldr(block.clone(), move |(op, op_span), rhs| {
            let rhs_span = rhs.to_span();
            let loc = ctx3.clone().gen_loc(merge_span(op_span, rhs_span));
            match op {
                Token::BackQuote => Expr::Bracket(rhs).into_id(loc.clone()),
                Token::Dollar => Expr::Escape(rhs).into_id(loc.clone()),
                _ => unreachable!("Unexpected block operator: {:?}", op),
            }
        })
}

fn exprgroup_parser<'src>(ctx: ParseContext) -> ParserTrait!('src, ExprNodeId) {
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
            .then(expr_group.clone())
            .then(just(Token::Else).ignore_then(expr_group.clone()).or_not())
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
            let t = if !tid.is_unknown() {
                tid.ty
            } else {
                Type::Unknown.into_id_with_location(loc.clone())
            };
            LabeledParam::new(tid.id, t)
        })
        .collect();
    Type::Function(
        LabeledParams::new(atypes),
        r_type.unwrap_or_else(|| Type::Unknown.into_id_with_location(loc.clone())),
        None,
    )
    .into_id_with_location(loc)
}
fn toplevel_parser<'src>(ctx: ParseContext) -> ParserTrait!('src, Program) {
    let exprgroup = exprgroup_parser(ctx.clone());
    let lvar = lvar_parser_typed(ctx.clone());
    let fnparams = lvar
        .clone()
        .separated_by(just(Token::Comma))
        .collect()
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .labelled("fnparams");

    let function_s = just(Token::Function)
        .ignore_then(ident_parser().clone().validate(|ident, e, emitter| {
            if let Err(e) = validate_reserved_ident(ident, e.span) {
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
            block_parser(exprgroup.clone(), ctx.clone()).map(|e| match e.to_expr() {
                Expr::Block(e) => e.unwrap(),
                _ => e,
            }),
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
    let stmt = choice((function_s, global_stmt, import));
    let separator = just(Token::LineBreak).or(just(Token::SemiColon)).repeated();
    // let stmts = stmt
    //     .map(|s: (Statement, Location)| vec![s])
    //     .or(
    //         preprocess_parser(ctx.clone()).map_with_span(move |e, span| {
    //             stmt_from_expr_top(e)
    //                 .into_iter()
    //                 .map(|st| (st, Location::new(span.clone(), ctx.file_path)))
    //                 .collect()
    //         }),
    //     )
    //     .separated_by(separator)
    //     .allow_leading()
    //     .allow_trailing()
    //     .recover_with(skip_until([Token::LineBreak, Token::SemiColon], |_| vec![]))
    //     .flatten()
    //     .map(|stmt| into_then_expr(&stmt).unwrap_or(Expr::Error.into_id_without_span()));
    let stmts = stmt
        .separated_by(separator.clone())
        .collect()
        .padded_by(separator)
        .then_ignore(end())
        .recover_with(skip_then_retry_until(
            one_of([Token::LineBreak, Token::SemiColon]).not().ignored(),
            one_of([Token::LineBreak, Token::SemiColon]).ignored(),
        ))
        .map(|stmts: Vec<(ProgramStatement, Location)>| Program {
            statements: stmts
                .into_iter()
                .map(|(s, loc)| (s, loc.span))
                .collect::<Vec<_>>(),
        });
    stmts
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
fn parser<'src>(current_file: Option<PathBuf>) -> ParserTrait!('src, Program) {
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
        TypedPattern {
            pat: Pattern::Single(GLOBAL_LABEL.to_symbol()),
            ty: Type::Unknown.into_id_with_location(loc.clone()),
        },
        Expr::Lambda(vec![], None, ast).into_id(loc.clone()),
        None,
    );
    res.into_id(loc)
}
pub fn lex(
    src: &str,
    current_file: Option<PathBuf>,
) -> (Option<Vec<(Token, Span)>>, Vec<Box<dyn ReportableError>>) {
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
    (tokens, lex_errs.collect())
}
pub(super) fn convert_parse_errors(
    errs: &[Rich<'_, Token>],
) -> impl Iterator<Item = Box<dyn ReportableError>> {
    errs.iter().map(|e| -> Box<dyn ReportableError> {
        Box::new(error::ParseError {
            content: e.clone(),
            file: Symbol::default(),
        })
    })
}

pub fn parse(src: &str, current_file: Option<PathBuf>) -> (Program, Vec<Box<dyn ReportableError>>) {
    let (tokens, lex_errs) = lex(src, current_file.clone());
    if let Some(t) = tokens {
        let tokens_comment_filtered = t
            .into_iter()
            .filter_map(|(tkn, span)| match tkn {
                Token::Comment(token::Comment::SingleLine(_)) => Some((Token::LineBreak, span)),
                Token::Comment(token::Comment::MultiLine(_)) => None,
                _ => Some((tkn.clone(), span)),
            })
            .collect::<Vec<_>>();
        let tok = tokens_comment_filtered
            .as_slice()
            .map((src.len()..src.len()).into(), |(t, s)| (t, s));
        log::trace!("tokens: {:?}", tokens_comment_filtered);
        let (ast, errs) = parser(current_file.clone())
            .map_with(move |p, e| (p, e.span()))
            .parse(tok)
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
