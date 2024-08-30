use crate::ast::*;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::types::{PType, Type};
use crate::utils::error::ReportableError;
use crate::utils::metadata::*;
use chumsky::{prelude::*, Parser};
// use chumsky::Parser;
mod token;
use token::{Comment, Op, Token};
mod error;
mod lexer;

#[cfg(test)]
mod test;

fn type_parser() -> impl Parser<Token, TypeNodeId, Error = Simple<Token>> + Clone {
    recursive(|ty| {
        let primitive = select! {
           Token::FloatType => Type::Primitive(PType::Numeric),
           Token::IntegerType => Type::Primitive(PType::Int),
           Token::StringType => Type::Primitive(PType::String)
        }
        .map_with_span(|t, s| t.into_id_with_span(s));

        let tuple = ty
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map_with_span(|t: Vec<TypeNodeId>, s: Span| Type::Tuple(t).into_id_with_span(s))
            .boxed()
            .labelled("Tuple");

        // let _struct_t = todo!();
        let atom = primitive.or(tuple);
        let func = atom
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .then(just(Token::Arrow).ignore_then(ty.clone()))
            .map_with_span(|(a, e), s| Type::Function(a, e, None).into_id_with_span(s))
            .boxed()
            .labelled("function");

        func.or(atom).boxed().labelled("Type")
    })
}
fn ident_parser() -> impl Parser<Token, Symbol, Error = Simple<Token>> + Clone {
    select! { Token::Ident(s) => s }.labelled("ident")
}
fn literals_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    select! {
        Token::Int(x) => Literal::Int(x),
        Token::Float(x) =>Literal::Float(x.parse().unwrap()),
        Token::Str(s) => Literal::String(s),
        Token::SelfLit => Literal::SelfLit,
        Token::Now => Literal::Now,
    }
    .map(Expr::Literal)
    .labelled("literal")
}
fn var_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    ident_parser().map(Expr::Var)
}
fn with_type_annotation<P, O>(
    parser: P,
) -> impl Parser<Token, (O, Option<TypeNodeId>), Error = Simple<Token>> + Clone
where
    P: Parser<Token, O, Error = Simple<Token>> + Clone,
{
    parser
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .map(|(id, t)| (id, t))
}

fn placement_parser() -> impl Parser<Token, Symbol, Error = Simple<Token>> + Clone {
    //todo! it can be the left hand expression of assignment i.e. can have memory address,
    //including var(`v`), Tuple dot access(`v.0`), array access(`v[2]`), struct member access...
    ident_parser().labelled("placement_values")
}
fn lvar_parser_typed() -> impl Parser<Token, TypedId, Error = Simple<Token>> + Clone {
    with_type_annotation(ident_parser())
        .map_with_span(|(sym, t), span| match t {
            Some(ty) => TypedId { id: sym, ty },
            None => TypedId {
                id: sym,
                ty: Type::Unknown.into_id_with_span(span),
            },
        })
        .labelled("lvar_typed")
}
fn pattern_parser() -> impl Parser<Token, TypedPattern, Error = Simple<Token>> + Clone {
    let pat = recursive(|pat| {
        pat.clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map(|ptns| Pattern::Tuple(ptns))
            .or(select! { Token::Ident(s) => Pattern::Single(s) })
            .labelled("Pattern")
    });
    with_type_annotation(pat).map_with_span(|(pat, ty), s| match ty {
        Some(ty) => TypedPattern { pat, ty },
        None => TypedPattern {
            pat,
            ty: Type::Unknown.into_id_with_span(s),
        },
    })
}
fn binop_folder<'a, I, OP>(prec: I, op: OP) -> BoxedParser<'a, Token, ExprNodeId, Simple<Token>>
where
    I: Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone + 'a,
    OP: Parser<Token, (Op, Span), Error = Simple<Token>> + Clone + 'a,
{
    prec.clone()
        .then(op.then(prec).repeated())
        .foldl(move |x, ((op, opspan), y)| {
            Expr::Apply(
                Expr::Var(op.get_associated_fn_name()).into_id(opspan),
                vec![x, y],
            )
            .into_id(x.to_span().start..y.to_span().end)
        })
        .boxed()
}

fn op_parser<'a, I>(apply: I) -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone + 'a
where
    I: Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone + 'a,
{
    let unary = select! { Token::Op(Op::Minus) => {} }
        .map_with_span(|e, s| (e, s))
        .repeated()
        .then(apply.clone())
        .foldr(|(_op, op_span), rhs| {
            let rhs_span = rhs.to_span();
            let neg_op = Expr::Var("neg".to_symbol()).into_id(op_span.start..rhs_span.start);
            Expr::Apply(neg_op, vec![rhs]).into_id(op_span.start..rhs_span.end)
        })
        .labelled("unary");

    let optoken = move |o: Op| {
        just(Token::Op(o))
            .map_with_span(|e, s| {
                (
                    match e {
                        Token::Op(o) => o,
                        _ => Op::Unknown(String::from("invalid")),
                    },
                    s,
                )
            })
            .boxed()
    };
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
        optoken(Op::Pipe),
    ];
    ops.into_iter().fold(unary.boxed(), binop_folder)

}
fn expr_parser() -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone {
    let lvar = lvar_parser_typed();
    let pattern = pattern_parser();
    let val = literals_parser().or(var_parser());
    let expr_group = recursive(|expr_group| {
        let expr = recursive(|expr: Recursive<Token, ExprNodeId, Simple<Token>>| {
            let parenexpr = expr
                .clone()
                .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
                .labelled("paren_expr");
            let let_e = just(Token::Let)
                .ignore_then(pattern.clone())
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
                .then(expr_group.clone().or_not())
                .map(|((ident, body), then)| Expr::Let(ident, body, then))
                .boxed()
                .labelled("let");

            let lambda = lvar
                .clone()
                .separated_by(just(Token::Comma))
                .delimited_by(
                    just(Token::LambdaArgBeginEnd),
                    just(Token::LambdaArgBeginEnd),
                )
                .then(just(Token::Arrow).ignore_then(type_parser()).or_not())
                .then(expr_group.clone())
                .map(|((ids, r_type), body)| Expr::Lambda(ids, r_type, body))
                .labelled("lambda");

            let macro_expand = select! { Token::MacroExpand(s) => Expr::Var(s) }
                .map_with_span(|e, s| e.into_id(s))
                .then_ignore(just(Token::ParenBegin))
                .then(expr_group.clone())
                .then_ignore(just(Token::ParenEnd))
                .map_with_span(|(id, then), s| {
                    Expr::Escape(Expr::Apply(id, vec![then]).into_id(s.clone()))
                })
                .labelled("macroexpand");

            let tuple = expr
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
                .map_with_span(|e, s| Expr::Tuple(e).into_id(s))
                .labelled("tuple");

            let atom = val
                .or(lambda)
                .or(macro_expand)
                .or(let_e)
                .map_with_span(|e, s| e.into_id(s))
                .or(parenexpr)
                .or(tuple)
                .boxed()
                .labelled("atoms");

            let items = expr
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>();

            let parenitems = items
                .clone()
                .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
                .repeated();
            let folder = |f: ExprNodeId, args: Vec<ExprNodeId>| {
                let f_span = f.to_span();
                // TODO: this doesn't include the span of the closing parenthesis
                let span_end = match args.as_slice() {
                    [.., end] => end.to_span().end,
                    _ => f_span.end,
                };
                let span = f_span.start..span_end;
                Expr::Apply(f, args).into_id(span)
            };
            let apply = atom.then(parenitems).foldl(folder).labelled("apply");

            op_parser(apply)
        });
        // expr_group contains let statement, assignment statement, function definiton,... they cannot be placed as an argument for apply directly.

        let block = expr_group
            .clone()
            .padded_by(just(Token::LineBreak).or_not())
            .delimited_by(just(Token::BlockBegin), just(Token::BlockEnd))
            .map(|e: ExprNodeId| Expr::Block(Some(e)));

        //todo: should be recursive(to paranthes be not needed)
        let if_ = just(Token::If)
            .ignore_then(
                expr_group
                    .clone()
                    .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
            )
            .then(expr_group.clone())
            .then(just(Token::Else).ignore_then(expr_group.clone()).or_not())
            .map_with_span(|((cond, then), opt_else), s| Expr::If(cond, then, opt_else).into_id(s))
            .labelled("if");

        block
            .map_with_span(|e, s| e.into_id(s))
            .or(if_)
            .or(expr.clone())
    });
    expr_group
}
fn comment_parser() -> impl Parser<Token, (), Error = Simple<Token>> + Clone {
    select! {Token::Comment(Comment::SingleLine(_t))=>(),
    Token::Comment(Comment::MultiLine(_t))=>()}
}
fn func_parser() -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone {
    let expr = expr_parser();
    let lvar = lvar_parser_typed();
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

    let stmt = recursive(|stmt| {
        let function_s = just(Token::Function)
            .ignore_then(lvar.clone())
            .then(fnparams.clone())
            .then(just(Token::Arrow).ignore_then(type_parser()).or_not())
            .then(
                expr.clone()
                    .delimited_by(blockstart.clone(), blockend.clone()),
            )
            .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
            .then(stmt.clone().or_not())
            .map_with_span(|((((fname, ids), r_type), block), then), s| {
                let atypes = ids
                    .iter()
                    .map(|tid| {
                        if !tid.is_unknown() {
                            tid.ty
                        } else {
                            Type::Unknown.into_id()
                        }
                    })
                    .collect::<Vec<_>>();
                let fname = TypedId {
                    id: fname.id,
                    ty: Type::Function(
                        atypes,
                        r_type.unwrap_or_else(|| Type::Unknown.into_id()),
                        None,
                    )
                    .into_id(),
                };
                Expr::LetRec(
                    fname,
                    Expr::Lambda(ids, r_type, block).into_id(s.clone()),
                    then,
                )
                .into_id(s)
            })
            .labelled("function decl");
        let macro_s = just(Token::Macro)
            .ignore_then(lvar.clone())
            .then(fnparams.clone())
            .then(
                expr.clone()
                    .delimited_by(blockstart.clone(), blockend.clone())
                    .map(Expr::Bracket),
            )
            .then(expr.clone().or_not())
            .map_with_span(|(((fname, ids), block), then), s| {
                Expr::LetRec(
                    fname,
                    Expr::Lambda(ids, None, block.into_id(s.clone())).into_id(s.clone()),
                    then,
                )
                .into_id(s)
            })
            .labelled("macro definition");
        let let_stmt = just(Token::Let)
            .ignore_then(pattern_parser().clone())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
            .then(stmt.clone().or_not())
            .map_with_span(|((ident, body), then), span| Expr::Let(ident, body, then).into_id(span))
            .boxed()
            .labelled("let_stmt");
        function_s.or(macro_s).or(let_stmt).or(expr_parser())
    });
    stmt
    // expr_parser().then_ignore(end())
}

fn parser() -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone {
    let ignored = comment_parser()
        .or(just(Token::LineBreak).ignored())
        .or(just(Token::SemiColon).ignored());
    func_parser()
        .padded_by(ignored.repeated())
        .then_ignore(end())
}

pub(crate) fn add_global_context(ast: ExprNodeId) -> ExprNodeId {
    let span = ast.to_span();
    let res = Expr::Let(
        TypedPattern {
            pat: Pattern::Single(GLOBAL_LABEL.to_symbol()),
            ty: Type::Unknown.into_id_with_span(span.clone()),
        },
        Expr::Lambda(vec![], None, ast).into_id(span.clone()),
        None,
    );
    res.into_id(span.clone())
}
pub fn parse(src: &str) -> Result<ExprNodeId, Vec<Box<dyn ReportableError>>> {
    let len = src.chars().count();
    let mut errs = Vec::<Box<dyn ReportableError>>::new();

    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src);
    lex_errs
        .iter()
        .for_each(|e| errs.push(Box::new(error::ParseError::<char>(e.clone()))));

    if let Some(t) = tokens {
        let (ast, parse_errs) =
            parser().parse_recovery(chumsky::Stream::from_iter(len..len + 1, t.into_iter()));
        match ast {
            Some(ast) => Ok(ast),
            None => {
                parse_errs
                    .iter()
                    .for_each(|e| errs.push(Box::new(error::ParseError::<Token>(e.clone()))));
                Err(errs)
            }
        }
    } else {
        Err(errs)
    }
}
