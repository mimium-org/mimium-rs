use chumsky::prelude::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Int(i64),
    String(String),
    Let((String, Box<Expr>, Box<Expr>)),
}

pub fn parsetester() -> impl Parser<char, Expr, Error = Simple<char>> {
    let int = text::int(10)
        .labelled("integer")
        .map(|v: String| Expr::Int(v.parse().unwrap()));
    let ident = text::ident().labelled("ident").map(|s: String| s);
    let atom = int.or(ident.map(|s| Expr::String(s))).padded();
    let expr = recursive(|expr| {
        let letp = text::keyword("let")
            .padded()
            .ignore_then(ident.padded())
            .then_ignore(just('=').padded())
            .then(atom.clone())
            .then_ignore(just(';'))
            .padded()
            .then(expr.clone())
            .map(|((ident, body), then)| Expr::Let((ident, Box::new(body), Box::new(then))));
        letp.or(atom).padded()
    })
    .padded();
    expr.then_ignore(end())
}

#[test]
pub fn test_hogehoge() {
    let src = "let hoge = 100; gufa".to_string();
    match parsetester().parse(src) {
        Ok(ast) => {
            // assert_eq!(ast, ans);
            println!("Parse Success: {:#?}", ast);
        }
        Err(parse_errs) => {
            parse_errs
                .into_iter()
                .for_each(|e| println!("Parse error: {}", e));
            panic!();
        }
    }
}
