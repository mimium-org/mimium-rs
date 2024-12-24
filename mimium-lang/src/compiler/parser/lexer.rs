use super::token::*;
use super::ToSymbol;
use crate::utils::metadata::*;
use chumsky::prelude::*;
use chumsky::Parser;

fn comment_parser() -> impl Parser<char, Comment, Error = Simple<char>> + Clone {
    // comment parser that keep its contents length, not to break line number for debugging.
    // replaces all characters except for newline.
    let single_line = (just("//"))
        .ignore_then(take_until(text::newline().or(end())))
        .map(|(c, _)| Comment::SingleLine(String::from_iter(c.iter())));

    let multi_line = just("/*")
        .ignore_then(take_until(just("*/")))
        .map(|(c, _)| Comment::MultiLine(String::from_iter(c.iter())));

    single_line.or(multi_line)
}
fn linebreak_parser() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    text::newline()
        .repeated()
        .at_least(1)
        .map(|_s| Token::LineBreak)
}
pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers
    let int = text::int(10).map(|s: String| Token::Int(s.parse().unwrap()));

    let float = text::int(10)
        .then(just('.'))
        .then(text::digits(10).or_not())
        .map(|((s, _dot), opt_n)| Token::Float(format!("{}.{}", s, opt_n.unwrap_or_default())));

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for operators, we must disallow // or /* */, not to mixed with comment parser.
    // let op_special = just("*")
    //     .repeated()
    //     .at_least(1)
    //     .or(just("/").repeated().at_most(1));
    // //     .collect::<String>();

    let op = one_of("+-*/!=&|%><^@")
        .repeated()
        .at_least(1)
        .collect()
        .map(|s: String| match s.as_str() {
            "->" => Token::Arrow,
            "|" => Token::LambdaArgBeginEnd,
            "+" => Token::Op(Op::Sum),
            "-" => Token::Op(Op::Minus),
            "*" => Token::Op(Op::Product),
            "/" => Token::Op(Op::Divide),
            "==" => Token::Op(Op::Equal),
            "!=" => Token::Op(Op::NotEqual),
            "<" => Token::Op(Op::LessThan),
            "<=" => Token::Op(Op::LessEqual),
            ">" => Token::Op(Op::GreaterThan),
            ">=" => Token::Op(Op::GreaterEqual),
            "=" => Token::Assign,
            "%" => Token::Op(Op::Modulo),
            "^" => Token::Op(Op::Exponent),
            "@" => Token::Op(Op::At),
            "&&" => Token::Op(Op::And),
            "||" => Token::Op(Op::Or),
            "|>" => Token::Op(Op::Pipe),
            _ => Token::Op(Op::Unknown(s)),
        });
    let separator = one_of(",.:;").map(|c| match c {
        ',' => Token::Comma,
        '.' => Token::Dot,
        ':' => Token::Colon,
        ';' => Token::SemiColon,
        _ => Token::Ident(c.to_string().to_symbol()),
    });
    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::Function,
        "macro" => Token::Macro,
        "self" => Token::SelfLit,
        "now" => Token::Now,
        "samplerate" => Token::SampleRate,
        "let" => Token::Let,
        "letrec" => Token::LetRec,
        "if" => Token::If,
        "else" => Token::Else,
        // "true" => Token::Bool(true),
        // "false" => Token::Bool(false),
        // "null" => Token::Null,
        "float" => Token::FloatType,
        "int" => Token::IntegerType,
        "string" => Token::StringType,
        "struct" => Token::StructType,
        "include" => Token::Include,
        "_" => Token::PlaceHolder,
        _ => Token::Ident(ident.to_symbol()),
    });
    let macro_expand = text::ident()
        .then_ignore(just('!'))
        .map(|ident: String| Token::MacroExpand(ident.to_symbol()));

    let parens = one_of::<_, _, Simple<char>>("(){}[]").map(|c| match c {
        '(' => Token::ParenBegin,
        ')' => Token::ParenEnd,
        '{' => Token::BlockBegin,
        '}' => Token::BlockEnd,
        '[' => Token::ArrayBegin,
        ']' => Token::ArrayEnd,
        _ => Token::Ident(c.to_string().to_symbol()),
    });

    // A single token can be one of the above
    let token = choice((
        comment_parser().map(Token::Comment),
        float,
        int,
        str_,
        macro_expand,
        separator,
        ident,
        op,
        parens,
        linebreak_parser(),
    ))
    .recover_with(skip_then_retry_until([]));

    let whitespaces = one_of(" \t\u{0020}").repeated().ignored();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(whitespaces)
        .repeated()
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_let() {
        let src = "let hoge = 36\nfuga";
        let (res, _errs) = lexer().parse_recovery(src);
        let ans = [
            (Token::Let, 0..3),
            (Token::Ident("hoge".to_symbol()), 4..8),
            (Token::Assign, 9..10),
            (Token::Int(36), 11..13),
            (Token::LineBreak, 13..14),
            (Token::Ident("fuga".to_symbol()), 14..18),
        ];
        // dbg!(res.clone());
        if let Some(tok) = res {
            assert_eq!(tok, ans);
        } else {
            println!("{:#?}", _errs);
            panic!()
        }
    }

    #[test]
    fn comment() {
        let test_parser = comment_parser()
            .map(Token::Comment)
            .or(text::ident().map(|s: String| Token::Ident(s.to_symbol())))
            .or(linebreak_parser())
            .map_with_span(|t, s| (t, s))
            .padded_by(just(" ").repeated())
            .repeated();
        let src = "test
//comment start
conrains src
/*multiline comment
here */
another line
";
        let ans = vec![
            (Token::Ident("test".to_symbol()), 0usize..4),
            (Token::LineBreak, 4..5),
            (
                Token::Comment(Comment::SingleLine("comment start".into())),
                5..21,
            ),
            (Token::Ident("conrains".to_symbol()), 21..29),
            (Token::Ident("src".to_symbol()), 30..33),
            (Token::LineBreak, 33..34),
            (
                Token::Comment(Comment::MultiLine("multiline comment\nhere ".into())),
                34..61,
            ),
            (Token::LineBreak, 61..62),
            (Token::Ident("another".to_symbol()), 62..69),
            (Token::Ident("line".to_symbol()), 70..74),
            (Token::LineBreak, 74..75),
        ];

        let (res, errs) = test_parser.parse_recovery(src);
        assert!(errs.is_empty());
        assert!(res.is_some());
        assert_eq!(ans, res.unwrap());
        assert_eq!(src.len(), 75);
    }

    #[test]
    fn test_whitespaces() {
        let cases = [
            "foo ",    // single white space at end
            "foo  ",   // multiple white spaces at end
            "foo  \n", // linebreak at end
        ];
        for c in cases {
            let (res, errs) = lexer().parse_recovery(c);
            assert!(errs.is_empty(), "failed to parse");
            assert_eq!(res.unwrap()[0], (Token::Ident("foo".to_symbol()), 0..3))
        }
    }
}
