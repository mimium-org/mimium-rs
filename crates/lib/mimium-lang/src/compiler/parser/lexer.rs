use super::ToSymbol;
use super::token::*;
use crate::ast::operators::Op;
use chumsky::Parser;
use chumsky::container::Seq;
use chumsky::input::StrInput;
use chumsky::prelude::*;

type LexerError<'src> = chumsky::extra::Err<Rich<'src, char, SimpleSpan>>;

fn comment_parser<'src, I>() -> impl Parser<'src, I, Comment, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    // comment parser that keep its contents length, not to break line number for debugging.
    // replaces all characters except for newline.
    let endline = text::newline().or(end());
    let single_line = just("//")
        .ignore_then(any().and_is(endline.not()).repeated().to_slice())
        .then_ignore(endline)
        .map(|c| Comment::SingleLine(String::from(c)));

    let multi_line = just("/*")
        .ignore_then(any().and_is(just("*/").not()).repeated().to_slice())
        .then_ignore(just("*/"))
        .map(|c| Comment::MultiLine(String::from(c)));

    single_line.or(multi_line)
}
fn linebreak_parser<'src, I>() -> impl Parser<'src, I, Token, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    text::newline()
        .repeated()
        .at_least(1)
        .map(|_| Token::LineBreak)
}
pub fn tokenizer<'src, I>() -> impl Parser<'src, I, Token, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    // A parser for numbers
    let int = text::int::<I, LexerError<'src>>(10)
        .map(|s| Token::Int(String::from_iter(s.seq_iter()).parse().unwrap()));

    let float = (text::int::<I, _>(10).to_slice())
        .then_ignore(just('.'))
        .then(text::digits::<I, _>(10).to_slice())
        .then_ignore(just('.').not().ignored().or(end()).rewind())
        .map(|(s, n)| Token::Float(format!("{s}.{n}")));

    // A parser for strings
    let str_ = none_of('"')
        .repeated()
        .to_slice()
        .delimited_by(just('"'), just('"'))
        .map(|s: &'src str| Token::Str(format!("{s}")));

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

    // Handle '..' as DoubleDot token, and '.' as Dot token
    let double_dot = just("..").map(|_| Token::DoubleDot);
    let single_dot = just('.').map(|_| Token::Dot);

    let separator = choice((
        double_dot,
        single_dot,
        one_of(",.:;`$#").map(|c| match c {
            ',' => Token::Comma,
            ':' => Token::Colon,
            ';' => Token::SemiColon,
            '`' => Token::BackQuote,
            '$' => Token::Dollar,
            '#' => Token::Sharp,
            _ => Token::Ident(c.to_string().to_symbol()),
        }),
    ));
    // A parser for identifiers and keywords
    let ident = text::ident()
        .to_slice()
        .map(|ident: &'src str| match ident {
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
            "stage" => Token::StageKwd,
            "main" => Token::Main,
            "_" => Token::PlaceHolder,
            _ => Token::Ident(ident.to_symbol()),
        });
    let macro_expand = text::ident()
        .and_is(just('!').not())
        .then_ignore(just('!'))
        .to_slice()
        .map(|ident: &'src str| {
            Token::MacroExpand(ident[0..ident.len() - 1].to_string().to_symbol())
        });

    let parens = one_of("(){}[]").map(|c| match c {
        '(' => Token::ParenBegin,
        ')' => Token::ParenEnd,
        '{' => Token::BlockBegin,
        '}' => Token::BlockEnd,
        '[' => Token::ArrayBegin,
        ']' => Token::ArrayEnd,
        _ => Token::Ident(c.to_string().to_symbol()),
    });

    // A single token can be one of the above

    choice((
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
}

pub fn lexer<'src, I>() -> impl Parser<'src, I, Vec<(Token, SimpleSpan)>, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    let whitespaces = one_of(" \t\u{0020}").repeated().ignored();

    tokenizer()
        .map_with(|t, e| (t, e.span()))
        .padded_by(whitespaces)
        .repeated()
        .collect::<Vec<_>>()
}
#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_str() {
        let src = r#""hello world""#;
        let (res, errs) = lexer().parse(src).into_output_errors();
        assert!(errs.is_empty());
        assert!(res.is_some());
        let res = res.unwrap();
        assert_eq!(res.len(), 1);
        assert_eq!(res[0].0, Token::Str("hello world".to_string()));
    }
    #[test]
    fn test_let() {
        let src = "let hoge = 36\nfuga";
        let (output, errs) = lexer().parse(src).into_output_errors();
        let tok = output.map(|o| {
            o.into_iter()
                .map(|(t, s)| (t, s.start..s.end))
                .collect::<Vec<_>>()
        });
        let ans = [
            (Token::Let, 0..3),
            (Token::Ident("hoge".to_symbol()), 4..8),
            (Token::Assign, 9..10),
            (Token::Int(36), 11..13),
            (Token::LineBreak, 13..14),
            (Token::Ident("fuga".to_symbol()), 14..18),
        ];
        // dbg!(res.clone());
        if let Some(tok) = tok {
            assert_eq!(*tok, ans);
        } else {
            println!("{errs:#?}");
            panic!()
        }
    }

    #[test]
    fn comment() {
        let src = "test
//comment start
contains src
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
            (Token::Ident("contains".to_symbol()), 21..29),
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

        let (res, errs) = lexer().parse(src).into_output_errors();
        let res = res.map(|tokens| {
            tokens
                .iter()
                .map(|(t, s)| (t.clone(), s.start()..s.end()))
                .collect::<Vec<_>>()
        });
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
            let (res, errs) = lexer().parse(c).into_output_errors();
            let res = res.map(|tokens| {
                tokens
                    .iter()
                    .map(|(t, s)| (t.clone(), s.start()..s.end()))
                    .collect::<Vec<_>>()
            });
            assert!(errs.is_empty(), "failed to parse");
            assert_eq!(res.unwrap()[0], (Token::Ident("foo".to_symbol()), 0..3))
        }
    }
    #[test]
    fn test_dotoperator() {
        // Test for dot operator with index and field access.
        // The source contains "1.0" but it should not be parsed as a float.
        // The float tokenizer read one character ahead and ensure the dot operator do not comes next.
        let src = "x.0.field1.1.0.field2";
        let (res, _errs) = lexer().parse(src).into_output_errors();
        let res = res.map(|tokens| {
            tokens
                .iter()
                .map(|(t, s)| (t.clone(), s.start()..s.end()))
                .collect::<Vec<_>>()
        });
        let ans = vec![
            (Token::Ident("x".to_symbol()), 0..1),
            (Token::Dot, 1..2),
            (Token::Int(0), 2..3),
            (Token::Dot, 3..4),
            (Token::Ident("field1".to_symbol()), 4..10),
            (Token::Dot, 10..11),
            (Token::Int(1), 11..12),
            (Token::Dot, 12..13),
            (Token::Int(0), 13..14),
            (Token::Dot, 14..15),
            (Token::Ident("field2".to_symbol()), 15..21),
        ];
        // dbg!(res.clone());
        if let Some(tok) = res {
            assert_eq!(tok, ans);
        } else {
            panic!("failed to parse dot operator");
        }
    }
    #[test]
    fn test_dotoperator2() {
        //test if the normal float parser works
        let src = "3466.0+2000.0 ";
        let res = lexer().parse(src).output().map(|tokens| {
            tokens
                .iter()
                .map(|(t, s)| (t.clone(), s.start()..s.end()))
                .collect::<Vec<_>>()
        });
        let ans = vec![
            (Token::Float("3466.0".to_string()), 0..6),
            (Token::Op(Op::Sum), 6..7),
            (Token::Float("2000.0".to_string()), 7..13),
        ];
        // dbg!(res.clone());
        if let Some(tok) = res {
            assert_eq!(tok, ans);
        } else {
            panic!("failed to parse dot operator");
        }
    }
    #[test]
    fn test_macroexpand() {
        let src = "macro! test";
        let (res, _errs) = lexer().parse(src).into_output_errors();
        let res = res.map(|tokens| {
            tokens
                .iter()
                .map(|(t, s)| (t.clone(), s.start()..s.end()))
                .collect::<Vec<_>>()
        });
        let ans = vec![
            (Token::MacroExpand("macro".to_symbol()), 0..6),
            (Token::Ident("test".to_symbol()), 7..11),
        ];
        // dbg!(res.clone());
        if let Some(tok) = res {
            assert_eq!(tok, ans);
        } else {
            panic!("failed to parse macro expand");
        }
    }
}
