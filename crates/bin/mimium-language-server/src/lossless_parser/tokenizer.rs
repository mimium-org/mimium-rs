/// Lossless tokenizer for mimium language using chumsky
/// Converts source text into a sequence of position-aware tokens
use super::token::{LosslessToken, TokenKind};
use chumsky::prelude::*;
use chumsky::input::StrInput;

type LexerError<'src> = chumsky::extra::Err<Rich<'src, char, SimpleSpan>>;

/// Parser for whitespace (not including newlines)
fn whitespace_parser<'src, I>() -> impl Parser<'src, I, TokenKind, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    one_of(" \t\r")
        .repeated()
        .at_least(1)
        .to(TokenKind::Whitespace)
}

/// Parser for linebreaks
fn linebreak_parser<'src, I>() -> impl Parser<'src, I, TokenKind, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    text::newline()
        .repeated()
        .at_least(1)
        .to(TokenKind::LineBreak)
}

/// Parser for comments
fn comment_parser<'src, I>() -> impl Parser<'src, I, TokenKind, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    let endline = text::newline().or(end());
    let single_line = just("//")
        .ignore_then(any().and_is(endline.not()).repeated())
        .then_ignore(endline.rewind())
        .to(TokenKind::SingleLineComment);

    let multi_line = just("/*")
        .ignore_then(any().and_is(just("*/").not()).repeated())
        .then_ignore(just("*/"))
        .to(TokenKind::MultiLineComment);

    single_line.or(multi_line)
}

/// Parser for string literals
fn string_parser<'src, I>() -> impl Parser<'src, I, TokenKind, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    none_of('"')
        .repeated()
        .delimited_by(just('"'), just('"'))
        .to(TokenKind::Str)
}

/// Parser for numbers
fn number_parser<'src, I>() -> impl Parser<'src, I, TokenKind, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    let float = text::int::<I, _>(10)
        .then_ignore(just('.'))
        .then(text::digits::<I, _>(10))
        .then_ignore(just('.').not().ignored().or(end()).rewind())
        .to(TokenKind::Float);

    let int = text::int::<I, LexerError<'src>>(10).to(TokenKind::Int);

    float.or(int)
}

/// Parser for operators
fn operator_parser<'src, I>() -> impl Parser<'src, I, TokenKind, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    choice((
        just("->").to(TokenKind::Arrow),
        just("<-").to(TokenKind::LeftArrow),
        just("==").to(TokenKind::OpEqual),
        just("!=").to(TokenKind::OpNotEqual),
        just("<=").to(TokenKind::OpLessEqual),
        just(">=").to(TokenKind::OpGreaterEqual),
        just("&&").to(TokenKind::OpAnd),
        just("||").to(TokenKind::OpOr),
        just("|>").to(TokenKind::OpPipe),
        just("+").to(TokenKind::OpSum),
        just("-").to(TokenKind::OpMinus),
        just("*").to(TokenKind::OpProduct),
        just("/").to(TokenKind::OpDivide),
        just("%").to(TokenKind::OpModulo),
        just("^").to(TokenKind::OpExponent),
        just("@").to(TokenKind::OpAt),
        just("<").to(TokenKind::OpLessThan),
        just(">").to(TokenKind::OpGreaterThan),
        just("=").to(TokenKind::Assign),
        just("!").to(TokenKind::OpUnknown),
    ))
}

/// Parser for punctuation
fn punctuation_parser<'src, I>() -> impl Parser<'src, I, TokenKind, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    choice((
        just("..").to(TokenKind::DoubleDot),
        just(".").to(TokenKind::Dot),
        just(",").to(TokenKind::Comma),
        just(":").to(TokenKind::Colon),
        just(";").to(TokenKind::SemiColon),
        just("(").to(TokenKind::ParenBegin),
        just(")").to(TokenKind::ParenEnd),
        just("[").to(TokenKind::ArrayBegin),
        just("]").to(TokenKind::ArrayEnd),
        just("{").to(TokenKind::BlockBegin),
        just("}").to(TokenKind::BlockEnd),
        just("`").to(TokenKind::BackQuote),
        just("$").to(TokenKind::Dollar),
        just("#").to(TokenKind::Sharp),
        just("|").to(TokenKind::LambdaArgBeginEnd),
    ))
}

/// Parser for identifiers and keywords
fn identifier_parser<'src, I>() -> impl Parser<'src, I, TokenKind, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    // Macro expansion: identifier followed by !
    let macro_expand = text::ident()
        .then_ignore(just('!'))
        .to(TokenKind::MacroExpand);

    let ident = text::ident().to_slice().map(|ident: &'src str| match ident {
        "fn" => TokenKind::Function,
        "macro" => TokenKind::Macro,
        "self" => TokenKind::SelfLit,
        "now" => TokenKind::Now,
        "samplerate" => TokenKind::SampleRate,
        "let" => TokenKind::Let,
        "letrec" => TokenKind::LetRec,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "float" => TokenKind::FloatType,
        "int" => TokenKind::IntegerType,
        "string" => TokenKind::StringType,
        "struct" => TokenKind::StructType,
        "include" => TokenKind::Include,
        "stage" => TokenKind::StageKwd,
        "main" => TokenKind::Main,
        "_" => TokenKind::PlaceHolder,
        _ => TokenKind::Ident,
    });

    macro_expand.or(ident)
}

/// Main tokenizer that combines all parsers
fn token_parser<'src, I>() -> impl Parser<'src, I, TokenKind, LexerError<'src>> + Clone
where
    I: StrInput<'src, Token = char, Span = SimpleSpan, Slice = &'src str>,
{
    choice((
        comment_parser(),
        linebreak_parser(),
        whitespace_parser(),
        string_parser(),
        number_parser(),
        identifier_parser(),
        operator_parser(),  // Try operators before punctuation
        punctuation_parser(),
    ))
}

/// Tokenize the source text into a sequence of lossless tokens
pub fn tokenize(source: &str) -> Vec<LosslessToken> {
    let lexer = token_parser()
        .map_with(|kind, e| {
            let span: SimpleSpan = e.span();
            LosslessToken::new(kind, span.start, span.end - span.start)
        })
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end());

    match lexer.parse(source).into_result() {
        Ok(mut tokens) => {
            // Add EOF token
            tokens.push(LosslessToken::new(TokenKind::Eof, source.len(), 0));
            tokens
        }
        Err(errors) => {
            // On parse errors, try to recover by collecting what we can
            // This matches the behavior of mimium-lang's error recovery
            // For now, we return just EOF, but in a full implementation
            // we would use chumsky's error recovery features
            eprintln!("Tokenization errors: {:?}", errors);
            vec![LosslessToken::new(TokenKind::Eof, source.len(), 0)]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_tokenize_simple() {
        let source = "fn dsp() { 42 }";
        let tokens = tokenize(source);
        
        assert_eq!(tokens[0].kind, TokenKind::Function);
        assert_eq!(tokens[0].text(source), "fn");
        
        assert_eq!(tokens[1].kind, TokenKind::Whitespace);
        
        assert_eq!(tokens[2].kind, TokenKind::Ident);
        assert_eq!(tokens[2].text(source), "dsp");
    }
    
    #[test]
    fn test_tokenize_numbers() {
        let source = "42 3.14";
        let tokens = tokenize(source);
        
        assert_eq!(tokens[0].kind, TokenKind::Int);
        assert_eq!(tokens[0].text(source), "42");
        
        assert_eq!(tokens[2].kind, TokenKind::Float);
        assert_eq!(tokens[2].text(source), "3.14");
    }
    
    #[test]
    fn test_tokenize_string() {
        let source = r#""hello world""#;
        let tokens = tokenize(source);
        
        assert_eq!(tokens[0].kind, TokenKind::Str);
        assert_eq!(tokens[0].text(source), r#""hello world""#);
    }
    
    #[test]
    fn test_tokenize_comments() {
        let source = "// single line\n/* multi\nline */";
        let tokens = tokenize(source);
        
        assert_eq!(tokens[0].kind, TokenKind::SingleLineComment);
        assert_eq!(tokens[0].text(source), "// single line");
        
        assert_eq!(tokens[1].kind, TokenKind::LineBreak);
        
        assert_eq!(tokens[2].kind, TokenKind::MultiLineComment);
        assert_eq!(tokens[2].text(source), "/* multi\nline */");
    }
    
    #[test]
    fn test_tokenize_operators() {
        let source = "+ - * / == != < <= > >= && || |>";
        let tokens = tokenize(source);
        
        let op_kinds: Vec<_> = tokens.iter()
            .filter(|t| !matches!(t.kind, TokenKind::Whitespace | TokenKind::Eof))
            .map(|t| t.kind)
            .collect();
        
        assert_eq!(op_kinds, vec![
            TokenKind::OpSum,
            TokenKind::OpMinus,
            TokenKind::OpProduct,
            TokenKind::OpDivide,
            TokenKind::OpEqual,
            TokenKind::OpNotEqual,
            TokenKind::OpLessThan,
            TokenKind::OpLessEqual,
            TokenKind::OpGreaterThan,
            TokenKind::OpGreaterEqual,
            TokenKind::OpAnd,
            TokenKind::OpOr,
            TokenKind::OpPipe,
        ]);
    }
    
    #[test]
    fn test_trivia_detection() {
        let source = "fn // comment\n dsp";
        let tokens = tokenize(source);
        
        assert!(!tokens[0].is_trivia()); // fn
        assert!(tokens[1].is_trivia());  // whitespace
        assert!(tokens[2].is_trivia());  // comment
        assert!(tokens[3].is_trivia());  // linebreak
        assert!(tokens[4].is_trivia());  // whitespace
        assert!(!tokens[5].is_trivia()); // dsp
    }
}
