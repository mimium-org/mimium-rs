use std::fmt;

use crate::ast::operators::Op;
use crate::interner::Symbol;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Comment {
    SingleLine(String),
    MultiLine(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(Symbol),
    MacroExpand(Symbol),

    FloatType,
    IntegerType,
    StringType,
    StructType,

    Float(String),
    Int(i64),
    Str(String),

    Op(Op),
    SelfLit,
    Now,
    SampleRate,
    Comma,
    /// Dot operator, used for field access, which may be concatenated with left associativity
    Dot, // .
    /// Double dot, used for omitting default values in record literals and function calls
    DoubleDot, // ..
    Colon,
    SemiColon,

    Let,
    LetRec,
    Assign,

    ParenBegin,
    ParenEnd,
    ArrayBegin,
    ArrayEnd,
    BlockBegin,
    BlockEnd,
    LambdaArgBeginEnd,
    BackQuote,
    Dollar,

    Function,    //"fn"
    Macro,       //"macro"
    Arrow,       // ->
    PlaceHolder, // _

    If,
    Else,

    // Type,
    // Alias,
    Include,

    LineBreak,

    Comment(Comment),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(x) => write!(f, "{x}"),
            Token::MacroExpand(x) => write!(f, "{x}!"),
            Token::FloatType => write!(f, "float"),
            Token::IntegerType => write!(f, "int"),
            Token::StringType => write!(f, "string"),
            Token::StructType => write!(f, "struct"),
            Token::Int(x) => write!(f, "{x}"),
            Token::Float(x) => write!(f, "{x}"),
            Token::Str(x) => write!(f, "\"{x}\""),
            Token::Op(x) => write!(f, "{x}"),
            Token::SelfLit => write!(f, "self"),
            Token::Now => write!(f, "now"),
            Token::SampleRate => write!(f, "samplerate"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::DoubleDot => write!(f, ".."),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::Let => write!(f, "let"),
            Token::LetRec => write!(f, "letrec"),
            Token::Assign => write!(f, "="),
            Token::ParenBegin => write!(f, "("),
            Token::ParenEnd => write!(f, ")"),
            Token::ArrayBegin => write!(f, "["),
            Token::ArrayEnd => write!(f, "]"),
            Token::BlockBegin => write!(f, "{{"),
            Token::BlockEnd => write!(f, "}}"),
            Token::LambdaArgBeginEnd => write!(f, "|"),
            Token::Function => write!(f, "fn"),
            Token::Macro => write!(f, "macro"),
            Token::Arrow => write!(f, "->"),
            Token::PlaceHolder => write!(f, "_"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Include => write!(f, "include"),
            Token::LineBreak => write!(f, "linebreak"),
            Token::Comment(_) => write!(f, "comment"),
            Token::BackQuote => write!(f, "`"),
            Token::Dollar => write!(f, "$"),
        }
    }
}
