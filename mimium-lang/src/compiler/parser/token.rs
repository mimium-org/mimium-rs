use std::fmt;

use crate::compiler::intrinsics;
use crate::interner::{Symbol, ToSymbol};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Comment {
    SingleLine(String),
    MultiLine(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Op {
    Sum,     // +
    Minus,   // -
    Product, // *
    Divide,  // /

    Equal,        // ==
    NotEqual,     // !=
    LessThan,     // <
    LessEqual,    // <=
    GreaterThan,  // >
    GreaterEqual, // >=

    Modulo,   // %
    Exponent, // ^

    And, // &&
    Or,  // ||

    At, // @

    Pipe, // |>
    /// Dot operator, used for field access, which may be concatenated with left associativity
    Dot, // .
    Unknown(String),
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
impl Op {
    pub fn get_associated_fn_name(&self) -> Symbol {
        match self {
            Op::Sum => intrinsics::ADD,
            Op::Minus => intrinsics::SUB,
            Op::Product => intrinsics::MULT,
            Op::Divide => intrinsics::DIV,
            Op::Equal => intrinsics::EQ,
            Op::NotEqual => intrinsics::NE,
            Op::LessThan => intrinsics::LT,
            Op::LessEqual => intrinsics::LE,
            Op::GreaterThan => intrinsics::GT,
            Op::GreaterEqual => intrinsics::GE,
            Op::Modulo => intrinsics::MODULO,
            Op::Exponent => intrinsics::POW,
            Op::And => intrinsics::AND,
            Op::Or => intrinsics::OR,
            Op::At => "_mimium_schedule_at",
            Op::Pipe | Op::Dot => unreachable!(), // pipe is a syntax sugar, not a function
            Op::Unknown(x) => x.as_str(),
        }
        .to_symbol()
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Sum => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Product => write!(f, "*"),
            Op::Divide => write!(f, "/"),
            Op::Equal => write!(f, "=="),
            Op::NotEqual => write!(f, "!="),
            Op::LessThan => write!(f, "<"),
            Op::LessEqual => write!(f, "<="),
            Op::GreaterThan => write!(f, ">"),
            Op::GreaterEqual => write!(f, ">="),
            Op::Modulo => write!(f, "%"),
            Op::Exponent => write!(f, "^"),
            Op::And => write!(f, "&&"),
            Op::Or => write!(f, "||"),
            Op::At => write!(f, "@"),
            Op::Pipe => write!(f, "|>"),
            Op::Dot => write!(f, "."),
            Op::Unknown(x) => write!(f, "{}", x),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(x) => write!(f, "{}", x),
            Token::MacroExpand(x) => write!(f, "{}!", x),
            Token::FloatType => write!(f, "float"),
            Token::IntegerType => write!(f, "int"),
            Token::StringType => write!(f, "string"),
            Token::StructType => write!(f, "struct"),
            Token::Int(x) => write!(f, "{}", x),
            Token::Float(x) => write!(f, "{}", x),
            Token::Str(x) => write!(f, "\"{}\"", x),
            Token::Op(x) => write!(f, "{}", x),
            Token::SelfLit => write!(f, "self"),
            Token::Now => write!(f, "now"),
            Token::SampleRate => write!(f, "samplerate"),
            Token::Comma => write!(f, ","),
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

            // Token::Type => write!(f, "type"),
            // Token::Alias => write!(f, "newtype"),
            Token::Include => write!(f, "include"),
            Token::LineBreak => write!(f, "linebreak"),
            Token::Comment(_) => write!(f, "comment"),
        }
    }
}
