/// Token definition for parsing
/// Each token stores its kind, start position (byte offset), and length
use std::fmt;

/// Token kinds - types of tokens without embedded data
/// Literals store only position information, not the actual value
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Identifiers and literals
    Ident,          // Generic identifier
    IdentFunction,  // Function name in declaration
    IdentParameter, // Function parameter
    IdentVariable,  // Variable name (for future use)
    MacroExpand,

    // Type keywords
    FloatType,
    IntegerType,
    StringType,
    StructType,

    // Literals - value can be reconstructed from source text and position
    Float,
    Int,
    Str,

    // Operators
    OpSum,          // +
    OpMinus,        // -
    OpProduct,      // *
    OpDivide,       // /
    OpEqual,        // ==
    OpNotEqual,     // !=
    OpLessThan,     // <
    OpLessEqual,    // <=
    OpGreaterThan,  // >
    OpGreaterEqual, // >=
    OpModulo,       // %
    OpExponent,     // ^
    OpAt,           // @
    OpAnd,          // &&
    OpOr,           // ||
    OpPipe,         // |>
    OpUnknown,      // Other operators

    // Special literals
    SelfLit,
    Now,
    SampleRate,

    // Punctuation
    Comma,     // ,
    Dot,       // .
    DoubleDot, // ..
    Colon,     // :
    SemiColon, // ;

    // Keywords
    Let,
    LetRec,
    Assign, // =

    // Brackets
    ParenBegin,        // (
    ParenEnd,          // )
    ArrayBegin,        // [
    ArrayEnd,          // ]
    BlockBegin,        // {
    BlockEnd,          // }
    LambdaArgBeginEnd, // |
    BackQuote,         // `
    Dollar,            // $

    // Function and flow control
    Function,    // fn
    Macro,       // macro
    Arrow,       // ->
    LeftArrow,   // <-
    PlaceHolder, // _
    If,
    Else,

    // Directives
    Include,
    Sharp,    // #
    StageKwd, // stage
    Main,     // main

    // Trivia (whitespace and comments)
    LineBreak,
    Whitespace,
    SingleLineComment,
    MultiLineComment,

    // Special
    Error, // Error token for recovery
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::IdentFunction => write!(f, "identifier_function"),
            TokenKind::IdentParameter => write!(f, "identifier_parameter"),
            TokenKind::IdentVariable => write!(f, "identifier_variable"),
            TokenKind::MacroExpand => write!(f, "macro_expand"),
            TokenKind::FloatType => write!(f, "float"),
            TokenKind::IntegerType => write!(f, "int"),
            TokenKind::StringType => write!(f, "string"),
            TokenKind::StructType => write!(f, "struct"),
            TokenKind::Float => write!(f, "float_literal"),
            TokenKind::Int => write!(f, "int_literal"),
            TokenKind::Str => write!(f, "string_literal"),
            TokenKind::OpSum => write!(f, "+"),
            TokenKind::OpMinus => write!(f, "-"),
            TokenKind::OpProduct => write!(f, "*"),
            TokenKind::OpDivide => write!(f, "/"),
            TokenKind::OpEqual => write!(f, "=="),
            TokenKind::OpNotEqual => write!(f, "!="),
            TokenKind::OpLessThan => write!(f, "<"),
            TokenKind::OpLessEqual => write!(f, "<="),
            TokenKind::OpGreaterThan => write!(f, ">"),
            TokenKind::OpGreaterEqual => write!(f, ">="),
            TokenKind::OpModulo => write!(f, "%"),
            TokenKind::OpExponent => write!(f, "^"),
            TokenKind::OpAt => write!(f, "@"),
            TokenKind::OpAnd => write!(f, "&&"),
            TokenKind::OpOr => write!(f, "||"),
            TokenKind::OpPipe => write!(f, "|>"),
            TokenKind::OpUnknown => write!(f, "unknown_op"),
            TokenKind::SelfLit => write!(f, "self"),
            TokenKind::Now => write!(f, "now"),
            TokenKind::SampleRate => write!(f, "samplerate"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::DoubleDot => write!(f, ".."),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::SemiColon => write!(f, ";"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::LetRec => write!(f, "letrec"),
            TokenKind::Assign => write!(f, "="),
            TokenKind::ParenBegin => write!(f, "("),
            TokenKind::ParenEnd => write!(f, ")"),
            TokenKind::ArrayBegin => write!(f, "["),
            TokenKind::ArrayEnd => write!(f, "]"),
            TokenKind::BlockBegin => write!(f, "{{"),
            TokenKind::BlockEnd => write!(f, "}}"),
            TokenKind::LambdaArgBeginEnd => write!(f, "|"),
            TokenKind::Function => write!(f, "fn"),
            TokenKind::Macro => write!(f, "macro"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::LeftArrow => write!(f, "<-"),
            TokenKind::PlaceHolder => write!(f, "_"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Include => write!(f, "include"),
            TokenKind::LineBreak => write!(f, "linebreak"),
            TokenKind::Whitespace => write!(f, "whitespace"),
            TokenKind::SingleLineComment => write!(f, "single_line_comment"),
            TokenKind::MultiLineComment => write!(f, "multi_line_comment"),
            TokenKind::BackQuote => write!(f, "`"),
            TokenKind::Dollar => write!(f, "$"),
            TokenKind::StageKwd => write!(f, "stage"),
            TokenKind::Main => write!(f, "main"),
            TokenKind::Sharp => write!(f, "#"),
            TokenKind::Error => write!(f, "error"),
            TokenKind::Eof => write!(f, "eof"),
        }
    }
}

/// A token with position information
/// The actual value can be retrieved from the source text using start and length
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,  // byte offset in source
    pub length: usize, // length in bytes
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, length: usize) -> Self {
        Self {
            kind,
            start,
            length,
        }
    }

    /// Get the end position of this token
    pub fn end(&self) -> usize {
        self.start + self.length
    }

    /// Get the text of this token from the source
    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end()]
    }

    /// Check if this token is trivia (whitespace or comment)
    pub fn is_trivia(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::LineBreak
                | TokenKind::Whitespace
                | TokenKind::SingleLineComment
                | TokenKind::MultiLineComment
        )
    }

    /// Check if this token is an error token
    pub fn is_error(&self) -> bool {
        self.kind == TokenKind::Error
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}@{}:{}", self.kind, self.start, self.length)
    }
}
