use std::fmt;

use crate::compiler::intrinsics;
use crate::interner::{Symbol, ToSymbol};

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

    Unknown(String),
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
            Op::Pipe => unreachable!(), // pipe is a syntax sugar, not a function
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
            Op::Unknown(x) => write!(f, "{x}"),
        }
    }
}
