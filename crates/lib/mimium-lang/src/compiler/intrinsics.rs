use std::cell::LazyCell;

use crate::interner::{Symbol, ToSymbol};

// unary
pub(crate) const NEG: &str = "neg";
pub(crate) const TOFLOAT: &str = "tofloat";

// binary
pub(crate) const ADD: &str = "add";
pub(crate) const SUB: &str = "sub";
pub(crate) const MULT: &str = "mult";
pub(crate) const DIV: &str = "div";
pub(crate) const EQ: &str = "eq";
pub(crate) const NE: &str = "ne";
pub(crate) const LE: &str = "le";
pub(crate) const LT: &str = "lt";
pub(crate) const GE: &str = "ge";
pub(crate) const GT: &str = "gt";
pub(crate) const MODULO: &str = "modulo";
pub(crate) const POW: &str = "pow";
pub(crate) const AND: &str = "and";
pub(crate) const OR: &str = "or";

// arithmetics
pub(crate) const SIN: &str = "sin";
pub(crate) const COS: &str = "cos";
pub(crate) const TAN: &str = "tan";
pub(crate) const ATAN: &str = "atan";
pub(crate) const ATAN2: &str = "atan2";
pub(crate) const SQRT: &str = "sqrt";
pub(crate) const ABS: &str = "abs";
pub(crate) const LOG: &str = "log";
pub(crate) const MIN: &str = "min";
pub(crate) const MAX: &str = "max";
pub(crate) const CEIL: &str = "ceil";
pub(crate) const FLOOR: &str = "floor";
pub(crate) const ROUND: &str = "round";

// other operations
pub(crate) const DELAY: &str = "delay";
pub(crate) const MEM: &str = "mem";
const BUILTIN_SYMS_UNSORTED: [&str; 31] = [
    NEG, TOFLOAT, ADD, SUB, MULT, DIV, EQ, NE, LE, LT, GE, GT, MODULO, POW, AND, OR, SIN, COS, TAN,
    ATAN, ATAN2, SQRT, ABS, LOG, MIN, MAX, CEIL, FLOOR, ROUND, DELAY, MEM,
];
thread_local!(pub (crate) static BUILTIN_SYMS: LazyCell<Vec<Symbol>> = LazyCell::new(|| {
    let mut v = BUILTIN_SYMS_UNSORTED
        .iter()
        .map(|s| s.to_symbol())
        .collect::<Vec<_>>();
    v.sort();
    v
}));
