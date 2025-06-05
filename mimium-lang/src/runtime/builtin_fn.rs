//! Built-in numeric functions used by the interpreter and standard library.
//!
//! These are mostly thin wrappers around Rust arithmetic which are exposed to
//! mimium code when no external implementation is provided.

use crate::{function, integer, interner::TypeNodeId, numeric, types::*, unit};

fn b_to_f(b: bool) -> f64 {
    if b {
        1.
    } else {
        0.
    }
}
fn b_to_i(b: bool) -> i64 {
    if b {
        1
    } else {
        0
    }
}

mod intrinsic {
    pub mod integer {
        use super::super::b_to_i;
        pub fn neg(x: i64) -> i64 {
            -x
        }
        pub fn not(x: i64) -> i64 {
            if x <= 0 {
                1
            } else {
                0
            }
        }
        pub fn abs(x: i64) -> i64 {
            x.abs()
        }
        pub fn add(x: i64, y: i64) -> i64 {
            x + y
        }
        pub fn sub(x: i64, y: i64) -> i64 {
            x - y
        }
        pub fn mult(x: i64, y: i64) -> i64 {
            x * y
        }
        pub fn div(x: i64, y: i64) -> i64 {
            x / y
        }
        pub fn modulo(x: i64, y: i64) -> i64 {
            x % y
        }
        pub fn eq(x: i64, y: i64) -> i64 {
            b_to_i(x == y)
        }
        pub fn ne(x: i64, y: i64) -> i64 {
            b_to_i(x != y)
        }
        pub fn le(x: i64, y: i64) -> i64 {
            b_to_i(x <= y)
        }
        pub fn lt(x: i64, y: i64) -> i64 {
            b_to_i(x < y)
        }
        pub fn ge(x: i64, y: i64) -> i64 {
            b_to_i(x >= y)
        }
        pub fn gt(x: i64, y: i64) -> i64 {
            b_to_i(x > y)
        }
        pub fn pow(x: i64, y: i64) -> i64 {
            x.pow(y as u32)
        }
        pub fn max(x: i64, y: i64) -> i64 {
            x.max(y)
        }
        pub fn min(x: i64, y: i64) -> i64 {
            x.min(y)
        }
        pub fn print(x: i64) {
            print!("{x}");
        }
        pub fn println(x: i64) {
            println!("{x}");
        }
    }
    pub mod numeric {
        use super::super::b_to_f;
        pub fn sin(x: f64) -> f64 {
            x.sin()
        }
        pub fn cos(x: f64) -> f64 {
            x.cos()
        }
        pub fn neg(x: f64) -> f64 {
            -x
        }
        pub fn not(x: f64) -> f64 {
            if x <= 0.0 {
                1.0
            } else {
                0.0
            }
        }
        pub fn round(x: f64) -> f64 {
            x.round()
        }
        pub fn floor(x: f64) -> f64 {
            x.floor()
        }
        pub fn ceil(x: f64) -> f64 {
            x.ceil()
        }
        pub fn atan(x: f64) -> f64 {
            x.atan()
        }
        pub fn sqrt(x: f64) -> f64 {
            x.sqrt()
        }
        pub fn abs(x: f64) -> f64 {
            x.abs()
        }
        pub fn print(x: f64) {
            print!("{x}");
        }
        pub fn probe(x: f64) -> f64 {
            print!("{x}");
            x
        }
        pub fn println(x: f64) {
            println!("{x}");
        }
        pub fn probeln(x: f64) -> f64 {
            println!("{x}");
            x
        }
        pub fn add(x: f64, y: f64) -> f64 {
            x + y
        }
        pub fn sub(x: f64, y: f64) -> f64 {
            x - y
        }
        pub fn mult(x: f64, y: f64) -> f64 {
            x * y
        }
        pub fn div(x: f64, y: f64) -> f64 {
            x / y
        }
        pub fn modulo(x: f64, y: f64) -> f64 {
            x % y
        }
        pub fn eq(x: f64, y: f64) -> f64 {
            b_to_f(x == y)
        }
        pub fn ne(x: f64, y: f64) -> f64 {
            b_to_f(x != y)
        }
        pub fn le(x: f64, y: f64) -> f64 {
            b_to_f(x < y)
        }
        pub fn lt(x: f64, y: f64) -> f64 {
            b_to_f(x <= y)
        }
        pub fn ge(x: f64, y: f64) -> f64 {
            b_to_f(x > y)
        }
        pub fn gt(x: f64, y: f64) -> f64 {
            b_to_f(x >= y)
        }
        pub fn pow(x: f64, y: f64) -> f64 {
            x.powf(y)
        }
        pub fn log(x: f64, y: f64) -> f64 {
            x.log(y)
        }
        pub fn atan2(x: f64, y: f64) -> f64 {
            x.atan2(y)
        }
        pub fn max(x: f64, y: f64) -> f64 {
            x.max(y)
        }
        pub fn min(x: f64, y: f64) -> f64 {
            x.min(y)
        }
    }
}

type BuiltinFn = (&'static str, TypeNodeId, *const ());

macro_rules! i_i {
    ($name:ident) => {
        (
            stringify!($name),
            function!(vec![integer!()], integer!()),
            intrinsic::integer::$name as *const (),
        )
    };
}
macro_rules! i2_i {
    ($name:ident) => {
        (
            stringify!($name),
            function!(vec![integer!(), integer!()], integer!()),
            intrinsic::integer::$name as *const (),
        )
    };
}

macro_rules! f_f {
    ($name:ident) => {
        (
            stringify!($name),
            function!(vec![numeric!()], numeric!()),
            intrinsic::numeric::$name as *const (),
        )
    };
}
macro_rules! f2toftype {
    () => {
        function!(vec![numeric!(), numeric!()], numeric!())
    };
}
macro_rules! f2_f {
    ($name:ident) => {
        (
            stringify!($name),
            f2toftype!(),
            intrinsic::numeric::$name as *const (),
        )
    };
}

pub fn get_builtin_fns() -> [BuiltinFn; 49] {
    [
        i_i!(neg),
        i_i!(not),
        i_i!(abs),
        i2_i!(add),
        i2_i!(sub),
        i2_i!(mult),
        i2_i!(div),
        i2_i!(modulo),
        i2_i!(eq),
        i2_i!(ne),
        i2_i!(le),
        i2_i!(lt),
        i2_i!(ge),
        i2_i!(gt),
        i2_i!(pow),
        i2_i!(max),
        i2_i!(min),
        (
            "print",
            function!(vec![integer!()], unit!()),
            intrinsic::integer::print as *const (),
        ),
        (
            "println",
            function!(vec![integer!()], unit!()),
            intrinsic::integer::println as *const (),
        ),
        f_f!(sin),
        f_f!(cos),
        f_f!(neg),
        f_f!(not),
        f_f!(round),
        f_f!(floor),
        f_f!(ceil),
        f_f!(atan),
        f_f!(sqrt),
        f_f!(abs),
        f2_f!(add),
        f2_f!(sub),
        f2_f!(mult),
        f2_f!(div),
        f2_f!(modulo),
        f2_f!(eq),
        f2_f!(ne),
        f2_f!(le),
        f2_f!(lt),
        f2_f!(ge),
        f2_f!(gt),
        f2_f!(pow),
        f2_f!(log),
        f2_f!(atan2),
        f2_f!(max),
        f2_f!(min),
        (
            "print",
            function!(vec![numeric!()], unit!()),
            intrinsic::numeric::print as *const (),
        ),
        (
            "println",
            function!(vec![numeric!()], unit!()),
            intrinsic::numeric::println as *const (),
        ),
        (
            "probe",
            function!(vec![numeric!()], numeric!()),
            intrinsic::numeric::probe as *const (),
        ),
        (
            "probeln",
            function!(vec![numeric!()], numeric!()),
            intrinsic::numeric::probeln as *const (),
        ),
    ]
}

pub fn eval_float1(name: &str, x: f64) -> Option<f64> {
    match name {
        "sin" => Some(x.sin()),
        "cos" => Some(x.cos()),
        "neg" => Some(-x),
        "not" => Some(if x == 0.0 { 1.0 } else { 0.0 }),
        "round" => Some(x.round()),
        "floor" => Some(x.floor()),
        "ceil" => Some(x.ceil()),
        "atan" => Some(x.atan()),
        "sqrt" => Some(x.sqrt()),
        "abs" => Some(x.abs()),
        _ => None,
    }
}
pub fn eval_float2(name: &str, x: f64, y: f64) -> Option<f64> {
    let b_to_f = move |b| if b { 1. } else { 0. };
    match name {
        "add" => Some(x + y),
        "sub" => Some(x - y),
        "mult" => Some(x * y),
        "div" => Some(x / y),
        "mod" => Some(x % y),
        "eq" => Some(b_to_f(x == y)),
        "ne" => Some(b_to_f(x != y)),
        "le" => Some(b_to_f(x <= y)),
        "lt" => Some(b_to_f(x < y)),
        "ge" => Some(b_to_f(x >= y)),
        "gt" => Some(b_to_f(x > y)),
        "pow" => Some(x.powf(y)),
        "log" => Some(x.log(y)),
        "atan2" => Some(x.atan2(y)),
        "max" => Some(x.max(y)),
        "min" => Some(x.min(y)),
        _ => None,
    }
}

pub fn eval_int1(name: &str, x: i64) -> Option<i64> {
    match name {
        "neg" => Some(-x),
        "not" => Some(if x == 0 { 1 } else { 0 }),
        "abs" => Some(x.abs()),
        _ => None,
    }
}

pub fn eval_int2(name: &str, x: i64, y: i64) -> Option<i64> {
    let b_to_i = move |b| if b { 1 } else { 0 };
    match name {
        "add" => Some(x + y),
        "sub" => Some(x - y),
        "mult" => Some(x * y),
        "div" => Some(x / y),
        "mod" => Some(x % y),
        "eq" => Some(b_to_i(x == y)),
        "ne" => Some(b_to_i(x != y)),
        "le" => Some(b_to_i(x <= y)),
        "lt" => Some(b_to_i(x < y)),
        "ge" => Some(b_to_i(x >= y)),
        "gt" => Some(b_to_i(x > y)),
        "pow" => Some(x.pow(y as u32)),
        "max" => Some(x.max(y)),
        "min" => Some(x.min(y)),
        _ => None,
    }
}
