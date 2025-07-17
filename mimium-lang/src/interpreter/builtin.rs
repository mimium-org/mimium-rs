use crate::ast::Expr;
use crate::ast::Literal;
use crate::interner::ToSymbol;

use super::ExtFunction;
use super::Value;

fn new_fn_f1_unit(name: &'static str, f: impl Fn(f64) + 'static) -> ExtFunction {
    ExtFunction::new(name.to_symbol(), move |args: Vec<Value>| {
        let v = args[0].clone();
        match v {
            Value::Number(v) => {
                f(v);
                Value::Unit
            }
            _ => panic!("{} requires a numeric argument", name),
        }
    })
}
fn new_fn_f1_f(name: &'static str, f: impl Fn(f64) -> f64 + 'static) -> ExtFunction {
    ExtFunction::new(name.to_symbol(), move |args: Vec<Value>| {
        let v = args[0].clone();
        match v {
            Value::Number(v) => Value::Number(f(v)),
            _ => panic!("{} requires a numeric argument", name),
        }
    })
}
fn new_fn_f2_f(name: &'static str, f: impl Fn(f64, f64) -> f64 + 'static) -> ExtFunction {
    ExtFunction::new(name.to_symbol(), move |args: Vec<Value>| {
        match args.as_slice() {
            [Value::Number(v1), Value::Number(v2)] => Value::Number(f(*v1, *v2)),
            _ => panic!("{name} requires two numeric arguments"),
        }
    })
}
fn lift_float() -> ExtFunction {
    ExtFunction::new("lift_f".to_symbol(), |args: Vec<Value>| {
        match args.as_slice() {
            [Value::Number(v)] => Value::Code(
                Expr::Literal(Literal::Float(v.to_string().to_symbol())).into_id_without_span(),
            ),
            _ => panic!("lift_f requires a 1numeric argument"),
        }
    })
}
pub(super) fn gen_default_fns() -> Vec<ExtFunction> {
    vec![
        new_fn_f1_f("neg", |v| -v),
        new_fn_f2_f("add", |a, b| a + b),
        new_fn_f2_f("sub", |a, b| a - b),
        new_fn_f2_f("mult", |a, b| a * b),
        new_fn_f2_f("div", |a, b| a / b),
        //not,abs,modulo,sqrt,round,floor,ceil
        new_fn_f1_f("abs", |v| v.abs()),
        new_fn_f2_f("modulo", |a, b| a % b),
        new_fn_f1_f("sqrt", |v| v.sqrt()),
        new_fn_f1_f("round", |v| v.round()),
        new_fn_f1_f("floor", |v| v.floor()),
        new_fn_f1_f("ceil", |v| v.ceil()),
        //boolean(not,eq,ne,lt,le,gt,ge)
        new_fn_f1_f("not", |v| if v == 0.0 { 1.0 } else { 0.0 }),
        new_fn_f2_f("eq", |a, b| if a == b { 1.0 } else { 0.0 }),
        new_fn_f2_f("ne", |a, b| if a != b { 1.0 } else { 0.0 }),
        new_fn_f2_f("lt", |a, b| if a < b { 1.0 } else { 0.0 }),
        new_fn_f2_f("le", |a, b| if a <= b { 1.0 } else { 0.0 }),
        new_fn_f2_f("gt", |a, b| if a > b { 1.0 } else { 0.0 }),
        new_fn_f2_f("ge", |a, b| if a >= b { 1.0 } else { 0.0 }),
        //sinusoidal
        new_fn_f1_f("sin", |v| v.sin()),
        new_fn_f1_f("cos", |v| v.cos()),
        new_fn_f1_f("tan", |v| v.tan()),
        new_fn_f1_f("asin", |v| v.asin()),
        new_fn_f1_f("acos", |v| v.acos()),
        new_fn_f1_f("atan", |v| v.atan()),
        new_fn_f2_f("atan2", |a, b| a.atan2(b)),
        //pow,max,min
        new_fn_f2_f("pow", |a, b| a.powf(b)),
        new_fn_f2_f("max", |a, b| a.max(b)),
        new_fn_f2_f("min", |a, b| a.min(b)),
        //print,probe
        new_fn_f1_unit("print", |f| {
            print!("{}", f);
        }),
        new_fn_f1_unit("println", |f| {
            println!("{}", f);
        }),
        new_fn_f1_f("probe", |f| {
            print!("{f}");
            f
        }),
        new_fn_f1_f("probeln", |f| {
            println!("{f}");
            f
        }),
        lift_float(),
    ]
}
