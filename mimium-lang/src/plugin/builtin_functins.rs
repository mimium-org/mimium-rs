use crate::{
    interner::ToSymbol,
    mir::print,
    plugin::{CommonFunction, InstantPlugin, MacroInfo, Plugin},
};
mod lift_f {
    use super::*;
    use crate::ast::{Expr, Literal};
    use crate::code;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::{
        function, numeric,
        types::{PType, Type},
    };

    fn machine_function(
        machine: &mut crate::runtime::vm::Machine,
    ) -> crate::runtime::vm::ReturnCode {
        let v = crate::runtime::vm::Machine::get_as::<f64>(machine.get_stack(0));
        let res = crate::runtime::vm::Machine::to_value(v);
        machine.set_stack(0, res);
        1
    }

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        let v = &args[0].0;
        match v {
            Value::Number(lhs) => Value::Code(
                Expr::Literal(Literal::Float(lhs.to_string().to_symbol())).into_id_without_span(),
            ),
            _ => panic!("Invalid argument types for function lift_f"),
        }
    }

    pub(super) fn signature() -> MacroInfo {
        fn fun_name(args: &[(Value, TypeNodeId)]) -> Value {
            macro_function(args)
        }
        MacroInfo {
            name: "lift_f".to_symbol(),
            ty: function!(vec![numeric!()], code!(numeric!())),
            fun: std::rc::Rc::new(std::cell::RefCell::new(fun_name)),
        }
    }
}
mod length_array {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::CommonFunction;
    use crate::{
        function, numeric,
        types::{PType, Type},
    };

    fn machine_function(
        machine: &mut crate::runtime::vm::Machine,
    ) -> crate::runtime::vm::ReturnCode {
        let arr = machine.get_stack(0);
        let array = machine.arrays.get_array(arr);
        let res = array.get_length_array() as f64;
        machine.set_stack(0, crate::runtime::vm::Machine::to_value(res));
        1
    }

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        let arr = &args[0].0;
        match arr {
            Value::Array(array) => {
                let res = array.len() as f64;
                Value::Number(res)
            }
            _ => panic!("Invalid argument types for function get_length_array"),
        }
    }

    pub(super) fn signature() -> CommonFunction {
        CommonFunction {
            name: "length_array".to_symbol(),
            ty: function!(
                vec![Type::Array(Type::TypeScheme(u64::MAX).into_id()).into_id()],
                numeric!()
            ),
            macro_fun: macro_function,
            fun: machine_function,
        }
    }
}
macro_rules! declare_f1f_common {
    ($name:ident, $f:expr) => {
        mod $name {

            use super::*;
            use crate::interpreter::Value;
            use crate::plugin::CommonFunction;
            use crate::{
                function, numeric,
                types::{PType, Type},
            };
            fn machine_function(
                machine: &mut crate::runtime::vm::Machine,
            ) -> crate::runtime::vm::ReturnCode {
                let v = crate::runtime::vm::Machine::get_as::<f64>(machine.get_stack(0));
                let res = crate::runtime::vm::Machine::to_value($f(v));
                machine.set_stack(0, crate::runtime::vm::Machine::to_value(res));
                1
            }
            fn macro_function(
                args: &[(crate::interpreter::Value, crate::interner::TypeNodeId)],
            ) -> Value {
                assert_eq!(args.len(), 1);
                let v = &args[0].0;
                match v {
                    Value::Number(lhs) => {
                        let res = $f(*lhs);
                        Value::Number(res)
                    }
                    _ => panic!("Invalid argument types for function {}", stringify!($name)),
                }
            }
            pub(super) fn signature() -> CommonFunction {
                CommonFunction {
                    name: stringify!($name).to_symbol(),
                    ty: function!(vec![numeric!()], numeric!()),
                    macro_fun: macro_function,
                    fun: machine_function,
                }
            }
        }
    };
}
macro_rules! declare_f2f_common {
    ($name:ident, $f:expr) => {
        mod $name {

            use super::*;
            use crate::interpreter::Value;
            use crate::plugin::CommonFunction;
            use crate::{
                function, numeric,
                types::{PType, Type},
            };
            fn machine_function(
                machine: &mut crate::runtime::vm::Machine,
            ) -> crate::runtime::vm::ReturnCode {
                let lhs = crate::runtime::vm::Machine::get_as::<f64>(machine.get_stack(0));
                let rhs = crate::runtime::vm::Machine::get_as::<f64>(machine.get_stack(1));
                let res = crate::runtime::vm::Machine::to_value($f(lhs, rhs));
                machine.set_stack(0, crate::runtime::vm::Machine::to_value(res));
                1
            }
            fn macro_function(
                args: &[(crate::interpreter::Value, crate::interner::TypeNodeId)],
            ) -> Value {
                assert_eq!(args.len(), 2);
                let lhs = &args[0].0;
                let rhs = &args[1].0;
                match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => {
                        let res = $f(*lhs, *rhs);
                        Value::Number(res)
                    }
                    _ => panic!("Invalid argument types for function {}", stringify!($name)),
                }
            }
            pub(super) fn signature() -> CommonFunction {
                CommonFunction {
                    name: stringify!($name).to_symbol(),
                    ty: function!(vec![numeric!(), numeric!()], numeric!()),
                    macro_fun: macro_function,
                    fun: machine_function,
                }
            }
        }
    };
}

declare_f1f_common!(neg, |x: f64| -x);
declare_f1f_common!(abs, |x: f64| x.abs());
declare_f1f_common!(sqrt, |x: f64| x.sqrt());
declare_f1f_common!(round, |x: f64| x.round());
declare_f1f_common!(floor, |x: f64| x.floor());
declare_f1f_common!(ceil, |x: f64| x.ceil());
//these basic arithmetics are not actually used in the vm level because the primitive operations cover them.
// but we still define them for the macro expansion.
declare_f2f_common!(add, |a: f64, b: f64| a + b);
declare_f2f_common!(sub, |a: f64, b: f64| a - b);
declare_f2f_common!(mult, |a: f64, b: f64| a * b);
declare_f2f_common!(div, |a: f64, b: f64| a / b);
declare_f2f_common!(modulo, |a: f64, b: f64| a % b);
declare_f2f_common!(eq, |a: f64, b: f64| if a == b { 1.0 } else { 0.0 });
declare_f2f_common!(ne, |a: f64, b: f64| if a != b { 1.0 } else { 0.0 });
declare_f2f_common!(lt, |a: f64, b: f64| if a < b { 1.0 } else { 0.0 });
declare_f2f_common!(le, |a: f64, b: f64| if a <= b { 1.0 } else { 0.0 });
declare_f2f_common!(gt, |a: f64, b: f64| if a > b { 1.0 } else { 0.0 });
declare_f2f_common!(ge, |a: f64, b: f64| if a >= b { 1.0 } else { 0.0 });
declare_f1f_common!(not, |x: f64| if x == 0.0 { 1.0 } else { 0.0 });
declare_f1f_common!(sin, |x: f64| x.sin());
declare_f1f_common!(cos, |x: f64| x.cos());
declare_f1f_common!(tan, |x: f64| x.tan());
declare_f1f_common!(asin, |x: f64| x.asin());
declare_f1f_common!(acos, |x: f64| x.acos());
declare_f1f_common!(atan, |x: f64| x.atan());
declare_f2f_common!(atan2, |y: f64, x: f64| y.atan2(x));
declare_f2f_common!(pow, |base: f64, exp: f64| base.powf(exp));
declare_f2f_common!(min, f64::min);
declare_f2f_common!(max, f64::max);
declare_f1f_common!(probe, |x: f64| {
    print!("{x}");
    x
});
declare_f1f_common!(probeln, |x: f64| {
    println!("{x}");
    x
});

/// Main function to expose the definitions of built-in functions.
fn generate_builtin_functions() -> impl ExactSizeIterator<Item = CommonFunction> {
    [
        min::signature(),
        max::signature(),
        neg::signature(),
        add::signature(),
        sub::signature(),
        mult::signature(),
        div::signature(),
        abs::signature(),
        modulo::signature(),
        sqrt::signature(),
        round::signature(),
        floor::signature(),
        ceil::signature(),
        not::signature(),
        eq::signature(),
        ne::signature(),
        lt::signature(),
        le::signature(),
        gt::signature(),
        ge::signature(),
        sin::signature(),
        cos::signature(),
        tan::signature(),
        asin::signature(),
        acos::signature(),
        atan::signature(),
        atan2::signature(),
        pow::signature(),
        length_array::signature(),
        probe::signature(),
        probeln::signature(),
    ]
    .into_iter()
}
fn generate_default_macros() -> impl ExactSizeIterator<Item = MacroInfo> {
    vec![lift_f::signature()].into_iter()
}
pub fn get_builtin_fns_as_plugins() -> Box<dyn Plugin> {
    let commonfns = generate_builtin_functions().collect();
    let macros = generate_default_macros().collect();

    Box::new(InstantPlugin {
        macros,
        extcls: vec![],
        commonfns,
    })
}
