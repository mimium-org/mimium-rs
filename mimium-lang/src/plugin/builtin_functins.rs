use crate::interner::ToSymbol;
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
            fn signature() -> CommonFunction {
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
            fn signature() -> CommonFunction {
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

declare_f2f_common!(min, f64::min);
