use crate::plugin::ExtFunTypeInfo;
use crate::interner::ToSymbol;
use crate::runtime::vm::ArrayIdx;
use crate::types::{PType, Type};
use crate::{code, function, numeric};

use super::{ExtFunInfo, Machine, ReturnCode};

fn probef(machine: &mut Machine) -> ReturnCode {
    let rv = machine.get_stack(0);
    let i = super::Machine::get_as::<f64>(rv);
    print!("{i}");
    machine.set_stack(0, rv);
    1
}

fn probelnf(machine: &mut Machine) -> ReturnCode {
    let rv = machine.get_stack(0);
    let i = super::Machine::get_as::<f64>(rv);
    println!("{} ", i);
    machine.set_stack(0, rv);
    1
}
fn min(machine: &mut Machine) -> ReturnCode {
    let lhs = super::Machine::get_as::<f64>(machine.get_stack(0));
    let rhs = super::Machine::get_as::<f64>(machine.get_stack(1));
    let res = lhs.min(rhs);
    machine.set_stack(0, super::Machine::to_value(res));
    1
}
fn max(machine: &mut Machine) -> ReturnCode {
    let lhs = super::Machine::get_as::<f64>(machine.get_stack(0));
    let rhs = super::Machine::get_as::<f64>(machine.get_stack(1));
    let res = lhs.max(rhs);
    machine.set_stack(0, super::Machine::to_value(res));
    1
}
// the special generic function to get the length of an array
fn get_length_array(machine: &mut Machine) -> ReturnCode {
    let arr = machine.get_stack(0);
    let array = unsafe {
        machine
            .arrays
            .data
            .get_unchecked(super::Machine::get_as::<ArrayIdx>(arr))
    };
    let res = array.get_length_array() as f64;
    machine.set_stack(0, super::Machine::to_value(res));
    1
}
fn lift_float_dummy(_machine: &mut Machine) -> ReturnCode {
    unreachable!("lift_float_dummy should not be called at VM runtime");
    1
}
pub fn get_builtin_fns() -> [ExtFunInfo; 6] {
    [
        ExtFunInfo::new(
            "probe".to_symbol(),
            function!(vec![numeric!()], numeric!()),
            probef,
        ),
        ExtFunInfo::new(
            "probeln".to_symbol(),
            function!(vec![numeric!()], numeric!()),
            probelnf,
        ),
        ExtFunInfo::new(
            "min".to_symbol(),
            function!(vec![numeric!(), numeric!()], numeric!()),
            min,
        ),
        ExtFunInfo::new(
            "max".to_symbol(),
            function!(vec![numeric!(), numeric!()], numeric!()),
            max,
        ),
        //The function is generic but we don't know appropriate typescheme id, so we use u64::MAX for the workaround
        ExtFunInfo::new(
            "length_array".to_symbol(),
            function!(
                vec![Type::Array(Type::TypeScheme(u64::MAX).into_id()).into_id()],
                numeric!()
            ),
            get_length_array,
        ),
        ExtFunInfo::new(
            "lift_f".to_symbol(),
            function!(vec![numeric!()], code!(numeric!())),
            lift_float_dummy,
        ),
    ]
}

pub fn get_builtin_fn_types() -> Vec<ExtFunTypeInfo> {
    get_builtin_fns()
        .into_iter()
        .map(ExtFunTypeInfo::from)
        .collect()
}
