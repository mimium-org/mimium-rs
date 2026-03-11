use crate::{
    interner::{Symbol, ToSymbol, TypeNodeId},
    plugin::{CommonFunction, ExtClsInfo, InstantPlugin, MacroInfo, Plugin},
};
use std::{cell::RefCell, rc::Rc};

fn lift_value_to_code(value: crate::interpreter::Value) -> crate::interpreter::Value {
    match value.try_into() {
        Ok(expr) => crate::interpreter::Value::Code(expr),
        Err(err) => panic!("Invalid argument for lift: cannot convert value to code: {err:?}"),
    }
}
// Polymorphic version of lift, which can lift any value to code.
mod lift {
    use super::*;
    use crate::code;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::{
        function,
        types::{Type, TypeSchemeId},
    };
    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        super::lift_value_to_code(args[0].0.clone())
    }

    pub(super) fn signature() -> MacroInfo {
        let t = Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id();
        MacroInfo {
            name: "lift".to_symbol(),
            ty: function!(vec![t], code!(t)),
            fun: std::rc::Rc::new(std::cell::RefCell::new(macro_function)),
        }
    }
}

mod lift_f {
    use super::*;
    use crate::code;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::{
        function, numeric,
        types::{PType, Type},
    };

    #[deprecated(
        since = "4.0.0",
        note = "lift_f will be replaced by polymorphic lift function."
    )]
    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        super::lift_value_to_code(args[0].0.clone())
    }

    pub(super) fn signature() -> MacroInfo {
        MacroInfo {
            name: "lift_f".to_symbol(),
            ty: function!(vec![numeric!()], code!(numeric!())),
            fun: std::rc::Rc::new(std::cell::RefCell::new(macro_function)),
        }
    }
}

mod lift_arrayf {
    use super::*;
    use crate::code;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;

    use crate::{
        function, numeric,
        types::{PType, Type},
    };

    #[deprecated(
        since = "4.0.0",
        note = "lift_arrayf will be replaced by polymorphic lift function."
    )]
    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        super::lift_value_to_code(args[0].0.clone())
    }

    pub(super) fn signature() -> MacroInfo {
        let array_type = Type::Array(numeric!()).into_id();
        MacroInfo {
            name: "lift_arrayf".to_symbol(),
            ty: function!(vec![array_type], code!(array_type)),
            fun: std::rc::Rc::new(std::cell::RefCell::new(macro_function)),
        }
    }
}

mod lift_array_code {
    use super::*;
    use crate::code;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::{
        function,
        types::{Type, TypeSchemeId},
    };

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        super::lift_value_to_code(args[0].0.clone())
    }

    pub(super) fn signature() -> MacroInfo {
        let t = Type::TypeScheme(TypeSchemeId(u64::MAX - 2)).into_id();
        let array_type = Type::Array(code!(t)).into_id();
        MacroInfo {
            name: "lift_array_code".to_symbol(),
            ty: function!(vec![array_type], code!(Type::Array(t).into_id())),
            fun: std::rc::Rc::new(std::cell::RefCell::new(macro_function)),
        }
    }
}

mod length_array {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::CommonFunction;
    use crate::types::TypeSchemeId;
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
                vec![Type::Array(Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id()).into_id()],
                numeric!()
            ),
            macro_fun: macro_function,
            fun: machine_function,
        }
    }
}

mod split_tail {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::CommonFunction;
    use crate::types::TypeSchemeId;
    use crate::{function, types::Type};

    fn machine_function(
        machine: &mut crate::runtime::vm::Machine,
    ) -> crate::runtime::vm::ReturnCode {
        let arr_idx = machine.get_stack(0);
        let array = machine.arrays.get_array(arr_idx);
        let len = array.get_length_array();

        if len == 0 {
            panic!("Cannot split_tail on empty array");
        }

        let elem_size = array.get_elem_word_size();

        // Get the last element (tail) before we create new arrays
        let tail_offset = ((len - 1) * elem_size) as usize;
        let tail_words = array.get_data()[tail_offset..tail_offset + elem_size as usize].to_vec();

        // Create new array for rest (all elements except last)
        let rest_len = len - 1;
        let rest_arr_idx = machine.arrays.alloc_array(rest_len, elem_size);

        // Copy all elements except the last one
        // Need to do this in two steps to avoid borrow conflicts
        let copy_len = (rest_len * elem_size) as usize;
        let src_slice: Vec<u64> = machine.arrays.get_array(arr_idx).get_data()[..copy_len].to_vec();
        let rest_data = machine.arrays.get_array_mut(rest_arr_idx).get_data_mut();
        rest_data[..copy_len].copy_from_slice(&src_slice);

        // Allocate tuple on stack to return (rest_array, tail)
        // Stack layout: [rest_array_idx, tail_words...]
        machine.set_stack(0, rest_arr_idx);
        tail_words
            .iter()
            .enumerate()
            .for_each(|(i, word)| machine.set_stack((i + 1) as i64, *word));

        1 + elem_size as i64
    }

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        let arr = &args[0].0;
        match arr {
            Value::Array(array) => {
                let len = array.len();
                if len == 0 {
                    panic!("Cannot split_tail on empty array");
                }

                // Split into rest (all but last) and tail (last element)
                let rest: Vec<Value> = array[..len - 1].to_vec();
                let tail = array[len - 1].clone();

                // Return as tuple (rest, tail)
                Value::Tuple(vec![Value::Array(rest), tail])
            }
            _ => panic!("Invalid argument types for function split_tail"),
        }
    }

    pub(super) fn signature() -> CommonFunction {
        let array_type = Type::Array(Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id()).into_id();
        let elem_type = Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id();
        CommonFunction {
            name: "split_tail".to_symbol(),
            ty: function!(
                vec![array_type],
                Type::Tuple(vec![array_type, elem_type]).into_id()
            ),
            macro_fun: macro_function,
            fun: machine_function,
        }
    }
}

mod split_head {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::CommonFunction;
    use crate::types::TypeSchemeId;
    use crate::{function, types::Type};

    fn machine_function(
        machine: &mut crate::runtime::vm::Machine,
    ) -> crate::runtime::vm::ReturnCode {
        let arr_idx = machine.get_stack(0);
        let array = machine.arrays.get_array(arr_idx);
        let len = array.get_length_array();

        if len == 0 {
            panic!("Cannot split_head on empty array");
        }

        let elem_size = array.get_elem_word_size();

        // Get the first element (head)
        let head_words = array.get_data()[0..elem_size as usize].to_vec();

        // Create new array for rest (all elements except first)
        let rest_len = len - 1;
        let rest_arr_idx = machine.arrays.alloc_array(rest_len, elem_size);

        // Copy all elements except the first one
        // Need to do this in two steps to avoid borrow conflicts
        let copy_len = (rest_len * elem_size) as usize;
        let start_offset = elem_size as usize;
        let src_slice: Vec<u64> = machine.arrays.get_array(arr_idx).get_data()
            [start_offset..start_offset + copy_len]
            .to_vec();
        let rest_data = machine.arrays.get_array_mut(rest_arr_idx).get_data_mut();
        rest_data[..copy_len].copy_from_slice(&src_slice);

        // Allocate tuple on stack to return (head, rest_array)
        // Stack layout: [head_words..., rest_array_idx]
        head_words
            .iter()
            .enumerate()
            .for_each(|(i, word)| machine.set_stack(i as i64, *word));
        machine.set_stack(elem_size as i64, rest_arr_idx);

        elem_size as i64 + 1
    }

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        let arr = &args[0].0;
        match arr {
            Value::Array(array) => {
                let len = array.len();
                if len == 0 {
                    panic!("Cannot split_head on empty array");
                }

                // Split into head (first element) and rest (all but first)
                let head = array[0].clone();
                let rest: Vec<Value> = array[1..].to_vec();

                // Return as tuple (head, rest)
                Value::Tuple(vec![head, Value::Array(rest)])
            }
            _ => panic!("Invalid argument types for function split_head"),
        }
    }

    pub(super) fn signature() -> CommonFunction {
        let array_type = Type::Array(Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id()).into_id();
        let elem_type = Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id();
        CommonFunction {
            name: "split_head".to_symbol(),
            ty: function!(
                vec![array_type],
                Type::Tuple(vec![elem_type, array_type]).into_id()
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
declare_f1f_common!(abs, f64::abs);
declare_f1f_common!(sqrt, f64::sqrt);
declare_f1f_common!(round, f64::round);
declare_f1f_common!(floor, f64::floor);
declare_f1f_common!(ceil, f64::ceil);
//these basic arithmetics are not actually used in the vm level because the primitive operations cover them.
// but we still define them for the macro expansion.
declare_f2f_common!(add, std::ops::Add::add);
declare_f2f_common!(sub, std::ops::Sub::sub);
declare_f2f_common!(mult, std::ops::Mul::mul);
declare_f2f_common!(div, std::ops::Div::div);
declare_f2f_common!(modulo, std::ops::Rem::rem);
declare_f2f_common!(eq, |a: f64, b: f64| if a == b { 1.0 } else { 0.0 });
declare_f2f_common!(ne, |a: f64, b: f64| if a != b { 1.0 } else { 0.0 });
declare_f2f_common!(lt, |a: f64, b: f64| if a < b { 1.0 } else { 0.0 });
declare_f2f_common!(le, |a: f64, b: f64| if a <= b { 1.0 } else { 0.0 });
declare_f2f_common!(gt, |a: f64, b: f64| if a > b { 1.0 } else { 0.0 });
declare_f2f_common!(ge, |a: f64, b: f64| if a >= b { 1.0 } else { 0.0 });
declare_f1f_common!(not, |x: f64| if x == 0.0 { 1.0 } else { 0.0 });
declare_f1f_common!(sin, f64::sin);
declare_f1f_common!(cos, f64::cos);
declare_f1f_common!(tan, f64::tan);
declare_f1f_common!(sinh, f64::sinh);
declare_f1f_common!(cosh, f64::cosh);
declare_f1f_common!(tanh, f64::tanh);
declare_f1f_common!(asin, f64::asin);
declare_f1f_common!(acos, f64::acos);
declare_f1f_common!(atan, f64::atan);
declare_f2f_common!(atan2, f64::atan2);
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

mod prepend {
    use super::*;
    use crate::interpreter::Value;
    use crate::plugin::CommonFunction;
    use crate::types::{Type, TypeSchemeId};
    use crate::{function, interner::TypeNodeId};

    fn machine_function(
        machine: &mut crate::runtime::vm::Machine,
    ) -> crate::runtime::vm::ReturnCode {
        let arr_idx = machine.get_stack(1);
        let array = machine.arrays.get_array(arr_idx);
        let len = array.get_length_array() as usize;
        let elem_size = array.get_elem_word_size() as usize;

        if elem_size != 1 {
            panic!(
                "prepend runtime base implementation expects elem word size 1, found {elem_size}. use monomorphized prepend$arityN"
            );
        }

        let new_arr_idx = machine
            .arrays
            .alloc_array((len + 1) as u64, elem_size as u64);
        let copy_len = len * elem_size;
        let elem_words = vec![machine.get_stack(0)];
        let arr_data = machine.arrays.get_array(arr_idx).get_data().to_vec();

        {
            let dst = machine.arrays.get_array_mut(new_arr_idx).get_data_mut();
            dst[..elem_size].copy_from_slice(&elem_words);
            dst[elem_size..elem_size + copy_len].copy_from_slice(&arr_data[..copy_len]);
        }

        machine.set_stack(0, new_arr_idx);
        1
    }

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 2);
        let (elem, _) = &args[0];
        let (arr, _) = &args[1];
        match (elem, arr) {
            (elem, Value::Array(array)) => {
                let mut new_vec = vec![elem.clone()];
                new_vec.extend_from_slice(array);
                Value::Array(new_vec)
            }
            _ => panic!("Invalid argument types for function prepend"),
        }
    }

    pub(super) fn signature() -> CommonFunction {
        let ty_var = Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id();
        CommonFunction {
            name: "prepend".to_symbol(),
            ty: function!(
                vec![ty_var, Type::Array(ty_var).into_id()],
                Type::Array(ty_var).into_id()
            ),
            macro_fun: macro_function,
            fun: machine_function,
        }
    }
}

mod append {
    use super::*;
    use crate::interpreter::Value;
    use crate::plugin::CommonFunction;
    use crate::types::{Type, TypeSchemeId};
    use crate::{function, interner::TypeNodeId};

    fn machine_function(
        machine: &mut crate::runtime::vm::Machine,
    ) -> crate::runtime::vm::ReturnCode {
        let arr_idx = machine.get_stack(0);
        let array = machine.arrays.get_array(arr_idx);
        let len = array.get_length_array() as usize;
        let elem_size = array.get_elem_word_size() as usize;

        if elem_size != 1 {
            panic!(
                "append runtime base implementation expects elem word size 1, found {elem_size}. use monomorphized append$arityN"
            );
        }

        let new_arr_idx = machine
            .arrays
            .alloc_array((len + 1) as u64, elem_size as u64);
        let copy_len = len * elem_size;
        let elem_words = vec![machine.get_stack(1)];
        let arr_data = machine.arrays.get_array(arr_idx).get_data().to_vec();

        {
            let dst = machine.arrays.get_array_mut(new_arr_idx).get_data_mut();
            dst[..copy_len].copy_from_slice(&arr_data[..copy_len]);
            dst[copy_len..copy_len + elem_size].copy_from_slice(&elem_words);
        }

        machine.set_stack(0, new_arr_idx);
        1
    }

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 2);
        let (arr, _) = &args[0];
        let (elem, _) = &args[1];
        match (arr, elem) {
            (Value::Array(array), elem) => {
                let mut new_vec = array.clone();
                new_vec.push(elem.clone());
                Value::Array(new_vec)
            }
            _ => panic!("Invalid argument types for function append"),
        }
    }

    pub(super) fn signature() -> CommonFunction {
        let ty_var = Type::TypeScheme(TypeSchemeId(u64::MAX)).into_id();
        CommonFunction {
            name: "append".to_symbol(),
            ty: function!(
                vec![Type::Array(ty_var).into_id(), ty_var],
                Type::Array(ty_var).into_id()
            ),
            macro_fun: macro_function,
            fun: machine_function,
        }
    }
}

fn parse_arity_specialized_name(name: Symbol, base: &str) -> Option<usize> {
    name.as_str()
        .strip_prefix(base)
        .and_then(|s| s.strip_prefix("$arity"))
        .and_then(|n| n.parse::<usize>().ok())
}

pub(crate) fn try_get_monomorphized_ext_fn_name(
    fn_name: Symbol,
    concrete_arg_ty: TypeNodeId,
    concrete_ret_ty: TypeNodeId,
) -> Option<Symbol> {
    let resolved_word_size = |ty: TypeNodeId| {
        if ty.to_type().contains_unresolved() {
            None
        } else {
            Some(ty.word_size())
        }
    };

    let base_name = {
        let name = fn_name.as_str();
        if name == "__probe_value_intercept" || name.starts_with("__probe_value_intercept$arity") {
            "__probe_value_intercept"
        } else if name == "prepend" || name.starts_with("prepend$arity") {
            "prepend"
        } else if name == "append" || name.starts_with("append$arity") {
            "append"
        } else if name == "split_head" || name.starts_with("split_head$arity") {
            "split_head"
        } else if name == "split_tail" || name.starts_with("split_tail$arity") {
            "split_tail"
        } else {
            return None;
        }
    };

    let elem_word_size = match base_name {
        "__probe_value_intercept" => {
            // ProbeValue intercept returns the probed value as-is.
            // Use return word size as arity source so tuple/record passthrough
            // values resolve to the correct specialized runtime symbol.
            resolved_word_size(concrete_ret_ty)?
        }
        "prepend" => match concrete_arg_ty.to_type() {
            crate::types::Type::Tuple(args) if args.len() == 2 => match args[1].to_type() {
                crate::types::Type::Array(elem_ty) => resolved_word_size(elem_ty)?,
                _ => return None,
            },
            crate::types::Type::Array(elem_ty) => resolved_word_size(elem_ty)?,
            _ => return None,
        },
        "append" => match concrete_arg_ty.to_type() {
            crate::types::Type::Tuple(args) if args.len() == 2 => match args[0].to_type() {
                crate::types::Type::Array(elem_ty) => resolved_word_size(elem_ty)?,
                _ => return None,
            },
            crate::types::Type::Array(elem_ty) => resolved_word_size(elem_ty)?,
            _ => return None,
        },
        "split_head" => match concrete_arg_ty.to_type() {
            crate::types::Type::Array(elem_ty) => resolved_word_size(elem_ty)?,
            crate::types::Type::Tuple(args) if args.len() == 1 => match args[0].to_type() {
                crate::types::Type::Array(elem_ty) => resolved_word_size(elem_ty)?,
                _ => return None,
            },
            _ => return None,
        },
        "split_tail" => match concrete_arg_ty.to_type() {
            crate::types::Type::Array(elem_ty) => resolved_word_size(elem_ty)?,
            crate::types::Type::Tuple(args) if args.len() == 1 => match args[0].to_type() {
                crate::types::Type::Array(elem_ty) => resolved_word_size(elem_ty)?,
                _ => return None,
            },
            _ => return None,
        },
        _ => return None,
    };

    Some(format!("{}$arity{}", base_name, elem_word_size).to_symbol())
}

pub(crate) fn try_make_specialized_extcls(name: Symbol, ty: TypeNodeId) -> Option<ExtClsInfo> {
    let make_prepend = |elem_size: usize| {
        let f = Rc::new(RefCell::new(
            move |machine: &mut crate::runtime::vm::Machine| -> crate::runtime::vm::ReturnCode {
                let arr_idx = machine.get_stack(elem_size as i64);
                let arr = machine.arrays.get_array(arr_idx);
                if arr.get_elem_word_size() as usize != elem_size {
                    panic!(
                        "prepend$arity{} called with array elem size {}",
                        elem_size,
                        arr.get_elem_word_size()
                    );
                }
                let len = arr.get_length_array() as usize;
                let src = arr.get_data().to_vec();
                let new_arr_idx = machine
                    .arrays
                    .alloc_array((len + 1) as u64, elem_size as u64);
                let elem_words = (0..elem_size)
                    .map(|i| machine.get_stack(i as i64))
                    .collect::<Vec<_>>();
                let dst = machine.arrays.get_array_mut(new_arr_idx).get_data_mut();
                dst[..elem_size].copy_from_slice(&elem_words);
                dst[elem_size..elem_size + len * elem_size]
                    .copy_from_slice(&src[..len * elem_size]);
                machine.set_stack(0, new_arr_idx);
                1
            },
        ));
        ExtClsInfo::new(name, ty, f)
    };

    let make_append = |elem_size: usize| {
        let f = Rc::new(RefCell::new(
            move |machine: &mut crate::runtime::vm::Machine| -> crate::runtime::vm::ReturnCode {
                let arr_idx = machine.get_stack(0);
                let arr = machine.arrays.get_array(arr_idx);
                if arr.get_elem_word_size() as usize != elem_size {
                    panic!(
                        "append$arity{} called with array elem size {}",
                        elem_size,
                        arr.get_elem_word_size()
                    );
                }
                let len = arr.get_length_array() as usize;
                let src = arr.get_data().to_vec();
                let new_arr_idx = machine
                    .arrays
                    .alloc_array((len + 1) as u64, elem_size as u64);
                let elem_words = (0..elem_size)
                    .map(|i| machine.get_stack((i + 1) as i64))
                    .collect::<Vec<_>>();
                let dst = machine.arrays.get_array_mut(new_arr_idx).get_data_mut();
                dst[..len * elem_size].copy_from_slice(&src[..len * elem_size]);
                dst[len * elem_size..(len + 1) * elem_size].copy_from_slice(&elem_words);
                machine.set_stack(0, new_arr_idx);
                1
            },
        ));
        ExtClsInfo::new(name, ty, f)
    };

    let make_split_head = |elem_size: usize| {
        let f = Rc::new(RefCell::new(
            move |machine: &mut crate::runtime::vm::Machine| -> crate::runtime::vm::ReturnCode {
                let arr_idx = machine.get_stack(0);
                let arr = machine.arrays.get_array(arr_idx);
                let actual_elem_size = arr.get_elem_word_size() as usize;
                if actual_elem_size != elem_size {
                    panic!(
                        "split_head$arity{} called with array elem size {}",
                        elem_size, actual_elem_size
                    );
                }
                let len = arr.get_length_array() as usize;
                if len == 0 {
                    panic!("Cannot split_head on empty array");
                }
                let data = arr.get_data().to_vec();
                let head = data[..elem_size].to_vec();
                let rest_arr_idx = machine
                    .arrays
                    .alloc_array((len - 1) as u64, elem_size as u64);
                let rest_copy_len = (len - 1) * elem_size;
                let rest_src_offset = elem_size;
                machine.arrays.get_array_mut(rest_arr_idx).get_data_mut()[..rest_copy_len]
                    .copy_from_slice(&data[rest_src_offset..rest_src_offset + rest_copy_len]);
                head.iter()
                    .enumerate()
                    .for_each(|(i, v)| machine.set_stack(i as i64, *v));
                machine.set_stack(elem_size as i64, rest_arr_idx);
                elem_size as i64 + 1
            },
        ));
        ExtClsInfo::new(name, ty, f)
    };

    let make_split_tail = |elem_size: usize| {
        let f = Rc::new(RefCell::new(
            move |machine: &mut crate::runtime::vm::Machine| -> crate::runtime::vm::ReturnCode {
                let arr_idx = machine.get_stack(0);
                let arr = machine.arrays.get_array(arr_idx);
                let actual_elem_size = arr.get_elem_word_size() as usize;
                if actual_elem_size != elem_size {
                    panic!(
                        "split_tail$arity{} called with array elem size {}",
                        elem_size, actual_elem_size
                    );
                }
                let len = arr.get_length_array() as usize;
                if len == 0 {
                    panic!("Cannot split_tail on empty array");
                }
                let data = arr.get_data().to_vec();
                let tail_offset = (len - 1) * elem_size;
                let tail = data[tail_offset..tail_offset + elem_size].to_vec();
                let rest_arr_idx = machine
                    .arrays
                    .alloc_array((len - 1) as u64, elem_size as u64);
                let rest_copy_len = (len - 1) * elem_size;
                machine.arrays.get_array_mut(rest_arr_idx).get_data_mut()[..rest_copy_len]
                    .copy_from_slice(&data[..rest_copy_len]);
                machine.set_stack(0, rest_arr_idx);
                tail.iter()
                    .enumerate()
                    .for_each(|(i, v)| machine.set_stack((i + 1) as i64, *v));
                elem_size as i64 + 1
            },
        ));
        ExtClsInfo::new(name, ty, f)
    };

    if let Some(arity) = parse_arity_specialized_name(name, "prepend") {
        return Some(make_prepend(arity));
    }
    if let Some(arity) = parse_arity_specialized_name(name, "append") {
        return Some(make_append(arity));
    }
    if let Some(arity) = parse_arity_specialized_name(name, "split_head") {
        return Some(make_split_head(arity));
    }
    if let Some(arity) = parse_arity_specialized_name(name, "split_tail") {
        return Some(make_split_tail(arity));
    }
    None
}

mod map {

    use crate::interner::ToSymbol;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::types::{RecordTypeField, Type, TypeSchemeId};

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        // This is a placeholder. The `map` function's logic should be handled
        // by monomorphization in the MIR generation stage, so this macro
        // should not be called directly.
        panic!(
            "'map' function is not implemented as a macro. It should be handled by the compiler."
        );
    }

    pub(super) fn signature() -> MacroInfo {
        let a = Type::TypeScheme(TypeSchemeId(0)).into_id();
        let b = Type::TypeScheme(TypeSchemeId(1)).into_id();

        let f_ty = Type::Function { arg: a, ret: b }.into_id();
        let arr_a_ty = Type::Array(a).into_id();
        let arr_b_ty = Type::Array(b).into_id();

        // map takes two arguments as a record: the array and the function
        // map: ({arr: [a], f: (a -> b)}) -> [b]
        let arg_record = Type::Record(vec![
            RecordTypeField::new("arr".to_symbol(), arr_a_ty, false),
            RecordTypeField::new("f".to_symbol(), f_ty, false),
        ])
        .into_id();

        let map_fn_ty = Type::Function {
            arg: arg_record,
            ret: arr_b_ty,
        }
        .into_id();

        MacroInfo {
            name: "map".to_symbol(),
            ty: map_fn_ty,
            fun: std::rc::Rc::new(std::cell::RefCell::new(macro_function)),
        }
    }
}

// ---------------------------------------------------------------------------
// String primitive functions (Stage 0 / macro-only)
//
// These operate on `Value::String(Symbol)` at macro expansion time.
// Internal encoding follows Rust's UTF-8 `str` – random access is O(N).
// ---------------------------------------------------------------------------

/// Returns the number of Unicode scalar values (chars) in the string.
/// Type: `(String) -> Numeric`
mod str_length {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::{function, numeric, string_t, types::Type};

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        match &args[0].0 {
            Value::String(s) => Value::Number(s.as_str().chars().count() as f64),
            _ => panic!("str_length: expected String argument"),
        }
    }

    pub(super) fn signature() -> MacroInfo {
        MacroInfo {
            name: "str_length".to_symbol(),
            ty: function!(vec![string_t!()], numeric!()),
            fun: Rc::new(RefCell::new(macro_function)),
        }
    }
}

/// Takes the first N characters from a string.
/// Type: `(String, Numeric) -> String`
mod str_take {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::{function, numeric, string_t, types::Type};

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 2);
        match (&args[0].0, &args[1].0) {
            (Value::String(s), Value::Number(n)) => {
                let n = *n as usize;
                let result: String = s.as_str().chars().take(n).collect();
                Value::String(result.to_symbol())
            }
            _ => panic!("str_take: expected (String, Numeric) arguments"),
        }
    }

    pub(super) fn signature() -> MacroInfo {
        MacroInfo {
            name: "str_take".to_symbol(),
            ty: function!(vec![string_t!(), numeric!()], string_t!()),
            fun: Rc::new(RefCell::new(macro_function)),
        }
    }
}

/// Drops the first N characters from a string.
/// Type: `(String, Numeric) -> String`
mod str_drop {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::{function, numeric, string_t, types::Type};

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 2);
        match (&args[0].0, &args[1].0) {
            (Value::String(s), Value::Number(n)) => {
                let n = *n as usize;
                let result: String = s.as_str().chars().skip(n).collect();
                Value::String(result.to_symbol())
            }
            _ => panic!("str_drop: expected (String, Numeric) arguments"),
        }
    }

    pub(super) fn signature() -> MacroInfo {
        MacroInfo {
            name: "str_drop".to_symbol(),
            ty: function!(vec![string_t!(), numeric!()], string_t!()),
            fun: Rc::new(RefCell::new(macro_function)),
        }
    }
}

/// Concatenates two strings.
/// Type: `(String, String) -> String`
mod str_concat {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::{function, string_t, types::Type};

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 2);
        match (&args[0].0, &args[1].0) {
            (Value::String(a), Value::String(b)) => {
                let result = format!("{}{}", a.as_str(), b.as_str());
                Value::String(result.to_symbol())
            }
            _ => panic!("str_concat: expected (String, String) arguments"),
        }
    }

    pub(super) fn signature() -> MacroInfo {
        MacroInfo {
            name: "str_concat".to_symbol(),
            ty: function!(vec![string_t!(), string_t!()], string_t!()),
            fun: Rc::new(RefCell::new(macro_function)),
        }
    }
}

/// Checks equality of two strings. Returns 1.0 if equal, 0.0 otherwise.
/// Type: `(String, String) -> Numeric`
mod str_eq {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::{function, numeric, string_t, types::Type};

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 2);
        match (&args[0].0, &args[1].0) {
            (Value::String(a), Value::String(b)) => {
                Value::Number(if a.as_str() == b.as_str() { 1.0 } else { 0.0 })
            }
            _ => panic!("str_eq: expected (String, String) arguments"),
        }
    }

    pub(super) fn signature() -> MacroInfo {
        MacroInfo {
            name: "str_eq".to_symbol(),
            ty: function!(vec![string_t!(), string_t!()], numeric!()),
            fun: Rc::new(RefCell::new(macro_function)),
        }
    }
}

/// Extracts a substring by character index range [start, end).
/// Type: `(String, Numeric, Numeric) -> String`
mod str_slice {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::{function, numeric, string_t, types::Type};

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 3);
        match (&args[0].0, &args[1].0, &args[2].0) {
            (Value::String(s), Value::Number(start), Value::Number(end)) => {
                let start = *start as usize;
                let end = *end as usize;
                let result: String = s
                    .as_str()
                    .chars()
                    .skip(start)
                    .take(end.saturating_sub(start))
                    .collect();
                Value::String(result.to_symbol())
            }
            _ => panic!("str_slice: expected (String, Numeric, Numeric) arguments"),
        }
    }

    pub(super) fn signature() -> MacroInfo {
        MacroInfo {
            name: "str_slice".to_symbol(),
            ty: function!(vec![string_t!(), numeric!(), numeric!()], string_t!()),
            fun: Rc::new(RefCell::new(macro_function)),
        }
    }
}

/// Returns the character at the given index as a single-character string.
/// Type: `(String, Numeric) -> String`
mod str_char_at {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::{function, numeric, string_t, types::Type};

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 2);
        match (&args[0].0, &args[1].0) {
            (Value::String(s), Value::Number(idx)) => {
                let idx = *idx as usize;
                match s.as_str().chars().nth(idx) {
                    Some(c) => Value::String(c.to_string().to_symbol()),
                    None => panic!(
                        "str_char_at: index {} out of bounds for string of length {}",
                        idx,
                        s.as_str().chars().count()
                    ),
                }
            }
            _ => panic!("str_char_at: expected (String, Numeric) arguments"),
        }
    }

    pub(super) fn signature() -> MacroInfo {
        MacroInfo {
            name: "str_char_at".to_symbol(),
            ty: function!(vec![string_t!(), numeric!()], string_t!()),
            fun: Rc::new(RefCell::new(macro_function)),
        }
    }
}

/// Parses a string as a floating-point number. Panics on invalid input.
/// Type: `(String) -> Numeric`
mod str_to_number {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::{function, numeric, string_t, types::Type};

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        match &args[0].0 {
            Value::String(s) => {
                let n: f64 = s.as_str().parse().unwrap_or_else(|e| {
                    panic!("str_to_number: failed to parse \"{}\": {e}", s.as_str())
                });
                Value::Number(n)
            }
            _ => panic!("str_to_number: expected String argument"),
        }
    }

    pub(super) fn signature() -> MacroInfo {
        MacroInfo {
            name: "str_to_number".to_symbol(),
            ty: function!(vec![string_t!()], numeric!()),
            fun: Rc::new(RefCell::new(macro_function)),
        }
    }
}

/// Converts a number to its string representation.
/// Type: `(Numeric) -> String`
mod number_to_str {
    use super::*;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::{function, numeric, string_t, types::Type};

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        match &args[0].0 {
            Value::Number(n) => Value::String(n.to_string().to_symbol()),
            _ => panic!("number_to_str: expected Numeric argument"),
        }
    }

    pub(super) fn signature() -> MacroInfo {
        MacroInfo {
            name: "number_to_str".to_symbol(),
            ty: function!(vec![numeric!()], string_t!()),
            fun: Rc::new(RefCell::new(macro_function)),
        }
    }
}

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
        sinh::signature(),
        cosh::signature(),
        tanh::signature(),
        asin::signature(),
        acos::signature(),
        atan::signature(),
        atan2::signature(),
        pow::signature(),
        length_array::signature(),
        append::signature(),
        prepend::signature(),
        split_tail::signature(),
        split_head::signature(),
        probe::signature(),
        probeln::signature(),
    ]
    .into_iter()
}
fn generate_default_macros() -> impl ExactSizeIterator<Item = MacroInfo> {
    vec![
        lift::signature(),
        lift_f::signature(),
        lift_arrayf::signature(),
        lift_array_code::signature(),
        map::signature(),
        // String primitives (Stage 0 only)
        str_length::signature(),
        str_take::signature(),
        str_drop::signature(),
        str_concat::signature(),
        str_eq::signature(),
        str_slice::signature(),
        str_char_at::signature(),
        str_to_number::signature(),
        number_to_str::signature(),
    ]
    .into_iter()
}
pub fn get_builtin_fns_as_plugins() -> Box<dyn Plugin> {
    let commonfns = generate_builtin_functions().collect();
    let macros = generate_default_macros().collect();
    let extcls = super::codegen_combinators::codegen_combinator_signatures();

    Box::new(InstantPlugin {
        macros,
        extcls,
        commonfns,
    })
}

#[cfg(test)]
mod tests {
    use super::{get_builtin_fns_as_plugins, try_get_monomorphized_ext_fn_name};
    use crate::{
        compiler,
        interner::ToSymbol,
        numeric,
        plugin,
        plugin::Plugin,
        types::{PType, Type},
        Config, ExecContext,
    };

    #[test]
    fn probe_value_monomorphized_name_uses_return_word_size() {
        let arg_ty = Type::Tuple(vec![Type::Unknown.into_id(), numeric!()]).into_id();
        let ret_ty = Type::Tuple(vec![numeric!(), numeric!()]).into_id();

        let resolved = try_get_monomorphized_ext_fn_name(
            "__probe_value_intercept".to_symbol(),
            arg_ty,
            ret_ty,
        );

        assert_eq!(resolved, Some("__probe_value_intercept$arity2".to_symbol()));
    }

    #[test]
    fn probe_value_monomorphized_name_none_for_unresolved_return() {
        let arg_ty = Type::Tuple(vec![numeric!(), numeric!()]).into_id();
        let ret_ty = Type::Unknown.into_id();

        let resolved = try_get_monomorphized_ext_fn_name(
            "__probe_value_intercept".to_symbol(),
            arg_ty,
            ret_ty,
        );

        assert_eq!(resolved, None);
    }

    #[test]
    fn probe_value_monomorphized_name_one_for_scalar_return() {
        let arg_ty =
            Type::Tuple(vec![numeric!(), Type::Primitive(PType::Numeric).into_id()]).into_id();
        let ret_ty = numeric!();

        let resolved = try_get_monomorphized_ext_fn_name(
            "__probe_value_intercept".to_symbol(),
            arg_ty,
            ret_ty,
        );

        assert_eq!(resolved, Some("__probe_value_intercept$arity1".to_symbol()));
    }

    #[test]
    fn builtin_plugin_exposes_lift_array_code() {
        let plugin = get_builtin_fns_as_plugins();
        let type_infos = plugin.get_type_infos();
        let macro_functions = plugin.get_macro_functions();

        assert!(
            type_infos
                .iter()
                .any(|info| info.name == "lift_array_code".to_symbol()),
            "type infos should contain lift_array_code"
        );
        assert!(
            macro_functions
                .iter()
                .any(|fun| fun.get_name() == "lift_array_code".to_symbol()),
            "macro functions should contain lift_array_code"
        );
    }

    #[test]
    fn compiler_context_keeps_lift_array_code() {
        let plugin = get_builtin_fns_as_plugins();
        let plugins = [plugin];
        let ext_fns = plugin::get_extfun_types(&plugins).collect::<Vec<_>>();
        let macros = plugin::get_macro_functions(&plugins).collect::<Vec<_>>();
        let ctx = compiler::Context::new(ext_fns, macros, None, compiler::Config::default());

        assert!(
            ctx.get_ext_typeinfos()
                .iter()
                .any(|(name, _)| *name == "lift_array_code".to_symbol()),
            "compiler context should contain lift_array_code"
        );
    }

    #[test]
    fn exec_context_compiles_lift_array_code_source() {
        let src = r#"
// @test {"times":1,"stereo":false,"expected":[31.0],"web":true}

#stage(macro)
fn mk_functions(){
   let funcs = [
      `|x| x + 1.0,
      `|x| x * 2.0,
   ]
   funcs |> lift_array_code
}

#stage(main)
fn dsp(){
  let funcs = mk_functions!()
  funcs[0](10.0) + funcs[1](10.0)
}
"#;
        let mut ctx = ExecContext::new(std::iter::empty(), None, Config::default());
        let result = ctx.prepare_machine(src);
        assert!(result.is_ok(), "prepare_machine failed: {result:?}");
    }

    #[test]
    fn compiler_with_file_path_compiles_lift_array_code_source() {
        let src = r#"
// @test {"times":1,"stereo":false,"expected":[31.0],"web":true}

#stage(macro)
fn mk_functions(){
   let funcs = [
      `|x| x + 1.0,
      `|x| x * 2.0,
   ]
   funcs |> lift_array_code
}

#stage(main)
fn dsp(){
  let funcs = mk_functions!()
  funcs[0](10.0) + funcs[1](10.0)
}
"#;
        let plugin = get_builtin_fns_as_plugins();
        let plugins = [plugin];
        let ext_fns = plugin::get_extfun_types(&plugins).collect::<Vec<_>>();
        let macros = plugin::get_macro_functions(&plugins).collect::<Vec<_>>();
        let ctx = compiler::Context::new(
            ext_fns,
            macros,
            Some(std::path::PathBuf::from("tmp/lift_array_code_test.mmm")),
            compiler::Config::default(),
        );
        let result = ctx.emit_mir(src);
        assert!(result.is_ok(), "emit_mir failed: {result:?}");
    }
}
