use crate::{
    interner::ToSymbol,
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
        MacroInfo {
            name: "lift_f".to_symbol(),
            ty: function!(vec![numeric!()], code!(numeric!())),
            fun: std::rc::Rc::new(std::cell::RefCell::new(macro_function)),
        }
    }
}

mod lift_arrayf {
    use super::*;
    use crate::ast::{Expr, Literal};
    use crate::code;
    use crate::interner::TypeNodeId;
    use crate::interpreter::Value;
    use crate::types::TypeSchemeId;
    use crate::{
        function, numeric,
        types::{PType, Type},
    };

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(args.len(), 1);
        let v = &args[0].0;
        match v {
            Value::Array(arr) => {
                // Convert each element of the array to a code literal
                let lifted_elements: Vec<_> = arr
                    .iter()
                    .map(|elem| match elem {
                        Value::Number(n) => {
                            Expr::Literal(Literal::Float(n.to_string().to_symbol()))
                                .into_id_without_span()
                        }
                        _ => panic!("Array elements must be numbers for lift_arrayf"),
                    })
                    .collect();

                // Return as code containing an array literal
                Value::Code(Expr::ArrayLiteral(lifted_elements).into_id_without_span())
            }
            _ => panic!("Invalid argument types for function lift_arrayf"),
        }
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
    use crate::{
        function,
        types::{PType, Type},
    };

    fn machine_function(
        machine: &mut crate::runtime::vm::Machine,
    ) -> crate::runtime::vm::ReturnCode {
        log::warn!(
            "split_tail used at runtime may cause memory leaks. Consider using it at macro stage (stage 0) instead."
        );

        let arr_idx = machine.get_stack(0);
        let array = machine.arrays.get_array(arr_idx);
        let len = array.get_length_array();

        if len == 0 {
            panic!("Cannot split_tail on empty array");
        }

        let elem_size = array.get_elem_word_size();

        // Get the last element (tail) before we create new arrays
        let tail_offset = ((len - 1) * elem_size) as usize;
        let tail = array.get_data()[tail_offset];

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
        // We need to return 2 values, so we use a tuple representation
        // Stack layout: [rest_array_idx, tail]
        machine.set_stack(0, rest_arr_idx);
        machine.set_stack(1, tail);

        2 // Return 2 values (tuple)
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
    use crate::{
        function,
        types::{PType, Type},
    };

    fn machine_function(
        machine: &mut crate::runtime::vm::Machine,
    ) -> crate::runtime::vm::ReturnCode {
        log::warn!(
            "split_head used at runtime may cause memory leaks. Consider using it at macro stage (stage 0) instead."
        );

        let arr_idx = machine.get_stack(0);
        let array = machine.arrays.get_array(arr_idx);
        let len = array.get_length_array();

        if len == 0 {
            panic!("Cannot split_head on empty array");
        }

        let elem_size = array.get_elem_word_size();

        // Get the first element (head)
        let head = array.get_data()[0];

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
        // Stack layout: [head, rest_array_idx]
        machine.set_stack(0, head);
        machine.set_stack(1, rest_arr_idx);

        2 // Return 2 values (tuple)
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
    use crate::plugin::MacroInfo;
    use crate::types::{Type, TypeSchemeId};
    use crate::{function, interner::TypeNodeId};

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

    pub(super) fn signature() -> MacroInfo {
        let ty_var = Type::TypeScheme(TypeSchemeId(0)).into_id();
        MacroInfo {
            name: "prepend".to_symbol(),
            ty: function!(
                vec![ty_var, Type::Array(ty_var).into_id()],
                Type::Array(ty_var).into_id()
            ),
            fun: std::rc::Rc::new(std::cell::RefCell::new(macro_function)),
        }
    }
}

mod map {
    use super::*;
    use crate::interner::ToSymbol;
    use crate::interpreter::Value;
    use crate::plugin::MacroInfo;
    use crate::types::{RecordTypeField, Type, TypeSchemeId};
    use crate::interner::TypeNodeId;

    fn macro_function(args: &[(Value, TypeNodeId)]) -> Value {
        // This is a placeholder. The `map` function's logic should be handled
        // by monomorphization in the MIR generation stage, so this macro
        // should not be called directly.
        panic!("'map' function is not implemented as a macro. It should be handled by the compiler.");
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
        split_tail::signature(),
        split_head::signature(),
        probe::signature(),
        probeln::signature(),
    ]
    .into_iter()
}
fn generate_default_macros() -> impl ExactSizeIterator<Item = MacroInfo> {
    vec![
        lift_f::signature(),
        prepend::signature(),
        lift_arrayf::signature(),
        map::signature(),
    ]
    .into_iter()
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
