use super::{PType, Type};

#[macro_export]
macro_rules! unit {
    () => {
        $crate::types::Type::Primitive($crate::types::PType::Unit).into_id()
    };
}

#[macro_export]
macro_rules! integer {
    () => {
        $crate::types::Type::Primitive($crate::types::PType::Int).into_id()
    };
}
#[macro_export]
macro_rules! numeric {
    () => {
        $crate::types::Type::Primitive($crate::types::PType::Numeric).into_id()
    };
}
#[macro_export]
macro_rules! string_t {
    () => {
        $crate::types::Type::Primitive($crate::types::PType::String).into_id()
    };
}
#[macro_export]
macro_rules! function {
    ($params:expr, $return:expr) => {
        Type::Function {
            arg: Type::Tuple($params).into_id(),
            ret: $return,
        }
        .into_id()
    };
}

#[macro_export]
macro_rules! named_function {
    ($named_params:expr, $return:expr) => {
        Type::Function($named_params, $return, None).into_id()
    };
}

#[macro_export]
macro_rules! refer {
    ($t:expr) => {
        Type::Ref($t).into_id()
    };
}

#[macro_export]
macro_rules! tuple {

    ($($t:expr),*) => {
        Type::Tuple(vec![$($t,)*]).into_id()
    };
}
#[macro_export]
macro_rules! code {
    ($t:expr) => {
        Type::Code($t).into_id()
    };
}

#[cfg(test)]
mod typemacro_test {

    use super::*;
    #[test]
    fn buildertest() {
        let t = tuple!(
            refer!(function!(vec![integer!(), integer!()], numeric!())),
            string_t!()
        );
        let answer = Type::Tuple(vec![
            Type::Ref(
                Type::Function {
                    arg: Type::Tuple(vec![
                        Type::Primitive(PType::Int).into_id(),
                        Type::Primitive(PType::Int).into_id(),
                    ])
                    .into_id(),
                    ret: Type::Primitive(PType::Numeric).into_id(),
                }
                .into_id(),
            )
            .into_id(),
            Type::Primitive(PType::String).into_id(),
        ])
        .into_id();
        assert_eq!(t, answer);
    }
}
