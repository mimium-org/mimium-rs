use super::PType;
use super::Type;

#[macro_export]
macro_rules! unit {
    () => {
        Type::Primitive(PType::Unit).into_id()
    };
}

#[macro_export]
macro_rules! integer {
    () => {
        Type::Primitive(PType::Int).into_id()
    };
}
#[macro_export]
macro_rules! numeric {
    () => {
        Type::Primitive(PType::Numeric).into_id()
    };
}
#[macro_export]
macro_rules! string_t {
    () => {
        Type::Primitive(PType::String).into_id()
    };
}
#[macro_export]
macro_rules! function {
    ($params:expr, $return:expr) => {
        Type::Function(
            $params.into_iter().map(|t| (None, t)).collect(),
            $return,
            None
        ).into_id()
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
                Type::Function(
                    vec![
                        (None,Type::Primitive(PType::Int).into_id()),
                        (None,Type::Primitive(PType::Int).into_id()),
                    ],
                    Type::Primitive(PType::Numeric).into_id(),
                    None,
                )
                .into_id(),
            )
            .into_id(),
            Type::Primitive(PType::String).into_id(),
        ])
        .into_id();
        assert_eq!(t, answer);
    }
}
