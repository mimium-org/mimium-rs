use super::{PType, Type};

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
            $crate::types::LabeledParams::new(
                $params
                    .into_iter()
                    .map(|t| $crate::types::LabeledParam {
                        label: None,
                        ty: t,
                        has_default: false,
                    })
                    .collect(),
            ),
            $return,
            None,
        )
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
    use crate::types::{LabeledParam, LabeledParams};

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
                    LabeledParams::new(vec![
                        LabeledParam {
                            label: None,
                            ty: Type::Primitive(PType::Int).into_id(),
                            has_default: false,
                        },
                        LabeledParam {
                            label: None,
                            ty: Type::Primitive(PType::Int).into_id(),
                            has_default: false,
                        },
                    ]),
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
