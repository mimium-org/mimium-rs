pub use crate::ast::{Expr, Literal};

use super::Symbol;

pub fn str_to_symbol<T: ToString>(x: T) -> Symbol {
    use crate::interner::ToSymbol;
    x.to_string().to_symbol()
}

#[macro_export]
macro_rules! dummy_span {
    () => {
        0..0
    };
}

#[macro_export]
macro_rules! number {
    ($n:literal) => {
        Expr::Literal(Literal::Float($n)).into_id_without_span()
    };
}

#[macro_export]
macro_rules! string {
    ($n:expr) => {
        Expr::Literal(Literal::String($n)).into_id_without_span()
    };
}
#[macro_export]
macro_rules! var {
    ($n:literal) => {
        Expr::Var($crate::ast::builder::str_to_symbol($n)).into_id_without_span()
    };
}

#[macro_export]
macro_rules! app {
    ($a:expr,$b:expr) => {
        Expr::Apply($a, $b).into_id_without_span()
    };
}

#[macro_export]
macro_rules! lambda_args {
    ($args:expr) => {
        //expect vec![id]
        $args
            .iter()
            .map(|a| TypedId {
                id: $crate::ast::builder::str_to_symbol(a),
                ty: $crate::types::Type::Unknown.into_id_with_span(0..0),
            })
            .collect::<Vec<_>>()
    };
}

#[macro_export]
macro_rules! lambda {
    ($args:expr,$body:expr) => {
        Expr::Lambda(
            $args
                .iter()
                .map(|a: &&'static str| {
                    $crate::pattern::TypedId::new(
                        $crate::ast::builder::str_to_symbol(a),
                        $crate::types::Type::Unknown.into_id(),
                    )
                })
                .collect::<Vec<_>>(),
            None,
            $body,
        )
        .into_id_without_span()
    };
}

#[macro_export]
macro_rules! let_ {
    ($id:literal,$body:expr,$then:expr) => {
        Expr::Let(
            $crate::pattern::TypedPattern {
                pat: $crate::pattern::Pattern::Single($crate::ast::builder::str_to_symbol($id)),
                ty: $crate::types::Type::Unknown.into_id(),
                default_value: None,
            },
            $body,
            Some($then),
        )
        .into_id_without_span()
    };
    ($id:literal,$body:expr) => {
        Expr::Let(
            $crate::pattern::TypedPattern {
                pat: $crate::pattern::Pattern::Single($crate::ast::builder::str_to_symbol($id)),
                ty: $crate::types::Type::Unknown.into_id_with_span(0..0),
            },
            Box::new($body),
            None,
        )
        .into_id(0..0)
    };
}

#[macro_export]
macro_rules! letrec {
    ($id:literal,$ty:expr,$body:expr,$then:expr) => {
        Expr::LetRec(
            TypedId::new(
                $crate::ast::builder::str_to_symbol($id),
                $ty.unwrap_or($crate::types::Type::Unknown.into_id()),
            ),
            $body,
            $then,
        )
        .into_id_without_span()
    };
}

#[macro_export]
macro_rules! assign {
    ($lhs:literal,$rhs:expr) => {
        Expr::Assign($crate::ast::builder::str_to_symbol($lhs), Box::new($rhs)).into_id(0..0)
    };
}
#[macro_export]
macro_rules! then {
    ($first:expr,$second:expr) => {
        Expr::Then(Box::new($first), Box::new($second)).into_id(0..0)
    };
}

#[macro_export]
macro_rules! ifexpr {
    ($cond:expr,$then:expr,$else_:expr) => {
        Expr::If($cond, $then, Some($else_)).into_id_without_span()
    };
}
