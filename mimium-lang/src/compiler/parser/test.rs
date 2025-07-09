use itertools::Itertools;

use super::*;
use crate::pattern::TypedId;
use crate::utils;
use crate::utils::miniprint::MiniPrint;

macro_rules! test_string {
    ($src:literal, $ans:expr) => {
        let srcstr = $src.to_string();
        let (ast, errs) = parse(&srcstr, None);
        if errs.is_empty() {
            assert!(
                ast.to_expr() == $ans.to_expr(),
                "res:{:?}\nans:{:?}",
                ast.simple_print(),
                $ans.simple_print()
            );
        } else {
            utils::error::report(&srcstr, "".to_symbol(), &errs);
            panic!();
        }
    };
}
fn test_lex(src: &str) -> Vec<Token> {
    let (tokens, errs) = lex(src, None);
    if errs.is_empty() {
        tokens
            .unwrap_or_default()
            .into_iter()
            .map(|(t, _)| t)
            .collect()
    } else {
        panic!("lex error: {errs:?}");
    }
}
fn test_expr_parser() -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> {
    let ctx = ParseContext {
        file_path: "/".to_symbol(),
    };
    chumsky::prelude::recursive(|e| expr_parser(e, ctx))
}
fn test_expr_string(src: &str, expr: ExprNodeId) {
    let (ast, errs) = test_expr_parser().parse_recovery(test_lex(src));
    if errs.is_empty() && ast.is_some() {
        assert!(
            ast.unwrap().to_expr() == expr.to_expr(),
            "res:{ast:?}\nans:{expr:?}",
        );
    } else {
        utils::error::report(
            src,
            "".to_symbol(),
            &convert_parse_errors(&errs).collect_vec(),
        );
        panic!();
    }
}

//dummy location
fn loc(span: Span) -> Location {
    Location {
        span,
        path: "/".to_symbol(),
    }
}
#[test]
fn test_let() {
    let ans = Expr::Let(
        TypedPattern {
            pat: Pattern::Single("goge".to_symbol()),
            ty: Type::Unknown.into_id_with_location(loc(4..8)),
        },
        Expr::Literal(Literal::Float("36".to_symbol())).into_id(loc(11..13)),
        Some(Expr::Var("goge".to_symbol()).into_id(loc(15..19))),
    )
    .into_id(loc(0..19));
    test_string!("let goge = 36\n goge", ans);
}
#[test]
fn test_lettuple() {
    let ans = Expr::Let(
        TypedPattern {
            pat: Pattern::Tuple(vec![
                Pattern::Single("a".to_symbol()),
                Pattern::Single("b".to_symbol()),
            ]),
            ty: Type::Unknown.into_id_with_location(loc(4..9)),
        },
        Expr::Tuple(vec![
            Expr::Literal(Literal::Float("36".to_symbol())).into_id(loc(13..15)),
            Expr::Literal(Literal::Float("89".to_symbol())).into_id(loc(16..18)),
        ])
        .into_id(loc(12..19)),
        Some(Expr::Var("hoge".to_symbol()).into_id(loc(21..25))),
    )
    .into_id(loc(0..25));
    test_string!("let (a,b) = (36,89)\n hoge", ans);
}
#[test]
fn test_if() {
    let ans = Expr::If(
        Expr::Literal(Literal::Float("100".to_symbol())).into_id(loc(4..7)),
        Expr::Var("hoge".to_symbol()).into_id(loc(9..13)),
        Some(Expr::Var("fuga".to_symbol()).into_id(loc(19..23))),
    )
    .into_id(loc(0..23));
    test_string!("if (100) hoge else fuga", ans);
}
#[test]
fn test_if_noelse() {
    let ans = Expr::If(
        Expr::Literal(Literal::Float("100".to_symbol())).into_id(loc(4..7)),
        Expr::Var("hoge".to_symbol()).into_id(loc(9..13)),
        None,
    )
    .into_id(loc(0..13));
    test_string!("if (100) hoge ", ans);
}

#[test]
fn test_int() {
    let ans = Expr::Literal(Literal::Float("3466".to_symbol())).into_id(loc(0..4));
    test_string!("3466", ans);
}
#[test]
fn test_string() {
    let ans = Expr::Literal(Literal::String("teststr".to_symbol())).into_id(loc(0..9));
    test_string!("\"teststr\"", ans);
}
#[test]
fn test_block() {
    let ans = Expr::Block(Some(
        Expr::Let(
            TypedPattern {
                pat: Pattern::Single("hoge".to_symbol()),
                ty: Type::Unknown.into_id_with_location(loc(5..9)),
            },
            Expr::Literal(Literal::Float("100".to_symbol())).into_id(loc(12..15)),
            Some(Expr::Var("hoge".to_symbol()).into_id(loc(16..20))),
        )
        .into_id(loc(1..20)),
    ))
    .into_id(loc(0..21));
    test_string!(
        "{let hoge = 100
hoge}",
        ans
    );
}
#[test]
fn test_add() {
    let ans = Expr::Apply(
        Expr::Var("add".to_symbol()).into_id(loc(6..7)),
        vec![
            Expr::Literal(Literal::Float("3466.0".to_symbol())).into_id(loc(0..6)),
            Expr::Literal(Literal::Float("2000.0".to_symbol())).into_id(loc(7..13)),
        ],
    )
    .into_id(loc(0..13));
    test_string!("3466.0+2000.0", ans);
}
#[test]
fn test_at() {
    let ans1 = Expr::Apply(
        Expr::Var("_mimium_schedule_at".to_symbol()).into_id(loc(3..4)),
        vec![
            Expr::Literal(Literal::Float("1.0".to_symbol())).into_id(loc(4..7)),
            Expr::Var("foo".to_symbol()).into_id(loc(0..3)),
        ],
    )
    .into_id(loc(0..7));
    test_string!("foo@1.0", ans1);

    let time = Expr::Apply(
        Expr::Var("pow".to_symbol()).into_id(loc(7..8)),
        vec![
            Expr::Literal(Literal::Float("1.0".to_symbol())).into_id(loc(4..7)),
            Expr::Literal(Literal::Float("2.0".to_symbol())).into_id(loc(8..11)),
        ],
    )
    .into_id(loc(4..11));
    let ans2 = Expr::Apply(
        Expr::Var("_mimium_schedule_at".to_symbol()).into_id(loc(3..4)),
        vec![time, Expr::Var("foo".to_symbol()).into_id(loc(0..3))],
    )
    .into_id(loc(0..11));
    test_string!("foo@1.0^2.0", ans2);
}
#[test]
fn test_var() {
    let ans = Expr::Var("hoge".to_symbol()).into_id(loc(0..4));
    test_string!("hoge", ans);
}
#[test]
fn test_apply() {
    let ans = Expr::Apply(
        Expr::Var("myfun".to_symbol()).into_id(loc(0..5)),
        vec![Expr::Var("callee".to_symbol()).into_id(loc(6..12))],
    )
    .into_id(loc(0..13));
    test_string!("myfun(callee)", ans);
}

#[test]
fn test_assign1() {
    let ans = Expr::Then(
        Expr::Assign(
            Expr::Var("hoge".to_symbol()).into_id(loc(0..4)),
            Expr::Var("fuga".to_symbol()).into_id(loc(7..11)),
        )
        .into_id(loc(0..11)),
        None,
    )
    .into_id(loc(0..11));
    test_string!("hoge = fuga", ans);
}
#[test]
fn test_assign2() {
    let ans = Expr::Then(
        Expr::Assign(
            Expr::Var("hoge".to_symbol()).into_id(loc(0..4)),
            Expr::Var("fuga".to_symbol()).into_id(loc(7..11)),
        )
        .into_id(loc(0..11)),
        Some(Expr::Literal(Literal::Float("100.0".to_symbol())).into_id(loc(13..18))),
    )
    .into_id(loc(0..18));
    test_string!("hoge = fuga\n 100.0", ans);
}
#[test]
fn test_applynested() {
    let ans = Expr::Apply(
        Expr::Var("myfun".to_symbol()).into_id(loc(0..5)),
        vec![
            Expr::Apply(
                Expr::Var("myfun2".to_symbol()).into_id(loc(6..12)),
                vec![Expr::Var("callee".to_symbol()).into_id(loc(13..19))],
            )
            .into_id(loc(6..20)),
        ],
    )
    .into_id(loc(0..20));
    test_string!("myfun(myfun2(callee))", ans);
}
#[test]
fn test_macroexpand() {
    let ans = Expr::Escape(
        Expr::Apply(
            Expr::Var("myfun".to_symbol()).into_id(loc(0..6)),
            vec![Expr::Var("callee".to_symbol()).into_id(loc(7..13))],
        )
        .into_id(loc(0..14)),
    )
    .into_id(loc(0..14));
    test_string!("myfun!(callee)", ans);
}

#[test]
fn test_fndef() {
    let ans = Expr::LetRec(
        TypedId {
            ty: Type::Function(
                vec![
                    (None, Type::Unknown.into_id_with_location(loc(0..28))),
                    (None, Type::Unknown.into_id_with_location(loc(0..28))),
                ],
                Type::Unknown.into_id_with_location(loc(0..28)),
                None,
            )
            .into_id_with_location(loc(0..28)),

            id: "hoge".to_symbol(),
        },
        Expr::Lambda(
            vec![
                TypedId {
                    id: "input".to_symbol(),
                    ty: Type::Unknown.into_id_with_location(loc(8..13)),
                },
                TypedId {
                    id: "gue".to_symbol(),
                    ty: Type::Unknown.into_id_with_location(loc(14..17)),
                },
            ],
            None,
            Expr::Var("input".to_symbol()).into_id(loc(21..26)),
        )
        .into_id(loc(0..28)),
        None,
    )
    .into_id(loc(0..28));
    test_string!("fn hoge(input,gue){\n input\n}", ans);
}
#[test]
fn global_fnmultiple() {
    let ans = Expr::LetRec(
        TypedId {
            id: "hoge".to_symbol(),
            ty: Type::Function(
                vec![
                    (None, Type::Unknown.into_id_with_location(loc(0..28))),
                    (None, Type::Unknown.into_id_with_location(loc(0..28))),
                ],
                Type::Unknown.into_id_with_location(loc(0..28)),
                None,
            )
            .into_id_with_location(loc(0..28)),
        },
        Expr::Lambda(
            vec![
                TypedId {
                    id: "input".to_symbol(),
                    ty: Type::Unknown.into_id_with_location(loc(8..13)),
                },
                TypedId {
                    id: "gue".to_symbol(),
                    ty: Type::Unknown.into_id_with_location(loc(14..17)),
                },
            ],
            None,
            Expr::Var("input".to_symbol()).into_id(loc(21..26)),
        )
        .into_id(loc(0..28)),
        Some(
            Expr::LetRec(
                TypedId {
                    id: "hoge".to_symbol(),
                    ty: Type::Function(
                        vec![
                            (None, Type::Unknown.into_id_with_location(loc(29..57))),
                            (None, Type::Unknown.into_id_with_location(loc(29..57))),
                        ],
                        Type::Unknown.into_id_with_location(loc(29..57)),
                        None,
                    )
                    .into_id_with_location(loc(29..57)),
                },
                Expr::Lambda(
                    vec![
                        TypedId {
                            id: "input".to_symbol(),
                            ty: Type::Unknown.into_id_with_location(loc(37..42)),
                        },
                        TypedId {
                            id: "gue".to_symbol(),
                            ty: Type::Unknown.into_id_with_location(loc(43..46)),
                        },
                    ],
                    None,
                    Expr::Var("input".to_symbol()).into_id(loc(50..55)),
                )
                .into_id(loc(29..57)),
                None,
            )
            .into_id(loc(29..57)),
        ),
    )
    .into_id(loc(0..57));
    test_string!(
        "fn hoge(input,gue){\n input\n}\nfn hoge(input,gue){\n input\n}",
        ans
    );
}

#[test]
fn test_macrodef() {
    let ans = Expr::LetRec(
        TypedId {
            id: "hoge".to_symbol(),
            ty: Type::Unknown.into_id_with_location(loc(6..10)),
        },
        Expr::Lambda(
            vec![
                TypedId {
                    id: "input".to_symbol(),
                    ty: Type::Unknown.into_id_with_location(loc(11..16)),
                },
                TypedId {
                    id: "gue".to_symbol(),
                    ty: Type::Unknown.into_id_with_location(loc(17..20)),
                },
            ],
            None,
            Expr::Bracket(Expr::Var("input".to_symbol()).into_id(loc(24..29))).into_id(loc(0..31)),
        )
        .into_id(loc(0..31)),
        None,
    )
    .into_id(loc(0..31));
    test_string!("macro hoge(input,gue){\n input\n}", ans);
}

#[test]
fn test_tuple() {
    let tuple_items = vec![
        Expr::Literal(Literal::Float("1.0".to_symbol())).into_id(loc(1..4)),
        Expr::Literal(Literal::Float("2.0".to_symbol())).into_id(loc(6..9)),
    ];

    let ans = Expr::Tuple(tuple_items.clone()).into_id(loc(0..10));
    test_string!("(1.0, 2.0)", ans);

    // with trailing comma
    let ans = Expr::Tuple(tuple_items.clone()).into_id(loc(0..12));
    test_string!("(1.0, 2.0, )", ans);

    // trailing comma is mandatory for a single-element tuple
    let ans = Expr::Tuple(vec![tuple_items[0]]).into_id(loc(0..7));
    test_string!("(1.0, )", ans);

    // This is not a tuple
    let ans = tuple_items[0];
    test_string!("(1.0)", ans);
}

#[test]
fn test_array_literal() {
    // Basic array with multiple elements
    let array_items = vec![
        Expr::Literal(Literal::Float("1.0".to_symbol())).into_id(loc(1..4)),
        Expr::Literal(Literal::Float("2.0".to_symbol())).into_id(loc(6..9)),
        Expr::Literal(Literal::Float("3.0".to_symbol())).into_id(loc(11..14)),
    ];
    let ans = Expr::ArrayLiteral(array_items).into_id(loc(0..15));
    test_string!("[1.0, 2.0, 3.0]", ans);

    // Empty array
    let ans = Expr::ArrayLiteral(vec![]).into_id(loc(0..2));
    test_string!("[]", ans);

    // Array with single element
    let ans = Expr::ArrayLiteral(vec![
        Expr::Literal(Literal::Float("42.0".to_symbol())).into_id(loc(1..5)),
    ])
    .into_id(loc(0..6));
    test_string!("[42.0]", ans);

    // Array with trailing comma
    let array_items = vec![
        Expr::Literal(Literal::Float("10.0".to_symbol())).into_id(loc(1..5)),
        Expr::Literal(Literal::Float("20.0".to_symbol())).into_id(loc(7..11)),
    ];
    let ans = Expr::ArrayLiteral(array_items).into_id(loc(0..13));
    test_string!("[10.0, 20.0,]", ans);
}

#[test]
fn test_array_access() {
    // Basic array access with integer index
    let ans = Expr::ArrayAccess(
        Expr::Var("arr".to_symbol()).into_id(loc(0..3)),
        Expr::Literal(Literal::Float("0".to_symbol())).into_id(loc(4..5)),
    )
    .into_id(loc(0..6));
    test_string!("arr[0]", ans);

    // Array access with float index for interpolation
    let ans = Expr::ArrayAccess(
        Expr::Var("arr".to_symbol()).into_id(loc(0..3)),
        Expr::Literal(Literal::Float("0.5".to_symbol())).into_id(loc(4..7)),
    )
    .into_id(loc(0..8));
    test_string!("arr[0.5]", ans);

    // Array access with expression index
    let index_expr = Expr::Apply(
        Expr::Var("add".to_symbol()).into_id(loc(5..6)),
        vec![
            Expr::Literal(Literal::Float("1".to_symbol())).into_id(loc(4..5)),
            Expr::Literal(Literal::Float("2".to_symbol())).into_id(loc(6..7)),
        ],
    )
    .into_id(loc(4..7));

    let ans = Expr::ArrayAccess(Expr::Var("arr".to_symbol()).into_id(loc(0..3)), index_expr)
        .into_id(loc(0..8));
    test_string!("arr[1+2]", ans);

    // Nested array access
    let inner_access = Expr::ArrayAccess(
        Expr::Var("inner".to_symbol()).into_id(loc(6..11)),
        Expr::Literal(Literal::Float("0".to_symbol())).into_id(loc(12..13)),
    )
    .into_id(loc(6..14));

    let ans = Expr::ArrayAccess(
        Expr::Var("outer".to_symbol()).into_id(loc(0..5)),
        inner_access,
    )
    .into_id(loc(0..15));
    test_string!("outer[inner[0]]", ans);
}

#[test]
fn test_record_type_decl() {
    let x_s = "x".to_symbol();
    let y_s = "y".to_symbol();
    let ans = Expr::RecordLiteral(vec![
        RecordField {
            name: x_s,
            expr: Expr::Literal(Literal::Float("0.0".to_symbol())).into_id(loc(3..4)),
        },
        RecordField {
            name: y_s,
            expr: Expr::Literal(Literal::Float("2.0".to_symbol())).into_id(loc(7..8)),
        },
    ]);
    test_expr_string("{x = 0.0,y = 2.0}", ans.into_id(loc(0..9)));
}
#[test]
fn test_field_access() {
    let ans = Expr::FieldAccess(
        Expr::Proj(
            Expr::Proj(
                Expr::FieldAccess(
                    Expr::Proj(Expr::Var("x".to_symbol()).into_id(loc(0..1)), 0).into_id(loc(0..3)),
                    "field1".to_symbol(),
                )
                .into_id(loc(0..5)),
                1,
            )
            .into_id(loc(0..7)),
            0,
        )
        .into_id(loc(0..9)),
        "field2".to_symbol(),
    )
    .into_id(loc(0..11));
    test_expr_string("x.0.field1.1.0.field2", ans);
}
#[test]
fn test_stmt_without_return() {
    let ans = Expr::LetRec(
        TypedId {
            id: "test".to_symbol(),
            ty: Type::Function(
                vec![(None, Type::Unknown.into_id_with_location(loc(0..56)))],
                Type::Unknown.into_id_with_location(loc(0..56)),
                None,
            )
            .into_id_with_location(loc(0..56)),
        },
        Expr::Lambda(
            vec![TypedId {
                id: "input".to_symbol(),
                ty: Type::Unknown.into_id_with_location(loc(8..13)),
            }],
            None,
            Expr::Let(
                TypedPattern {
                    pat: Pattern::Single("v".to_symbol()),
                    ty: Type::Unknown.into_id_with_location(loc(24..25)),
                },
                Expr::Apply(
                    Expr::Var("add".to_symbol()).into_id(loc(33..34)),
                    vec![
                        Expr::Var("input".to_symbol()).into_id(loc(28..33)),
                        Expr::Literal(Literal::Float("1".to_symbol())).into_id(loc(34..35)),
                    ],
                )
                .into_id(loc(28..35)),
                Some(
                    Expr::Then(
                        Expr::Apply(
                            Expr::Var("print".to_symbol()).into_id(loc(40..45)),
                            vec![Expr::Var("v".to_symbol()).into_id(loc(46..47))],
                        )
                        .into_id(loc(40..48)),
                        Some(Expr::Var("v".to_symbol()).into_id(loc(53..54))),
                    )
                    .into_id(loc(40..54)),
                ),
            )
            .into_id(loc(20..54)),
        )
        .into_id(loc(0..56)),
        None,
    )
    .into_id(loc(0..56));
    test_string!(
        r"fn test(input){
    let v = input+1
    print(v)
    v
}",
        ans
    );
}

#[test]
#[should_panic]
fn test_fail() {
    let src = "let 100 == hoge\n fuga";
    let (ast, errs) = parse(&src.to_string(), None);

    if !errs.is_empty() {
        panic!("{}", utils::error::dump_to_string(&errs))
    };
}

#[test]
fn test_err_builtin_redefine() {
    let src = r"fn div(){
    0.0
}
100.0";
    let (_ast, err) = &parse(&src.to_string(), None);

    assert_eq!(err.len(), 1);

    let err_ans: Box<dyn ReportableError> = Box::new(error::ParseError::<Token> {
        content: Simple::custom(3..6, "Builtin functions cannot be re-defined.")
            .with_label("function decl"),
        file: "/".to_symbol(),
    });
    assert_eq!(err[0].to_string(), err_ans.to_string())
}
