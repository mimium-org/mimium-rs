use crate::{
    ast::Expr,
    interner::{ExprNodeId, Symbol, ToSymbol},
    pattern::{Pattern, TypedPattern},
    types::Type,
};

/// Decompose patterns into the sequence of single let bindings.
/// let ((name1,name2), name3) = value;
/// (...body...)
/// becomes
/// let _anonymous_pat_0 = value;
/// let _anonymous_pat_1 = _anonymous_pat_0.0;
/// let name1 = _anonymous_pat_1.0;
/// let name2 = _anonymous_pat_1.1;
/// let name3 = _anonymous_pat_0.1;
/// (...body...)

#[derive(Clone, Debug, Default)]
struct PatternDestructor {
    anonymous_count: u64,
}

impl PatternDestructor {
    fn get_new_name(&mut self) -> Symbol {
        let name = format!("_anonymous_pat_{}", self.anonymous_count).to_symbol();
        self.anonymous_count += 1;
        name
    }

    fn destruct_pattern(&mut self, pattern: &Pattern, value: ExprNodeId,then_body:Option<ExprNodeId>) -> (Symbol, ExprNodeId) {
        match pattern {
            Pattern::Single(name) => (*name, value),
            Pattern::Tuple(patterns) => {
                let new_name = self.get_new_name();
                let loc = value.to_location();
                let newrvar = Expr::Var(new_name).into_id(loc.clone());
                let body = patterns
                    .iter()
                    .enumerate()
                    .map(|(i, p)| {
                        let field_value = Expr::Proj(newrvar, i as i64).into_id(loc.clone());
                        self.destruct_pattern(p, field_value, None)
                    })
                    .rfold(then_body, |acc: Option<ExprNodeId>, (name, value)| {
                        let tpat = TypedPattern {
                            pat: Pattern::Single(name),
                            ty: Type::Unknown.into_id(),
                        };
                        Some(Expr::Let(tpat, value, acc).into_id(loc.clone()))
                    });
                let res = Expr::Let(
                    TypedPattern {
                        pat: Pattern::Single(new_name),
                        ty: Type::Unknown.into_id(),
                    },
                    value,
                    body,
                )
                .into_id(loc.clone());
                (new_name, res)
            }
            Pattern::Record(fields) => {
                let new_name = self.get_new_name();
                let loc = value.to_location();
                let newrvar = Expr::Var(new_name).into_id(loc.clone());
                let body = fields
                    .iter()
                    .map(|(name, p)| {
                        let field_value = Expr::FieldAccess(newrvar, *name).into_id(loc.clone());
                        self.destruct_pattern(p, field_value,None)
                    })
                    .rfold(None, |acc: Option<ExprNodeId>, (name, value)| {
                        let tpat = TypedPattern {
                            pat: Pattern::Single(name),
                            ty: Type::Unknown.into_id(),
                        };
                        Some(Expr::Let(tpat, value, acc).into_id(loc.clone()))
                    });
                let res = Expr::Let(
                    TypedPattern {
                        pat: Pattern::Single(new_name),
                        ty: Type::Unknown.into_id(),
                    },
                    newrvar,
                    body,
                )
                .into_id(loc.clone());
                (new_name, res)
            }
            Pattern::Error => {
                panic!("Error pattern cannot be destructed")
            }
        }
    }
    pub(self) fn destruct_top(&mut self, expr: ExprNodeId) -> ExprNodeId {
        match expr.to_expr() {
            Expr::Let(tpat, value, body) => {
                let (_, single) = self.destruct_pattern(&tpat.pat, value,body);
                single
            }
            _ => expr
                .to_expr()
                .apply_fn(&mut |e: Expr| self.destruct_top(e.into_id(expr.to_location())).to_expr())
                .into_id(expr.to_location()),
        }
    }
}

pub fn destruct_let_pattern(expr: ExprNodeId) -> ExprNodeId {
    let mut destructor = PatternDestructor::default();
    destructor.destruct_top(expr)
}
#[cfg(test)]
mod test {
    use super::*;
    use crate::{dummy_span, interner::ToSymbol, utils::metadata::Location};

    #[test]
    fn test_destruct_pattern() {
        //let (x,y) = target;
        // into

        // let _anonymous_pat_0 = target;
        // let x = _anonymous_pat_0.0;
        // let y = _anonymous_pat_0.1;
        let dummy_loc = Location {
            span: dummy_span!(),
            path: "".to_symbol(),
        };
        let value = Expr::Let(
            TypedPattern {
                pat: Pattern::Tuple(vec![
                    Pattern::Single("x".to_symbol()),
                    Pattern::Single("y".to_symbol()),
                ]),
                ty: Type::Unknown.into_id(),
            },
            Expr::Var("target".to_symbol()).into_id(dummy_loc.clone()),
            Some(Expr::Var("dummy".to_symbol()).into_id(dummy_loc.clone())),
        )
        .into_id(dummy_loc.clone());
        let new_rvar = Expr::Var("_anonymous_pat_0".to_symbol()).into_id(dummy_loc.clone());
        let answer = Expr::Let(
            TypedPattern {
                pat: Pattern::Single("_anonymous_pat_0".to_symbol()),
                ty: Type::Unknown.into_id(),
            },
            Expr::Var("target".to_symbol()).into_id(dummy_loc.clone()),
            Some(
                Expr::Let(
                    TypedPattern {
                        pat: Pattern::Single("x".to_symbol()),
                        ty: Type::Unknown.into_id(),
                    },
                    Expr::Proj(new_rvar, 0).into_id(dummy_loc.clone()),
                    Some(
                        Expr::Let(
                            TypedPattern {
                                pat: Pattern::Single("y".to_symbol()),
                                ty: Type::Unknown.into_id(),
                            },
                            Expr::Proj(new_rvar, 1).into_id(dummy_loc.clone()),
                            Some(Expr::Var("dummy".to_symbol()).into_id(dummy_loc.clone())),
                        )
                        .into_id(dummy_loc.clone()),
                    ),
                )
                .into_id(dummy_loc.clone()),
            ),
        )
        .into_id(dummy_loc.clone());

        let result = destruct_let_pattern(value);
        assert_eq!(result.to_expr(), answer.to_expr());
    }
}
