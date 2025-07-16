use super::*;
use crate::{pattern::Pattern, types::Type};

#[derive(Clone, Debug, Default)]
pub(super) struct TypeDestructor {
    pub(crate) anonymous_count: u64,
}

impl TypeDestructor {
    pub(crate) fn get_new_name(&mut self) -> Symbol {
        let name = format!("_anonymous_pat_{}", self.anonymous_count).to_symbol();
        self.anonymous_count += 1;
        name
    }

    pub fn destruct_pattern(&mut self, pattern: &Pattern, value: ExprNodeId) -> ExprNodeId {
        match pattern {
            Pattern::Single(_name) => value,
            Pattern::Tuple(patterns) => {
                let new_name = self.get_new_name();
                let loc = value.to_location();
                let newrvar = Expr::Var(new_name).into_id(loc.clone());
                let body = patterns
                    .iter()
                    .enumerate()
                    .map(|(i, p)| {
                        let field_value = Expr::Proj(value, i as i64).into_id(loc.clone());
                        self.destruct_pattern(p, field_value)
                    })
                    .reduce(|a, b| {
                        let tpat = TypedPattern {
                            pat: pattern.clone(),
                            ty: Type::Unknown.into_id(),
                        };
                        Expr::Let(tpat, a, Some(b)).into_id(loc.clone())
                    })
                    .unwrap();
                Expr::Let(
                    TypedPattern {
                        pat: Pattern::Single(new_name),
                        ty: Type::Unknown.into_id(),
                    },
                    newrvar,
                    Some(body),
                )
                .into_id(loc.clone())
            }
            Pattern::Record(fields) => {
                let new_name = self.get_new_name();
                let loc = value.to_location();
                let newrvar = Expr::Var(new_name).into_id(loc.clone());
                let body = fields
                    .iter()
                    .map(|(name, p)| {
                        let field_value = Expr::FieldAccess(newrvar, *name).into_id(loc.clone());
                        self.destruct_pattern(p, field_value)
                    })
                    .reduce(|a, b| {
                        let tpat = TypedPattern {
                            pat: pattern.clone(),
                            ty: Type::Unknown.into_id(),
                        };
                        Expr::Let(tpat, a, Some(b)).into_id(loc.clone())
                    })
                    .unwrap();
                Expr::Let(
                    TypedPattern {
                        pat: Pattern::Single(new_name),
                        ty: Type::Unknown.into_id(),
                    },
                    newrvar,
                    Some(body),
                )
                .into_id(loc.clone())
            }
            Pattern::Error => {
                panic!("Error pattern cannot be destructed")
            }
        }
    }
}
