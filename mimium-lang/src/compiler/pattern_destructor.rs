use crate::{
    ast::{Expr, RecordField},
    interner::{ExprNodeId, Symbol, ToSymbol},
    pattern::{Pattern, TypedPattern},
    types::Type,
    utils::metadata::Location,
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

    fn destruct_pattern(
        &mut self,
        pattern: &TypedPattern,
        value: ExprNodeId,
        then_body: Option<ExprNodeId>,
        loc: Location,
    ) -> Option<ExprNodeId> {
        match &pattern.pat {
            Pattern::Single(name) => Some(
                Expr::Let(
                    TypedPattern {
                        pat: Pattern::Single(*name),
                        ty: pattern.ty,
                    },
                    value,
                    then_body,
                )
                .into_id(loc),
            ),
            Pattern::Tuple(patterns) => patterns.iter().enumerate().rfold(
                then_body,
                |acc: Option<ExprNodeId>, (i, sub_pat)| {
                    let field_value = Expr::Proj(value, i as i64).into_id(loc.clone());
                    self.destruct_pattern(
                        &TypedPattern {
                            pat: sub_pat.clone(),
                            ty: Type::Unknown.into_id(),
                        },
                        field_value,
                        acc,
                        field_value.to_location(),
                    )
                },
            ),
            Pattern::Record(fields) => {
                fields
                    .iter()
                    .rfold(then_body, |acc: Option<ExprNodeId>, (name, sub_pat)| {
                        let field_value = Expr::FieldAccess(value, *name).into_id(loc.clone());
                        self.destruct_pattern(
                            &TypedPattern {
                                pat: sub_pat.clone(),
                                ty: Type::Unknown.into_id(),
                            },
                            field_value,
                            acc,
                            field_value.to_location(),
                        )
                    })
            }
            Pattern::Error => Some(Expr::Error.into_id(loc.clone())),
        }
    }
    pub(self) fn destruct_top(&mut self, expr: ExprNodeId) -> ExprNodeId {
        let mut apply_node = |e: ExprNodeId| self.destruct_top(e);
        let res = match expr.to_expr() {
            Expr::Let(tpat, value, body) => {
                let desugard_v = self.destruct_top(value);
                let desugard_body = body.map(|b| self.destruct_top(b));
                match tpat.pat {
                    Pattern::Single(_) => Expr::Let(tpat, desugard_v, desugard_body),
                    Pattern::Tuple(_) | Pattern::Record(_) => {
                        let new_name = self.get_new_name();
                        let new_rvar = Expr::Var(new_name).into_id(expr.to_location());
                        let destructed_pattern_body = self.destruct_pattern(
                            &tpat,
                            new_rvar,
                            desugard_body,
                            expr.to_location(),
                        );
                        Expr::Let(
                            TypedPattern {
                                pat: Pattern::Single(new_name),
                                ty: Type::Unknown.into_id(),
                            },
                            desugard_v,
                            destructed_pattern_body,
                        )
                    }
                    _ => Expr::Error,
                }
            }
            Expr::Tuple(e) => {
                let apply_vec = |vec: &Vec<_>| vec.clone().into_iter().map(apply_node).collect();
                Expr::Tuple(apply_vec(&e))
            }
            Expr::Block(e) => Expr::Block(e.map(apply_node)),
            Expr::Proj(e, idx) => Expr::Proj(apply_node(e), idx),
            Expr::ArrayAccess(e, i) => Expr::ArrayAccess(apply_node(e), apply_node(i)),
            Expr::ArrayLiteral(items) => {
                let apply_vec = |vec: &Vec<_>| vec.clone().into_iter().map(apply_node).collect();
                Expr::ArrayLiteral(apply_vec(&items))
            }
            Expr::RecordLiteral(fields) => Expr::RecordLiteral(
                fields
                    .iter()
                    .map(|f| RecordField {
                        name: f.name,
                        expr: apply_node(f.expr),
                    })
                    .collect(),
            ),
            Expr::Apply(func, args) => Expr::Apply(
                apply_node(func),
                args.clone().into_iter().map(apply_node).collect(),
            ),
            Expr::PipeApply(lhs, rhs) => Expr::PipeApply(apply_node(lhs), apply_node(rhs)),
            Expr::FieldAccess(record, field) => Expr::FieldAccess(apply_node(record), field),
            Expr::Lambda(params, ty, body) => Expr::Lambda(params.clone(), ty, apply_node(body)),
            Expr::Feed(id, body) => Expr::Feed(id, apply_node(body)),
            Expr::LetRec(id, body, then) => {
                Expr::LetRec(id.clone(), apply_node(body), then.map(apply_node))
            }
            Expr::Assign(lid, rhs) => Expr::Assign(apply_node(lid), apply_node(rhs)),
            Expr::Then(first, second) => Expr::Then(apply_node(first), second.map(apply_node)),
            Expr::If(cond, then, optelse) => {
                Expr::If(apply_node(cond), apply_node(then), optelse.map(apply_node))
            }
            Expr::Bracket(e) => Expr::Bracket(apply_node(e)),
            Expr::Escape(e) => Expr::Escape(apply_node(e)),
            _ => {
                return expr;
            }
        };
        res.into_id(expr.to_location())
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
