use std::cell::RefCell;
use std::rc::Rc;

use itertools::Itertools;

use crate::ast::Expr;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedPattern};
use crate::types::Type;
use crate::utils::environment::{Environment, LookupRes};

#[derive(Clone, Debug)]
pub struct TypeDestructor {
    anonymous_count: u64,
}

impl TypeDestructor {
    fn get_new_name(&mut self) -> Symbol {
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
                        let field_value =
                            Expr::FieldAccess(newrvar, *name).into_id(loc.clone());
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
pub trait ValueTrait {
    fn make_closure(e: ExprNodeId, names: Vec<Symbol>, env: Environment<Self>) -> Self
    where
        Self: std::marker::Sized;
    fn get_as_closure(&self) -> Option<(Environment<Self>, Vec<Symbol>, ExprNodeId)>
    where
        Self: std::marker::Sized;
    fn make_fixpoint(e: ExprNodeId) -> Self;
}
pub trait GeneralInterpreter {
    type Value: Clone + ValueTrait;
    type SideEffect;
    fn interpret_expr(&mut self, expr: ExprNodeId) -> (Self::Value, Self::SideEffect);
    fn get_empty_val(&self) -> Self::Value;
    fn get_val_env(&mut self) -> Rc<RefCell<Environment<Self::Value>>>;
    fn get_pattern_destructor(&mut self) -> &mut TypeDestructor;
    fn get_type_env(&mut self) -> &mut Environment<TypeNodeId>;
    fn eval_in_new_env(&mut self, binds: &[(Symbol, Self::Value)], e: ExprNodeId) -> Self::Value {
        let env = self.get_val_env();
        self.eval_with_env(binds, &mut env.borrow_mut(), e)
    }
    fn eval_with_env(
        &mut self,
        binds: &[(Symbol, Self::Value)],
        env: &mut Environment<Self::Value>,
        e: ExprNodeId,
    ) -> Self::Value {
        env.extend();
        env.add_bind(binds);
        let res = self.eval(e);
        env.to_outer();
        res
    }
    fn eval(&mut self, expr: ExprNodeId) -> Self::Value {
        match expr.to_expr() {
            Expr::Var(name) => match self.get_val_env().borrow().lookup_cls(&name) {
                LookupRes::Local(val) => val.clone(),
                LookupRes::UpValue(_, val) => val.clone(),
                LookupRes::Global(val) => val.clone(),
                LookupRes::None => panic!("Variable {name} not found"),
            },
            Expr::Let(pattern,_e, _body) => {
                let single_let = self
                    .get_pattern_destructor()
                    .destruct_pattern(&pattern.pat, expr);
                let (name, e, body) = match single_let.to_expr() {
                    Expr::Let(
                        TypedPattern {
                            pat: Pattern::Single(name),
                            ..
                        },
                        e,
                        body,
                    ) => (name, e, body),
                    _ => panic!("Expected single pattern in let expression"),
                };
                let v = self.eval(e);
                let empty = self.get_empty_val();
                body.map_or(empty, |e| self.eval_in_new_env(&[(name, v)], e))
            }
            Expr::LetRec(typed_id, e, body) => {
                let fixpoint = ValueTrait::make_fixpoint(e);
                let empty = self.get_empty_val();
                body.map_or(empty, |e| {
                    self.eval_in_new_env(&[(typed_id.id, fixpoint)], e)
                })
            }
            Expr::Lambda(names, _, body) => {
                let env = self.get_val_env().borrow().clone();
                let names = names.iter().map(|name| name.id).collect_vec();

                ValueTrait::make_closure(body, names, env)
            }
            Expr::Apply(f, a) => {
                let (mut c_env, names, body) = self
                    .eval(f)
                    .get_as_closure()
                    .expect("not a function type");
                let args = a.iter().map(|arg| self.eval(*arg));
                let binds = names.into_iter().zip(args).collect_vec();
                self.eval_with_env(binds.as_slice(), &mut c_env, body)
            }
            Expr::Escape(e) => {
                todo!("")
            }
            Expr::Bracket(e) => {
                todo!("")
            }

            _ => self.interpret_expr(expr).0,
        }
    }
}
