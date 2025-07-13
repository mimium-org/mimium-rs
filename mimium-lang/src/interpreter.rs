use std::cell::RefCell;
use std::rc::Rc;

use itertools::Itertools;

use crate::ast::{Expr, RecordField};
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
pub trait ValueTrait {
    fn make_closure(e: ExprNodeId, names: Vec<Symbol>, env: Environment<Self>) -> Self
    where
        Self: std::marker::Sized;
    fn get_as_closure(self) -> Option<(Environment<Self>, Vec<Symbol>, ExprNodeId)>
    where
        Self: std::marker::Sized;
    fn make_fixpoint(name: Symbol, e: ExprNodeId) -> Self;
    fn get_as_fixpoint(self) -> Option<(Symbol, ExprNodeId)>
    where
        Self: std::marker::Sized;
}

///Trait for defining general reduction rules of Lambda calculus, independent of the primitive types, even if it is untyped.
pub trait GeneralInterpreter {
    type Value: Clone + ValueTrait;
    fn interpret_expr(&mut self, expr: ExprNodeId) -> Self::Value;
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
            Expr::Let(pattern, _e, _body) => {
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
                let fixpoint = ValueTrait::make_fixpoint(typed_id.id, e);
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
                let fv = self.eval(f);
                let (mut c_env, names, body) = fv
                    .clone()
                    .get_as_closure()
                    .or_else(|| {
                        fv.get_as_fixpoint().map(|(name, e)| {
                            let names = vec![name];
                            let body = e;
                            let mut env = self.get_val_env().borrow().clone();
                            env.extend();
                            env.add_bind(&[(name, ValueTrait::make_fixpoint(name, e))]);
                            (env, names, body)
                        })
                    })
                    .expect("Expected closure or fixpoint value");
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

            _ => self.interpret_expr(expr),
        }
    }
}

/// Evalueated result of the expression. Theoritically, it can be tagless union because it is statically typed, but we use enum for better readability.
#[derive(Clone, Debug)]
enum Value {
    None,
    Expr(ExprNodeId),
    Closure(ExprNodeId, Vec<Symbol>, Environment<Value>),
    Fixpoint(Symbol, ExprNodeId),
}
impl ValueTrait for Value {
    fn make_closure(e: ExprNodeId, names: Vec<Symbol>, env: Environment<Self>) -> Self {
        // Create a closure value with the given expression, names, and environment
        Value::Closure(e, names, env)
    }

    fn get_as_closure(self) -> Option<(Environment<Self>, Vec<Symbol>, ExprNodeId)> {
        match self {
            Value::Closure(e, names, env) => Some((env, names, e)),
            _ => None,
        }
    }

    fn make_fixpoint(name: Symbol, e: ExprNodeId) -> Self {
        // Create a fixpoint value with the given expression
        Value::Fixpoint(name, e)
    }

    fn get_as_fixpoint(self) -> Option<(Symbol, ExprNodeId)>
    where
        Self: std::marker::Sized,
    {
        match self {
            Value::Fixpoint(name, e) => Some((name, e)),
            _ => None,
        }
    }
}
impl Value {
    pub fn to_expr(&self) -> Option<ExprNodeId> {
        match self {
            Value::Expr(e) => Some(*e),
            _ => None,
        }
    }
}
struct StageInterpreter {
    val_env: Rc<RefCell<Environment<Value>>>,
    type_env: Environment<TypeNodeId>,
    pattern_destructor: TypeDestructor,
}
impl GeneralInterpreter for StageInterpreter {
    type Value = Value;

    fn interpret_expr(&mut self, expr: ExprNodeId) -> Self::Value {
        // Implement the logic for interpreting expressions here
        match expr.to_expr() {
            Expr::ArrayLiteral(ev) => {
                let elements = ev
                    .into_iter()
                    .map(|e| self.eval(e).to_expr().unwrap())
                    .collect_vec();
                Value::Expr(Expr::ArrayLiteral(elements).into_id(expr.to_location()))
            }
            Expr::RecordLiteral(fields) => {
                let evaluated_fields = fields
                    .into_iter()
                    .map(|RecordField { name, expr }| RecordField {
                        name,
                        expr: self.eval(expr).to_expr().unwrap(),
                    })
                    .collect();
                Value::Expr(Expr::RecordLiteral(evaluated_fields).into_id(expr.to_location()))
            }
            Expr::Tuple(elements) => {
                let evaluated_elements = elements
                    .into_iter()
                    .map(|e| self.eval(e).to_expr().unwrap())
                    .collect_vec();
                Value::Expr(Expr::Tuple(evaluated_elements).into_id(expr.to_location()))
            }
            Expr::Proj(e, idx) => {
                let evaluated_expr = self.eval(e).to_expr().unwrap();
                Value::Expr(Expr::Proj(evaluated_expr, idx).into_id(expr.to_location()))
            }
            Expr::ArrayAccess(e, i) => {
                let evaluated_expr = self.eval(e).to_expr().unwrap();
                let evaluated_index = self.eval(i).to_expr().unwrap();
                Value::Expr(
                    Expr::ArrayAccess(evaluated_expr, evaluated_index).into_id(expr.to_location()),
                )
            }
            Expr::FieldAccess(e, name) => {
                let evaluated_expr = self.eval(e).to_expr().unwrap();
                Value::Expr(Expr::FieldAccess(evaluated_expr, name).into_id(expr.to_location()))
            }
            Expr::Block(e) => {
                let evaluated_expr = e.map_or(Expr::Error.into_id_without_span(), |eid| {
                    self.eval_in_new_env(&[], eid).to_expr().unwrap()
                });
                Value::Expr(evaluated_expr)
            }
            Expr::Literal(_) | Expr::Error => Value::Expr(expr),
            _ => self.eval(expr),
        }
    }

    fn get_empty_val(&self) -> Self::Value {
        Value::None
    }

    fn get_val_env(&mut self) -> Rc<RefCell<Environment<Self::Value>>> {
        self.val_env.clone()
    }

    fn get_pattern_destructor(&mut self) -> &mut TypeDestructor {
        &mut self.pattern_destructor
    }

    fn get_type_env(&mut self) -> &mut Environment<TypeNodeId> {
        &mut self.type_env
    }
}
