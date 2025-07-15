use std::cell::RefCell;
use std::rc::Rc;

use itertools::Itertools;

use crate::ast::{Expr, Literal, RecordField};
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedPattern};
use crate::types::Type;
use crate::utils::environment::{Environment, LookupRes};

mod builtin;

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
    fn make_closure(e: ExprNodeId, names: Vec<Symbol>, env: Environment<(Self, Stage)>) -> Self
    where
        Self: std::marker::Sized;
    fn get_as_closure(self) -> Option<(Environment<(Self, Stage)>, Vec<Symbol>, ExprNodeId)>
    where
        Self: std::marker::Sized;
    fn make_fixpoint(name: Symbol, e: ExprNodeId) -> Self;
    fn get_as_fixpoint(self) -> Option<(Symbol, ExprNodeId)>
    where
        Self: std::marker::Sized;
    fn get_as_external_fn(self) -> Option<Rc<dyn Fn(Vec<Self>) -> Self>>
    where
        Self: std::marker::Sized;
}
type Stage = i64;
///Trait for defining general reduction rules of Lambda calculus, independent of the primitive types, even if it is untyped.
pub trait GeneralInterpreter {
    type Value: Clone + ValueTrait;
    fn interpret_expr(&mut self, stage: Stage, expr: ExprNodeId) -> Self::Value;
    fn get_empty_val(&self) -> Self::Value;
    fn get_val_env(&mut self) -> Rc<RefCell<Environment<(Self::Value, Stage)>>>;
    fn get_pattern_destructor(&mut self) -> &mut TypeDestructor;
    fn eval_in_new_env(
        &mut self,
        binds: &[(Symbol, Self::Value)],
        stage: Stage,
        e: ExprNodeId,
    ) -> Self::Value {
        let env = self.get_val_env();
        self.eval_with_env(binds, stage, &mut env.borrow_mut(), e)
    }
    fn eval_with_env(
        &mut self,
        binds: &[(Symbol, Self::Value)],
        stage: Stage,
        env: &mut Environment<(Self::Value, Stage)>,
        e: ExprNodeId,
    ) -> Self::Value {
        let binds = binds
            .iter()
            .map(|(name, val)| {
                let v = val.clone();
                (name.clone(), (v, stage))
            })
            .collect_vec();
        env.extend();
        env.add_bind(binds.as_slice());
        let res = self.eval(stage, e);
        env.to_outer();
        res
    }
    fn eval(&mut self, stage: Stage, expr: ExprNodeId) -> Self::Value {
        match expr.to_expr() {
            Expr::Var(name) => match self.get_val_env().borrow().lookup_cls(&name) {
                LookupRes::Local((val, bounded_stage)) if stage == *bounded_stage => val.clone(),
                LookupRes::UpValue(_, (val, bounded_stage)) if stage == *bounded_stage => {
                    val.clone()
                }
                LookupRes::Global((val, bounded_stage)) if stage == *bounded_stage => val.clone(),
                LookupRes::None => panic!("Variable {name} not found"),
                LookupRes::Local((_, bounded_stage))
                | LookupRes::UpValue(_, (_, bounded_stage))
                | LookupRes::Global((_, bounded_stage)) => {
                    panic!(
                        "Variable {name} found, but stage mismatch: expected {}, found {}",
                        stage, *bounded_stage
                    )
                }
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
                let v = self.eval(stage, e);
                let empty = self.get_empty_val();
                body.map_or(empty, |e| self.eval_in_new_env(&[(name, v)], stage, e))
            }
            Expr::LetRec(typed_id, e, body) => {
                let fixpoint = ValueTrait::make_fixpoint(typed_id.id, e);
                let empty = self.get_empty_val();
                body.map_or(empty, |e| {
                    self.eval_in_new_env(&[(typed_id.id, fixpoint)], stage, e)
                })
            }
            Expr::Lambda(names, _, body) => {
                let env = self.get_val_env().borrow().clone();
                let names = names.iter().map(|name| name.id).collect_vec();
                //todo: need to deep-copy the expr?
                ValueTrait::make_closure(body, names, env)
            }
            Expr::Apply(f, a) => {
                let fv = self.eval(stage, f);
                let args = a.into_iter().map(|arg| self.eval(stage, arg)).collect();
                if let Some(ext_fn) = fv.clone().get_as_external_fn() {
                    ext_fn(args)
                } else {
                    let (mut c_env, names, body) = fv
                        .clone()
                        .get_as_closure()
                        .or_else(|| {
                            fv.get_as_fixpoint().map(|(name, e)| {
                                let names = vec![name];
                                let body = e;
                                let mut env = self.get_val_env().borrow().clone();
                                env.extend();
                                env.add_bind(&[(
                                    name,
                                    (ValueTrait::make_fixpoint(name, e), stage),
                                )]);
                                (env, names, body)
                            })
                        })
                        .expect("Expected closure or fixpoint value");
                    let binds = names.into_iter().zip(args).collect_vec();
                    self.eval_with_env(binds.as_slice(), stage, &mut c_env, body)
                }
            }

            _ => self.interpret_expr(stage, expr),
        }
    }
}

/// Evalueated result of the expression. Theoritically, it can be tagless union because it is statically typed, but we use enum for better readability.
#[derive(Clone)]
enum Value {
    Error,
    Unit,
    Number(f64),
    String(Symbol),
    Array(Vec<Value>),
    Record(Vec<(Symbol, Value)>),
    Tuple(Vec<Value>),
    Closure(ExprNodeId, Vec<Symbol>, Environment<(Value, Stage)>),
    Fixpoint(Symbol, ExprNodeId),
    Code(ExprNodeId),
    ExternalFn(Rc<dyn Fn(Vec<Value>) -> Value>),
}
impl ValueTrait for Value {
    fn make_closure(e: ExprNodeId, names: Vec<Symbol>, env: Environment<(Self, Stage)>) -> Self {
        // Create a closure value with the given expression, names, and environment
        Value::Closure(e, names, env)
    }

    fn get_as_closure(self) -> Option<(Environment<(Self, Stage)>, Vec<Symbol>, ExprNodeId)> {
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
    fn get_as_external_fn(self) -> Option<Rc<dyn Fn(Vec<Self>) -> Self>>
    where
        Self: std::marker::Sized,
    {
        match self {
            Value::ExternalFn(f) => Some(f),
            _ => None,
        }
    }
}
type ExtFunction = Rc<dyn Fn(Vec<Value>) -> Value>;
struct StageInterpreter {
    val_env: Rc<RefCell<Environment<(Value, Stage)>>>,
    pattern_destructor: TypeDestructor,
}
impl GeneralInterpreter for StageInterpreter {
    type Value = Value;

    fn interpret_expr(&mut self, stage: Stage, expr: ExprNodeId) -> Self::Value {
        // Implement the logic for interpreting expressions here
        match expr.to_expr() {
            Expr::ArrayLiteral(ev) => {
                let elements = ev.into_iter().map(|e| self.eval(stage, e)).collect();
                Value::Array(elements)
            }
            Expr::RecordLiteral(fields) => {
                let evaluated_fields = fields
                    .into_iter()
                    .map(|RecordField { name, expr }| (name, self.eval(stage, expr)))
                    .collect();
                Value::Record(evaluated_fields)
            }
            Expr::Tuple(elements) => {
                let evaluated_elements =
                    elements.into_iter().map(|e| self.eval(stage, e)).collect();
                Value::Tuple(evaluated_elements)
            }
            Expr::Proj(e, idx) => {
                let evaluated_expr = self.eval(stage, e);
                match evaluated_expr {
                    Value::Tuple(elements) => {
                        if idx < 0 || idx as usize >= elements.len() {
                            panic!("Index out of bounds for tuple projection");
                        }
                        elements[idx as usize].clone()
                    }
                    _ => panic!("Projection can only be applied to tuples"),
                }
            }
            Expr::ArrayAccess(e, i) => {
                let evaluated_expr = self.eval(stage, e);
                let evaluated_index = self.eval(stage, i);
                match evaluated_expr {
                    Value::Array(elements) => {
                        if let Value::Number(idx) = evaluated_index {
                            let idx = idx as usize;
                            if idx < elements.len() {
                                elements[idx].clone()
                            } else {
                                panic!("Index out of bounds for array access");
                            }
                        } else {
                            panic!("Index must be a number for array access");
                        }
                    }
                    _ => panic!("Array access can only be applied to arrays"),
                }
            }
            Expr::FieldAccess(e, name) => {
                let evaluated_expr = self.eval(stage, e);
                match evaluated_expr {
                    Value::Record(fields) => fields
                        .into_iter()
                        .find(|(field_name, _)| field_name == &name)
                        .map_or_else(
                            || panic!("Field {name} not found in record"),
                            |(_, value)| value,
                        ),
                    _ => panic!("Field access can only be applied to records"),
                }
            }
            Expr::Block(e) => e.map_or(Value::Unit, |eid| self.eval_in_new_env(&[], stage, eid)),
            Expr::Literal(Literal::Float(f)) => Value::Number(f.to_string().parse().unwrap()),
            Expr::Literal(Literal::Int(i)) => Value::Number(i as f64),
            Expr::Literal(Literal::String(s)) => Value::String(s),
            Expr::Literal(Literal::SelfLit) => {
                panic!("Self literal cannot be evaluated in macro expansion")
            }
            Expr::Literal(Literal::Now) => {
                panic!("Now literal cannot be evaluated in macro expansion")
            }
            Expr::Literal(Literal::SampleRate) => {
                panic!("Samplerate literal cannot be evaluated in macro expansion")
            }
            Expr::Error => Value::Error,
            Expr::Feed(_, _) => {
                panic!("Feed expression cannot be evaluated in macro expansion")
            }
            Expr::PipeApply(_, _) => {
                panic!("Pipe apply expression should be removed in the previous stage")
            }
            Expr::If(cond, then, else_opt) => {
                let cond_val = self.eval(stage, cond);
                match cond_val {
                    Value::Number(n) if n > 0.0 => self.eval(stage, then),
                    Value::Number(_) => else_opt.map_or(Value::Unit, |e| self.eval(stage, e)),
                    _ => panic!("Condition must be a number for if expression"),
                }
            }
            Expr::Then(e1, e2) => {
                let _v1 = self.eval(stage, e1);
                e2.map_or(Value::Unit, |e| self.eval(stage, e))
            }
            Expr::Assign(target, e) => {
                let target_val = self.eval(stage, target);
                let new_val = self.eval(stage, e);
                panic!("assignment cannot be used in macro expansion currently")
            }
            Expr::Escape(e) => {
                let c = self.eval(stage - 1, e);
                match c {
                    Value::Code(e) => self.eval(stage, e),
                    _ => panic!("Escape expression must be a closure or fixpoint"),
                }
            }
            Expr::Bracket(e) => Value::Code(e),
            // apply, lambda, let, letrec, escape, bracket, etc. will be handled by the interpreter trait
            _ => self.eval(stage, expr),
        }
    }

    fn get_empty_val(&self) -> Self::Value {
        Value::Unit
    }

    fn get_val_env(&mut self) -> Rc<RefCell<Environment<(Self::Value, Stage)>>> {
        self.val_env.clone()
    }

    fn get_pattern_destructor(&mut self) -> &mut TypeDestructor {
        &mut self.pattern_destructor
    }
}

impl StageInterpreter {
    pub fn new(ext_fns: Vec<(Symbol, ExtFunction)>) -> Self {
        let env = Rc::new(RefCell::new(Environment::new()));
        env.borrow_mut().add_bind(
            ext_fns
                .iter()
                .map(|(name, func)| {
                    let v = Value::ExternalFn(func.clone());
                    (name.clone(), (v, 0)) // Stage is set to 0 for external functions
                })
                .collect::<Vec<_>>()
                .as_slice(),
        );
        Self {
            val_env: env,
            pattern_destructor: TypeDestructor { anonymous_count: 0 },
        }
    }
}

pub fn create_default_interpreter() -> StageInterpreter {
    let ext_fns = builtin::gen_default_fns();
    StageInterpreter::new(ext_fns)
}
