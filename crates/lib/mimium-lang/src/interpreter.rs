use std::cell::RefCell;
/// A tree walk interpreter of mimium, primarily used for macro expansion.
/// This macro system is based on the multi-stage programming paradigm, like MetaML, MetaOCaml, Scala3, where expressions can be evaluated at multiple stages.
use std::rc::Rc;

use itertools::Itertools;

use crate::ast::{Expr, Literal, RecordField};
use crate::compiler::EvalStage;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedPattern};
use crate::plugin::{MacroFunType, MacroFunction, MacroInfo};
use crate::types::Type;
use crate::utils::environment::{Environment, LookupRes};

type ClosureContent<T> = (Environment<(T, EvalStage)>, Vec<Symbol>, ExprNodeId);
pub trait ValueTrait {
    fn make_closure(e: ExprNodeId, names: Vec<Symbol>, env: Environment<(Self, EvalStage)>) -> Self
    where
        Self: std::marker::Sized;
    fn get_as_closure(self) -> Option<ClosureContent<Self>>
    where
        Self: std::marker::Sized;
    fn make_fixpoint(name: Symbol, e: ExprNodeId) -> Self;
    fn get_as_fixpoint(self) -> Option<(Symbol, ExprNodeId)>
    where
        Self: std::marker::Sized;
    fn get_as_external_fn(self) -> Option<Rc<RefCell<dyn Fn(&[(Self, TypeNodeId)]) -> Self>>>
    where
        Self: std::marker::Sized;
}
pub struct Context<V>
where
    V: Clone + ValueTrait + std::fmt::Debug,
{
    pub stage: EvalStage,
    pub env: Environment<(V, EvalStage)>,
}

///Trait for defining general reduction rules of Lambda calculus, independent of the primitive types, even if it is untyped.
pub trait GeneralInterpreter {
    type Value: Clone + ValueTrait + std::fmt::Debug;
    fn interpret_expr(&mut self, ctx: &mut Context<Self::Value>, expr: ExprNodeId) -> Self::Value;
    fn get_empty_val(&self) -> Self::Value;
    fn eval_in_new_env(
        &mut self,
        binds: &[(Symbol, Self::Value)],
        ctx: &mut Context<Self::Value>,
        e: ExprNodeId,
    ) -> Self::Value {
        ctx.env.extend();
        let binds = binds
            .iter()
            .map(|(name, val)| {
                let v = val.clone();
                (*name, (v, ctx.stage))
            })
            .collect_vec();
        ctx.env.add_bind(binds.as_slice());
        let res = self.eval(ctx, e);
        ctx.env.to_outer();
        res
    }
    fn eval_with_closure_env(
        &mut self,
        binds: &[(Symbol, (Self::Value, TypeNodeId))],
        mut ctx: Context<Self::Value>,
        e: ExprNodeId,
    ) -> Self::Value {
        let binds = binds
            .iter()
            .map(|(name, val)| {
                let (v, _ty) = val.clone();
                (*name, (v, ctx.stage))
            })
            .collect_vec();
        ctx.env.extend();
        ctx.env.add_bind(binds.as_slice());
        let res = self.eval(&mut ctx, e);
        ctx.env.to_outer();
        res
    }
    fn eval(&mut self, ctx: &mut Context<Self::Value>, expr: ExprNodeId) -> Self::Value {
        match expr.to_expr() {
            Expr::Var(name) => match ctx.env.lookup_cls(&name) {
                LookupRes::Local((val, bounded_stage)) if ctx.stage == *bounded_stage => {
                    val.clone()
                }
                LookupRes::UpValue(_, (val, bounded_stage)) if ctx.stage == *bounded_stage => {
                    val.clone()
                }
                LookupRes::Global((val, bounded_stage))
                    if ctx.stage == *bounded_stage || *bounded_stage == EvalStage::Persistent =>
                {
                    val.clone()
                }
                LookupRes::None => panic!("Variable {name} not found"),
                LookupRes::Local((_, bounded_stage))
                | LookupRes::UpValue(_, (_, bounded_stage))
                | LookupRes::Global((_, bounded_stage)) => {
                    panic!(
                        "Variable {name} found, but stage mismatch: expected {:?}, found {:?}",
                        ctx.stage, bounded_stage
                    )
                }
            },
            Expr::Let(
                TypedPattern {
                    pat: Pattern::Single(name),
                    ..
                },
                e,
                body,
            ) => {
                let v = self.eval(ctx, e);
                log::trace!("letting {} at stage: {}", name, ctx.stage);
                let empty = self.get_empty_val();
                body.map_or(empty, |e| self.eval_in_new_env(&[(name, v)], ctx, e))
            }
            Expr::Let(_, _, _) => {
                panic!("Let with multiple patterns should be destructed before evaluation")
            }
            Expr::LetRec(typed_id, e, body) => {
                let fixpoint = ValueTrait::make_fixpoint(typed_id.id, e);
                log::trace!(
                    "Creating fixpoint for {}, stage: {}",
                    typed_id.id,
                    ctx.stage
                );
                let res = self.eval_in_new_env(&[(typed_id.id, fixpoint)], ctx, e);

                let empty = self.get_empty_val();
                body.map_or(empty, |e| {
                    self.eval_in_new_env(&[(typed_id.id, res)], ctx, e)
                })
            }
            Expr::Lambda(names, _, body) => {
                let names = names.iter().map(|name| name.id).collect_vec();
                //todo: need to deep-copy the expr?
                ValueTrait::make_closure(body, names, ctx.env.clone())
            }
            Expr::Apply(f, a) => {
                let fv = self.eval(ctx, f);
                let args = a
                    .clone()
                    .into_iter()
                    .map(|arg| {
                        (
                            self.eval(ctx, arg),
                            Type::Unknown.into_id_with_location(arg.to_location()),
                        )
                    })
                    .collect::<Vec<_>>();
                if let Some(ext_fn) = fv.clone().get_as_external_fn() {
                    ext_fn.borrow()(args.as_slice())
                } else if let Some((c_env, names, body)) = fv.clone().get_as_closure() {
                    log::trace!("entering closure app with names: {:?}", names);
                    let binds = names.into_iter().zip(args).collect_vec();
                    let new_ctx = Context {
                        env: c_env,
                        stage: ctx.stage,
                    };
                    self.eval_with_closure_env(binds.as_slice(), new_ctx, body)
                } else if let Some((_, e)) = fv.get_as_fixpoint() {
                    let new_app = Expr::Apply(e, a.clone()).into_id(expr.to_location());
                    self.eval(ctx, new_app)
                } else {
                    panic!("apply to non-fuctional type")
                }
            }

            _ => self.interpret_expr(ctx, expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueToExprError {
    CodeToExpr,
    ClosureToExpr,
    FixpointToExpr,
    ExternalFnToExpr,
}
trait MultiStageInterpreter {
    type Value: Clone + ValueTrait + std::fmt::Debug + TryInto<ExprNodeId, Error = ValueToExprError>;
}

#[derive(Clone)]
pub struct ExtFunction {
    name: Symbol,
    f: Rc<RefCell<dyn Fn(&[(Value, TypeNodeId)]) -> Value>>,
}
impl ExtFunction {
    pub fn new(name: Symbol, f: impl Fn(&[(Value, TypeNodeId)]) -> Value + 'static) -> Self {
        ExtFunction {
            name,
            f: Rc::new(RefCell::new(f)),
        }
    }
}
impl std::fmt::Debug for ExtFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "External function: {}", self.name)
    }
}
/// Evalueated result of the expression. Theoritically, it can be tagless union because it is statically typed, but we use enum for better readability.
#[derive(Clone, Debug)]
pub enum Value {
    ErrorV(ExprNodeId),
    Unit,
    Number(f64),
    String(Symbol),
    Array(Vec<Value>),
    Record(Vec<(Symbol, Value)>),
    Tuple(Vec<Value>),
    Closure(ExprNodeId, Vec<Symbol>, Environment<(Value, EvalStage)>),
    Fixpoint(Symbol, ExprNodeId),
    Code(ExprNodeId),
    ExternalFn(ExtFunction),
    Store(Rc<RefCell<Value>>),
}
impl From<&Box<dyn MacroFunction>> for Value {
    fn from(macro_fn: &Box<dyn MacroFunction>) -> Self {
        Value::ExternalFn(ExtFunction {
            name: macro_fn.get_name(),
            f: macro_fn.get_fn().clone(),
        })
    }
}
impl ValueTrait for Value {
    fn make_closure(
        e: ExprNodeId,
        names: Vec<Symbol>,
        env: Environment<(Self, EvalStage)>,
    ) -> Self {
        // Create a closure value with the given expression, names, and environment
        Value::Closure(e, names, env)
    }

    fn get_as_closure(self) -> Option<(Environment<(Self, EvalStage)>, Vec<Symbol>, ExprNodeId)> {
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
    fn get_as_external_fn(self) -> Option<Rc<RefCell<dyn Fn(&[(Self, TypeNodeId)]) -> Self>>> {
        match self {
            Value::ExternalFn(f) => Some(f.f.clone()),
            _ => None,
        }
    }
}

impl TryInto<ExprNodeId> for Value {
    type Error = ValueToExprError;

    fn try_into(self) -> Result<ExprNodeId, Self::Error> {
        match self {
            Value::Number(e) => {
                Ok(Expr::Literal(Literal::Float(e.to_string().to_symbol())).into_id_without_span())
            }
            Value::String(s) => Ok(Expr::Literal(Literal::String(s)).into_id_without_span()),
            Value::Array(elements) => {
                let elements = elements.into_iter().map(|v| v.try_into()).try_collect()?;
                Ok(Expr::ArrayLiteral(elements).into_id_without_span())
            }
            Value::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, value)| {
                        value
                            .try_into()
                            .map(|expr: ExprNodeId| RecordField { name, expr })
                    })
                    .try_collect()?;
                Ok(Expr::RecordLiteral(fields).into_id_without_span())
            }
            Value::Tuple(elements) => {
                let elements = elements.into_iter().map(|e| e.try_into()).try_collect()?;
                Ok(Expr::Tuple(elements).into_id_without_span())
            }
            Value::Closure(_, _, _) => {
                // Closure cannot be converted to ExprNodeId directly
                Err(ValueToExprError::ClosureToExpr)
            }
            Value::Fixpoint(_, _) => {
                // Fixpoint cannot be converted to ExprNodeId directly
                Err(ValueToExprError::FixpointToExpr)
            }
            Value::Code(e) => Ok(e),
            Value::ExternalFn(_) => {
                // External function cannot be converted to ExprNodeId directly
                Err(ValueToExprError::ExternalFnToExpr)
            }
            Value::Unit => Ok(Expr::Block(None).into_id_without_span()),
            Value::ErrorV(e) => Ok(e),
            Value::Store(store) => {
                // Dereference the Store and convert the inner value
                store.borrow().clone().try_into()
            }
        }
    }
}

#[derive(Default)]
pub struct StageInterpreter {}

impl MultiStageInterpreter for StageInterpreter {
    type Value = Value;
}

impl GeneralInterpreter for StageInterpreter {
    type Value = Value;

    fn interpret_expr(&mut self, ctx: &mut Context<Value>, expr: ExprNodeId) -> Self::Value {
        // Implement the logic for interpreting expressions here
        if ctx.stage != EvalStage::Stage(0) {
            Value::Code(self.rebuild(ctx, expr))
        } else {
            match expr.to_expr() {
                Expr::ArrayLiteral(ev) => {
                    let elements = ev.into_iter().map(|e| self.eval(ctx, e)).collect();
                    Value::Array(elements)
                }
                Expr::RecordLiteral(fields) => {
                    let evaluated_fields = fields
                        .into_iter()
                        .map(|RecordField { name, expr }| (name, self.eval(ctx, expr)))
                        .collect();
                    Value::Record(evaluated_fields)
                }
                Expr::Tuple(elements) => {
                    let evaluated_elements =
                        elements.into_iter().map(|e| self.eval(ctx, e)).collect();
                    Value::Tuple(evaluated_elements)
                }
                Expr::Proj(e, idx) => {
                    let evaluated_expr = self.eval(ctx, e);
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
                    let evaluated_expr = self.eval(ctx, e);
                    let evaluated_index = self.eval(ctx, i);
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
                    let evaluated_expr = self.eval(ctx, e);
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
                Expr::Block(e) => e.map_or(Value::Unit, |eid| self.eval_in_new_env(&[], ctx, eid)),
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
                Expr::Error => Value::ErrorV(expr),
                Expr::Feed(_, _) => {
                    panic!("Feed expression cannot be evaluated in macro expansion")
                }
                Expr::BinOp(_, _, _) => {
                    panic!("BinOp expression should be removed in the previous stage")
                }
                Expr::UniOp(_, _) => {
                    panic!("UniOp expression should be removed in the previous stage")
                }
                Expr::If(cond, then, else_opt) => {
                    let cond_val = self.eval(ctx, cond);
                    match cond_val {
                        Value::Number(n) if n > 0.0 => self.eval(ctx, then),
                        Value::Number(_) => else_opt.map_or(Value::Unit, |e| self.eval(ctx, e)),
                        _ => panic!("Condition must be a number for if expression"),
                    }
                }
                Expr::Then(e1, e2) => {
                    let _v1 = self.eval(ctx, e1);
                    e2.map_or(Value::Unit, |e| self.eval(ctx, e))
                }
                Expr::Assign(target, e) => {
                    // Evaluate the new value
                    let new_val = self.eval(ctx, e);

                    // Get the target variable name
                    if let Expr::Var(name) = target.to_expr() {
                        // Look up the Store in the environment
                        match ctx.env.lookup_cls(&name) {
                            LookupRes::Local((Value::Store(store), _))
                            | LookupRes::UpValue(_, (Value::Store(store), _))
                            | LookupRes::Global((Value::Store(store), _)) => {
                                // Update the Store's contents
                                *store.borrow_mut() = new_val;
                                Value::Unit
                            }
                            _ => panic!("Assignment target must be a Store-bound variable"),
                        }
                    } else {
                        panic!("Assignment target must be a variable")
                    }
                }
                Expr::Escape(_e) => {
                    panic!("escape expression cannot be evaluated in stage 0")
                }
                Expr::Bracket(e) => {
                    ctx.stage = EvalStage::Stage(1); // Increase the stage for bracket
                    log::trace!("Bracketting expression, stage => {:?}", ctx.stage);

                    let res = Value::Code(self.rebuild(ctx, e));
                    ctx.stage = EvalStage::Stage(0); // Decrease the stage back
                    res
                }
                // apply, lambda, let, letrec, escape, bracket, etc. will be handled by the interpreter trait
                _ => self.eval(ctx, expr),
            }
        }
    }

    fn get_empty_val(&self) -> Self::Value {
        Value::Unit
    }

    fn eval(&mut self, ctx: &mut Context<Self::Value>, expr: ExprNodeId) -> Self::Value {
        match expr.to_expr() {
            Expr::Var(name) => match ctx.env.lookup_cls(&name) {
                LookupRes::Local((val, bounded_stage)) if ctx.stage == *bounded_stage => {
                    // Dereference Store values when looking up variables
                    match val {
                        Value::Store(store) => store.borrow().clone(),
                        _ => val.clone(),
                    }
                }
                LookupRes::UpValue(_, (val, bounded_stage)) if ctx.stage == *bounded_stage => {
                    // Dereference Store values when looking up variables
                    match val {
                        Value::Store(store) => store.borrow().clone(),
                        _ => val.clone(),
                    }
                }
                LookupRes::Global((val, bounded_stage))
                    if ctx.stage == *bounded_stage || *bounded_stage == EvalStage::Persistent =>
                {
                    // Dereference Store values when looking up variables
                    match val {
                        Value::Store(store) => store.borrow().clone(),
                        _ => val.clone(),
                    }
                }
                LookupRes::None => panic!("Variable {name} not found"),
                LookupRes::Local((_, bounded_stage))
                | LookupRes::UpValue(_, (_, bounded_stage))
                | LookupRes::Global((_, bounded_stage)) => {
                    panic!(
                        "Variable {name} found, but stage mismatch: expected {:?}, found {:?}",
                        ctx.stage, bounded_stage
                    )
                }
            },
            Expr::Let(
                TypedPattern {
                    pat: Pattern::Single(name),
                    ..
                },
                e,
                body,
            ) => {
                let v = self.eval(ctx, e);
                log::trace!("letting {} at stage: {}", name, ctx.stage);
                let empty = self.get_empty_val();
                // Wrap the value in a Store to allow mutable assignment
                let store = Value::Store(Rc::new(RefCell::new(v)));
                body.map_or(empty, |e| self.eval_in_new_env(&[(name, store)], ctx, e))
            }
            _ => {
                // For all other expressions, delegate to the default implementation
                // This calls the GeneralInterpreter::eval which will call interpret_expr
                // We need to use a trick here to call the default implementation
                match expr.to_expr() {
                    Expr::Var(_) | Expr::Let(_, _, _) => unreachable!(),
                    Expr::LetRec(typed_id, e, body) => {
                        let fixpoint = ValueTrait::make_fixpoint(typed_id.id, e);
                        log::trace!(
                            "Creating fixpoint for {}, stage: {}",
                            typed_id.id,
                            ctx.stage
                        );
                        let res = self.eval_in_new_env(&[(typed_id.id, fixpoint)], ctx, e);

                        let empty = self.get_empty_val();
                        body.map_or(empty, |e| {
                            self.eval_in_new_env(&[(typed_id.id, res)], ctx, e)
                        })
                    }
                    Expr::Lambda(names, _, body) => {
                        let names = names.iter().map(|name| name.id).collect_vec();
                        ValueTrait::make_closure(body, names, ctx.env.clone())
                    }
                    Expr::Apply(f, a) => {
                        let fv = self.eval(ctx, f);
                        let args = a
                            .clone()
                            .into_iter()
                            .map(|arg| {
                                (
                                    self.eval(ctx, arg),
                                    Type::Unknown.into_id_with_location(arg.to_location()),
                                )
                            })
                            .collect::<Vec<_>>();
                        if let Some(ext_fn) = fv.clone().get_as_external_fn() {
                            ext_fn.borrow()(args.as_slice())
                        } else if let Some((c_env, names, body)) = fv.clone().get_as_closure() {
                            log::trace!("entering closure app with names: {:?}", names);
                            let binds = names.into_iter().zip(args).collect_vec();
                            let new_ctx = Context {
                                env: c_env,
                                stage: ctx.stage,
                            };
                            self.eval_with_closure_env(binds.as_slice(), new_ctx, body)
                        } else if let Some((_, e)) = fv.get_as_fixpoint() {
                            let new_app = Expr::Apply(e, a.clone()).into_id(expr.to_location());
                            self.eval(ctx, new_app)
                        } else {
                            panic!("apply to non-fuctional type")
                        }
                    }
                    _ => self.interpret_expr(ctx, expr),
                }
            }
        }
    }
}

impl StageInterpreter {
    fn rebuild(&mut self, ctx: &mut Context<Value>, e: ExprNodeId) -> ExprNodeId {
        match e.to_expr() {
            Expr::Bracket(inner) => {
                ctx.stage = ctx.stage.increment();
                log::trace!("staging bracket expression, stage => {:?}", ctx.stage);
                let res = self.rebuild(ctx, inner);
                ctx.stage = ctx.stage.decrement();
                res
            }
            Expr::Escape(inner) => {
                ctx.stage = ctx.stage.decrement();
                log::trace!("Unstaging escape expression, stage => {:?}", ctx.stage);

                let v = self.eval(ctx, inner);
                ctx.stage = ctx.stage.increment();
                v.try_into()
                    .expect("Failed to convert escape expression to ExprNodeId")
            }
            Expr::Apply(f, a) => {
                let f_val = self.rebuild(ctx, f);
                let a_vals = a.into_iter().map(|arg| self.rebuild(ctx, arg)).collect();
                Expr::Apply(f_val, a_vals).into_id(e.to_location())
            }
            Expr::Lambda(params, r_ty, body) => {
                let body_val = self.rebuild(ctx, body);
                Expr::Lambda(params, r_ty, body_val).into_id(e.to_location())
            }
            Expr::Let(id, value, body) => Expr::Let(
                id,
                self.rebuild(ctx, value),
                body.map(|b| self.rebuild(ctx, b)),
            )
            .into_id(e.to_location()),
            Expr::LetRec(id, value, body) => Expr::LetRec(
                id,
                self.rebuild(ctx, value),
                body.map(|b| self.rebuild(ctx, b)),
            )
            .into_id(e.to_location()),
            Expr::Feed(id, body) => {
                Expr::Feed(id, self.rebuild(ctx, body)).into_id(e.to_location())
            }
            Expr::If(cond, then, else_opt) => Expr::If(
                self.rebuild(ctx, cond),
                self.rebuild(ctx, then),
                else_opt.map(|e| self.rebuild(ctx, e)),
            )
            .into_id(e.to_location()),
            Expr::Then(e1, e2) => {
                Expr::Then(self.rebuild(ctx, e1), e2.map(|e| self.rebuild(ctx, e)))
                    .into_id(e.to_location())
            }
            Expr::Assign(target, value) => {
                Expr::Assign(self.rebuild(ctx, target), self.rebuild(ctx, value))
                    .into_id(e.to_location())
            }
            Expr::ArrayLiteral(elements) => {
                Expr::ArrayLiteral(elements.into_iter().map(|e| self.rebuild(ctx, e)).collect())
                    .into_id(e.to_location())
            }
            Expr::RecordLiteral(fields) => Expr::RecordLiteral(
                fields
                    .into_iter()
                    .map(|RecordField { name, expr }| RecordField {
                        name,
                        expr: self.rebuild(ctx, expr),
                    })
                    .collect(),
            )
            .into_id(e.to_location()),
            Expr::Tuple(elements) => {
                Expr::Tuple(elements.into_iter().map(|e| self.rebuild(ctx, e)).collect())
                    .into_id(e.to_location())
            }
            Expr::Proj(e, idx) => Expr::Proj(self.rebuild(ctx, e), idx).into_id(e.to_location()),
            Expr::ArrayAccess(e, i) => {
                Expr::ArrayAccess(self.rebuild(ctx, e), self.rebuild(ctx, i))
                    .into_id(e.to_location())
            }
            Expr::FieldAccess(e, name) => {
                Expr::FieldAccess(self.rebuild(ctx, e), name).into_id(e.to_location())
            }
            Expr::Block(b) => {
                Expr::Block(b.map(|eid| self.rebuild(ctx, eid))).into_id(e.to_location())
            }
            _ => e,
        }
    }
}

pub fn create_default_interpreter(
    extern_macros: &[Box<dyn MacroFunction>],
) -> (StageInterpreter, Context<Value>) {
    let mut env = Environment::new();

    env.extend();
    env.add_bind(
        extern_macros
            .iter()
            .map(|m| {
                (m.get_name(), (Value::from(m), EvalStage::Persistent)) // Stage is set to persistent for external functions
            })
            .collect::<Vec<_>>()
            .as_slice(),
    );
    let ctx = Context {
        stage: EvalStage::Stage(0),
        env,
    };
    (StageInterpreter::default(), ctx)
}

/// Evaluate root expression. If the result value is not code, return original expression as is.
/// if the result value is code, this function recursively evaluate the expression inside the code until the result becomes non-code.
pub fn expand_macro(
    expr: ExprNodeId,
    top_type: TypeNodeId,
    extern_macros: &[Box<dyn MacroFunction>],
) -> ExprNodeId {
    let (mut interpreter, mut ctx) = create_default_interpreter(extern_macros);
    expand_macro_rec(expr, &mut ctx, &mut interpreter, top_type)
}
fn expand_macro_rec(
    expr: ExprNodeId,
    ctx: &mut Context<Value>,
    interpreter: &mut StageInterpreter,
    ty: TypeNodeId,
) -> ExprNodeId {
    if let Type::Code(t) = ty.to_type() {
        let res = interpreter.eval(ctx, expr);
        match res {
            Value::Code(e) => {
                ctx.stage = EvalStage::Stage(0);
                expand_macro_rec(e, ctx, interpreter, t)
            }
            _ => panic!("macro expansion failed, possible typing error"),
        }
    } else {
        expr
    }
}
