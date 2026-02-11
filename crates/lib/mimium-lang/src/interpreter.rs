mod serde_impl;

use std::cell::RefCell;
/// A tree walk interpreter of mimium, primarily used for macro expansion.
/// This macro system is based on the multi-stage programming paradigm, like MetaML, MetaOCaml, Scala3, where expressions can be evaluated at multiple stages.
use std::rc::Rc;

use itertools::Itertools;

use crate::ast::{Expr, Literal, RecordField};
use crate::compiler::EvalStage;
use crate::compiler::typing::ConstructorEnv;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedPattern};
use crate::plugin::MacroFunction;
use crate::types::Type;
use crate::utils::environment::{Environment, LookupRes};

type ClosureContent<T> = (Environment<(T, EvalStage)>, Vec<Symbol>, ExprNodeId);
type ExternalFunc<T> = Rc<RefCell<dyn Fn(&[(T, TypeNodeId)]) -> T>>;
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
    fn get_as_external_fn(self) -> Option<ExternalFunc<Self>>
    where
        Self: std::marker::Sized;
}
/// Evaluation context containing stage, environment, and constructor information.
///
/// The `stage` field determines evaluation strategy:
/// - Stage 0: Macro expansion with full evaluation
/// - Stage 1+: Code construction for VM
/// - Persistent: Available at all stages (builtins)
pub struct Context<V>
where
    V: Clone + ValueTrait + std::fmt::Debug,
{
    pub stage: EvalStage,
    pub env: Environment<(V, EvalStage)>,
    /// Maps constructor names to their tag indices and type information.
    /// Only used at stage 0 for resolving sum type constructors.
    pub constructor_env: ConstructorEnv,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueToExprError {
    CodeToExpr,
    ClosureToExpr,
    FixpointToExpr,
    ExternalFnToExpr,
    TaggedUnionToExpr,
    ConstructorFnToExpr,
}

#[derive(Clone)]
pub struct ExtFunction {
    name: Symbol,
    f: ExternalFunc<Value>,
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
    TaggedUnion(u64, Box<Value>),
    ConstructorFn(u64, Symbol, TypeNodeId),
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
            Value::TaggedUnion(_, _) => {
                // TaggedUnion cannot be converted to ExprNodeId without constructor_env
                Err(ValueToExprError::TaggedUnionToExpr)
            }
            Value::ConstructorFn(_, _, _) => {
                // Constructor functions cannot be converted to ExprNodeId
                Err(ValueToExprError::ConstructorFnToExpr)
            }
        }
    }
}

/// The main interpreter for multi-stage evaluation in mimium.
///
/// # Stage Semantics
///
/// - **Stage 0** (Macro expansion): Full evaluation of expressions including:
///   - Constructor resolution via `constructor_env`
///   - Value computation (arithmetic, data structures)
///   - Pattern matching and control flow
///   - Bracket expressions (transition to stage 1)
///
/// - **Stage 1+** (Code generation): AST reconstruction for VM execution:
///   - Structural recursion through expressions
///   - Escape expressions are evaluated at stage 0
///   - No constructor resolution needed (already typed)
///
/// - **Persistent**: Built-in functions accessible from all stages
#[derive(Default)]
pub struct StageInterpreter {}

impl StageInterpreter {
    /// Unified evaluation method that handles all expressions at all stages.
    /// Stage 0: Full evaluation with constructor resolution.
    /// Stage 1+: Code construction via rebuild().
    pub fn eval_expr(&mut self, ctx: &mut Context<Value>, expr: ExprNodeId) -> Value {
        // Stage check: non-zero stages use rebuild for code construction
        if ctx.stage != EvalStage::Stage(0) {
            return Value::Code(self.rebuild(ctx, expr));
        }

        // Stage 0: Full evaluation with constructor support
        match expr.to_expr() {
            // Variable lookup: constructor_env takes priority over environment
            Expr::Var(name) => {
                // Priority 1: Check constructor_env for sum type constructors
                if let Some(ctor_info) = ctx.constructor_env.get(&name) {
                    return if ctor_info.payload_type.is_some() {
                        // Constructor with payload - return ConstructorFn
                        Value::ConstructorFn(ctor_info.tag_index as u64, name, ctor_info.sum_type)
                    } else {
                        // Constructor without payload - return TaggedUnion with Unit
                        Value::TaggedUnion(ctor_info.tag_index as u64, Box::new(Value::Unit))
                    };
                }

                // Priority 2: Check environment for regular variables
                match ctx.env.lookup_cls(&name) {
                    LookupRes::Local((val, bounded_stage)) if ctx.stage == *bounded_stage => {
                        self.deref_store(val)
                    }
                    LookupRes::UpValue(_, (val, bounded_stage)) if ctx.stage == *bounded_stage => {
                        self.deref_store(val)
                    }
                    LookupRes::Global((val, bounded_stage))
                        if ctx.stage == *bounded_stage
                            || *bounded_stage == EvalStage::Persistent =>
                    {
                        self.deref_store(val)
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
                }
            }

            // Let binding with single variable
            Expr::Let(
                TypedPattern {
                    pat: Pattern::Single(name),
                    ..
                },
                e,
                body,
            ) => {
                let v = self.eval_expr(ctx, e);
                log::trace!("letting {} at stage: {}", name, ctx.stage);
                let empty = self.get_empty_val();
                let wrapped_v = self.wrap_store(v);
                body.map_or(empty, |e| {
                    self.eval_in_new_env(&[(name, wrapped_v)], ctx, e)
                })
            }

            // Let with placeholder (evaluate but don't bind)
            Expr::Let(
                TypedPattern {
                    pat: Pattern::Placeholder,
                    ..
                },
                e,
                body,
            ) => {
                let _ = self.eval_expr(ctx, e);
                log::trace!("letting _ (placeholder) at stage: {}", ctx.stage);
                let empty = self.get_empty_val();
                body.map_or(empty, |e| self.eval_expr(ctx, e))
            }

            Expr::Let(_, _, _) => {
                panic!("Let with multiple patterns should be destructed before evaluation")
            }

            // Recursive let binding
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

            // Lambda abstraction
            Expr::Lambda(names, _, body) => {
                let names = names.iter().map(|name| name.id).collect_vec();
                ValueTrait::make_closure(body, names, ctx.env.clone())
            }

            // Function application
            Expr::Apply(f, a) => {
                // Evaluate function (may return ConstructorFn at stage 0)
                let fv = self.eval_expr(ctx, f);

                // Handle constructor function application
                if let Value::ConstructorFn(tag, _name, _sum_type) = fv {
                    let args: Vec<Value> = a
                        .clone()
                        .into_iter()
                        .map(|arg| self.eval_expr(ctx, arg))
                        .collect();
                    // If multiple arguments, treat as tuple construction
                    let payload = if args.len() == 1 {
                        args[0].clone()
                    } else {
                        Value::Tuple(args)
                    };
                    return Value::TaggedUnion(tag, Box::new(payload));
                }

                // Prepare arguments for regular function application
                let args = a
                    .clone()
                    .into_iter()
                    .map(|arg| {
                        (
                            self.eval_expr(ctx, arg),
                            Type::Unknown.into_id_with_location(arg.to_location()),
                        )
                    })
                    .collect::<Vec<_>>();

                // Handle different function types
                if let Some(ext_fn) = fv.clone().get_as_external_fn() {
                    ext_fn.borrow()(args.as_slice())
                } else if let Some((c_env, names, body)) = fv.clone().get_as_closure() {
                    log::trace!("entering closure app with names: {names:?}");
                    let binds = names.into_iter().zip(args).collect_vec();
                    let new_ctx = Context {
                        env: c_env,
                        stage: ctx.stage,
                        constructor_env: ctx.constructor_env.clone(),
                    };
                    self.eval_with_closure_env(binds.as_slice(), new_ctx, body)
                } else if let Some((_, e)) = fv.get_as_fixpoint() {
                    let new_app = Expr::Apply(e, a.clone()).into_id(expr.to_location());
                    self.eval_expr(ctx, new_app)
                } else {
                    panic!("apply to non-functional type")
                }
            }

            Expr::ArrayLiteral(ev) => {
                let elements = ev.into_iter().map(|e| self.eval_expr(ctx, e)).collect();
                Value::Array(elements)
            }
            Expr::RecordLiteral(fields) => {
                let evaluated_fields = fields
                    .into_iter()
                    .map(|RecordField { name, expr }| (name, self.eval_expr(ctx, expr)))
                    .collect();
                Value::Record(evaluated_fields)
            }
            Expr::Tuple(elements) => {
                let evaluated_elements = elements
                    .into_iter()
                    .map(|e| self.eval_expr(ctx, e))
                    .collect();
                Value::Tuple(evaluated_elements)
            }
            Expr::Proj(e, idx) => {
                let evaluated_expr = self.eval_expr(ctx, e);
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
                let evaluated_expr = self.eval_expr(ctx, e);
                let evaluated_index = self.eval_expr(ctx, i);
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
                let evaluated_expr = self.eval_expr(ctx, e);
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
            Expr::Literal(Literal::PlaceHolder) => {
                panic!("PlaceHolder literal cannot be evaluated in macro expansion")
            }
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
                let cond_val = self.eval_expr(ctx, cond);
                match cond_val {
                    Value::Number(n) if n > 0.0 => self.eval_expr(ctx, then),
                    Value::Number(_) => else_opt.map_or(Value::Unit, |e| self.eval_expr(ctx, e)),
                    _ => panic!("Condition must be a number for if expression"),
                }
            }
            Expr::Then(e1, e2) => {
                let _v1 = self.eval_expr(ctx, e1);
                e2.map_or(Value::Unit, |e| self.eval_expr(ctx, e))
            }
            Expr::Assign(target, e) => {
                // Evaluate the new value
                let new_val = self.eval_expr(ctx, e);

                // Get the Store for the target
                let store = self.eval_address(ctx, target);
                *store.borrow_mut() = new_val;
                Value::Unit
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
            Expr::Match(scrutinee, arms) => {
                // Evaluate scrutinee
                let scrutinee_val = self.eval_expr(ctx, scrutinee);

                // Try each arm in order
                for arm in arms {
                    let pattern = &arm.pattern;
                    let body = arm.body;

                    // Check if pattern matches
                    if let Some(bindings) =
                        Self::match_pattern(&ctx.constructor_env, pattern, &scrutinee_val)
                    {
                        // Pattern matched - evaluate body with bindings
                        return self.eval_in_new_env(&bindings, ctx, body);
                    }
                }

                panic!("No pattern matched in match expression");
            }

            // Unhandled expressions that should not appear in stage 0
            Expr::QualifiedVar(_) => {
                panic!("QualifiedVar should be resolved before macro expansion")
            }
            Expr::ImcompleteRecord(_) => {
                panic!("ImcompleteRecord should be resolved before macro expansion")
            }
            Expr::RecordUpdate(_, _) => {
                panic!("RecordUpdate should be resolved before macro expansion")
            }
            Expr::MacroExpand(_, _) => {
                panic!("MacroExpand should be resolved before macro expansion")
            }
            Expr::Paren(inner) => self.eval_expr(ctx, inner),
        }
    }

    fn get_empty_val(&self) -> Value {
        Value::Unit
    }

    fn deref_store(&self, val: &Value) -> Value {
        match val {
            Value::Store(store) => store.borrow().clone(),
            _ => val.clone(),
        }
    }

    fn wrap_store(&self, val: Value) -> Value {
        Value::Store(Rc::new(RefCell::new(val)))
    }

    fn eval_in_new_env(
        &mut self,
        binds: &[(Symbol, Value)],
        ctx: &mut Context<Value>,
        e: ExprNodeId,
    ) -> Value {
        ctx.env.extend();
        let binds = binds
            .iter()
            .map(|(name, val)| {
                let v = val.clone();
                (*name, (v, ctx.stage))
            })
            .collect_vec();
        ctx.env.add_bind(binds.as_slice());
        let res = self.eval_expr(ctx, e);
        ctx.env.to_outer();
        res
    }

    fn eval_with_closure_env(
        &mut self,
        binds: &[(Symbol, (Value, TypeNodeId))],
        mut ctx: Context<Value>,
        e: ExprNodeId,
    ) -> Value {
        let binds = binds
            .iter()
            .map(|(name, val)| {
                let (v, _ty) = val.clone();
                (*name, (v, ctx.stage))
            })
            .collect_vec();
        ctx.env.extend();
        ctx.env.add_bind(binds.as_slice());
        let res = self.eval_expr(&mut ctx, e);
        ctx.env.to_outer();
        res
    }

    /// Try to match a pattern against a value. Returns Some(bindings) if successful.
    fn match_pattern(
        constructor_env: &ConstructorEnv,
        pattern: &crate::ast::MatchPattern,
        value: &Value,
    ) -> Option<Vec<(Symbol, Value)>> {
        use crate::ast::MatchPattern;

        match (pattern, value) {
            // Wildcard matches anything
            (MatchPattern::Wildcard, _) => Some(vec![]),

            // Literal integer match
            (MatchPattern::Literal(Literal::Int(expected)), Value::Number(actual)) => {
                if (*expected as f64 - *actual).abs() < 1e-6 {
                    Some(vec![])
                } else {
                    None
                }
            }

            // Literal float match
            (MatchPattern::Literal(Literal::Float(expected)), Value::Number(actual)) => {
                let expected_val: f64 = expected.to_string().parse().unwrap();
                if (expected_val - *actual).abs() < 1e-6 {
                    Some(vec![])
                } else {
                    None
                }
            }

            // Variable pattern - binds the value
            (MatchPattern::Variable(name), _) => Some(vec![(*name, value.clone())]),

            // Constructor pattern
            (
                MatchPattern::Constructor(ctor_name, bind_pattern),
                Value::TaggedUnion(tag, payload),
            ) => {
                // Look up constructor info to check tag
                if let Some(ctor_info) = constructor_env.get(ctor_name) {
                    if ctor_info.tag_index as u64 == *tag {
                        // Tag matches - check inner pattern
                        match bind_pattern.as_ref() {
                            Some(inner_pattern) => {
                                Self::match_pattern(constructor_env, inner_pattern, payload)
                            }
                            None => Some(vec![]),
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }

            // Tuple pattern
            (MatchPattern::Tuple(patterns), Value::Tuple(values)) => {
                if patterns.len() != values.len() {
                    return None;
                }
                let mut bindings = vec![];
                for (pat, val) in patterns.iter().zip(values.iter()) {
                    match Self::match_pattern(constructor_env, pat, val) {
                        Some(mut b) => bindings.append(&mut b),
                        None => return None,
                    }
                }
                Some(bindings)
            }

            _ => None,
        }
    }

    /// Evaluate an address expression to get a mutable reference to a Store.
    /// Handles Var, FieldAccess, ArrayAccess, and Proj recursively.
    fn eval_address(&mut self, ctx: &mut Context<Value>, expr: ExprNodeId) -> Rc<RefCell<Value>> {
        match expr.to_expr() {
            Expr::Var(name) => {
                // Look up the Store in the environment
                match ctx.env.lookup_cls(&name) {
                    LookupRes::Local((Value::Store(store), _))
                    | LookupRes::UpValue(_, (Value::Store(store), _))
                    | LookupRes::Global((Value::Store(store), _)) => store.clone(),
                    _ => panic!("Assignment target must be a Store-bound variable: {name}"),
                }
            }
            Expr::FieldAccess(base, field) => {
                let base_store = self.eval_address(ctx, base);
                let base_val = base_store.borrow();
                match &*base_val {
                    Value::Record(fields) => fields
                        .iter()
                        .filter_map(|(name, v)| match (name, v) {
                            (n, Value::Store(s)) if *n == field => Some(s.clone()),
                            _ => None,
                        })
                        .next()
                        .unwrap_or_else(|| panic!("Field {field} not found in record")),
                    _ => panic!("Field access on non-record type"),
                }
            }
            Expr::ArrayAccess(base, idx) => {
                let base_store = self.eval_address(ctx, base);
                let idx_val = self.eval_expr(ctx, idx);
                let base_val = base_store.borrow();
                match (&*base_val, idx_val) {
                    (Value::Array(elements), Value::Number(i)) => {
                        if let Some(Value::Store(store)) = elements.get(i as usize) {
                            store.clone()
                        } else {
                            log::error!("Array index {i} out of bounds or non-store was found");
                            Rc::new(RefCell::new(Value::Number(0.0)))
                        }
                    }
                    _ => panic!("Invalid value type detected. Possible bug in type checking."),
                }
            }
            Expr::Proj(base, idx) => {
                let base_store = self.eval_address(ctx, base);
                let base_val = base_store.borrow();
                match &*base_val {
                    Value::Tuple(elements) => {
                        if let Some(Value::Store(store)) = elements.get(idx as usize) {
                            store.clone()
                        } else {
                            panic!("Tuple index {idx} out of bounds or non-store was found");
                        }
                    }
                    _ => panic!("Projection on non-tuple type"),
                }
            }
            _ => panic!("Invalid assignment target expression"),
        }
    }

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

                // Use eval_expr to evaluate
                let v = self.eval_expr(ctx, inner);
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
            Expr::Proj(inner, idx) => {
                Expr::Proj(self.rebuild(ctx, inner), idx).into_id(e.to_location())
            }
            Expr::ArrayAccess(base, i) => {
                Expr::ArrayAccess(self.rebuild(ctx, base), self.rebuild(ctx, i))
                    .into_id(e.to_location())
            }
            Expr::FieldAccess(inner, name) => {
                Expr::FieldAccess(self.rebuild(ctx, inner), name).into_id(e.to_location())
            }
            Expr::Block(b) => {
                Expr::Block(b.map(|eid| self.rebuild(ctx, eid))).into_id(e.to_location())
            }
            Expr::Match(scrutinee, arms) => {
                let rebuilt_scrutinee = self.rebuild(ctx, scrutinee);
                let rebuilt_arms = arms
                    .into_iter()
                    .map(|arm| crate::ast::MatchArm {
                        pattern: arm.pattern,
                        body: self.rebuild(ctx, arm.body),
                    })
                    .collect();
                Expr::Match(rebuilt_scrutinee, rebuilt_arms).into_id(e.to_location())
            }
            _ => e,
        }
    }
}

pub fn create_default_interpreter(
    extern_macros: &[Box<dyn MacroFunction>],
    constructor_env: ConstructorEnv,
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
        constructor_env,
    };
    (StageInterpreter::default(), ctx)
}

/// Evaluate root expression. If the result value is not code, return original expression as is.
/// if the result value is code, this function recursively evaluate the expression inside the code until the result becomes non-code.
pub fn expand_macro(
    expr: ExprNodeId,
    top_type: TypeNodeId,
    extern_macros: &[Box<dyn MacroFunction>],
    constructor_env: ConstructorEnv,
) -> ExprNodeId {
    let (mut interpreter, mut ctx) = create_default_interpreter(extern_macros, constructor_env);
    expand_macro_rec(expr, &mut ctx, &mut interpreter, top_type)
}
fn expand_macro_rec(
    expr: ExprNodeId,
    ctx: &mut Context<Value>,
    interpreter: &mut StageInterpreter,
    ty: TypeNodeId,
) -> ExprNodeId {
    if let Type::Code(t) = ty.to_type() {
        let res = interpreter.eval_expr(ctx, expr);
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
