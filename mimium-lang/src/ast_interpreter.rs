use crate::{
    ast::{self},
    compiler::{Error as CompileError, ErrorKind},
    integer,
    interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId},
    numeric,
    pattern::{TypedId, TypedPattern},
    runtime::builtin_fn,
    string_t,
    types::{PType, Type},
    unit,
    utils::{environment::Environment, metadata::Span},
};
use itertools::Itertools;

#[derive(Debug, Clone, Copy)]
pub enum PValue {
    Unit,
    Numeric(f64),
    Integer(i64),
}

#[derive(Debug, Clone)]
pub enum Value {
    Primitive(PValue),
    String(String),
    Tuple(Vec<Value>),
    Array(Vec<Value>), // Added Array variant
    //Function value holds return type
    Function(Vec<TypedId>, ExprNodeId, Context, Option<TypeNodeId>),
    FixPoint(TypedId, ExprNodeId),
    External(Symbol),
}
impl PValue {
    pub fn get_type(&self) -> Type {
        match self {
            PValue::Unit => unit!().to_type().clone(),
            PValue::Numeric(_) => numeric!().to_type().clone(),
            PValue::Integer(_) => integer!().to_type().clone(),
        }
    }

    pub fn get_type_id(&self) -> TypeNodeId {
        self.get_type().into_id()
    }
}
impl Value {
    fn get_type_id(&self) -> TypeNodeId {
        match self {
            Value::Primitive(p) => p.get_type_id(),
            Value::String(_) => string_t!(),
            Value::Tuple(v) => Type::Tuple(v.iter().map(|t| t.get_type_id()).collect()).into_id(),
            Value::Array(v) => {
                // If array is empty, default to numeric elements
                if v.is_empty() {
                    Type::Array(numeric!()).into_id()
                } else {
                    // Assume all elements have the same type as the first one
                    Type::Array(v[0].get_type_id()).into_id()
                }
            }
            Value::Function(a, _e, _ctx, r_type) => Type::Function(
                a.iter()
                    .map(|tid| {
                        if tid.is_unknown() {
                            panic!("function argument untyped");
                        }
                        tid.ty
                    })
                    .collect(),
                r_type.expect("Return type cannot inferred"), //todo!
                None,
            )
            .into_id(),
            Value::FixPoint(tid, _) => {
                if tid.is_unknown() {
                    unit!()
                } else {
                    tid.ty
                }
            }
            //todo!
            Value::External(_id) => Type::Unknown.into_id(),
        }
    }

    fn get_type(&self) -> Type {
        self.get_type_id().to_type().clone()
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    pub env: Environment<Value>,
    pub history: (u64, Vec<PValue>),
    pub extern_syms: Vec<Symbol>,
}
impl Default for Context {
    fn default() -> Self {
        let extern_syms = EXTERN_SYMS
            .iter()
            .map(|s| s.to_symbol())
            .collect::<Vec<_>>();
        Self {
            env: Environment::new(),
            history: (0, vec![]),
            extern_syms,
        }
    }
}

const EXTERN_SYMS: [&str; 28] = [
    "neg", "add", "sub", "mult", "div", "mod", "eq", "ne", "le", "lt", "ge", "gt", "atan2", "sin",
    "cos", "not", "round", "floor", "ceil", "atan", "sqrt", "abs", "min", "max", "pow", "log",
    "print", "println",
];

fn eval_literal(e: &ast::Literal) -> Value {
    match e {
        ast::Literal::String(s) => Value::String(s.to_string()),
        ast::Literal::Int(i) => Value::Primitive(PValue::Integer(i.clone())),
        ast::Literal::Float(f) => Value::Primitive(PValue::Numeric(f.as_str().parse().unwrap())),
        ast::Literal::SelfLit => {
            panic!("self literal should not be shown in evaluation stage.")
        }
        ast::Literal::Now => {
            panic!("now literal should not be shown in evaluation stage.")
        }
        ast::Literal::SampleRate => {
            panic!("samplerate literal should not be shown in evaluation stage.")
        }
        ast::Literal::PlaceHolder => {
            panic!("_ literal should not be shown in evaluation stage.")
        }
    }
}

fn eval_condition(e: ExprNodeId, ctx: &mut Context) -> Result<bool, CompileError> {
    let c_v = eval_ast(e, ctx)?;

    match c_v.clone() {
        Value::Primitive(PValue::Numeric(f)) => Ok(f > 0.0),
        Value::Primitive(PValue::Integer(i)) => Ok(i > 0),
        _ => Err(CompileError(
            ErrorKind::TypeMismatch(numeric!().to_type().clone(), c_v.get_type()),
            e.to_span().clone(),
        )),
    }
}

fn getcell<'a, 'ctx: 'a>(ctx: &'ctx mut Context) -> &'a mut PValue {
    let history_count = &mut ctx.history.0;
    let history = &mut ctx.history.1;
    let index = *history_count as usize;
    if history.len() < index {
        history.get_mut(index).unwrap()
    } else {
        history.push(PValue::Numeric(0.0));
        history.get_mut(index).unwrap()
    }
}

fn eval_with_new_env(
    e_meta: ExprNodeId,
    ctx: &mut Context,
    names: &mut Vec<(Symbol, Value)>,
) -> Result<Value, CompileError> {
    ctx.env.extend();
    ctx.env.add_bind(names);
    let res = eval_ast(e_meta, ctx);
    ctx.env.to_outer();
    res
}

fn find_matched_builtin_fn(n: Symbol, tv: &[TypeNodeId]) -> Option<(Type, *const ())> {
    builtin_fn::get_builtin_fns()
        .iter()
        .find_map(|(name, ty, ptr)| {
            let (ty_same, rt) = if let Type::Function(tv2, rt, _) = ty.to_type() {
                (tv.eq(&tv2), rt.to_type())
            } else {
                return None;
            };
            if n == name.to_symbol() && ty_same {
                Some((rt, *ptr))
            } else {
                None
            }
        })
}

pub fn eval_extern(n: Symbol, argv: &[Value], span: Span) -> Result<Value, CompileError> {
    let tv = argv.iter().map(|v| v.get_type_id()).collect::<Vec<_>>();

    let (rt, ptr) = match find_matched_builtin_fn(n, &tv) {
        Some(x) => x,
        None => return Err(CompileError(ErrorKind::NotApplicable, span)),
    };
    match argv.len() {
        1 => {
            let v = &argv[0];
            match (rt, v) {
                //f64 -> f64
                (Type::Primitive(PType::Numeric), Value::Primitive(PValue::Numeric(fv)))
                    if v.get_type() == Type::Primitive(PType::Numeric) =>
                {
                    let f = unsafe { std::mem::transmute::<*const (), fn(f64) -> f64>(ptr) };
                    Ok(Value::Primitive(PValue::Numeric(f(*fv))))
                }
                //i64 -> i64
                (Type::Primitive(PType::Int), Value::Primitive(PValue::Integer(iv)))
                    if v.get_type() == Type::Primitive(PType::Int) =>
                {
                    let f = unsafe { std::mem::transmute::<*const (), fn(i64) -> i64>(ptr) };
                    Ok(Value::Primitive(PValue::Integer(f(*iv))))
                }
                //f64 -> ()
                (Type::Primitive(PType::Unit), Value::Primitive(PValue::Numeric(fv)))
                    if v.get_type() == Type::Primitive(PType::Numeric) =>
                {
                    let f = unsafe { std::mem::transmute::<*const (), fn(f64) -> ()>(ptr) };
                    f(*fv);
                    Ok(Value::Primitive(PValue::Unit))
                }
                //i64 -> ()
                (Type::Primitive(PType::Unit), Value::Primitive(PValue::Integer(fv)))
                    if v.get_type() == Type::Primitive(PType::Int) =>
                {
                    let f = unsafe { std::mem::transmute::<*const (), fn(i64) -> ()>(ptr) };
                    f(*fv);
                    Ok(Value::Primitive(PValue::Unit))
                }
                _ => Err(CompileError(ErrorKind::NotApplicable, span)),
            }
        }
        2 => {
            let v1 = &argv[0];
            let v2 = &argv[1];
            match (rt, v1, v2) {
                // (f64,f64)->f64
                (
                    Type::Primitive(PType::Numeric),
                    Value::Primitive(PValue::Numeric(fv1)),
                    Value::Primitive(PValue::Numeric(fv2)),
                ) => {
                    let f = unsafe { std::mem::transmute::<*const (), fn(f64, f64) -> f64>(ptr) };
                    Ok(Value::Primitive(PValue::Numeric(f(*fv1, *fv2))))
                }
                // (i64,i64)->i64
                (
                    Type::Primitive(PType::Numeric),
                    Value::Primitive(PValue::Integer(iv1)),
                    Value::Primitive(PValue::Integer(iv2)),
                ) => {
                    let f = unsafe { std::mem::transmute::<*const (), fn(i64, i64) -> i64>(ptr) };
                    Ok(Value::Primitive(PValue::Integer(f(*iv1, *iv2))))
                }
                _ => Err(CompileError(ErrorKind::NotApplicable, span)),
            }
        }
        _ => Err(CompileError(ErrorKind::NotApplicable, span)),
    }
}

pub fn eval_ast(e_meta: ExprNodeId, ctx: &mut Context) -> Result<Value, CompileError> {
    let env = &mut ctx.env;
    let span = e_meta.to_span();
    match &e_meta.to_expr() {
        ast::Expr::Literal(l) => Ok(eval_literal(l)),
        ast::Expr::Var(v) => env
            .lookup(v)
            .map(|v| v.clone())
            .or(ctx
                .extern_syms
                .iter()
                .find(|n| *n == v)
                .map(|n| Value::External(*n)))
            .ok_or(CompileError(
                ErrorKind::VariableNotFound(v.to_string()),
                span.clone(),
            )),
        ast::Expr::Block(b) => b
            .as_ref()
            .map_or(Ok(Value::Primitive(PValue::Unit)), |body| {
                eval_ast(*body, ctx)
            }),
        ast::Expr::Tuple(v) => {
            let res = v
                .iter()
                //todo: collect multiple errors
                .map(|e| eval_ast(*e, ctx).unwrap())
                .collect();
            Ok(Value::Tuple(res))
        }
        ast::Expr::Proj(t, i) => {
            let v = eval_ast(*t, ctx)?;
            let span = t.to_span();
            match v {
                Value::Tuple(t) => t.get(*i as usize).map(|v| v.clone()).ok_or(CompileError(
                    ErrorKind::IndexOutOfRange(t.len() as u16, *i as u16),
                    span.clone(),
                )),
                _ => Err(CompileError(
                    ErrorKind::IndexForNonTuple(v.get_type()),
                    span.clone(),
                )),
            }
        }
        ast::Expr::Apply(f, args) => {
            let argv: Vec<_> = args.iter().map(|e| eval_ast(*e, ctx)).try_collect()?;
            let func = eval_ast(*f, ctx)?;
            let res = match func.clone() {
                Value::Function(params, b, mut n_ctx, _rt) => {
                    let mut argvec: Vec<_> = argv
                        .iter()
                        .zip(params.iter())
                        .map(|(v, TypedId { id, .. })| (id.clone(), v.clone()))
                        .collect();
                    eval_with_new_env(b, &mut n_ctx, &mut argvec)
                }
                Value::FixPoint(TypedId { id, .. }, e) => {
                    eval_with_new_env(e, ctx, &mut vec![(id, func)])
                }
                Value::External(n) => {
                    //todo: appropreate error type
                    eval_extern(n, &argv, span.clone())
                }
                _ => Err(CompileError(ErrorKind::NotApplicable, f.to_span().clone())),
            };
            res
        }
        ast::Expr::PipeApply(_, _) => {
            panic!("|> should not be shown in evaluation stage.")
        }
        ast::Expr::Lambda(a, r, e) => Ok(Value::Function(
            a.iter().map(|tid| tid.clone()).collect(),
            *e,
            ctx.clone(), //todo! do not copy
            *r,
        )),
        ast::Expr::Feed(a, e) => {
            let cellv = *getcell(ctx);
            let res = eval_with_new_env(*e, ctx, &mut vec![(*a, Value::Primitive(cellv))])?;
            let pres = match res {
                Value::Primitive(p) => Ok(p),
                _ => Err(CompileError(ErrorKind::NonPrimitiveInFeed, span.clone())),
            }?;
            *getcell(ctx) = pres;
            ctx.history.0 += 1;
            Ok(res)
        }
        ast::Expr::Let(TypedPattern { pat, .. }, e, then) => {
            let e_v = eval_ast(*e, ctx)?;
            todo!()
            // match then {
            //     Some(t) => eval_with_new_env(t, ctx, &mut vec![(id.clone(), e_v)]),
            //     None => Ok(Value::Primitive(PValue::Unit)),
            // }
        }
        ast::Expr::LetRec(tid, e, then) => {
            let res_rec = eval_with_new_env(
                *e,
                ctx,
                &mut vec![(tid.clone().id, Value::FixPoint(tid.clone(), *e))],
            )?;
            then.as_ref()
                .map(|t| eval_with_new_env(*t, ctx, &mut vec![(tid.id.clone(), res_rec)]))
                .unwrap_or(Ok(Value::Primitive(PValue::Unit)))
        }
        ast::Expr::If(cond, then, o_else) => {
            if eval_condition(*cond, ctx)? {
                eval_ast(*then, ctx)
            } else {
                o_else
                    .map(|e_else| eval_ast(e_else, ctx))
                    .unwrap_or(Ok(Value::Primitive(PValue::Unit)))
            }
        }
        ast::Expr::Bracket(_) => todo!(),
        ast::Expr::Escape(_) => todo!(),
        ast::Expr::Error => panic!("Some Error happend in previous stages"),
        ast::Expr::Assign(_, _) => todo!(),
        ast::Expr::Then(_, _) => todo!(),
        ast::Expr::ArrayAccess(array, index) => {
            let array_v = eval_ast(*array, ctx)?;
            let index_v = eval_ast(*index, ctx)?;

            // Extract the index as a numeric value
            let idx = match index_v {
                Value::Primitive(PValue::Numeric(n)) => n as usize,
                Value::Primitive(PValue::Integer(i)) => i as usize,
                _ => return Err(CompileError(ErrorKind::TypeError, span.clone())),
            };

            // Access the array at the specified index
            match array_v {
                Value::Array(items) => {
                    if idx < items.len() {
                        Ok(items[idx].clone())
                    } else {
                        Err(CompileError(ErrorKind::IndexOutOfBounds, span.clone()))
                    }
                }
                _ => Err(CompileError(ErrorKind::TypeError, span.clone())),
            }
        }
        ast::Expr::ArrayLiteral(items) => {
            // Create a vector of evaluated items
            let mut values = Vec::new();
            for item in items {
                values.push(eval_ast(*item, ctx)?);
            }
            Ok(Value::Array(values))
        }
        _ => todo!(),
    }
}
