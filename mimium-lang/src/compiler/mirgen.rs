use super::intrinsics;
use super::typing::{InferContext, infer_root};
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::{function, numeric, unit};
pub mod convert_pronoun;
pub(crate) mod recursecheck;
use crate::mir::{self, Argument, Instruction, Mir, StateSize, VPtr, VReg, Value};

use std::sync::Arc;

use crate::types::{PType, Type};
use crate::utils::environment::{Environment, LookupRes};
use crate::utils::error::ReportableError;
use crate::utils::metadata::{Location, Span};

use crate::ast::{Expr, Literal};

// pub mod closure_convert;
// pub mod feedconvert;
// pub mod hir_solve_stage;

const DELAY_ADDITIONAL_OFFSET: u64 = 3;

#[derive(Debug, Default)]
struct ContextData {
    pub func_i: usize,
    pub current_bb: usize,
    pub next_state_offset: Option<Vec<StateSize>>,
    pub cur_state_pos: Vec<StateSize>,
    pub push_sum: Vec<StateSize>,
}

#[derive(Debug)]
struct Context {
    typeenv: InferContext,
    valenv: Environment<VPtr>,
    fn_label: Option<Symbol>,
    anonymous_fncount: u64,
    reg_count: VReg,
    program: Mir,
    data: Vec<ContextData>,
    data_i: usize,
}

impl Context {
    pub fn new(typeenv: InferContext, file_path: Option<Symbol>) -> Self {
        Self {
            typeenv,
            valenv: Environment::new(),
            program: Mir::new(file_path),
            reg_count: 0,
            fn_label: None,
            anonymous_fncount: 0,
            data: vec![ContextData::default()],
            data_i: 0,
        }
    }
    fn get_loc_from_span(&self, span: &Span) -> Location {
        Location::new(span.clone(), self.program.file_path.unwrap_or_default())
    }
    fn get_ctxdata(&mut self) -> &mut ContextData {
        self.data.get_mut(self.data_i).unwrap()
    }

    fn consume_fnlabel(&mut self) -> Symbol {
        let res = self.fn_label.unwrap_or_else(|| {
            let res = format!("lambda_{}", self.anonymous_fncount);
            self.anonymous_fncount += 1;
            res.to_symbol()
        });
        self.fn_label = None;
        res
    }

    fn get_current_fn(&mut self) -> &mut mir::Function {
        let i = self.get_ctxdata().func_i;
        &mut self.program.functions[i]
    }
    fn try_make_delay(&mut self, f: &VPtr, args: &[ExprNodeId]) -> Option<VPtr> {
        let rt = match f.as_ref() {
            Value::ExtFunction(name, ft) if *name == "delay".to_symbol() => ft,
            _ => return None,
        };

        let (max, src, time) = match args {
            [max, src, time] => (max, src, time),
            _ => return None,
        };
        match max.to_expr() {
            Expr::Literal(Literal::Float(max)) => {
                //need to evaluate args first before calculate state offset because the argument for time contains stateful function call.
                let args = self.eval_args(&[*src, *time]);
                let max_time = max.as_str().parse::<f64>().unwrap();
                let shift_size = max_time as u64 + DELAY_ADDITIONAL_OFFSET;
                self.get_current_fn().state_sizes.push(StateSize {
                    size: shift_size,
                    ty: *rt,
                });
                let coffset = self.get_ctxdata().cur_state_pos.clone();
                if !coffset.is_empty() {
                    self.get_ctxdata().push_sum.extend_from_slice(&coffset);
                    self.get_current_basicblock()
                        .0
                        .push((Arc::new(Value::None), Instruction::PushStateOffset(coffset)));
                }
                let (args, _types): (Vec<VPtr>, Vec<TypeNodeId>) = args.into_iter().unzip();
                Some(self.push_inst(Instruction::Delay(
                    max_time as u64,
                    args[0].clone(),
                    args[1].clone(),
                )))
            }
            _ => unreachable!("unbounded delay access, should be an error at typing stage."),
        }
    }
    fn make_binop_intrinsic(
        &self,
        label: Symbol,
        args: &[(VPtr, TypeNodeId)],
    ) -> Option<Instruction> {
        debug_assert_eq!(args.len(), 2);
        let a0 = args[0].0.clone();
        let a1 = args[1].0.clone();
        match label.as_str() {
            intrinsics::ADD => Some(Instruction::AddF(a0, a1)),
            intrinsics::SUB => Some(Instruction::SubF(a0, a1)),
            intrinsics::MULT => Some(Instruction::MulF(a0, a1)),
            intrinsics::DIV => Some(Instruction::DivF(a0, a1)),
            intrinsics::POW => Some(Instruction::PowF(a0, a1)),
            intrinsics::MODULO => Some(Instruction::ModF(a0, a1)),
            intrinsics::GT => Some(Instruction::Gt(a0, a1)),
            intrinsics::GE => Some(Instruction::Ge(a0, a1)),
            intrinsics::LT => Some(Instruction::Lt(a0, a1)),
            intrinsics::LE => Some(Instruction::Le(a0, a1)),
            intrinsics::EQ => Some(Instruction::Eq(a0, a1)),
            intrinsics::NE => Some(Instruction::Ne(a0, a1)),
            intrinsics::AND => Some(Instruction::And(a0, a1)),
            intrinsics::OR => Some(Instruction::Or(a0, a1)),
            _ => None,
        }
    }
    fn make_uniop_intrinsic(
        &mut self,
        label: Symbol,
        args: &[(VPtr, TypeNodeId)],
    ) -> Option<Instruction> {
        debug_assert_eq!(args.len(), 1);
        let a0 = args[0].0.clone();
        let a0_ty = args[0].1;
        match label.as_str() {
            intrinsics::NEG => Some(Instruction::NegF(a0)),
            intrinsics::SQRT => Some(Instruction::SqrtF(a0)),
            intrinsics::LOG => Some(Instruction::LogF(a0)),
            intrinsics::ABS => Some(Instruction::AbsF(a0)),
            intrinsics::SIN => Some(Instruction::SinF(a0)),
            intrinsics::COS => Some(Instruction::CosF(a0)),
            intrinsics::MEM => {
                self.get_current_fn()
                    .state_sizes
                    .push(StateSize { size: 1, ty: a0_ty });
                Some(Instruction::Mem(a0))
            }
            _ => None,
        }
    }

    fn make_intrinsics(&mut self, label: Symbol, args: &[(VPtr, TypeNodeId)]) -> Option<VPtr> {
        let inst = match args.len() {
            1 => self.make_uniop_intrinsic(label, args),
            2 => self.make_binop_intrinsic(label, args),
            _ => return None,
        };
        inst.map(|i| self.push_inst(i))
    }
    fn get_current_basicblock(&mut self) -> &mut mir::Block {
        let bbid = self.get_ctxdata().current_bb;
        self.get_current_fn()
            .body
            .get_mut(bbid)
            .expect("no basic block found")
    }
    fn add_new_basicblock(&mut self) {
        let idx = self.get_current_fn().add_new_basicblock();
        self.get_ctxdata().current_bb = idx;
    }
    fn gen_new_register(&mut self) -> VPtr {
        let res = Arc::new(Value::Register(self.reg_count));
        self.reg_count += 1;
        res
    }
    fn push_inst(&mut self, inst: Instruction) -> VPtr {
        let res = self.gen_new_register();
        self.get_current_basicblock().0.push((res.clone(), inst));
        res
    }
    fn add_bind(&mut self, bind: (Symbol, VPtr)) {
        self.valenv.add_bind(&[bind]);
    }
    fn add_bind_pattern(
        &mut self,
        pattern: &TypedPattern,
        v: VPtr,
        ty: TypeNodeId,
        is_global: bool,
    ) {
        let TypedPattern { pat, .. } = pattern;
        let span = pattern.to_span();
        match (pat, ty.to_type()) {
            (Pattern::Single(id), t) => {
                if is_global && !matches!(v.as_ref(), Value::Function(_)) {
                    let gv = Arc::new(Value::Global(v.clone()));
                    if t.is_function() {
                        //globally allocated closures are immidiately closed, not to be disposed
                        self.push_inst(Instruction::CloseUpValues(v.clone(), ty));
                    }
                    self.push_inst(Instruction::SetGlobal(gv.clone(), v.clone(), ty));
                    self.add_bind((*id, gv))
                } else {
                    self.add_bind((*id, v))
                }
            }
            (Pattern::Tuple(patterns), Type::Tuple(tvec)) => {
                for ((i, pat), cty) in patterns.iter().enumerate().zip(tvec.iter()) {
                    let elem_v = self.push_inst(Instruction::GetElement {
                        value: v.clone(),
                        ty,
                        tuple_offset: i as u64,
                    });
                    let tid = Type::Unknown.into_id_with_location(self.get_loc_from_span(&span));
                    let tpat = TypedPattern {
                        pat: pat.clone(),
                        ty: tid,
                    };
                    self.add_bind_pattern(&tpat, elem_v, *cty, is_global);
                }
            }
            (Pattern::Record(patterns), Type::Record(kvvec)) => {
                for (k, pat) in patterns.iter() {
                    let i = kvvec.iter().position(|(kk, _)| kk == k);
                    if let Some(offset) = i {
                        let elem_v = self.push_inst(Instruction::GetElement {
                            value: v.clone(),
                            ty,
                            tuple_offset: offset as u64,
                        });
                        let tid =
                            Type::Unknown.into_id_with_location(self.get_loc_from_span(&span));
                        let tpat = TypedPattern {
                            pat: pat.clone(),
                            ty: tid,
                        };
                        let elem_t = kvvec[offset].1;
                        self.add_bind_pattern(&tpat, elem_v, elem_t, is_global);
                    };
                }
            }
            _ => {
                panic!("typing error in the previous stage")
            }
        }
    }
    fn make_new_function(
        &mut self,
        name: Symbol,
        args: &[VPtr],
        argtypes: &[TypeNodeId],
        parent_i: Option<usize>,
    ) -> usize {
        let index = self.program.functions.len();
        let newf = mir::Function::new(index, name, args, argtypes, parent_i);
        self.program.functions.push(newf);
        index
    }
    fn do_in_child_ctx<F: FnMut(&mut Self, usize) -> (VPtr, TypeNodeId)>(
        &mut self,
        fname: Symbol,
        abinds: &[(Symbol, VPtr)],
        types: &[TypeNodeId],
        mut action: F,
    ) -> (usize, VPtr) {
        self.valenv.extend();
        self.valenv.add_bind(abinds);
        let args = abinds.iter().map(|(_, a)| a.clone()).collect::<Vec<_>>();
        let label = self.get_ctxdata().func_i;
        let c_idx = self.make_new_function(fname, &args, types, Some(label));

        self.data.push(ContextData {
            func_i: c_idx,
            ..Default::default()
        });
        self.data_i += 1;
        //do action
        let (fptr, ty) = action(self, c_idx);

        // TODO: ideally, type should be infered before the actual action
        let f = self.program.functions.get_mut(c_idx).unwrap();
        f.return_type.get_or_init(|| ty);

        //post action
        let _ = self.data.pop();
        self.data_i -= 1;
        self.valenv.to_outer();
        (c_idx, fptr)
    }
    fn lookup(&self, key: &Symbol) -> LookupRes<VPtr> {
        match self.valenv.lookup_cls(key) {
            LookupRes::Local(v) => LookupRes::Local(v.clone()),
            LookupRes::UpValue(level, v) => LookupRes::UpValue(level, v.clone()),
            LookupRes::Global(v) => LookupRes::Global(v.clone()),
            LookupRes::None => LookupRes::None,
        }
    }

    pub fn eval_literal(&mut self, lit: &Literal, _span: &Span) -> VPtr {
        let v = match lit {
            Literal::String(s) => self.push_inst(Instruction::String(*s)),
            Literal::Int(i) => self.push_inst(Instruction::Integer(*i)),
            Literal::Float(f) => self.push_inst(Instruction::Float(
                f.as_str().parse::<f64>().expect("illegal float format"),
            )),
            Literal::Now => {
                let ftype = numeric!();
                let fntype = function!(vec![], ftype);
                let getnow = Arc::new(Value::ExtFunction("_mimium_getnow".to_symbol(), fntype));
                self.push_inst(Instruction::CallCls(getnow, vec![], ftype))
            }
            Literal::SampleRate => {
                let ftype = numeric!();
                let fntype = function!(vec![], ftype);
                let samplerate = Arc::new(Value::ExtFunction(
                    "_mimium_getsamplerate".to_symbol(),
                    fntype,
                ));
                self.push_inst(Instruction::CallCls(samplerate, vec![], ftype))
            }
            Literal::SelfLit | Literal::PlaceHolder => unreachable!(),
        };
        v
    }
    fn eval_rvar(&mut self, name: Symbol, t: TypeNodeId, span: &Span) -> VPtr {
        let loc = self.get_loc_from_span(span);
        log::trace!("rv t:{} {}", name.to_string(), t.to_type());
        let v = match self.lookup(&name) {
            LookupRes::Local(v) => match v.as_ref() {
                Value::Function(i) => {
                    let reg = self.push_inst(Instruction::Uinteger(*i as u64));
                    self.push_inst(Instruction::Closure(reg))
                }
                _ => self.push_inst(Instruction::Load(v.clone(), t)),
            },
            LookupRes::UpValue(level, v) => {
                (0..level)
                    .rev()
                    .fold(v.clone(), |upv, i| match upv.as_ref() {
                        Value::Function(_fi) => v.clone(),
                        _ => {
                            let res = self.gen_new_register();
                            let current = self.data.get_mut(self.data_i - i).unwrap();
                            let currentf = self.program.functions.get_mut(current.func_i).unwrap();
                            let upi = currentf.get_or_insert_upvalue(&upv) as _;
                            let currentbb = currentf.body.get_mut(current.current_bb).unwrap();
                            currentbb
                                .0
                                .push((res.clone(), Instruction::GetUpValue(upi, t)));
                            res
                        }
                    })
            }
            LookupRes::Global(v) => match v.as_ref() {
                Value::Global(_gv) => self.push_inst(Instruction::GetGlobal(v.clone(), t)),
                Value::Function(_) | Value::Register(_) => v.clone(),
                _ => unreachable!("non global_value"),
            },
            LookupRes::None => {
                let ty = self
                    .typeenv
                    .lookup(name, loc)
                    .expect("variable not found. it should be detected at type checking stage");
                Arc::new(Value::ExtFunction(name, ty))
            }
        };
        v
    }
    fn eval_assign(&mut self, assignee: ExprNodeId, src: VPtr, t: TypeNodeId, _span: &Span) {
        let name = match assignee.to_expr() {
            Expr::Var(v) => v,
            Expr::ArrayAccess(_, _) => {
                unimplemented!("Assignment to array is not implemented yet.")
            }
            _ => unreachable!(),
        };
        match self.lookup(&name) {
            LookupRes::Local(v) => match v.as_ref() {
                Value::Argument(_i, _a) => {
                    //todo: collect warning for the language server
                    log::warn!(
                        "assignment to argument {name} does not affect to the external environments."
                    );
                    self.push_inst(Instruction::Store(v.clone(), src, t));
                }
                _ => {
                    self.push_inst(Instruction::Store(v.clone(), src, t));
                }
            },
            LookupRes::UpValue(_level, upv) => {
                //todo: nested closure
                let currentf = self.get_current_fn();
                let upi = currentf.get_or_insert_upvalue(&upv) as _;
                self.push_inst(Instruction::SetUpValue(upi, src, t));
            }
            LookupRes::Global(dst) => match dst.as_ref() {
                Value::Global(_gv) => {
                    self.push_inst(Instruction::SetGlobal(dst.clone(), src.clone(), t));
                }
                _ => unreachable!("non global_value"),
            },
            LookupRes::None => {
                unreachable!("invalid value assignment")
            }
        }
    }
    fn emit_fncall(&mut self, idx: u64, args: Vec<(VPtr, TypeNodeId)>, ret_t: TypeNodeId) -> VPtr {
        // stack size of the function to be called
        let state_sizes = self.program.functions[idx as usize].state_sizes.clone();

        if let Some(offset) = self.get_ctxdata().next_state_offset.take() {
            self.get_ctxdata().push_sum.extend_from_slice(&offset);
            //insert pushstateoffset
            self.get_current_basicblock()
                .0
                .push((Arc::new(Value::None), Instruction::PushStateOffset(offset)));
        }

        let f = {
            self.get_current_fn()
                .state_sizes
                .extend_from_slice(&state_sizes);
            self.push_inst(Instruction::Uinteger(idx))
        };

        let res = self.push_inst(Instruction::Call(f.clone(), args, ret_t));

        if !state_sizes.is_empty() {
            self.get_ctxdata()
                .cur_state_pos
                .extend_from_slice(&state_sizes);

            self.get_ctxdata().next_state_offset = Some(state_sizes);
        }

        res
    }
    fn eval_args(&mut self, args: &[ExprNodeId]) -> Vec<(VPtr, TypeNodeId)> {
        args.iter()
            .map(|a_meta| {
                let (v, t) = self.eval_expr(*a_meta);
                let res = match v.as_ref() {
                    Value::Function(idx) => {
                        let f = self.push_inst(Instruction::Uinteger(*idx as u64));
                        self.push_inst(Instruction::Closure(f))
                    }
                    _ => v.clone(),
                };
                if t.to_type().contains_function() {
                    //higher-order function need to close immidiately
                    self.push_inst(Instruction::CloseUpValues(res.clone(), t));
                }
                (res, t)
            })
            .collect()
    }
    fn eval_block(&mut self, block: Option<ExprNodeId>) -> (VPtr, TypeNodeId) {
        self.add_new_basicblock();
        let (e, rt) = match block {
            Some(e) => self.eval_expr(e),
            None => (Arc::new(Value::None), unit!()),
        };
        //if returning non-closure function, make closure
        let e = match e.as_ref() {
            Value::Function(idx) => {
                let cpos = self.push_inst(Instruction::Uinteger(*idx as u64));
                self.push_inst(Instruction::Closure(cpos))
            }
            _ => e,
        };
        (e, rt)
    }
    fn alloc_aggregates(&mut self, items: &[ExprNodeId], ty: TypeNodeId) -> (VPtr, TypeNodeId) {
        let len = items.len();
        if len == 0 {
            unreachable!("0-length tuple is not supported");
        }
        let alloc_insert_point = self.get_current_basicblock().0.len();
        let dst = self.gen_new_register();
        for (i, e) in items.iter().enumerate() {
            let (v, elem_ty) = self.eval_expr(*e);
            let ptr = self.push_inst(Instruction::GetElement {
                value: dst.clone(),
                ty, // lazyly set after loops,
                tuple_offset: i as u64,
            });

            self.push_inst(Instruction::Store(ptr, v, elem_ty));
        }
        self.get_current_basicblock()
            .0
            .insert(alloc_insert_point, (dst.clone(), Instruction::Alloc(ty)));

        // pass only the head of the tuple, and the length can be known
        // from the type information.
        (dst, ty)
    }
    pub fn eval_expr(&mut self, e: ExprNodeId) -> (VPtr, TypeNodeId) {
        let span = e.to_span();
        let ty = self.typeenv.lookup_res(e);
        match &e.to_expr() {
            Expr::Literal(lit) => {
                let v = self.eval_literal(lit, &span);
                let t = InferContext::infer_type_literal(lit)
                    .expect("should be an error at type checker stage");
                (v, t)
            }
            Expr::Var(name) => (self.eval_rvar(*name, ty, &span), ty),
            Expr::Block(b) => {
                if let Some(block) = b {
                    self.eval_expr(*block)
                } else {
                    (Arc::new(Value::None), unit!())
                }
            }
            Expr::Tuple(items) => self.alloc_aggregates(items, ty),
            Expr::Proj(tup, idx) => {
                let i = *idx as usize;
                let (tup_v, tup_ty) = self.eval_expr(*tup);
                let elem_ty = match tup_ty.to_type() {
                    Type::Tuple(tys) if i < tys.len() => tys[i],
                    _ => panic!(
                        "expected tuple type for projection,perhaps type error bugs in the previous stage"
                    ),
                };
                let res = self.push_inst(Instruction::GetElement {
                    value: tup_v.clone(),
                    ty: tup_ty,
                    tuple_offset: i as u64,
                });
                (res, elem_ty)
            }
            Expr::RecordLiteral(fields) => {
                self.alloc_aggregates(&fields.iter().map(|f| f.expr).collect::<Vec<_>>(), ty)
            }
            Expr::ImcompleteRecord(fields) => {
                // For incomplete records, we also aggregate the available fields
                // The default values will be handled in the type system and during record construction
                self.alloc_aggregates(&fields.iter().map(|f| f.expr).collect::<Vec<_>>(), ty)
            }
            Expr::FieldAccess(expr, key) => {
                let (expr_v, expr_ty) = self.eval_expr(*expr);
                match expr_ty.to_type() {
                    Type::Record(fields) => {
                        let offset = fields
                            .iter()
                            .position(|(k, _)| *k == *key)
                            .expect("field access to non-existing field");

                        let res = self.push_inst(Instruction::GetElement {
                            value: expr_v.clone(),
                            ty: expr_ty,
                            tuple_offset: offset as u64,
                        });
                        (res, fields[offset].1)
                    }
                    _ => panic!("expected record type for field access"),
                }
            }
            Expr::ArrayLiteral(items) => {
                // For now, handle array literals similar to tuples

                let (values, tys): (Vec<_>, Vec<_>) = items
                    .iter()
                    .map(|item| {
                        let (v, t) = self.eval_expr(*item);
                        (v, t)
                    })
                    .unzip();
                // Assume all array elements have the same type (first element's type)
                debug_assert!(tys.windows(2).all(|w| w[0] == w[1]));
                let elem_ty = if !tys.is_empty() { tys[0] } else { numeric!() };
                let reg = self.push_inst(Instruction::Array(values.clone(), elem_ty));
                (reg, Type::Array(elem_ty).into_id())
            }
            Expr::ArrayAccess(array, index) => {
                let (array_v, array_ty) = self.eval_expr(*array);
                let (index_v, _) = self.eval_expr(*index);
                let elem_ty = match array_ty.to_type() {
                    Type::Array(elem_ty) => elem_ty,
                    _ => panic!("Expected array type for array access"),
                };
                // Get element at the specified index
                let result = self.push_inst(Instruction::GetArrayElem(
                    array_v.clone(),
                    index_v.clone(),
                    elem_ty,
                ));
                (result, elem_ty)
            }

            Expr::Apply(f, args) => {
                let (f, ft) = self.eval_expr(*f);
                let del = self.try_make_delay(&f, args);
                if let Some(d) = del {
                    (d, numeric!())
                } else {
                    // Get function parameter info
                    let (param_types, rt) = if let Type::Function(params, rt, _) = ft.to_type() {
                        (params, rt)
                    } else {
                        panic!("non function type {} {} ", ft.to_type(), ty.to_type());
                    };

                    // Handle parameter packing/unpacking if needed
                    let atvvec = if args.len() == 1 && param_types.get_as_slice().len() > 1 {
                        let (arg_val, ty) = self.eval_expr(args[0]);
                        log::trace!("Unpacking argument for {:?}", ty);
                        // Check if the argument is a tuple or record that we need to unpack
                        match ty.to_type() {
                            Type::Tuple(tys) => tys
                                .into_iter()
                                .enumerate()
                                .map(|(i, t)| {
                                    let elem_val = self.push_inst(Instruction::GetElement {
                                        value: arg_val.clone(),
                                        ty,
                                        tuple_offset: i as u64,
                                    });
                                    (elem_val, t)
                                })
                                .collect(),
                            Type::Record(kvs) => param_types.get_as_slice().iter().map(|param|{
                                kvs.iter().enumerate().find(|(_i,(k, _))| param.label.is_some_and(| l| l == *k))
                                    .map_or_else(
                                        || unreachable!("parameter pack failed, possible type inference bug"),
                                        |(i,(_, t))| {
                                            let field_val = self.push_inst(Instruction::GetElement {
                                                value: arg_val.clone(),
                                                ty,
                                                tuple_offset: i as u64,
                                            });
                                            (field_val, *t)
                                        },
                                    )
                            }).collect(),
                            _ => vec![self.eval_expr(args[0])],
                        }
                    } else {
                        self.eval_args(args)
                    };

                    let res = match f.as_ref() {
                        Value::Global(v) => match v.as_ref() {
                            Value::Function(idx) => {
                                self.emit_fncall(*idx as u64, atvvec.clone(), rt)
                            }
                            Value::Register(_) => {
                                self.push_inst(Instruction::CallCls(v.clone(), atvvec.clone(), rt))
                            }
                            _ => {
                                panic!("calling non-function global value")
                            }
                        },
                        Value::Register(_) => {
                            //closure
                            //do not increment state size for closure
                            self.push_inst(Instruction::CallCls(f.clone(), atvvec.clone(), rt))
                        }
                        Value::Function(idx) => self.emit_fncall(*idx as u64, atvvec.clone(), rt),
                        Value::ExtFunction(label, _ty) => {
                            if let Some(res) = self.make_intrinsics(*label, &atvvec) {
                                res
                            } else {
                                self.push_inst(Instruction::Call(f.clone(), atvvec.clone(), rt))
                            }
                        }
                        // Value::ExternalClosure(i) => todo!(),
                        Value::None => unreachable!(),
                        _ => todo!(),
                    };
                    (res, rt)
                }
            }
            Expr::PipeApply(_, _) => unreachable!(),
            Expr::Lambda(ids, _rett, body) => {
                let (atypes, rt) = match ty.to_type() {
                    Type::Function(atypes, rt, _) => (atypes.ty_iter().collect::<Vec<_>>(), rt),
                    _ => panic!(),
                };
                let binds = ids
                    .iter()
                    .enumerate()
                    .zip(atypes.iter())
                    .map(|((idx, name), t)| {
                        let label = name.id;
                        let a = Argument(label, *t);
                        (label, Arc::new(Value::Argument(idx, Arc::new(a))))
                    })
                    .collect::<Vec<_>>();

                let name = self.consume_fnlabel();
                let (c_idx, f) = self.do_in_child_ctx(name, &binds, &atypes, |ctx, c_idx| {
                    let (res, _) = ctx.eval_expr(*body);

                    let push_sum = ctx.get_ctxdata().push_sum.clone();
                    if !push_sum.is_empty() {
                        ctx.get_current_basicblock().0.push((
                            Arc::new(mir::Value::None),
                            Instruction::PopStateOffset(push_sum),
                        )); //todo:offset size
                    }
                    match (res.as_ref(), rt.to_type()) {
                        (_, Type::Primitive(PType::Unit)) => {
                            let _ = ctx.push_inst(Instruction::Return(Arc::new(Value::None), rt));
                        }
                        (Value::State(v), _) => {
                            let _ = ctx.push_inst(Instruction::ReturnFeed(v.clone(), rt));
                        }
                        (Value::Function(i), _) => {
                            let idx = ctx.push_inst(Instruction::Uinteger(*i as u64));
                            let cls = ctx.push_inst(Instruction::Closure(idx));
                            let _ = ctx.push_inst(Instruction::CloseUpValues(cls.clone(), rt));
                            let _ = ctx.push_inst(Instruction::Return(cls, rt));
                        }
                        (_, _) => {
                            if rt.to_type().contains_function() {
                                let _ = ctx.push_inst(Instruction::CloseUpValues(res.clone(), rt));
                                let _ = ctx.push_inst(Instruction::Return(res.clone(), rt));
                            } else {
                                let _ = ctx.push_inst(Instruction::Return(res.clone(), rt));
                            }
                        }
                    };

                    let f = Arc::new(Value::Function(c_idx));
                    (f, rt)
                });
                let child = self.program.functions.get_mut(c_idx).unwrap();
                let res = if child.upindexes.is_empty() {
                    //todo:make Closure
                    f
                } else {
                    let idxcell = self.push_inst(Instruction::Uinteger(c_idx as u64));
                    self.push_inst(Instruction::Closure(idxcell))
                };
                (res, ty)
            }
            Expr::Feed(id, expr) => {
                //set typesize lazily
                let statesize = StateSize { size: 1, ty };
                let res = self.push_inst(Instruction::GetState(ty));
                self.get_ctxdata().cur_state_pos.push(statesize);
                self.add_bind((*id, res.clone()));
                self.get_ctxdata().next_state_offset = Some(vec![statesize]);
                let (retv, _t) = self.eval_expr(*expr);
                self.get_current_fn().state_sizes.push(statesize);
                (Arc::new(Value::State(retv)), ty)
            }
            Expr::Let(pat, body, then) => {
                if let Ok(tid) = TypedId::try_from(pat.clone()) {
                    self.fn_label = Some(tid.id);
                    log::trace!(
                        "mirgen let {}",
                        self.fn_label.map_or("".to_string(), |s| s.to_string())
                    )
                };
                let insert_bb = self.get_ctxdata().current_bb;
                let insert_pos = if self.program.functions.is_empty() {
                    0
                } else {
                    self.get_current_basicblock().0.len()
                };
                let (bodyv, t) = self.eval_expr(*body);
                //todo:need to boolean and insert cast
                self.fn_label = None;

                match (
                    self.get_ctxdata().func_i == 0,
                    matches!(bodyv.as_ref(), Value::Function(_)),
                    then,
                ) {
                    (false, false, Some(then_e)) => {
                        let alloc_res = self.gen_new_register();
                        let block = &mut self.get_current_fn().body.get_mut(insert_bb).unwrap().0;
                        block.insert(insert_pos, (alloc_res.clone(), Instruction::Alloc(t)));
                        let _ =
                            self.push_inst(Instruction::Store(alloc_res.clone(), bodyv.clone(), t));
                        self.add_bind_pattern(pat, alloc_res, t, false);
                        self.eval_expr(*then_e)
                    }
                    (is_global, _, Some(then_e)) => {
                        self.add_bind_pattern(pat, bodyv, t, is_global);
                        self.eval_expr(*then_e)
                    }
                    (_, _, None) => (Arc::new(Value::None), unit!()),
                }
            }
            Expr::LetRec(id, body, then) => {
                let is_global = self.get_ctxdata().func_i == 0;
                self.fn_label = Some(id.id);
                let nextfunid = self.program.functions.len();
                let t = self.typeenv.lookup_res(e);
                let v = if is_global {
                    Arc::new(Value::Function(nextfunid))
                } else {
                    self.push_inst(Instruction::Alloc(t))
                };
                let bind = (id.id, v.clone());
                self.add_bind(bind);
                let (b, _bt) = self.eval_expr(*body);
                if !is_global {
                    let _ = self.push_inst(Instruction::Store(v.clone(), b.clone(), t));
                }
                if let Some(then_e) = then {
                    self.eval_expr(*then_e)
                } else {
                    (Arc::new(Value::None), unit!())
                }
            }

            Expr::Assign(assignee, body) => {
                let (src, ty) = self.eval_expr(*body);
                self.eval_assign(*assignee, src, ty, &span);
                (Arc::new(Value::None), unit!())
            }
            Expr::Then(body, then) => {
                let _ = self.eval_expr(*body);
                match then {
                    Some(t) => self.eval_expr(*t),
                    None => (Arc::new(Value::None), unit!()),
                }
            }
            Expr::If(cond, then, else_) => {
                let (c, _) = self.eval_expr(*cond);
                let cond_bidx = self.get_ctxdata().current_bb;

                // This is just a placeholder. At this point, the locations of
                // the block are not determined yet. These 0s will be
                // overwritten later.
                let _ = self.push_inst(Instruction::JmpIf(c, 0, 0, 0));

                //insert then block
                let then_bidx = cond_bidx + 1;
                let (t, _) = self.eval_block(Some(*then));
                //jmp to ret is inserted in bytecodegen
                //insert else block
                let else_bidx = self.get_ctxdata().current_bb + 1;
                let (e, _) = self.eval_block(*else_);
                //insert return block
                self.add_new_basicblock();
                let res = self.push_inst(Instruction::Phi(t, e));
                let phi_bidx = self.get_ctxdata().current_bb;

                // overwrite JmpIf
                let jmp_if = self
                    .get_current_fn()
                    .body
                    .get_mut(cond_bidx)
                    .expect("no basic block found")
                    .0
                    .last_mut()
                    .expect("the block contains no inst?");
                match &mut jmp_if.1 {
                    Instruction::JmpIf(_, then_dst, else_dst, phi_dst) => {
                        *then_dst = then_bidx as _;
                        *else_dst = else_bidx as _;
                        *phi_dst = phi_bidx as _;
                    }
                    _ => panic!("the last block should be Jmp"),
                }

                (res, ty)
            }
            Expr::Bracket(_) => todo!(),
            Expr::Escape(_) => todo!(),
            Expr::Error => {
                self.push_inst(Instruction::Error);
                (Arc::new(Value::None), unit!())
            }
        }
    }
}
/// Generate MIR from AST.
/// The input ast (`root_expr_id`) should contain global context. (See [[compiler::parser::add_global_context]].)
/// MIR generator itself does not emit any error, the any compile errors are analyzed before generating MIR, mostly in type checker.
pub fn compile(
    root_expr_id: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    file_path: Option<Symbol>,
) -> Result<Mir, Vec<Box<dyn ReportableError>>> {
    let ast2 = recursecheck::convert_recurse(root_expr_id, file_path.unwrap_or_default());
    let (expr2, convert_errs) =
        convert_pronoun::convert_pronoun(ast2, file_path.unwrap_or_default());
    let infer_ctx = infer_root(expr2, builtin_types, file_path.unwrap_or_default());
    let errors = infer_ctx
        .errors
        .iter()
        .cloned()
        .map(|e| -> Box<dyn ReportableError> { Box::new(e) })
        .chain(
            convert_errs
                .into_iter()
                .map(|e| -> Box<dyn ReportableError> { Box::new(e) }),
        )
        .collect::<Vec<_>>();

    if errors.is_empty() {
        let mut ctx = Context::new(infer_ctx, file_path);
        let _res = ctx.eval_expr(expr2);
        ctx.program.file_path = file_path;
        Ok(ctx.program.clone())
    } else {
        Err(errors)
    }
}

// #[cfg(test)]
// mod test;
