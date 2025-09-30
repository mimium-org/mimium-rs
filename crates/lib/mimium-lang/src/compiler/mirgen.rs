use super::intrinsics;
use super::typing::{InferContext, infer_root};

use crate::compiler::parser;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::plugin::MacroFunction;
use crate::utils::miniprint::MiniPrint;
use crate::{function, interpreter, numeric, unit};
pub mod convert_pronoun;
pub(crate) mod recursecheck;
// use super::pattern_destructor::destruct_let_pattern;
use crate::mir::{self, Argument, Instruction, Mir, StateSize, VPtr, VReg, Value};
use state_tree::tree::StateTreeSkeleton;

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::types::{PType, RecordTypeField, Type};
use crate::utils::environment::{Environment, LookupRes};
use crate::utils::error::ReportableError;
use crate::utils::metadata::{GLOBAL_LABEL, Location, Span};

use crate::ast::{Expr, Literal};

// pub mod closure_convert;
// pub mod feedconvert;
// pub mod hir_solve_stage;
type StateSkeleton = StateTreeSkeleton<TypeNodeId>;

const DELAY_ADDITIONAL_OFFSET: u64 = 3;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
struct FunctionId(pub u64);
#[derive(Debug, Default)]
struct ContextData {
    pub func_i: FunctionId,
    pub current_bb: usize,
    pub next_state_offset: Option<Vec<StateSize>>,
    pub push_sum: Vec<StateSize>,
}
#[derive(Debug, Default, Clone)]
struct DefaultArgData {
    pub name: Symbol,
    pub fid: FunctionId,
    pub ty: TypeNodeId,
}

#[derive(Debug)]
struct Context {
    typeenv: InferContext,
    valenv: Environment<VPtr>,
    fn_label: Option<Symbol>,
    anonymous_fncount: u64,
    reg_count: VReg,
    program: Mir,
    default_args_map: BTreeMap<FunctionId, Vec<DefaultArgData>>,
    data: Vec<ContextData>,
    data_i: usize,
}
enum AssignDestination {
    Local(VPtr),
    UpValue(u64, VPtr),
    Global(VPtr),
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
            default_args_map: BTreeMap::new(),
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
        let i = self.get_ctxdata().func_i.0 as usize;
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
                let delay_state_size = StateSize {
                    size: shift_size,
                    ty: *rt,
                };
                self.get_current_fn().state_sizes.push(delay_state_size);
                self.get_current_fn()
                    .push_state_skeleton(StateSkeleton::Delay {
                        len: max_time as u64,
                    });
                if let Some(offset) = self.get_ctxdata().next_state_offset.take() {
                    self.get_ctxdata().push_sum.extend_from_slice(&offset);
                    self.get_current_basicblock().0.push((
                        Arc::new(Value::None),
                        Instruction::PushStateOffset(offset.clone()),
                    ));
                    self.get_ctxdata().next_state_offset = Some(vec![delay_state_size]);
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
                self.get_current_fn()
                    .push_state_skeleton(StateSkeleton::Mem(numeric!()));
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
                } else if id.as_str() != GLOBAL_LABEL {
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
                    let tpat = TypedPattern::new(pat.clone(), tid);
                    self.add_bind_pattern(&tpat, elem_v, *cty, is_global);
                }
            }
            (Pattern::Record(patterns), Type::Record(kvvec)) => {
                for (k, pat) in patterns.iter() {
                    let i = kvvec
                        .iter()
                        .position(|RecordTypeField { key, .. }| key == k);
                    if let Some(offset) = i {
                        let elem_v = self.push_inst(Instruction::GetElement {
                            value: v.clone(),
                            ty,
                            tuple_offset: offset as u64,
                        });
                        let tid =
                            Type::Unknown.into_id_with_location(self.get_loc_from_span(&span));
                        let tpat = TypedPattern::new(pat.clone(), tid);
                        let elem_t = kvvec[offset].ty;
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
        args: &[Argument],
        parent_i: Option<FunctionId>,
    ) -> FunctionId {
        let index = self.program.functions.len();
        let newf = mir::Function::new(index, name, args, parent_i.map(|FunctionId(f)| f as _));
        self.program.functions.push(newf);
        FunctionId(index as _)
    }

    fn do_in_child_ctx<F: FnMut(&mut Self, FunctionId) -> (VPtr, TypeNodeId)>(
        &mut self,
        fname: Symbol,
        abinds: &[(Symbol, TypeNodeId, Option<ExprNodeId>)],
        mut action: F,
    ) -> (FunctionId, VPtr) {
        self.valenv.extend();
        self.valenv.add_bind(
            abinds
                .iter()
                .enumerate()
                .map(|(i, (s, _ty, _default))| (*s, Arc::new(Value::Argument(i))))
                .collect::<Vec<_>>()
                .as_slice(),
        );
        let args = abinds
            .iter()
            .map(|(s, ty, _default)| Argument(*s, *ty))
            .collect::<Vec<_>>();
        let parent_i = self.get_ctxdata().func_i;
        let c_idx = self.make_new_function(fname, &args, Some(parent_i));

        let def_args = abinds
            .iter()
            .filter_map(|(s, ty, default)| {
                default.map(|d| DefaultArgData {
                    name: *s,
                    fid: self.new_default_args_getter(c_idx, *s, d),
                    ty: *ty,
                })
            })
            .collect::<Vec<_>>();
        self.default_args_map.insert(c_idx, def_args);

        self.data.push(ContextData {
            func_i: c_idx,
            ..Default::default()
        });
        self.data_i += 1;
        //do action
        let (fptr, ty) = action(self, c_idx);

        // TODO: ideally, type should be infered before the actual action
        let f = self.program.functions.get_mut(c_idx.0 as usize).unwrap();
        f.return_type.get_or_init(|| ty);

        //post action
        let _ = self.data.pop();
        self.data_i -= 1;
        log::trace!("end of lexical scope {fname}");
        self.valenv.to_outer();
        (c_idx, fptr)
    }
    fn get_default_args_getter_name(name: Symbol, fid: FunctionId) -> Symbol {
        format!("__default_{}_{name}", fid.0).to_symbol()
    }
    fn new_default_args_getter(
        &mut self,
        fid: FunctionId,
        name: Symbol,
        e: ExprNodeId,
    ) -> FunctionId {
        let (fid, _v) = self.do_in_child_ctx(
            Self::get_default_args_getter_name(name, fid),
            &[],
            |ctx, c_idx| {
                let (v, ty) = ctx.eval_expr(e);
                let _v = ctx.push_inst(Instruction::Return(v, ty));
                let f = Arc::new(Value::Function(c_idx.0 as usize));
                (f, ty)
            },
        );
        fid
    }
    fn get_default_arg_call(&mut self, name: Symbol, fid: FunctionId) -> Option<VPtr> {
        let args = self.default_args_map.get(&fid);
        args.cloned().and_then(|defv_fn_ids| {
            defv_fn_ids
                .iter()
                .find(|default_arg_data| default_arg_data.name == name)
                .map(|default_arg_data| {
                    let fid = self.push_inst(Instruction::Uinteger(default_arg_data.fid.0));
                    self.push_inst(Instruction::Call(fid, vec![], default_arg_data.ty))
                })
        })
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
        match lit {
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
        }
    }
    fn eval_rvar(&mut self, e: ExprNodeId, t: TypeNodeId) -> VPtr {
        let span = &e.to_span();
        let loc = self.get_loc_from_span(span);
        let name = match e.to_expr() {
            Expr::Var(name) => name,
            _ => unreachable!("eval_rvar called on non-variable expr"),
        };
        log::trace!("rv t:{} {}", name, t.to_type());

        match self.lookup(&name) {
            LookupRes::Local(v) => match v.as_ref() {
                Value::Function(i) => {
                    let reg = self.push_inst(Instruction::Uinteger(*i as u64));
                    self.push_inst(Instruction::Closure(reg))
                }
                _ => {
                    let ptr = self.eval_expr_as_address(e);
                    self.push_inst(Instruction::Load(ptr, t))
                }
            },
            LookupRes::UpValue(level, v) => {
                (0..level)
                    .rev()
                    .fold(v.clone(), |upv, i| match upv.as_ref() {
                        Value::Function(_fi) => v.clone(),
                        _ => {
                            let res = self.gen_new_register();
                            let current = self.data.get_mut(self.data_i - i).unwrap();
                            let currentf = self
                                .program
                                .functions
                                .get_mut(current.func_i.0 as usize)
                                .unwrap();
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
                let ty = self.typeenv.lookup(name, loc).expect(
                    format!(
                        "variable \"{name}\" not found. it should be detected at type checking stage"
                    )
                    .as_str(),
                );
                Arc::new(Value::ExtFunction(name, ty))
            }
        }
    }
    /// Evaluates an assignee expression and returns a VPtr that is a pointer to the destination.
    fn eval_destination_ptr(&mut self, assignee: ExprNodeId) -> AssignDestination {
        match assignee.to_expr() {
            Expr::Var(name) => {
                // For a simple variable, lookup its pointer from the environment.
                match self.lookup(&name) {
                    LookupRes::Local(v_ptr) => AssignDestination::Local(v_ptr.clone()),
                    LookupRes::Global(v_ptr) => AssignDestination::Global(v_ptr.clone()),
                    LookupRes::UpValue(_level, v_ptr) => {
                        let currentf = self.get_current_fn();
                        let upi = currentf.get_or_insert_upvalue(&v_ptr) as _;
                        AssignDestination::UpValue(upi, v_ptr.clone())
                    }
                    LookupRes::None => {
                        unreachable!("Invalid assignment target: variable not found")
                    }
                }
            }
            Expr::FieldAccess(expr, accesskey) => {
                // For a field access, we need to calculate the pointer to the field.
                let base_ptr = self.eval_expr_as_address(expr);

                let record_ty_id = self.typeenv.infer_type(expr).unwrap();
                let record_ty = record_ty_id.to_type();

                if let Type::Record(fields) = record_ty {
                    let offset = fields
                        .iter()
                        .position(|RecordTypeField { key, .. }| *key == accesskey)
                        .expect("field access to non-existing field");

                    // Use GetElementPtr to calculate the pointer to the specific field.
                    let ptr = self.push_inst(Instruction::GetElement {
                        value: base_ptr.clone(),
                        ty: record_ty_id,
                        tuple_offset: offset as u64,
                    });
                    AssignDestination::Local(ptr)
                } else {
                    panic!("Expected record type for field access assignment, but got {record_ty}");
                }
            }
            Expr::ArrayAccess(_, _) => {
                unimplemented!("Assignment to array is not implemented yet.")
            }
            _ => unreachable!("Invalid assignee expression"),
        }
    }

    fn eval_assign(&mut self, assignee: ExprNodeId, src: VPtr, t: TypeNodeId) {
        match self.eval_destination_ptr(assignee) {
            AssignDestination::Local(value) => {
                self.push_inst(Instruction::Store(value, src, t));
            }
            AssignDestination::UpValue(upi, _value) => {
                self.push_inst(Instruction::SetUpValue(upi, src, t));
            }
            AssignDestination::Global(value) => {
                self.push_inst(Instruction::SetGlobal(value, src, t));
            }
        }
    }
    fn emit_fncall(&mut self, idx: u64, args: Vec<(VPtr, TypeNodeId)>, ret_t: TypeNodeId) -> VPtr {
        // stack size of the function to be called
        let target_fn = &self.program.functions[idx as usize];
        let is_stateful = target_fn.is_stateful();
        let state_sizes = target_fn.state_sizes.clone();
        let child_skeleton = target_fn.state_skeleton.clone();

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
            self.get_current_fn().push_state_skeleton(child_skeleton);
            self.push_inst(Instruction::Uinteger(idx))
        };

        let res = self.push_inst(Instruction::Call(f.clone(), args, ret_t));

        if is_stateful {
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
        log::trace!("alloc_aggregates: items = {items:?}, ty = {ty:?}");
        let len = items.len();
        if len == 0 {
            return (Arc::new(Value::None), Type::Record(vec![]).into_id());
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
    /// Evaluates an expression as an l-value, returning a pointer to its memory location.
    fn eval_expr_as_address(&mut self, e: ExprNodeId) -> VPtr {
        match e.to_expr() {
            Expr::Var(name) => {
                // do not load here
                match self.lookup(&name) {
                    LookupRes::Local(ptr) => ptr,
                    LookupRes::Global(ptr) => ptr,
                    _ => unreachable!("Cannot get address of this expression"),
                }
            }
            Expr::FieldAccess(base_expr, accesskey) => {
                let base_ptr = self.eval_expr_as_address(base_expr);

                let record_ty_id = self.typeenv.infer_type(base_expr).unwrap();
                let record_ty = record_ty_id.to_type();

                if let Type::Record(fields) = record_ty {
                    let offset = fields
                        .iter()
                        .position(|f| f.key == accesskey)
                        .expect("Field not found");

                    self.push_inst(Instruction::GetElement {
                        value: base_ptr,
                        ty: record_ty_id,
                        tuple_offset: offset as u64,
                    })
                } else {
                    panic!("Cannot access field on a non-record type");
                }
            }
            Expr::ArrayAccess(_, _) => {
                unimplemented!("Array element assignment is not implemented yet.")
            }
            _ => unreachable!("This expression cannot be used as an l-value"),
        }
    }
    pub fn eval_expr(&mut self, e: ExprNodeId) -> (VPtr, TypeNodeId) {
        let span = e.to_span();
        let ty = self
            .typeenv
            .infer_type(e)
            .expect("type inference failed, should be an error at type checker stage");
        let ty = InferContext::substitute_type(ty);
        match &e.to_expr() {
            Expr::Literal(lit) => {
                let v = self.eval_literal(lit, &span);
                (v, ty)
            }
            Expr::Var(_name) => (self.eval_rvar(e, ty), ty),
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
            Expr::RecordUpdate(_, _) => {
                // Record update syntax should be expanded during conversion phase
                // This case should not be reached after syntax sugar expansion
                unreachable!("RecordUpdate should be expanded during syntax sugar conversion")
            }
            Expr::FieldAccess(expr, accesskey) => {
                let (expr_v, expr_ty) = self.eval_expr(*expr);
                match expr_ty.to_type() {
                    Type::Record(fields) => {
                        let offset = fields
                            .iter()
                            .position(|RecordTypeField { key, .. }| *key == *accesskey)
                            .expect("field access to non-existing field");

                        let res = self.push_inst(Instruction::GetElement {
                            value: expr_v.clone(),
                            ty: expr_ty,
                            tuple_offset: offset as u64,
                        });
                        (res, fields[offset].ty)
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
                let (array_v, _array_ty) = self.eval_expr(*array);
                let (index_v, _) = self.eval_expr(*index);

                // Get element at the specified index
                let result = self.push_inst(Instruction::GetArrayElem(
                    array_v.clone(),
                    index_v.clone(),
                    ty,
                ));
                (result, ty)
            }
            Expr::Apply(f, args) => {
                let (f, ft) = self.eval_expr(*f);
                let del = self.try_make_delay(&f, args);
                if let Some(d) = del {
                    (d, numeric!())
                } else {
                    // Get function parameter info
                    let (at, rt) = if let Type::Function { arg, ret } = ft.to_type() {
                        (arg, ret)
                    } else {
                        panic!("non function type {} {} ", ft.to_type(), ty.to_type());
                    };

                    // Handle parameter packing/unpacking if needed
                    // How can we distinguish when the function takes a single tuple and argument is just a single tuple
                    let atvvec = if args.len() == 1 {
                        let (arg_val, ty) = self.eval_args(args).first().unwrap().clone();
                        if ty.to_type().can_be_unpacked() {
                            log::trace!("Unpacking argument {ty} for {at}");
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
                                Type::Record(kvs) => {
                                    enum SearchRes {
                                        Found(usize),
                                        Default,
                                    }
                                    if let Type::Record(param_types) = at.to_type() {
                                        let search_res = param_types.iter().map(|param| {
                                            kvs.iter()
                                                .enumerate()
                                                .find_map(|(i, kv)| {
                                                    (param.key == kv.key)
                                                        .then_some((SearchRes::Found(i), kv))
                                                })
                                                .or(param
                                                    .has_default
                                                    .then_some((SearchRes::Default, param)))
                                        });
                                        search_res.map(
                                            |searchres| match searchres {
                                                Some((SearchRes::Found(i), kv)) => {
                                                    log::trace!("non-default argument {} found", kv.key);

                                                    let field_val = self.push_inst(
                                                                    Instruction::GetElement {
                                                                        value: arg_val.clone(),
                                                                        ty,
                                                                        tuple_offset: i as u64,
                                                                    },
                                                                );
                                                                (field_val, kv.ty)
                                                            },
                                                Some((SearchRes::Default, kv)) => {
                                                  if let Value::Function(fid) = f.as_ref() {
                                                     let fid=      FunctionId(*fid as u64);
                                                        log::trace!("searching default argument for {} in function {}", kv.key, self.program.functions[fid.0 as usize].label.as_str());
                                                        let default_val = self.get_default_arg_call(kv.key, fid).expect(format!("msg: default argument {} not found", kv.key).as_str());
                                                        (default_val, kv.ty)
                                                    } else {
                                                        log::error!("default argument cannot be supported with closure currently");
                                                        (Arc::new(Value::None), Type::Failure.into_id())
                                                    }
                                                }
                                                None=>{
                                                    panic!("parameter pack failed, possible type inference bug")
                                                }
                                            }).collect::<Vec<_>>()
                                    } else {
                                        unreachable!(
                                            "parameter pack failed, possible type inference bug"
                                        )
                                    }
                                }
                                _ => vec![(arg_val, ty)],
                            }
                        } else {
                            vec![(arg_val, ty)]
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

            Expr::Lambda(ids, _rett, body) => {
                let (atype, rt) = match ty.to_type() {
                    Type::Function { arg, ret } => (arg, ret),
                    _ => panic!(),
                };
                let binds = match ids.len() {
                    0 => vec![],
                    1 => {
                        let id = ids[0].clone();
                        let label = id.id;
                        vec![(label, atype, id.default_value)]
                    }
                    _ => {
                        let tys = atype
                            .to_type()
                            .get_as_tuple()
                            .expect("must be tuple or record type. type inference failed");
                        //multiple arguments
                        ids.iter()
                            .zip(tys.iter())
                            .map(|(id, ty)| {
                                let label = id.id;
                                (label, *ty, id.default_value)
                            })
                            .collect()
                    }
                };

                let name = self.consume_fnlabel();
                let (c_idx, f) = self.do_in_child_ctx(name, &binds, |ctx, c_idx| {
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

                    let f = Arc::new(Value::Function(c_idx.0 as usize));
                    (f, rt)
                });
                let child = self.program.functions.get_mut(c_idx.0 as usize).unwrap();
                let res = if child.upindexes.is_empty() {
                    //todo:make Closure
                    f
                } else {
                    let idxcell = self.push_inst(Instruction::Uinteger(c_idx.0));
                    self.push_inst(Instruction::Closure(idxcell))
                };
                (res, ty)
            }
            Expr::Feed(id, expr) => {
                //set typesize lazily
                let statesize = StateSize { size: 1, ty };
                let res = self.push_inst(Instruction::GetState(ty));
                self.add_bind((*id, res.clone()));
                self.get_ctxdata().next_state_offset = Some(vec![statesize]);
                let (retv, _t) = self.eval_expr(*expr);
                self.get_current_fn().state_sizes.push(statesize);
                //todo:move word size function to type.rs
                self.get_current_fn()
                    .push_state_skeleton(StateTreeSkeleton::Feed(ty));
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
                // let insert_bb = self.get_ctxdata().current_bb;
                // let insert_pos = if self.program.functions.is_empty() {
                //     0
                // } else {
                //     self.get_current_basicblock().0.len()
                // };
                let (bodyv, t) = self.eval_expr(*body);
                self.fn_label = None;

                let is_global = self.get_ctxdata().func_i.0 == 0;
                let is_function = matches!(bodyv.as_ref(), Value::Function(_));

                if !is_global && !is_function {
                    // ローカル変数の場合、常にAllocaとStoreを使う
                    let ptr = self.push_inst(Instruction::Alloc(t));
                    self.push_inst(Instruction::Store(ptr.clone(), bodyv, t));
                    self.add_bind_pattern(pat, ptr, t, false);
                } else {
                    // グローバル変数や関数はこれまで通りの扱い
                    self.add_bind_pattern(pat, bodyv, t, is_global);
                }

                if let Some(then_e) = then {
                    self.eval_expr(*then_e)
                } else {
                    (Arc::new(Value::None), unit!())
                }
            }
            Expr::LetRec(id, body, then) => {
                let is_global = self.get_ctxdata().func_i.0 == 0;
                self.fn_label = Some(id.id);
                let nextfunid = self.program.functions.len();
                let t = self
                    .typeenv
                    .infer_type(e)
                    .expect("type inference failed, should be an error at type checker stage");
                let t = InferContext::substitute_type(t);

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
                self.eval_assign(*assignee, src, ty);
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
            Expr::Bracket(_) | Expr::Escape(_) | Expr::MacroExpand(_, _) => {
                unreachable!("Macro code should be expanded before mirgen")
            }
            Expr::BinOp(_, _, _) | Expr::UniOp(_, _) | Expr::Paren(_) => {
                unreachable!(
                    "syntactic sugar for infix&unary operators are removed before this stage"
                )
            }
            Expr::Error => {
                self.push_inst(Instruction::Error);
                (Arc::new(Value::None), unit!())
            }
        }
    }
}

fn is_toplevel_macro(typeenv: &mut InferContext, top_ast: ExprNodeId) -> bool {
    typeenv.infer_type(top_ast).is_ok_and(|t| {
        log::trace!("toplevel type: {}", t.to_type());
        matches!(t.to_type(), Type::Code(_))
    })
}

pub fn typecheck(
    root_expr_id: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    file_path: Option<Symbol>,
) -> (ExprNodeId, InferContext, Vec<Box<dyn ReportableError>>) {
    let (expr, convert_errs) =
        convert_pronoun::convert_pronoun(root_expr_id, file_path.unwrap_or_default());
    let expr = recursecheck::convert_recurse(expr, file_path.unwrap_or_default());
    // let expr = destruct_let_pattern(expr);
    let infer_ctx = infer_root(expr, builtin_types, file_path.unwrap_or_default());
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
    (expr, infer_ctx, errors)
}

/// Generate MIR from AST.
/// The input ast (`root_expr_id`) should contain global context. (See [[compiler::parser::add_global_context]].)
/// MIR generator itself does not emit any error, the any compile errors are analyzed before generating MIR, mostly in type checker.
/// Note that the AST may contain partial error nodes, to do type check and report them as possible.
pub fn compile(
    root_expr_id: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    macro_env: &[Box<dyn MacroFunction>],
    file_path: Option<Symbol>,
) -> Result<Mir, Vec<Box<dyn ReportableError>>> {
    let (expr, infer_ctx, errors) = typecheck(root_expr_id, builtin_types, file_path);
    if errors.is_empty() {
        let expr = if expr.contains_macro() {
            let expr = if is_toplevel_macro(&mut infer_ctx.clone(), expr) {
                expr
            } else {
                // wrap toplevel expression with `{} because global expression is stage 1(main)
                Expr::Bracket(expr).into_id(expr.to_location())
            };
            interpreter::expand_macro(expr, macro_env)
        } else {
            expr
        };
        log::trace!(
            "ast after macro expansion: {:?}",
            expr.to_expr().simple_print()
        );
        let expr = parser::add_global_context(expr, file_path.unwrap_or_default());
        let mut ctx = Context::new(infer_ctx, file_path);
        let _res = ctx.eval_expr(expr);
        ctx.program.file_path = file_path;
        Ok(ctx.program.clone())
    } else {
        Err(errors)
    }
}

// #[cfg(test)]
// mod test;
