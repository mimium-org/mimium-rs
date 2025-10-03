use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use crate::interner::{Symbol, TypeNodeId};
use crate::mir::{self, Mir, StateSize};
use crate::runtime::vm::bytecode::{ConstPos, GlobalPos, Reg};
use crate::runtime::vm::program::WordSize;
use crate::runtime::vm::{self, StateOffset};
use crate::types::{PType, RecordTypeField, Type, TypeSize};
use crate::utils::half_float::HFloat;
use vm::bytecode::Instruction as VmInstruction;

#[derive(Debug, Default)]
struct MemoryRegion(Reg, TypeSize);
#[derive(Debug, Default)]
struct VRegister(HashMap<Arc<mir::Value>, MemoryRegion>);

impl VRegister {
    pub fn push_stack(&mut self, v: &Arc<mir::Value>, size: u64) -> Reg {
        self.add_newvalue_range(v, size)
    }
    pub fn add_newvalue(&mut self, v: &Arc<mir::Value>) -> Reg {
        let pos = self
            .0
            .iter()
            .max_by_key(|(_v, MemoryRegion(address, size))| address + size)
            .map(|(_, MemoryRegion(address, size))| address + size)
            .unwrap_or(0);
        self.0.insert(v.clone(), MemoryRegion(pos, 1));
        log::trace!("add {:?}", self.0);
        pos as Reg
    }
    pub fn add_newvalue_range(&mut self, v: &Arc<mir::Value>, size: u64) -> Reg {
        let pos = self
            .0
            .iter()
            .max_by_key(|(_v, MemoryRegion(address, size))| address + size)
            .map(|(_, MemoryRegion(address, size))| address + size)
            .unwrap_or(0);
        self.0.insert(v.clone(), MemoryRegion(pos, size as _));
        log::trace!("add_range {:#?}", self.0);
        pos as Reg
    }
    pub fn find(&mut self, v: &Arc<mir::Value>) -> Option<Reg> {
        log::trace!("find {v}");
        let res = self.0.get(v).map(|r| r.0);
        match (res, v.as_ref()) {
            //argument is registered in absolute position
            (Some(_), mir::Value::Argument(_)) | (Some(_), mir::Value::Global(_)) => res,
            (Some(_), _) => {
                self.0.remove(v);
                res
            }
            _ => None,
        }
    }
    //find for load and store instruction
    pub fn find_keep(&self, v: &Arc<mir::Value>) -> Option<Reg> {
        log::trace!("findkeep {v}");
        self.0.get(v).map(|r| r.0)
    }
}

#[derive(Debug, Default)]
struct VStack(Vec<VRegister>);
impl VStack {
    fn get_top(&mut self) -> &mut VRegister {
        self.0.last_mut().unwrap()
    }
    fn find_upvalue(&self, v: &Arc<mir::Value>) -> Option<Reg> {
        self.0
            .iter()
            .rev()
            .skip(1)
            .find_map(|vreg| vreg.find_keep(v))
    }
    pub fn push_stack(&mut self, v: &Arc<mir::Value>, size: u64) -> Reg {
        self.get_top().push_stack(v, size)
    }
    pub fn add_newvalue(&mut self, v: &Arc<mir::Value>) -> Reg {
        self.get_top().add_newvalue(v)
    }
    pub fn find(&mut self, v: &Arc<mir::Value>) -> Option<Reg> {
        self.get_top().find(v)
    }
    pub fn find_keep(&mut self, v: &Arc<mir::Value>) -> Option<Reg> {
        self.get_top().find_keep(v)
    }
}

#[derive(Debug, Default)]
pub struct ByteCodeGenerator {
    vregister: VStack,
    varray: Vec<Arc<mir::Value>>,
    fnmap: HashMap<Symbol, usize>,
    globals: HashMap<Arc<mir::Value>, usize>, //index to Program.global_vals
    program: vm::Program,
}

/// Option for scecifying the evaluation strategy of the function that uses `self`.
/// `SimpleState` inteprets `self` as simply internal state of the function which is initialized as 0 at time = 0.
/// `ZeroAtInit` inteprets *the return value of the function which uses `self`* returns 0 at time = 0.
/// For example, `| | {self + 1}` will return 1 at time = 0 in `SimpleState` mode, but 0 in `ZeroAtInit` mode.
/// For most of the cases, `SimpleState` is the default and recommended mode.
#[derive(Default, Debug, Clone, Copy)]
pub enum SelfEvalMode {
    #[default]
    SimpleState,
    ZeroAtInit,
}
#[derive(Default, Debug, Clone, Copy)]
pub struct Config {
    pub self_eval_mode: SelfEvalMode,
}

fn gen_raw_int(n: &i64) -> vm::RawVal {
    let raw = {
        let iptr = n as *const i64;
        iptr as *const vm::RawVal
    };
    unsafe { *raw }
}

fn gen_raw_float(n: &f64) -> vm::RawVal {
    let raw = {
        let iptr = n as *const f64;
        iptr as *const vm::RawVal
    };
    unsafe { *raw }
}

impl ByteCodeGenerator {
    /// Calculate byte size of the value for type T based on 1 word size (=currently 64bit).
    /// The base word size may change depending on the backend in the future.
    /// Currently, the string type is a immutable Symbol, so the word size is 1.
    /// In the future, string may be represented as a fat pointer, pair of pointer and length.
    pub(crate) fn word_size_for_type(ty: TypeNodeId) -> TypeSize {
        match ty.to_type() {
            Type::Primitive(PType::Unit) => 0,
            Type::Primitive(PType::String) => 1,
            Type::Primitive(_) => 1,
            Type::Array(_) => 1, //array is represented as a pointer to the special storage
            Type::Tuple(types) => types.iter().map(|t| Self::word_size_for_type(*t)).sum(),
            Type::Record(types) => types
                .iter()
                .map(|RecordTypeField { ty, .. }| Self::word_size_for_type(*ty))
                .sum(),
            Type::Function { arg: _, ret: _ } => 1,
            Type::Ref(_) => 1,
            Type::Code(_) => todo!(),
            _ => {
                //todo: this may contain intermediate types
                1
            }
        }
    }
    pub fn calc_state_size<T: AsRef<[StateSize]>>(state_sizes: T) -> u64 {
        state_sizes
            .as_ref()
            .iter()
            .map(|x| x.size * Self::word_size_for_type(x.ty) as u64)
            .sum()
    }
    fn get_binop(&mut self, v1: Arc<mir::Value>, v2: Arc<mir::Value>) -> (Reg, Reg) {
        let r1 = self.find(&v1);
        let r2 = self.find(&v2);
        (r1, r2)
    }
    fn emit_binop1<F>(
        &mut self,
        inst: F,
        dst: Arc<mir::Value>,
        v1: Arc<mir::Value>,
    ) -> Option<VmInstruction>
    where
        F: FnOnce(Reg, Reg) -> VmInstruction,
    {
        let r1 = self.find(&v1);
        let dst = self.get_destination(dst.clone(), 1);
        let i = inst(dst, r1);
        Some(i)
    }
    fn emit_binop2<F>(
        &mut self,
        inst: F,
        dst: Arc<mir::Value>,
        v1: Arc<mir::Value>,
        v2: Arc<mir::Value>,
    ) -> Option<VmInstruction>
    where
        F: FnOnce(Reg, Reg, Reg) -> VmInstruction,
    {
        //the order matters! get destination later on arguments
        let (r1, r2) = self.get_binop(v1, v2);
        let dst = self.get_destination(dst.clone(), 1);
        let i = inst(dst, r1, r2);
        Some(i)
    }
    fn get_destination(&mut self, dst: Arc<mir::Value>, size: TypeSize) -> Reg {
        self.vregister.push_stack(&dst, size as _)
    }
    fn get_or_insert_global(&mut self, gv: Arc<mir::Value>, ty: TypeNodeId) -> GlobalPos {
        match self.globals.get(&gv) {
            Some(idx) => *idx as GlobalPos,
            None => {
                let size = Self::word_size_for_type(ty) as usize;
                let idx = if size == 0 && self.program.global_vals.is_empty() {
                    0
                } else {
                    self.program.global_vals.len() + size - 1
                };
                self.globals.insert(gv.clone(), idx);
                let size = WordSize(size as _);
                self.program.global_vals.push(size);
                idx as GlobalPos
            }
        }
    }
    fn find(&mut self, v: &Arc<mir::Value>) -> Reg {
        self.vregister
            .find(v)
            .or_else(|| self.globals.get(v).map(|&v| v as Reg))
            .or_else(|| self.varray.iter().position(|av| v == av).map(|v| v as Reg))
            .expect(format!("value {v} not found").as_str())
    }
    fn find_keep(&mut self, v: &Arc<mir::Value>) -> Reg {
        self.vregister
            .find_keep(v)
            .or_else(|| self.globals.get(v).map(|&v| v as Reg))
            .expect(format!("value {v} not found").as_str())
    }
    fn find_upvalue(&self, upval: &Arc<mir::Value>) -> Reg {
        self.vregister
            .find_upvalue(upval)
            .expect("failed to find upvalue")
    }
    fn prepare_function(
        &mut self,
        bytecodes_dst: &mut Vec<VmInstruction>,
        faddress: &Arc<mir::Value>,
        args: &[(Arc<mir::Value>, TypeNodeId)],
    ) -> (Reg, TypeSize) {
        let mut aoffsets = vec![];
        let mut offset = 0;
        for (a, ty) in args.iter() {
            let src = self.find(a);
            let size = Self::word_size_for_type(*ty);
            aoffsets.push((offset, src, size));
            offset += size;
        }
        let faddress = self.find_keep(faddress);
        // bytecodes_dst.push(VmInstruction::Move())
        for (adst, src, size) in aoffsets.iter() {
            let address = *adst + faddress + 1;
            let is_samedst = address == *src;
            if !is_samedst {
                match size {
                    0 => unreachable!(),
                    1 => bytecodes_dst.push(VmInstruction::Move(address as Reg, *src)),
                    _ => bytecodes_dst.push(VmInstruction::MoveRange(address as Reg, *src, *size)),
                }
            }
        }
        (faddress, offset)
    }
    fn get_or_insert_extfunid(&mut self, label: Symbol, ty: TypeNodeId) -> ConstPos {
        self.program
            .ext_fun_table
            .iter()
            .position(|(name, _ty)| name.as_str() == label.as_str())
            .unwrap_or_else(|| {
                self.program.ext_fun_table.push((label.to_string(), ty));
                self.program.ext_fun_table.len() - 1
            }) as ConstPos
    }
    // fn get_or_insert_extclsid(&mut self, label: Symbol, ty: TypeNodeId) -> ConstPos {
    //     self.program
    //         .ext_cls_table
    //         .iter()
    //         .position(|(name, _ty)| *name == label)
    //         .unwrap_or_else(|| {
    //             self.program.ext_cls_table.push((label, ty));
    //             self.program.ext_cls_table.len() - 1
    //         }) as ConstPos
    // }
    fn prepare_extfun_or_cls(
        &mut self,
        funcproto: &mut vm::FuncProto,
        bytecodes_dst: Option<&mut Vec<VmInstruction>>,
        dst: Arc<mir::Value>,
        args: &[(Arc<mir::Value>, TypeNodeId)],
        idx: ConstPos,
        ty: TypeNodeId,
    ) -> (Reg, Reg, TypeSize) {
        let fi = funcproto.add_new_constant(idx as u64);
        let rsize = Self::word_size_for_type(ty);
        let bytecodes_dst = bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
        let f = self.vregister.push_stack(&dst, rsize as _);
        bytecodes_dst.push(VmInstruction::MoveConst(f, fi as ConstPos));
        let (dst, argsize) = self.prepare_function(bytecodes_dst, &dst, args);
        (dst, argsize, rsize)
    }
    fn prepare_extfun(
        &mut self,
        funcproto: &mut vm::FuncProto,
        bytecodes_dst: Option<&mut Vec<VmInstruction>>,
        dst: Arc<mir::Value>,
        args: &[(Arc<mir::Value>, TypeNodeId)],
        label: Symbol,
        ty: TypeNodeId,
    ) -> (Reg, Reg, TypeSize) {
        let idx = self.get_or_insert_extfunid(label, ty);
        self.prepare_extfun_or_cls(funcproto, bytecodes_dst, dst, args, idx, ty)
    }
    // fn prepare_extcls(
    //     &mut self,
    //     funcproto: &mut vm::FuncProto,
    //     bytecodes_dst: Option<&mut Vec<VmInstruction>>,
    //     dst: Arc<mir::Value>,
    //     args: &[(Arc<mir::Value>, TypeNodeId)],
    //     label: Symbol,
    //     ty: TypeNodeId,
    // ) -> (Reg, Reg, TypeSize) {
    //     let idx = self.get_or_insert_extclsid(label, ty);
    //     self.prepare_extfun_or_cls(funcproto, bytecodes_dst, dst, args, idx, ty)
    // }
    fn emit_instruction(
        &mut self,
        funcproto: &mut vm::FuncProto,
        bytecodes_dst: Option<&mut Vec<VmInstruction>>,
        mirfunc: mir::Function,
        dst: Arc<mir::Value>,
        mirinst: mir::Instruction,
        config: Config,
    ) -> Option<VmInstruction> {
        match mirinst {
            mir::Instruction::Uinteger(u) => {
                let pos = funcproto.add_new_constant(u);
                Some(VmInstruction::MoveConst(
                    self.get_destination(dst, 1),
                    pos as ConstPos,
                ))
            }
            mir::Instruction::Integer(i) => {
                let pos = funcproto.add_new_constant(gen_raw_int(&i));
                Some(VmInstruction::MoveConst(
                    self.get_destination(dst, 1),
                    pos as ConstPos,
                ))
            }
            mir::Instruction::Float(n) => {
                let dst = self.get_destination(dst, 1);
                if let Ok(half_f) = HFloat::try_from(n) {
                    Some(VmInstruction::MoveImmF(dst, half_f))
                } else {
                    let pos = funcproto.add_new_constant(gen_raw_float(&n));
                    Some(VmInstruction::MoveConst(dst, pos as ConstPos))
                }
            }
            mir::Instruction::String(s) => {
                let pos = self.program.add_new_str(s.to_string());
                let cpos = funcproto.add_new_constant(pos as u64);
                Some(VmInstruction::MoveConst(
                    self.get_destination(dst, 1),
                    cpos as ConstPos,
                ))
            }
            mir::Instruction::Alloc(t) => {
                let size = Self::word_size_for_type(t) as u64;
                let _ = self.vregister.push_stack(&dst, size);
                None
            }
            mir::Instruction::Load(ptr, ty) => {
                let d = self.get_destination(dst, Self::word_size_for_type(ty));
                let s = self.find_keep(&ptr);
                let size = Self::word_size_for_type(ty);
                match (d, s, size) {
                    (d, s, 1) if d != s => Some(VmInstruction::Move(d, s)),
                    (d, s, size) if d != s => Some(VmInstruction::MoveRange(d, s, size)),
                    _ => None,
                }
            }
            mir::Instruction::Store(dst, src, ty) => {
                let s = self.find(&src);
                let d = self.find_keep(&dst);
                let size = Self::word_size_for_type(ty);
                match (d, s, size) {
                    (d, s, 1) if d != s => Some(VmInstruction::Move(d, s)),
                    (d, s, size) if d != s => Some(VmInstruction::MoveRange(d, s, size)),
                    _ => None,
                }
            }
            mir::Instruction::GetGlobal(v, ty) => {
                let dst = self.get_destination(dst, Self::word_size_for_type(ty));
                let idx = self.get_or_insert_global(v.clone(), ty);
                Some(VmInstruction::GetGlobal(
                    dst,
                    idx,
                    Self::word_size_for_type(ty),
                ))
            }
            mir::Instruction::SetGlobal(v, src, ty) => {
                let idx = self.get_or_insert_global(v.clone(), ty);
                let s = self.find(&src);
                Some(VmInstruction::SetGlobal(
                    idx,
                    s,
                    Self::word_size_for_type(ty),
                ))
            }
            mir::Instruction::GetElement {
                value,
                ty,
                tuple_offset,
            } => {
                let ptr = self.find_keep(&value) as usize;
                let ty = ty.to_type();
                let tvec = ty.get_as_tuple().unwrap();
                let tsize = Self::word_size_for_type(tvec[tuple_offset as usize]);
                let t_offset: u64 = tvec[0..(tuple_offset as _)]
                    .iter()
                    .map(|t| Self::word_size_for_type(*t) as u64)
                    .sum();
                let address = (ptr + t_offset as usize) as Reg;
                self.vregister
                    .get_top()
                    .0
                    .insert(dst, MemoryRegion(address, tsize));
                None
            }
            mir::Instruction::Call(v, args, r_ty) => {
                let rsize = Self::word_size_for_type(r_ty);
                match v.as_ref() {
                    mir::Value::Register(_address) => {
                        let bytecodes_dst =
                            bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                        let d = self.get_destination(dst.clone(), rsize);
                        let s = self.find(&v);
                        bytecodes_dst.push(VmInstruction::Move(d, s));
                        let (fadd, argsize) = self.prepare_function(bytecodes_dst, &dst, &args);
                        Some(VmInstruction::Call(fadd, argsize, rsize))
                    }
                    mir::Value::Function(_idx) => {
                        unreachable!();
                    }
                    mir::Value::ExtFunction(label, _ty) => {
                        //todo: use btreemap
                        let (dst, argsize, nret) =
                            self.prepare_extfun(funcproto, bytecodes_dst, dst, &args, *label, r_ty);
                        Some(VmInstruction::CallExtFun(dst, argsize, nret))
                    }
                    _ => unreachable!(),
                }
            }
            mir::Instruction::CallCls(f, args, r_ty) => {
                let rsize = Self::word_size_for_type(r_ty);
                match f.as_ref() {
                    mir::Value::Register(_address) => {
                        let bytecodes_dst =
                            bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());

                        let (fadd, argsize) = self.prepare_function(bytecodes_dst, &f, &args);
                        let s = self.find(&f);
                        let d = self.get_destination(dst.clone(), rsize);
                        bytecodes_dst.push(VmInstruction::CallCls(fadd, argsize, rsize));
                        match rsize {
                            0 => None,
                            1 => Some(VmInstruction::Move(d, s)),
                            n => Some(VmInstruction::MoveRange(d, s, n)),
                        }
                    }
                    mir::Value::Function(_idx) => {
                        unreachable!();
                    }
                    mir::Value::ExtFunction(label, _ty) => {
                        let (dst, argsize, nret) =
                            self.prepare_extfun(funcproto, bytecodes_dst, dst, &args, *label, r_ty);
                        Some(VmInstruction::CallExtFun(dst, argsize, nret))
                    }
                    _ => unreachable!(),
                }
            }
            mir::Instruction::Closure(idxcell) => {
                let idx = self.find(&idxcell);
                let dst = self.get_destination(dst, 1);
                Some(VmInstruction::Closure(dst, idx))
            }
            mir::Instruction::CloseUpValues(src, ty) => {
                // src might contain multiple upvalues (e.g. tuple)
                let flattened = ty.flatten();
                let base = self.vregister.find_keep(&src).unwrap();

                let mut offset = 0;
                let bytecodes_dst = bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                for elem_t in flattened {
                    let tsize = Self::word_size_for_type(elem_t);
                    if elem_t.to_type().is_function() {
                        bytecodes_dst.push(VmInstruction::Close(base + offset))
                    }
                    offset += tsize;
                }
                None
            }
            mir::Instruction::GetUpValue(i, ty) => {
                let upval = &mirfunc.upindexes[i as usize];
                let v = self.find_upvalue(upval);
                let size: u8 = Self::word_size_for_type(ty);
                let ouv = mir::OpenUpValue {
                    pos: v as usize,
                    size,
                    is_closure: ty.to_type().is_function(),
                };
                if let Some(ui) = funcproto.upindexes.get_mut(i as usize) {
                    *ui = ouv;
                } else {
                    funcproto.upindexes.push(ouv);
                }
                let d = self.vregister.get_top().add_newvalue_range(&dst, size as _);
                Some(VmInstruction::GetUpValue(
                    d,
                    i as Reg,
                    Self::word_size_for_type(ty),
                ))
            }
            mir::Instruction::SetUpValue(dst, src, ty) => {
                let upval = &mirfunc.upindexes[dst as usize];
                let v = self.find_upvalue(upval);
                let size: u8 = Self::word_size_for_type(ty);
                let ouv = mir::OpenUpValue {
                    pos: v as usize,
                    size,
                    is_closure: ty.to_type().is_function(),
                };
                if let Some(ui) = funcproto.upindexes.get_mut(dst as usize) {
                    *ui = ouv;
                } else {
                    funcproto.upindexes.push(ouv);
                }
                let s = self.find(&src);
                Some(VmInstruction::SetUpValue(
                    dst as Reg,
                    s,
                    Self::word_size_for_type(ty),
                ))
            }
            mir::Instruction::PushStateOffset(v) => {
                let state_size = StateOffset::try_from(Self::calc_state_size(v))
                    .expect("too much large state offset.");
                Some(VmInstruction::PushStatePos(state_size))
            }
            mir::Instruction::PopStateOffset(v) => {
                let state_size = StateOffset::try_from(Self::calc_state_size(v))
                    .expect("too much large state offset.");
                Some(VmInstruction::PopStatePos(state_size))
            }
            mir::Instruction::GetState(ty) => {
                let size = Self::word_size_for_type(ty);
                let d = self.vregister.push_stack(&dst, size as _);
                Some(VmInstruction::GetState(d, size))
            }

            mir::Instruction::JmpIf(cond, tbb, ebb, pbb) => {
                let c = self.find(&cond);

                // TODO: to allow &mut match, but there should be nicer way...
                let mut bytecodes_dst = bytecodes_dst;

                let mut then_bytecodes: Vec<VmInstruction> = vec![];
                let mut else_bytecodes: Vec<VmInstruction> = vec![];
                mirfunc.body[tbb as usize]
                    .0
                    .iter()
                    .for_each(|(dst, t_inst)| {
                        let res = self.emit_instruction(
                            funcproto,
                            Some(&mut then_bytecodes),
                            mirfunc.clone(),
                            dst.clone(),
                            t_inst.clone(),
                            config,
                        );
                        if let Some(inst) = res {
                            then_bytecodes.push(inst);
                        }
                    });

                mirfunc.body[ebb as usize]
                    .0
                    .iter()
                    .for_each(|(dst, t_inst)| {
                        if let Some(inst) = self.emit_instruction(
                            funcproto,
                            Some(&mut else_bytecodes),
                            mirfunc.clone(),
                            dst.clone(),
                            t_inst.clone(),
                            config,
                        ) {
                            else_bytecodes.push(inst);
                        };
                    });
                let phiblock = &mirfunc.body[pbb as usize].0;
                let (phidst, pinst) = phiblock.first().unwrap();
                let phi = self.vregister.add_newvalue(phidst);
                if let mir::Instruction::Phi(t, e) = pinst {
                    let t = self.find(t);
                    then_bytecodes.push(VmInstruction::Move(phi, t));
                    let e = self.find(e);
                    else_bytecodes.push(VmInstruction::Move(phi, e));
                } else {
                    unreachable!("Unexpected inst: {pinst:?}");
                }
                let else_offset = then_bytecodes.len() + 2; // +1 for Jmp, which will be added later
                let inst = VmInstruction::JmpIfNeg(c, else_offset as _);
                match &mut bytecodes_dst {
                    Some(dst) => dst.push(inst),
                    None => funcproto.bytecodes.push(inst),
                }

                // bytes between the bottom of then block and phi
                let ret_offset = else_bytecodes.len() + 1;

                then_bytecodes.push(VmInstruction::Jmp(ret_offset as i16));

                for mut b in [then_bytecodes, else_bytecodes] {
                    match &mut bytecodes_dst {
                        Some(dst) => dst.append(&mut b),
                        None => funcproto.bytecodes.append(&mut b),
                    }
                }

                phiblock.iter().skip(1).for_each(|(dst, p_inst)| {
                    if let Some(inst) = self.emit_instruction(
                        funcproto,
                        None,
                        mirfunc.clone(),
                        dst.clone(),
                        p_inst.clone(),
                        config,
                    ) {
                        match &mut bytecodes_dst {
                            Some(dst) => dst.push(inst),
                            None => funcproto.bytecodes.push(inst),
                        }
                    };
                });
                None
            }
            mir::Instruction::Jmp(offset) => Some(VmInstruction::Jmp(offset)),
            mir::Instruction::Phi(_, _) => {
                unreachable!()
            }
            mir::Instruction::Return(v, rty) => {
                let nret = Self::word_size_for_type(rty);
                let inst = match v.as_ref() {
                    mir::Value::None => VmInstruction::Return0,
                    _ => VmInstruction::Return(self.find(&v), nret),
                };
                Some(inst)
            }
            mir::Instruction::ReturnFeed(new, rty) => {
                let size = Self::word_size_for_type(rty);
                let bytecodes_dst = bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                match config.self_eval_mode {
                    SelfEvalMode::SimpleState => {
                        let new = self.find(&new);
                        bytecodes_dst.push(VmInstruction::SetState(new, size));
                        Some(VmInstruction::Return(new, size))
                    }
                    SelfEvalMode::ZeroAtInit => {
                        //for returning always 0 at t=0
                        let old = self.vregister.add_newvalue(&dst);
                        bytecodes_dst.push(VmInstruction::GetState(old, size));
                        let new = self.find(&new);
                        bytecodes_dst.push(VmInstruction::SetState(new, size));
                        Some(VmInstruction::Return(old, size))
                    }
                }
            }
            mir::Instruction::Delay(max, src, time) => {
                let s = self.find(&src);
                let t = self.find(&time);

                let dst = self.vregister.add_newvalue(&dst);
                funcproto.delay_sizes.push(max);
                Some(VmInstruction::Delay(dst, s, t))
            }
            mir::Instruction::Mem(src) => {
                let s = self.find(&src);
                let dst = self.vregister.add_newvalue(&dst);
                Some(VmInstruction::Mem(dst, s))
            }
            mir::Instruction::NegF(v1) => self.emit_binop1(VmInstruction::NegF, dst, v1),
            mir::Instruction::AddF(v1, v2) => self.emit_binop2(VmInstruction::AddF, dst, v1, v2),
            mir::Instruction::SubF(v1, v2) => self.emit_binop2(VmInstruction::SubF, dst, v1, v2),
            mir::Instruction::MulF(v1, v2) => self.emit_binop2(VmInstruction::MulF, dst, v1, v2),
            mir::Instruction::DivF(v1, v2) => self.emit_binop2(VmInstruction::DivF, dst, v1, v2),
            mir::Instruction::ModF(v1, v2) => self.emit_binop2(VmInstruction::ModF, dst, v1, v2),
            mir::Instruction::PowF(v1, v2) => self.emit_binop2(VmInstruction::PowF, dst, v1, v2),
            mir::Instruction::LogF(v1) => self.emit_binop1(VmInstruction::LogF, dst, v1),

            mir::Instruction::SinF(v1) => self.emit_binop1(VmInstruction::SinF, dst, v1),
            mir::Instruction::CosF(v1) => self.emit_binop1(VmInstruction::CosF, dst, v1),
            mir::Instruction::AbsF(v1) => self.emit_binop1(VmInstruction::AbsF, dst, v1),
            mir::Instruction::SqrtF(v1) => self.emit_binop1(VmInstruction::SqrtF, dst, v1),
            mir::Instruction::AddI(v1, v2) => self.emit_binop2(VmInstruction::AddI, dst, v1, v2),
            mir::Instruction::SubI(v1, v2) => self.emit_binop2(VmInstruction::SubI, dst, v1, v2),
            mir::Instruction::MulI(v1, v2) => self.emit_binop2(VmInstruction::MulI, dst, v1, v2),
            mir::Instruction::DivI(v1, v2) => self.emit_binop2(VmInstruction::DivI, dst, v1, v2),
            mir::Instruction::ModI(v1, v2) => self.emit_binop2(VmInstruction::ModI, dst, v1, v2),
            mir::Instruction::Gt(v1, v2) => self.emit_binop2(VmInstruction::Gt, dst, v1, v2),
            mir::Instruction::Ge(v1, v2) => self.emit_binop2(VmInstruction::Ge, dst, v1, v2),
            mir::Instruction::Lt(v1, v2) => self.emit_binop2(VmInstruction::Lt, dst, v1, v2),
            mir::Instruction::Le(v1, v2) => self.emit_binop2(VmInstruction::Le, dst, v1, v2),
            mir::Instruction::Eq(v1, v2) => self.emit_binop2(VmInstruction::Eq, dst, v1, v2),
            mir::Instruction::Ne(v1, v2) => self.emit_binop2(VmInstruction::Ne, dst, v1, v2),
            mir::Instruction::And(v1, v2) => self.emit_binop2(VmInstruction::And, dst, v1, v2),
            mir::Instruction::Or(v1, v2) => self.emit_binop2(VmInstruction::Or, dst, v1, v2),

            mir::Instruction::Array(values, ty) => {
                let elem_ty_size = Self::word_size_for_type(ty);
                let size = values.len();
                // Move each value into the array
                let bytecodes_dst = bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                let dst_reg = self.get_destination(dst.clone(), 1 as _); //address for array is always 1 word;
                bytecodes_dst.push(VmInstruction::AllocArray(dst_reg, size as _, elem_ty_size));
                for (i, val) in values.iter().enumerate() {
                    let tmp_idx_ref = Arc::new(mir::Value::None);
                    let idx = self.vregister.add_newvalue(&tmp_idx_ref);
                    bytecodes_dst.push(VmInstruction::MoveImmF(
                        idx,
                        HFloat::try_from(i as f64).unwrap(),
                    ));
                    let idx = self.find(&tmp_idx_ref);
                    let src = self.find(val);
                    bytecodes_dst.push(VmInstruction::SetArrayElem(dst_reg, idx, src));
                }
                self.varray.push(dst);
                None // Instructions already added to bytecodes_dst
            }

            mir::Instruction::GetArrayElem(array, index, elem_ty) => {
                let array_reg = self.find(&array);
                let index_reg = self.find(&index);
                let dst_reg = self.get_destination(dst, Self::word_size_for_type(elem_ty));
                let bytecodes_dst = bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                bytecodes_dst.push(VmInstruction::GetArrayElem(dst_reg, array_reg, index_reg));
                None
            }

            mir::Instruction::SetArrayElem(_array, _index, _value, _elem_ty) => {
                todo!("SetArrayElem is not used in the current implementation");
            }

            _ => {
                unimplemented!()
            }
        }
    }
    fn generate_funcproto(
        &mut self,
        mirfunc: &mir::Function,
        fidx: usize,
        config: Config,
    ) -> (String, vm::FuncProto) {
        log::trace!("generating function {}", mirfunc.label.0);
        let mut func = vm::FuncProto {
            nparam: mirfunc.args.len(),
            nret: Self::word_size_for_type(
                *mirfunc
                    .return_type
                    .get()
                    .expect("return type not inferred correctly"),
            ) as _,
            state_skeleton: mirfunc.state_skeleton.clone(), // Transfer state skeleton from MIR
            ..Default::default()
        };
        self.vregister.0.push(VRegister::default());
        for (i, a) in mirfunc.args.iter().enumerate() {
            let size = Self::word_size_for_type(a.1);
            self.vregister
                .push_stack(&Arc::new(mir::Value::Argument(i)), size as _);
        }

        // succeeding block will be compiled recursively
        let block = &mirfunc.body[0];
        block.0.iter().for_each(|(dst, inst)| {
            let newinst = self.emit_instruction(
                &mut func,
                None,
                mirfunc.clone(),
                dst.clone(),
                inst.clone(),
                config,
            );
            if let Some(i) = newinst {
                func.bytecodes.push(i);
            }
        });
        (mirfunc.label.to_string(), func)
    }
    pub fn generate(&mut self, mir: Mir, config: Config) -> vm::Program {
        self.program.global_fn_table = mir
            .functions
            .iter()
            .enumerate()
            .map(|(i, func)| {
                self.fnmap.insert(func.label, i);
                self.generate_funcproto(func, i, config)
            })
            .collect();
        self.program.file_path = mir.file_path.clone();
        self.program.iochannels = mir.get_dsp_iochannels();
        log::debug!("iochannels: {:?}", self.program.iochannels.unwrap());
        let _io = self.program.iochannels.unwrap();
        self.program.dsp_index = self.program.get_fun_index("dsp");
        self.program.clone()
    }
}
fn remove_redundunt_mov(program: vm::Program) -> vm::Program {
    let mut res = program.clone();
    for (_, f) in res.global_fn_table.iter_mut() {
        let mut remove_idx = std::collections::HashSet::<usize>::new();
        let mut reduce_idx = std::collections::HashMap::<usize, VmInstruction>::new();

        let mut removeconst_idx = std::collections::HashMap::<usize, VmInstruction>::new();

        for (i, pair) in f.bytecodes.windows(2).enumerate() {
            match *pair {
                [
                    VmInstruction::Move(dst, src),
                    VmInstruction::Move(dst2, src2),
                ] if dst == src2 && src == dst2 =>
                //case of swapping
                {
                    remove_idx.insert(i);
                    remove_idx.insert(i + 1);
                }
                [
                    VmInstruction::Move(dst, src),
                    VmInstruction::Move(dst2, src2),
                ] if dst == src2 => {
                    reduce_idx.insert(i, VmInstruction::Move(dst2, src));
                    remove_idx.insert(i + 1);
                }
                [
                    VmInstruction::MoveConst(dst, src),
                    VmInstruction::Move(dst2, src2),
                ] if dst == src2 => {
                    removeconst_idx.insert(i, VmInstruction::MoveConst(dst2, src));
                    remove_idx.insert(i + 1);
                }
                _ => {}
            }
        }
        let mut res_bytecodes = vec![];
        for (i, inst) in f.bytecodes.iter().enumerate() {
            if remove_idx.contains(&i) {
                // log::trace!("removed redundunt mov")
            } else if let Some(inst) = removeconst_idx.get(&i) {
                res_bytecodes.push(*inst);
            } else if let Some(inst) = reduce_idx.get(&i) {
                res_bytecodes.push(*inst);
            } else {
                res_bytecodes.push(*inst);
            }
        }
        f.bytecodes = res_bytecodes;
    }
    res
}
fn optimize(program: vm::Program) -> vm::Program {
    // remove_redundunt_mov(program);
    program
}
pub fn gen_bytecode(mir: mir::Mir, config: Config) -> vm::Program {
    let mut generator = ByteCodeGenerator::default();
    let program = generator.generate(mir, config);
    optimize(program)
}

#[cfg(test)]
mod test {

    use crate::compiler::IoChannelInfo;
    use crate::interner::ToSymbol;
    #[test]
    fn build() {
        use super::*;
        use crate::numeric;
        use crate::types::PType;
        use crate::types::Type;
        extern crate colog;
        // colog::default_builder()
        //     .filter_level(log::LevelFilter::Trace)
        //     .init();
        // fn test(hoge){
        //   hoge+1
        //}
        let mut src = mir::Mir::default();
        let arg = mir::Argument("hoge".to_symbol(), numeric!());
        let argv = Arc::new(mir::Value::Argument(0));
        let mut func = mir::Function::new(
            0,
            "dsp".to_symbol(),
            std::slice::from_ref(&arg),
            vec![],
            None,
        );
        func.return_type.get_or_init(|| numeric!());
        let mut block = mir::Block::default();
        let resint = Arc::new(mir::Value::Register(1));
        block.0.push((resint.clone(), mir::Instruction::Integer(1)));
        let res = Arc::new(mir::Value::Register(2));
        block
            .0
            .push((res.clone(), mir::Instruction::AddF(argv, resint)));
        block.0.push((
            Arc::new(mir::Value::None),
            mir::Instruction::Return(res.clone(), numeric!()),
        ));
        func.body = vec![block];
        src.functions.push(func);
        let mut generator = ByteCodeGenerator::default();
        let config = Config::default();
        let res = generator.generate(src, config);

        let mut answer = vm::Program {
            iochannels: Some(IoChannelInfo {
                input: 1,
                output: 1,
            }),
            ..Default::default()
        };

        let mut main = vm::FuncProto::new(1, 1);
        main.constants.push(1);
        main.bytecodes = vec![
            VmInstruction::MoveConst(1, 0),
            VmInstruction::AddF(1, 0, 1),
            VmInstruction::Return(1, 1),
        ];
        answer.global_fn_table.push(("dsp".to_string(), main));
        answer.dsp_index = Some(0);
        assert_eq!(res, answer);
    }
}
