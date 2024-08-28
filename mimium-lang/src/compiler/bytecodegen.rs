use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use crate::interner::{Symbol, TypeNodeId};
use crate::mir::{self, Mir, StateSize};
use crate::runtime::vm::bytecode::{ConstPos, GlobalPos, Reg};
use crate::runtime::vm::{self};
use crate::types::{Type, TypeSize};
use crate::utils::error::ReportableError;
use vm::bytecode::Instruction as VmInstruction;

#[derive(Debug, Clone, PartialEq)]
enum Region {
    Value(Arc<mir::Value>),
    SubRegion(),
}
impl std::fmt::Display for Region {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Region::Value(v) => write!(f, "{}", v),
            Region::SubRegion() => {
                write!(f, "sub")
            }
        }
    }
}
#[derive(Debug, Default)]
struct MemoryRegion(Reg, TypeSize);
#[derive(Debug, Default)]
struct VRegister(HashMap<Arc<mir::Value>, MemoryRegion>);

fn tostring_helper(vec: &[Option<Region>]) -> String {
    vec[0..10].iter().fold("".to_string(), |s, a| {
        format!(
            "{s} {}",
            a.clone().map_or("()".to_string(), |a| format!("{a}"))
        )
    })
}

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
            (Some(_), mir::Value::Argument(_, _)) | (Some(_), mir::Value::Global(_)) => res,
            (Some(_), _) => {
                self.0.remove(v);
                res
            }
            _ => None,
        }
    }
    pub fn find_range(&mut self, v: &Arc<mir::Value>, _size: usize) -> Option<Reg> {
        self.find(v)
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
    fnmap: HashMap<Symbol, usize>,
    globals: Vec<Arc<mir::Value>>,
    program: vm::Program,
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
    //Calculate byte size of the value for type T based on 1 word size (=currently 64bit).
    //The base word size may change depending on the backend in the future.
    fn word_size_for_type(ty: TypeNodeId) -> TypeSize {
        match ty.to_type() {
            Type::Primitive(_) => 1,
            Type::Array(_ty) => todo!(),
            Type::Tuple(types) => types.iter().map(|t| Self::word_size_for_type(*t)).sum(),
            Type::Struct(types) => types
                .iter()
                .map(|(_s, t)| Self::word_size_for_type(*t))
                .sum(),
            Type::Function(_, _, _) => 1,
            Type::Ref(_) => 1,
            Type::Code(_) => todo!(),
            Type::TypeScheme(_, _) => todo!(),
            Type::Intermediate(_) => 1, // TODO
            Type::Unknown => todo!(),
        }
    }
    fn calc_state_size<T: AsRef<[StateSize]>>(state_sizes: T) -> u64 {
        state_sizes
            .as_ref()
            .iter()
            .map(|x| x.size * Self::word_size_for_type(x.ty) as u64)
            .sum()
    }
    fn get_binop(&mut self, v1: &Arc<mir::Value>, v2: &Arc<mir::Value>) -> (Reg, Reg) {
        let r1 = self.find(v1);
        let r2 = self.find(v2);
        (r1, r2)
    }
    fn emit_binop1<F>(
        &mut self,
        inst: F,
        dst: &Arc<mir::Value>,
        v1: &Arc<mir::Value>,
    ) -> Option<VmInstruction>
    where
        F: FnOnce(Reg, Reg) -> VmInstruction,
    {
        let r1 = self.find(v1);
        let dst = self.get_destination(dst.clone(), 1);
        let i = inst(dst, r1);
        Some(i)
    }
    fn emit_binop2<F>(
        &mut self,
        inst: F,
        dst: &Arc<mir::Value>,
        v1: &Arc<mir::Value>,
        v2: &Arc<mir::Value>,
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
    fn get_or_insert_global(&mut self, gv: Arc<mir::Value>) -> GlobalPos {
        match self.globals.iter().position(|v| gv == *v) {
            Some(idx) => idx as GlobalPos,
            None => {
                self.globals.push(gv.clone());
                let idx = (self.globals.len() - 1) as GlobalPos;
                self.program.global_vals.push(idx as u64);
                idx
            }
        }
    }
    fn find(&mut self, v: &Arc<mir::Value>) -> Reg {
        self.vregister
            .find(v)
            .or_else(|| self.globals.iter().position(|gv| v == gv).map(|v| v as Reg))
            .expect(format!("value {v} not found").as_str())
    }
    fn find_keep(&mut self, v: &Arc<mir::Value>) -> Reg {
        self.vregister
            .find_keep(v)
            .or_else(|| self.globals.iter().position(|gv| v == gv).map(|v| v as Reg))
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
        for (_i, (a, ty)) in args.iter().enumerate() {
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
    fn emit_instruction(
        &mut self,
        funcproto: &mut vm::FuncProto,
        bytecodes_dst: Option<&mut Vec<VmInstruction>>,
        fidx: usize,
        mirfunc: &mir::Function,
        dst: Arc<mir::Value>,
        mirinst: &mir::Instruction,
    ) -> Option<VmInstruction> {
        match mirinst {
            mir::Instruction::Uinteger(u) => {
                let pos = funcproto.add_new_constant(*u);
                Some(VmInstruction::MoveConst(
                    self.get_destination(dst, 1),
                    pos as ConstPos,
                ))
            }
            mir::Instruction::Integer(i) => {
                let pos = funcproto.add_new_constant(gen_raw_int(i));
                Some(VmInstruction::MoveConst(
                    self.get_destination(dst, 1),
                    pos as ConstPos,
                ))
            }
            mir::Instruction::Float(n) => {
                let pos = funcproto.add_new_constant(gen_raw_float(n));
                Some(VmInstruction::MoveConst(
                    self.get_destination(dst, 1),
                    pos as ConstPos,
                ))
            }
            mir::Instruction::Alloc(t) => {
                let size = Self::word_size_for_type(*t) as u64;
                let _ = self.vregister.push_stack(&dst, size);
                None
            }
            mir::Instruction::Load(ptr, ty) => {
                let d = self.get_destination(dst, Self::word_size_for_type(*ty));
                let s = self.find_keep(ptr);
                let size = Self::word_size_for_type(*ty);
                match (d, s, size) {
                    (d, s, 1) if d != s => Some(VmInstruction::Move(d, s)),
                    (d, s, size) if d != s => Some(VmInstruction::MoveRange(d, s, size)),
                    _ => None,
                }
            }
            mir::Instruction::Store(dst, src, ty) => {
                let s = self.find(src);
                let d = self.find_keep(dst);
                let size = Self::word_size_for_type(*ty);
                match (d, s, size) {
                    (d, s, 1) if d != s => Some(VmInstruction::Move(d, s)),
                    (d, s, size) if d != s => Some(VmInstruction::MoveRange(d, s, size)),
                    _ => None,
                }
            }
            mir::Instruction::GetGlobal(v, ty) => {
                let dst = self.get_destination(dst, Self::word_size_for_type(*ty));
                let idx = self.get_or_insert_global(v.clone());
                Some(VmInstruction::GetGlobal(
                    dst,
                    idx,
                    Self::word_size_for_type(*ty),
                ))
            }
            mir::Instruction::SetGlobal(v, src, ty) => {
                let idx = self.get_or_insert_global(v.clone());
                let s = self.find(src);
                Some(VmInstruction::SetGlobal(
                    idx,
                    s,
                    Self::word_size_for_type(*ty),
                ))
            }
            mir::Instruction::GetElement {
                value,
                ty,
                array_idx,
                tuple_offset,
            } => {
                let ptr = self.find_keep(value) as usize;
                let t_size = Self::word_size_for_type(*ty);
                let ty = ty.to_type();
                let tvec = ty.get_as_tuple().unwrap();
                let tsize = Self::word_size_for_type(tvec[*tuple_offset as usize]);
                let t_offset: u64 = tvec[0..(*tuple_offset as _)]
                    .iter()
                    .map(|t| Self::word_size_for_type(*t) as u64)
                    .sum();
                let offset = t_size as u64 * *array_idx + t_offset;
                let address = (ptr + offset as usize) as Reg;
                self.vregister
                    .get_top()
                    .0
                    .insert(dst, MemoryRegion(address, tsize));
                None
            }
            mir::Instruction::Call(v, args, r_ty) => {
                let rsize = Self::word_size_for_type(*r_ty);
                match v.as_ref() {
                    mir::Value::Register(address) => {
                        let bytecodes_dst =
                            bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                        let d = self.get_destination(dst.clone(), rsize);
                        let s = self.find(v);
                        bytecodes_dst.push(VmInstruction::Move(d, s));
                        let (fadd, argsize) = self.prepare_function(bytecodes_dst, &dst, args);
                        Some(VmInstruction::Call(fadd, argsize, rsize))
                    }
                    mir::Value::Function(_idx) => {
                        unreachable!();
                    }
                    mir::Value::ExtFunction(label, ty) => {
                        //todo: use btreemap
                        let idx = {
                            self.program.ext_fun_table.push((*label, *ty));
                            self.program.ext_fun_table.len() - 1
                        };
                        let fi = funcproto.add_new_constant(idx as u64);
                        let bytecodes_dst =
                            bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                        let f = self.vregister.push_stack(&dst, rsize as _);
                        bytecodes_dst.push(VmInstruction::MoveConst(f, fi as ConstPos));
                        let (dst, argsize) = self.prepare_function(bytecodes_dst, &dst, args);
                        let nret = Self::word_size_for_type(*ty);
                        Some(VmInstruction::CallExtFun(dst as Reg, argsize, nret))
                    }
                    mir::Value::FixPoint(_) => {
                        unreachable!("fixpoint should be called with callcls.")
                    }
                    _ => unreachable!(),
                }
            }
            mir::Instruction::CallCls(f, args, r_ty) => {
                let rsize = Self::word_size_for_type(*r_ty);
                match f.as_ref() {
                    mir::Value::Register(_address) => {
                        let bytecodes_dst =
                            bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());

                        let (fadd, argsize) = self.prepare_function(bytecodes_dst, f, args);
                        let s = self.find(f);
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
                    mir::Value::ExtFunction(_idx, _) => {
                        todo!()
                        // VmInstruction::CallExtFun(idx as Reg, nargs, 1)
                    }
                    _ => unreachable!(),
                }
            }
            mir::Instruction::Closure(idxcell) => {
                let idx = self.find(idxcell);
                let dst = self.get_destination(dst, 1);
                Some(VmInstruction::Closure(dst, idx))
            }
            mir::Instruction::GetUpValue(i, ty) => {
                let upval = &mirfunc.upindexes[*i as usize];
                let v = self.find_upvalue(upval);
                let size: u8 = Self::word_size_for_type(*ty);
                let ouv = mir::OpenUpValue(v as usize, size);
                if let Some(ui) = funcproto.upindexes.get_mut(*i as usize) {
                    *ui = ouv;
                } else {
                    funcproto.upindexes.push(ouv);
                }
                let d = self.vregister.get_top().add_newvalue_range(&dst, size as _);
                Some(VmInstruction::GetUpValue(
                    d,
                    *i as Reg,
                    Self::word_size_for_type(*ty),
                ))
            }
            mir::Instruction::SetUpValue(i, ty) => {
                let upval = &mirfunc.upindexes[*i as usize];
                let v = self.find_upvalue(upval);
                let size: u8 = Self::word_size_for_type(*ty);
                let ouv = mir::OpenUpValue(v as usize, size);
                if let Some(ui) = funcproto.upindexes.get_mut(*i as usize) {
                    *ui = ouv;
                } else {
                    funcproto.upindexes.push(ouv);
                }
                let d = self.vregister.get_top().add_newvalue_range(&dst, size as _);
                Some(VmInstruction::SetUpValue(
                    d,
                    *i as Reg,
                    Self::word_size_for_type(*ty),
                ))
            }
            mir::Instruction::PushStateOffset(v) => {
                Some(VmInstruction::ShiftStatePos(Self::calc_state_size(v) as i16))
            }
            mir::Instruction::PopStateOffset(v) => Some(VmInstruction::ShiftStatePos(
                -(Self::calc_state_size(v) as i16),
            )),
            mir::Instruction::GetState(ty) => {
                let size = Self::word_size_for_type(*ty);
                let d = self.vregister.push_stack(&dst, size as _);
                Some(VmInstruction::GetState(d, size))
            }

            mir::Instruction::JmpIf(cond, tbb, ebb) => {
                let c = self.find(cond);
                let mut then_bytecodes: Vec<VmInstruction> = vec![];
                let mut else_bytecodes: Vec<VmInstruction> = vec![];
                mirfunc.body[*tbb as usize]
                    .0
                    .iter()
                    .for_each(|(dst, t_inst)| {
                        if let Some(inst) = self.emit_instruction(
                            funcproto,
                            Some(&mut then_bytecodes),
                            fidx,
                            mirfunc,
                            dst.clone(),
                            t_inst,
                        ) {
                            then_bytecodes.push(inst);
                        }
                    });
                let else_offset = then_bytecodes.len() + 3; //add offset to jmp inst and loading phi

                mirfunc.body[*ebb as usize]
                    .0
                    .iter()
                    .for_each(|(dst, t_inst)| {
                        if let Some(inst) = self.emit_instruction(
                            funcproto,
                            Some(&mut else_bytecodes),
                            fidx,
                            mirfunc,
                            dst.clone(),
                            t_inst,
                        ) {
                            else_bytecodes.push(inst);
                        };
                    });
                let (phidst, pinst) = mirfunc.body[(*ebb + 1) as usize].0.first().unwrap();
                let phi = self.vregister.add_newvalue(phidst);
                if let mir::Instruction::Phi(t, e) = pinst {
                    let t = self.find(t);
                    then_bytecodes.push(VmInstruction::Move(phi, t));
                    let e = self.find(e);
                    else_bytecodes.push(VmInstruction::Move(phi, e));
                } else {
                    unreachable!();
                }
                funcproto
                    .bytecodes
                    .push(VmInstruction::JmpIfNeg(c, else_offset as i16));

                let ret_offset = else_bytecodes.len() + 1;

                then_bytecodes.push(VmInstruction::Jmp(ret_offset as i16));

                funcproto.bytecodes.append(&mut then_bytecodes);
                funcproto.bytecodes.append(&mut else_bytecodes);

                // TODO: probably need to infer type to determine the correct nret
                let nret = 1;
                Some(VmInstruction::Return(phi, nret))
            }
            mir::Instruction::Jmp(offset) => Some(VmInstruction::Jmp(*offset)),
            mir::Instruction::Phi(_, _) => {
                unreachable!()
            }
            mir::Instruction::Return(v, rty) => {
                let nret = Self::word_size_for_type(*rty);
                let inst = match v.as_ref() {
                    mir::Value::None => VmInstruction::Return0,
                    _ => VmInstruction::Return(self.find(v), nret),
                };
                Some(inst)
            }
            mir::Instruction::ReturnFeed(new, rty) => {
                //for returning always 0 at t=0
                let old = self.vregister.add_newvalue(&dst);
                let bytecodes_dst = bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                let size = Self::word_size_for_type(*rty);
                bytecodes_dst.push(VmInstruction::GetState(old, size));
                let new = self.find(new);
                bytecodes_dst.push(VmInstruction::SetState(new, size));
                Some(VmInstruction::Return(old, size))
                // Some(VmInstruction::Return(new, nret))
            }
            mir::Instruction::Delay(max, src, time) => {
                let s = self.find(src);
                let t = self.find(time);

                let dst = self.vregister.add_newvalue(&dst);
                funcproto.delay_sizes.push(*max as u64);
                Some(VmInstruction::Delay(dst, s, t))
            }
            mir::Instruction::Mem(src) => {
                let s = self.find(src);
                let dst = self.vregister.add_newvalue(&dst);
                Some(VmInstruction::Mem(dst, s))
            }
            mir::Instruction::NegF(v1) => self.emit_binop1(VmInstruction::NegF, &dst, v1),
            mir::Instruction::AddF(v1, v2) => self.emit_binop2(VmInstruction::AddF, &dst, v1, v2),
            mir::Instruction::SubF(v1, v2) => self.emit_binop2(VmInstruction::SubF, &dst, v1, v2),
            mir::Instruction::MulF(v1, v2) => self.emit_binop2(VmInstruction::MulF, &dst, v1, v2),
            mir::Instruction::DivF(v1, v2) => self.emit_binop2(VmInstruction::DivF, &dst, v1, v2),
            mir::Instruction::ModF(v1, v2) => self.emit_binop2(VmInstruction::ModF, &dst, v1, v2),
            mir::Instruction::PowF(v1, v2) => self.emit_binop2(VmInstruction::PowF, &dst, v1, v2),
            mir::Instruction::SinF(v1) => self.emit_binop1(VmInstruction::SinF, &dst, v1),
            mir::Instruction::CosF(v1) => self.emit_binop1(VmInstruction::CosF, &dst, v1),
            mir::Instruction::AbsF(v1) => self.emit_binop1(VmInstruction::AbsF, &dst, v1),
            mir::Instruction::SqrtF(v1) => self.emit_binop1(VmInstruction::SqrtF, &dst, v1),
            mir::Instruction::AddI(v1, v2) => self.emit_binop2(VmInstruction::AddI, &dst, v1, v2),
            mir::Instruction::SubI(v1, v2) => self.emit_binop2(VmInstruction::SubI, &dst, v1, v2),
            mir::Instruction::MulI(v1, v2) => self.emit_binop2(VmInstruction::MulI, &dst, v1, v2),
            mir::Instruction::DivI(v1, v2) => self.emit_binop2(VmInstruction::DivI, &dst, v1, v2),
            mir::Instruction::ModI(v1, v2) => self.emit_binop2(VmInstruction::ModI, &dst, v1, v2),
            mir::Instruction::Gt(v1, v2) => self.emit_binop2(VmInstruction::Gt, &dst, v1, v2),
            mir::Instruction::Ge(v1, v2) => self.emit_binop2(VmInstruction::Ge, &dst, v1, v2),
            mir::Instruction::Lt(v1, v2) => self.emit_binop2(VmInstruction::Lt, &dst, v1, v2),
            mir::Instruction::Le(v1, v2) => self.emit_binop2(VmInstruction::Le, &dst, v1, v2),
            mir::Instruction::Eq(v1, v2) => self.emit_binop2(VmInstruction::Eq, &dst, v1, v2),
            mir::Instruction::Ne(v1, v2) => self.emit_binop2(VmInstruction::Ne, &dst, v1, v2),
            mir::Instruction::And(v1, v2) => self.emit_binop2(VmInstruction::And, &dst, v1, v2),
            mir::Instruction::Or(v1, v2) => self.emit_binop2(VmInstruction::Or, &dst, v1, v2),

            _ => {
                unimplemented!()
            }
        }
    }
    fn generate_funcproto(
        &mut self,
        mirfunc: &mir::Function,
        fidx: usize,
    ) -> (Symbol, vm::FuncProto) {
        log::trace!("generating function {}", mirfunc.label.0);
        let state_size = Self::calc_state_size(mirfunc.get_state_sizes());
        let mut func = vm::FuncProto {
            nparam: mirfunc.args.len(),
            nret: Self::word_size_for_type(
                *mirfunc
                    .return_type
                    .get()
                    .expect("return type not inferred correctly"),
            ) as _,
            upindexes: vec![],
            bytecodes: vec![],
            constants: vec![],
            state_size,
            delay_sizes: vec![],
        };
        self.vregister.0.push(VRegister::default());
        for (a, t) in mirfunc.args.iter().zip(mirfunc.argtypes.iter()) {
            let size = Self::word_size_for_type(*t);
            self.vregister.push_stack(a, size as _);
        }

        // succeeding block will be compiled recursively
        let block = &mirfunc.body[0];
        block.0.iter().for_each(|(dst, inst)| {
            let newinst = self.emit_instruction(&mut func, None, fidx, mirfunc, dst.clone(), inst);
            if let Some(i) = newinst {
                func.bytecodes.push(i);
            }
        });
        (mirfunc.label, func)
    }
    pub fn generate(&mut self, mir: Mir) -> vm::Program {
        self.program.global_fn_table = mir
            .functions
            .iter()
            .enumerate()
            .map(|(i, func)| {
                self.fnmap.insert(func.label, i);
                self.generate_funcproto(func, i)
            })
            .collect();

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
            match pair {
                &[VmInstruction::Move(dst, src), VmInstruction::Move(dst2, src2)]
                    if dst == src2 && src == dst2 =>
                //case of swapping
                {
                    remove_idx.insert(i);
                    remove_idx.insert(i + 1);
                }
                &[VmInstruction::Move(dst, src), VmInstruction::Move(dst2, src2)]
                    if dst == src2 =>
                {
                    reduce_idx.insert(i, VmInstruction::Move(dst2, src));
                    remove_idx.insert(i + 1);
                }
                &[VmInstruction::MoveConst(dst, src), VmInstruction::Move(dst2, src2)]
                    if dst == src2 =>
                {
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
    // remove_redundunt_mov(program)
    program
}
pub fn gen_bytecode(mir: mir::Mir) -> Result<vm::Program, Vec<Box<dyn ReportableError>>> {
    let mut generator = ByteCodeGenerator::default();
    let program = generator.generate(mir);
    Ok(optimize(program))
}

#[cfg(test)]
mod test {
    use crate::interner::ToSymbol;

    #[test]
    fn build() {
        use super::*;
        use crate::numeric;
        use crate::types::PType;
        use crate::types::Type;
        extern crate colog;
        colog::default_builder()
            .filter_level(log::LevelFilter::Trace)
            .init();
        // fn test(hoge){
        //   hoge+1
        //}
        let mut src = mir::Mir::default();
        let arg = Arc::new(mir::Value::Argument(
            0,
            Arc::new(mir::Argument("hoge".to_symbol(), numeric!())),
        ));
        let mut func = mir::Function::new("test".to_symbol(), &[arg.clone()], &[numeric!()], None);
        func.return_type.get_or_init(|| numeric!());
        let mut block = mir::Block::default();
        let resint = Arc::new(mir::Value::Register(1));
        block.0.push((resint.clone(), mir::Instruction::Integer(1)));
        let res = Arc::new(mir::Value::Register(2));
        block
            .0
            .push((res.clone(), mir::Instruction::AddF(arg, resint)));
        block.0.push((
            Arc::new(mir::Value::None),
            mir::Instruction::Return(res.clone(), numeric!()),
        ));
        func.body = vec![block];
        src.functions.push(func);
        let mut generator = ByteCodeGenerator::default();
        let res = generator.generate(src);

        let mut answer = vm::Program::default();
        let mut main = vm::FuncProto::new(1, 1);

        main.constants.push(1);
        main.bytecodes = vec![
            VmInstruction::MoveConst(1, 0),
            VmInstruction::AddF(1, 0, 1),
            VmInstruction::Return(1, 1),
        ];
        answer.global_fn_table.push(("test".to_symbol(), main));
        assert_eq!(res, answer);
    }
}
