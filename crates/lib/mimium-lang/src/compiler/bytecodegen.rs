use std::collections::HashMap;
use std::sync::Arc;

use crate::interner::{Symbol, TypeNodeId};
use crate::mir::{self, Mir};
use crate::runtime::vm::bytecode::{ConstPos, GlobalPos, Reg};
use crate::runtime::vm::program::{JumpTable, WordSize};
use crate::runtime::vm::{self, StateOffset};
use crate::types::{PType, RecordTypeField, Type, TypeSize};
use crate::utils::half_float::HFloat;
use vm::bytecode::Instruction as VmInstruction;

#[derive(Debug, Default, Clone, Copy)]
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
    /// Calculate word size for a type. Now delegates to TypeNodeId::word_size().
    pub(crate) fn word_size_for_type(ty: TypeNodeId) -> TypeSize {
        ty.word_size()
    }

    fn tagged_union_payload_size(union_type: TypeNodeId, tag: u64) -> TypeSize {
        match union_type.to_type() {
            Type::Union(variants) => variants
                .get(tag as usize)
                .map(|variant_ty| Self::word_size_for_type(*variant_ty))
                .unwrap_or(0),
            Type::UserSum { variants, .. } => variants
                .get(tag as usize)
                .and_then(|(_, payload_ty)| *payload_ty)
                .map(Self::word_size_for_type)
                .unwrap_or(0),
            _ => 0,
        }
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

    fn emit_function_value_into_register(
        &mut self,
        bytecodes_dst: &mut Vec<VmInstruction>,
        fn_const_pos: ConstPos,
        dst: Reg,
    ) {
        let fn_reg_ref = Arc::new(mir::Value::None);
        let fn_reg = self.vregister.add_newvalue(&fn_reg_ref);
        bytecodes_dst.push(VmInstruction::MoveConst(fn_reg, fn_const_pos));
        bytecodes_dst.push(VmInstruction::MakeHeapClosure(dst, fn_reg, 0));
        bytecodes_dst.push(VmInstruction::CloneHeap(dst));
    }

    fn get_or_insert_global(&mut self, gv: Arc<mir::Value>, ty: TypeNodeId) -> GlobalPos {
        match self.globals.get(&gv) {
            Some(idx) => *idx as GlobalPos,
            None => {
                let size = Self::word_size_for_type(ty) as usize;
                // The start offset is the sum of all existing global sizes.
                let idx: usize = self.program.global_vals.iter().map(|w| w.0 as usize).sum();
                self.globals.insert(gv.clone(), idx);
                let size = WordSize(size as _);
                self.program.global_vals.push(size);
                idx as GlobalPos
            }
        }
    }
    fn find(&mut self, v: &Arc<mir::Value>) -> Reg {
        if let mir::Value::Function(idx) = v.as_ref() {
            return *idx as Reg;
        }
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
        let (aoffsets, offset): (Vec<(TypeSize, Reg, TypeSize)>, TypeSize) = args
            .iter()
            .filter_map(|(a, ty)| {
                let size = Self::word_size_for_type(*ty);
                (size != 0 && !matches!(a.as_ref(), mir::Value::None)).then_some((a, ty, size))
            })
            .fold((vec![], 0 as TypeSize), |(mut aoffsets, offset), (a, ty, size)| {
                let src = self.find(a);
                let next_offset = offset.checked_add(size).unwrap_or_else(|| {
                    panic!(
                        "bytecodegen: offset overflow in prepare_function: offset={}, size={}, type={:?}",
                        offset, size, ty.to_type()
                    )
                });
                aoffsets.push((offset, src, size));
                (aoffsets, next_offset)
            });
        let faddress = self.find_keep(faddress);
        let placements: Vec<(TypeSize, Reg, TypeSize)> = aoffsets
            .iter()
            .map(|(adst, src, size)| {
                let address = adst
                    .checked_add(faddress)
                    .and_then(|v| v.checked_add(1))
                    .unwrap_or_else(|| {
                        panic!(
                            "bytecodegen: address overflow in prepare_function: adst={}, faddress={}",
                            adst, faddress
                        )
                    });
                (address, *src, *size)
            })
            .collect();

        let ranges_overlap =
            |a_start: TypeSize, a_size: TypeSize, b_start: TypeSize, b_size: TypeSize| {
                let a_end = a_start + a_size;
                let b_end = b_start + b_size;
                a_start < b_end && b_start < a_end
            };

        let has_overlap = placements.iter().any(|(dst, _src, dsize)| {
            placements.iter().any(|(_odst, osrc, osize)| {
                ranges_overlap(*dst, *dsize, *osrc, *osize) && !(*dst == *osrc && *dsize == *osize)
            })
        });

        let staged: Vec<(TypeSize, Reg, TypeSize)> = if has_overlap {
            placements
                .iter()
                .enumerate()
                .map(|(idx, (dst, src, size))| {
                    let temp_key = Arc::new(mir::Value::Register(u64::MAX - idx as u64));
                    let tmp = self
                        .vregister
                        .get_top()
                        .add_newvalue_range(&temp_key, *size as u64);
                    match size {
                        0 => unreachable!(),
                        1 => bytecodes_dst.push(VmInstruction::Move(tmp, *src)),
                        _ => bytecodes_dst.push(VmInstruction::MoveRange(tmp, *src, *size)),
                    }
                    (*dst, tmp, *size)
                })
                .collect()
        } else {
            placements
        };

        for (dst, src, size) in staged.iter() {
            let is_samedst = *dst == *src;
            if !is_samedst {
                match size {
                    0 => unreachable!(),
                    1 => bytecodes_dst.push(VmInstruction::Move(*dst as Reg, *src)),
                    _ => bytecodes_dst.push(VmInstruction::MoveRange(*dst as Reg, *src, *size)),
                }
            }
        }
        (faddress, offset)
    }
    fn get_or_insert_extfunid(&mut self, label: Symbol, ty: TypeNodeId) -> ConstPos {
        self.program
            .ext_fun_table
            .iter()
            .position(|(name, existing_ty)| name.as_str() == label.as_str() && *existing_ty == ty)
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
        fn_ty: TypeNodeId,
        ret_ty: TypeNodeId,
    ) -> (Reg, Reg, TypeSize) {
        let idx = self.get_or_insert_extfunid(label, fn_ty);
        self.prepare_extfun_or_cls(funcproto, bytecodes_dst, dst, args, idx, ret_ty)
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
                let d = self.find_keep(&dst);
                let size = Self::word_size_for_type(ty);

                if matches!(ty.to_type(), Type::Function { .. })
                    && let mir::Value::Function(fn_idx) = src.as_ref()
                {
                    let fn_const_pos = funcproto.add_new_constant(*fn_idx as vm::RawVal) as ConstPos;
                    let bytecodes_dst =
                        bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                    self.emit_function_value_into_register(bytecodes_dst, fn_const_pos, d);
                    return None;
                }

                let s = self.find(&src);
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

                if matches!(ty.to_type(), Type::Function { .. })
                    && let mir::Value::Function(fn_idx) = src.as_ref()
                {
                    let tmp_ref = Arc::new(mir::Value::None);
                    let tmp_reg = self.vregister.add_newvalue(&tmp_ref);
                    let fn_const_pos = funcproto.add_new_constant(*fn_idx as vm::RawVal) as ConstPos;
                    let bytecodes_dst =
                        bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                    self.emit_function_value_into_register(bytecodes_dst, fn_const_pos, tmp_reg);
                    return Some(VmInstruction::SetGlobal(idx, tmp_reg, 1));
                }

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
                let elem_tys: Vec<TypeNodeId> = match ty {
                    Type::Tuple(elems) => elems,
                    Type::Record(fields) => fields.iter().map(|f| f.ty).collect(),
                    _ => panic!("GetElement expects tuple or record type, got {:?}", ty),
                };
                let tsize = Self::word_size_for_type(elem_tys[tuple_offset as usize]);
                let t_offset: u64 = elem_tys[0..(tuple_offset as _)]
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
                        Some(VmInstruction::Call(fadd, argsize as u8, rsize))
                    }
                    mir::Value::Function(_idx) => {
                        unreachable!();
                    }
                    mir::Value::ExtFunction(label, f_ty) => {
                        //todo: use btreemap
                        let (dst, argsize, nret) = self.prepare_extfun(
                            funcproto,
                            bytecodes_dst,
                            dst,
                            &args,
                            *label,
                            *f_ty,
                            r_ty,
                        );
                        Some(VmInstruction::CallExtFun(dst, argsize as u8, nret))
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
                        bytecodes_dst.push(VmInstruction::CallCls(fadd, argsize as u8, rsize));
                        match rsize {
                            0 => None,
                            1 => Some(VmInstruction::Move(d, s)),
                            n => Some(VmInstruction::MoveRange(d, s, n)),
                        }
                    }
                    mir::Value::Function(_idx) => {
                        unreachable!();
                    }
                    mir::Value::ExtFunction(label, f_ty) => {
                        let (dst, argsize, nret) = self.prepare_extfun(
                            funcproto,
                            bytecodes_dst,
                            dst,
                            &args,
                            *label,
                            *f_ty,
                            r_ty,
                        );
                        Some(VmInstruction::CallExtFun(dst, argsize as u8, nret))
                    }
                    _ => unreachable!(),
                }
            }
            mir::Instruction::Closure(idxcell) => {
                let idx = match idxcell.as_ref() {
                    mir::Value::Function(idx) => *idx as Reg,
                    _ => self.find(&idxcell),
                };
                let dst = self.get_destination(dst, 1);
                Some(VmInstruction::Closure(dst, idx))
            }
            // New heap-based instructions (Phase 3)
            mir::Instruction::MakeClosure { fn_proto, size } => {
                let fn_idx = match fn_proto.as_ref() {
                    mir::Value::Function(idx) => *idx as Reg,
                    _ => self.find(&fn_proto),
                };
                let dst = self.get_destination(dst, 1);
                Some(VmInstruction::MakeHeapClosure(
                    dst,
                    fn_idx,
                    size as TypeSize,
                ))
            }
            mir::Instruction::CloseHeapClosure(src) => {
                // Close upvalues of the heap-based closure
                // This is called when a closure escapes its defining scope
                let base = self.vregister.find_keep(&src).unwrap();
                Some(VmInstruction::CloseHeapClosure(base))
            }
            mir::Instruction::CloneHeap(src) => {
                // Increment reference count for call-by-value cloning
                let base = self.vregister.find_keep(&src).unwrap();
                Some(VmInstruction::CloneHeap(base))
            }
            mir::Instruction::CallIndirect(f, args, r_ty) => {
                let rsize = Self::word_size_for_type(r_ty);
                match f.as_ref() {
                    mir::Value::Register(_address) => {
                        let bytecodes_dst =
                            bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());

                        let (fadd, argsize) = self.prepare_function(bytecodes_dst, &f, &args);
                        let s = self.find(&f);
                        let d = self.get_destination(dst.clone(), rsize);
                        bytecodes_dst.push(VmInstruction::CallIndirect(fadd, argsize as u8, rsize));
                        match rsize {
                            0 => None,
                            1 => Some(VmInstruction::Move(d, s)),
                            n => Some(VmInstruction::MoveRange(d, s, n)),
                        }
                    }
                    mir::Value::Function(_idx) => {
                        unreachable!();
                    }
                    mir::Value::ExtFunction(label, f_ty) => {
                        let (dst, argsize, nret) = self.prepare_extfun(
                            funcproto,
                            bytecodes_dst,
                            dst,
                            &args,
                            *label,
                            *f_ty,
                            r_ty,
                        );
                        Some(VmInstruction::CallExtFun(dst, argsize as u8, nret))
                    }
                    _ => unreachable!(),
                }
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
                let size: TypeSize = Self::word_size_for_type(ty);
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
                let size: TypeSize = Self::word_size_for_type(ty);
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
                let state_size = StateOffset::try_from(v).expect("too much large state offset.");
                Some(VmInstruction::PushStatePos(state_size))
            }
            mir::Instruction::PopStateOffset(v) => {
                let state_size = StateOffset::try_from(v).expect("too much large state offset.");
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
                if let mir::Instruction::Phi(t, e) = pinst {
                    let t_size = self
                        .vregister
                        .get_top()
                        .0
                        .get(t)
                        .map(|MemoryRegion(_, size)| *size)
                        .unwrap_or(1);
                    let e_size = self
                        .vregister
                        .get_top()
                        .0
                        .get(e)
                        .map(|MemoryRegion(_, size)| *size)
                        .unwrap_or(1);
                    let phi_size = std::cmp::max(t_size, e_size);
                    let phi = self.get_destination(phidst.clone(), phi_size);

                    let t = self.find(t);
                    then_bytecodes.push(if phi_size == 1 {
                        VmInstruction::Move(phi, t)
                    } else {
                        VmInstruction::MoveRange(phi, t, phi_size)
                    });
                    let e = self.find(e);
                    else_bytecodes.push(if phi_size == 1 {
                        VmInstruction::Move(phi, e)
                    } else {
                        VmInstruction::MoveRange(phi, e, phi_size)
                    });
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

                phiblock
                    .iter()
                    .skip(1)
                    .fold(vec![], |mut bytes: Vec<VmInstruction>, (dst, p_inst)| {
                        if let Some(inst) = self.emit_instruction(
                            funcproto,
                            Some(&mut bytes),
                            mirfunc.clone(),
                            dst.clone(),
                            p_inst.clone(),
                            config,
                        ) {
                            bytes.push(inst);
                        }
                        bytes
                    })
                    .into_iter()
                    .for_each(|inst| match &mut bytecodes_dst {
                        Some(dst) => dst.push(inst),
                        None => funcproto.bytecodes.push(inst),
                    });
                None
            }
            mir::Instruction::Jmp(offset) => Some(VmInstruction::Jmp(offset)),
            mir::Instruction::Phi(_, _) => {
                unreachable!()
            }
            mir::Instruction::PhiSwitch(_) => {
                unreachable!("PhiSwitch should be handled within Switch processing")
            }
            // Tagged Union instructions (Phase 2)
            // Represents tagged unions as (tag, value) tuples using consecutive registers
            mir::Instruction::TaggedUnionWrap {
                tag,
                value,
                union_type,
            } => {
                let total_size = Self::word_size_for_type(union_type);
                let payload_capacity = total_size.saturating_sub(1);
                let payload_size = Self::tagged_union_payload_size(union_type, tag);

                let dst_reg = self.vregister.push_stack(&dst, total_size as u64);
                let tag_pos = funcproto.add_new_constant(tag);
                let zero_pos = funcproto.add_new_constant(0);
                let target = bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());

                target.push(VmInstruction::MoveConst(dst_reg, tag_pos as ConstPos));

                (0..payload_capacity).for_each(|index| {
                    target.push(VmInstruction::MoveConst(
                        dst_reg + 1 + index,
                        zero_pos as ConstPos,
                    ));
                });

                if !matches!(value.as_ref(), mir::Value::None) && payload_size > 0 {
                    let val_reg = self.find(&value);
                    let val_dst = dst_reg + 1;
                    if payload_size == 1 {
                        target.push(VmInstruction::Move(val_dst, val_reg));
                    } else {
                        target.push(VmInstruction::MoveRange(val_dst, val_reg, payload_size));
                    }
                }

                None
            }
            mir::Instruction::TaggedUnionGetTag(union_val) => {
                // Extract tag (first register of the tagged union tuple)
                let union_reg = self.find_keep(&union_val);
                let dst_reg = self.get_destination(dst, 1);

                // Tag is at union_reg + 0
                Some(VmInstruction::Move(dst_reg, union_reg))
            }
            mir::Instruction::TaggedUnionGetValue(union_val, value_ty) => {
                // Extract value (second register onwards of the tagged union tuple)
                let union_reg = self.find_keep(&union_val);
                let value_size = Self::word_size_for_type(value_ty);
                let dst_reg = self.get_destination(dst, value_size);

                // Value starts at union_reg + 1
                let value_reg = union_reg + 1;

                if value_size == 1 {
                    Some(VmInstruction::Move(dst_reg, value_reg))
                } else {
                    Some(VmInstruction::MoveRange(dst_reg, value_reg, value_size))
                }
            }
            mir::Instruction::BoxAlloc { value, inner_type } => {
                let inner_size = Self::word_size_for_type(inner_type);
                let src_reg = self.find(&value);
                let dst_reg = self.get_destination(dst, 1); // HeapIdx is 1 word
                Some(VmInstruction::BoxAlloc(dst_reg, src_reg, inner_size))
            }
            mir::Instruction::BoxLoad { ptr, inner_type } => {
                let inner_size = Self::word_size_for_type(inner_type);
                let src_reg = self.find(&ptr);
                let dst_reg = self.get_destination(dst, inner_size);
                Some(VmInstruction::BoxLoad(dst_reg, src_reg, inner_size))
            }
            mir::Instruction::BoxClone { ptr } => {
                let src_reg = self.vregister.find_keep(&ptr).unwrap();
                Some(VmInstruction::BoxClone(src_reg))
            }
            mir::Instruction::BoxRelease { ptr, .. } => {
                let src_reg = self.vregister.find_keep(&ptr).unwrap();
                Some(VmInstruction::BoxRelease(src_reg))
            }
            mir::Instruction::BoxStore {
                ptr,
                value,
                inner_type,
            } => {
                let inner_size = Self::word_size_for_type(inner_type);
                let ptr_reg = self.vregister.find_keep(&ptr).unwrap();
                let val_reg = self.find(&value);
                Some(VmInstruction::BoxStore(ptr_reg, val_reg, inner_size))
            }
            mir::Instruction::CloneUserSum { value, ty } => {
                // Clone all boxed references within a UserSum value
                let value_reg = self.vregister.find_keep(&value).unwrap();
                let size = Self::word_size_for_type(ty);
                // Register type in type table and get index
                let type_idx = self
                    .program
                    .add_type_to_table(ty)
                    .expect("Type table overflow - too many UserSum types");
                Some(VmInstruction::CloneUserSum(value_reg, size, type_idx))
            }
            mir::Instruction::ReleaseUserSum { value, ty } => {
                // Release all boxed references within a UserSum value
                let value_reg = self.vregister.find_keep(&value).unwrap();
                let size = Self::word_size_for_type(ty);
                // Register type in type table and get index
                let type_idx = self
                    .program
                    .add_type_to_table(ty)
                    .expect("Type table overflow - too many UserSum types");
                Some(VmInstruction::ReleaseUserSum(value_reg, size, type_idx))
            }
            mir::Instruction::Switch {
                scrutinee,
                cases,
                default_block,
                merge_block,
            } => {
                // Switch is compiled using JmpTable instruction
                // For exhaustive matches (default_block = None):
                //   Structure: [JmpTable] [case0_body, Move, Jmp] ... [caseN_body, Move] [merge_block]
                // For non-exhaustive matches (default_block = Some):
                //   Structure: [JmpTable] [case0_body, Move, Jmp] ... [default_body, Move] [merge_block]
                let scrut_reg = self.find(&scrutinee);

                // Reserve jump table index BEFORE processing child blocks
                // This ensures the outer switch gets the correct index even if nested switches
                // add their tables first during child block processing
                let table_idx = funcproto.jump_tables.len() as u8;
                // Add placeholder - will be filled in later
                funcproto.jump_tables.push(JumpTable {
                    min: 0,
                    offsets: vec![],
                });

                // Get PhiSwitch destination register
                let merge_block_mir = &mirfunc.body[merge_block as usize];
                let (phi_dst, phi_inst) = merge_block_mir.0.first().unwrap();
                let phi_inputs = if let mir::Instruction::PhiSwitch(results) = phi_inst {
                    results.clone()
                } else {
                    panic!("Expected PhiSwitch in merge block");
                };

                // Helper closure to emit instructions for a basic block
                let mut emit_block = |this: &mut Self, block: &mir::Block| -> Vec<VmInstruction> {
                    block.0.iter().fold(vec![], |mut bytes, (bdst, binst)| {
                        if let Some(vm_inst) = this.emit_instruction(
                            funcproto,
                            Some(&mut bytes),
                            mirfunc.clone(),
                            bdst.clone(),
                            binst.clone(),
                            config,
                        ) {
                            bytes.push(vm_inst);
                        }
                        bytes
                    })
                };

                // Collect all bytecodes for each case block
                let all_block_bytes: Vec<Vec<VmInstruction>> = cases
                    .iter()
                    .map(|(_, block_idx)| {
                        let block = &mirfunc.body[*block_idx as usize];
                        emit_block(self, block)
                    })
                    .collect();

                // Default block - only emit if explicitly present
                let default_bytes = if let Some(default_idx) = default_block {
                    let default_block_mir = &mirfunc.body[default_idx as usize];
                    emit_block(self, default_block_mir)
                } else {
                    vec![]
                };
                let has_default = default_block.is_some();

                // Resolve PhiSwitch source registers after case/default blocks are emitted.
                let result_regs: Vec<(Reg, TypeSize)> = phi_inputs
                    .iter()
                    .map(|r| {
                        let reg = self.find_keep(r);
                        let size = self
                            .vregister
                            .get_top()
                            .0
                            .get(r)
                            .map(|MemoryRegion(_, size)| *size)
                            .unwrap_or(1);
                        (reg, size)
                    })
                    .collect();
                let phi_size = result_regs.iter().map(|(_, size)| *size).max().unwrap_or(1);
                let phi_reg = self.get_destination(phi_dst.clone(), phi_size);

                // Merge block remaining instructions
                let merge_bytes: Vec<VmInstruction> =
                    merge_block_mir
                        .0
                        .iter()
                        .skip(1)
                        .fold(vec![], |mut bytes, (bdst, binst)| {
                            if let Some(vm_inst) = self.emit_instruction(
                                funcproto,
                                Some(&mut bytes),
                                mirfunc.clone(),
                                bdst.clone(),
                                binst.clone(),
                                config,
                            ) {
                                bytes.push(vm_inst);
                            }
                            bytes
                        });

                // Calculate sizes:
                // - Most case segments: [body..., Move, Jmp]
                // - Last segment (exhaustive case or default): [body..., Move] (no Jmp needed)
                let num_cases = all_block_bytes.len();
                let case_segment_sizes: Vec<usize> = all_block_bytes
                    .iter()
                    .enumerate()
                    .map(|(i, b)| {
                        if !has_default && i == num_cases - 1 {
                            // Last case in exhaustive match - no Jmp needed
                            b.len() + 1 // body + Move
                        } else {
                            b.len() + 2 // body + Move + Jmp
                        }
                    })
                    .collect();

                // Default segment size (only if has_default)
                let default_segment_size = if has_default {
                    default_bytes.len() + 1 // body + Move (no Jmp needed)
                } else {
                    0
                };

                // Build dense jump table for O(1) lookup
                // Calculate min/max values from cases
                let min_val = cases.iter().map(|(v, _)| *v).min().unwrap_or(0);
                let max_val = cases.iter().map(|(v, _)| *v).max().unwrap_or(0);

                // Calculate offsets for each case using scan (cumulative sum)
                let case_offsets: Vec<(i64, i16)> = cases
                    .iter()
                    .zip(case_segment_sizes.iter())
                    .scan(1i16, |offset, ((lit_val, _), seg_size)| {
                        let current = *offset;
                        *offset += *seg_size as i16;
                        Some((*lit_val, current))
                    })
                    .collect();

                // Default offset: for exhaustive matches, use last case; otherwise, after all cases
                let default_offset = if has_default {
                    // Separate default block - offset is after all case blocks
                    case_offsets
                        .last()
                        .map(|(_, off)| {
                            *off + case_segment_sizes.last().copied().unwrap_or(0) as i16
                        })
                        .unwrap_or(1)
                } else {
                    // Exhaustive match - use last case as default
                    case_offsets.last().map(|(_, off)| *off).unwrap_or(1)
                };

                // Build dense array: fill with default, then set specific case offsets
                // Add one extra slot at the end for the default offset (used for out-of-range values)
                let table_size = (max_val - min_val + 1) as usize + 1; // +1 for default slot
                let offsets = case_offsets.iter().fold(
                    vec![default_offset; table_size],
                    |mut offsets, (lit_val, offset)| {
                        offsets[(lit_val - min_val) as usize] = *offset;
                        offsets
                    },
                );

                // Update the placeholder jump table that was reserved earlier
                funcproto.jump_tables[table_idx as usize] = JumpTable {
                    min: min_val,
                    offsets,
                };

                // Build the bytecode sequence
                let switch_bytes: Vec<VmInstruction> =
                    if has_default {
                        // Non-exhaustive: has a separate default block
                        std::iter::once(VmInstruction::JmpTable(scrut_reg, table_idx))
                            .chain(all_block_bytes.iter().enumerate().flat_map(
                                |(i, block_bytes)| {
                                    let remaining_size: usize =
                                        case_segment_sizes[i + 1..].iter().sum::<usize>()
                                            + default_segment_size
                                            + 1;
                                    block_bytes
                                        .iter()
                                        .cloned()
                                        .chain(std::iter::once(if result_regs[i].1 == 1 {
                                            VmInstruction::Move(phi_reg, result_regs[i].0)
                                        } else {
                                            VmInstruction::MoveRange(
                                                phi_reg,
                                                result_regs[i].0,
                                                result_regs[i].1,
                                            )
                                        }))
                                        .chain(std::iter::once(VmInstruction::Jmp(
                                            remaining_size as i16,
                                        )))
                                },
                            ))
                            .chain(default_bytes.iter().cloned())
                            .chain(std::iter::once(if result_regs.last().unwrap().1 == 1 {
                                VmInstruction::Move(phi_reg, result_regs.last().unwrap().0)
                            } else {
                                VmInstruction::MoveRange(
                                    phi_reg,
                                    result_regs.last().unwrap().0,
                                    result_regs.last().unwrap().1,
                                )
                            }))
                            .chain(merge_bytes.iter().cloned())
                            .collect()
                    } else {
                        // Exhaustive: last case falls through to merge
                        std::iter::once(VmInstruction::JmpTable(scrut_reg, table_idx))
                            .chain(all_block_bytes.iter().enumerate().flat_map(
                                |(i, block_bytes)| {
                                    let is_last = i == num_cases - 1;
                                    let remaining_size: usize =
                                        case_segment_sizes[i + 1..].iter().sum::<usize>() + 1;
                                    block_bytes
                                        .iter()
                                        .cloned()
                                        .chain(std::iter::once(if result_regs[i].1 == 1 {
                                            VmInstruction::Move(phi_reg, result_regs[i].0)
                                        } else {
                                            VmInstruction::MoveRange(
                                                phi_reg,
                                                result_regs[i].0,
                                                result_regs[i].1,
                                            )
                                        }))
                                        .chain(if is_last {
                                            // Last case - no Jmp, falls through to merge
                                            None
                                        } else {
                                            Some(VmInstruction::Jmp(remaining_size as i16))
                                        })
                                },
                            ))
                            .chain(merge_bytes.iter().cloned())
                            .collect()
                    };

                // Output everything to the destination
                let bytecodes_dst = bytecodes_dst.unwrap_or_else(|| funcproto.bytecodes.as_mut());
                bytecodes_dst.extend(switch_bytes);

                None
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
            mir::Instruction::CastFtoI(v1) => self.emit_binop1(VmInstruction::CastFtoI, dst, v1),
            mir::Instruction::CastItoF(v1) => self.emit_binop1(VmInstruction::CastItoF, dst, v1),
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
                let elem_is_function = ty.to_type().is_function();
                let size = values.len();
                let dst_reg = self.get_destination(dst.clone(), 1 as _); //address for array is always 1 word;

                let emit_array_init =
                    |this: &mut Self,
                     bytecodes_dst: &mut Vec<VmInstruction>,
                     add_const: &mut dyn FnMut(vm::RawVal) -> ConstPos| {
                        bytecodes_dst.push(VmInstruction::AllocArray(
                            dst_reg,
                            size as _,
                            elem_ty_size,
                        ));
                        for (i, val) in values.iter().enumerate() {
                            let src = if elem_is_function {
                                match val.as_ref() {
                                    mir::Value::Function(fn_idx) => {
                                        let fn_reg_ref = Arc::new(mir::Value::None);
                                        let fn_reg = this.vregister.add_newvalue(&fn_reg_ref);
                                        bytecodes_dst.push(VmInstruction::MoveConst(
                                            fn_reg,
                                            add_const(*fn_idx as vm::RawVal),
                                        ));

                                        let cls_reg_ref = Arc::new(mir::Value::None);
                                        let cls_reg = this.vregister.add_newvalue(&cls_reg_ref);
                                        bytecodes_dst.push(VmInstruction::MakeHeapClosure(
                                            cls_reg, fn_reg, 0,
                                        ));
                                        bytecodes_dst.push(VmInstruction::CloneHeap(cls_reg));
                                        cls_reg
                                    }
                                    _ => this.find(val),
                                }
                            } else {
                                this.find(val)
                            };
                            let tmp_idx_ref = Arc::new(mir::Value::None);
                            let idx = this.vregister.add_newvalue(&tmp_idx_ref);
                            bytecodes_dst.push(VmInstruction::MoveImmF(
                                idx,
                                HFloat::try_from(i as f64).unwrap(),
                            ));
                            let idx = this.find(&tmp_idx_ref);
                            bytecodes_dst.push(VmInstruction::SetArrayElem(dst_reg, idx, src));
                        }
                    };

                if let Some(bytecodes_dst) = bytecodes_dst {
                    let mut add_const = |cval: vm::RawVal| funcproto.add_new_constant(cval);
                    emit_array_init(self, bytecodes_dst, &mut add_const);
                } else {
                    let bytecodes_dst = &mut funcproto.bytecodes;
                    let constants = &mut funcproto.constants;
                    let mut add_const = |cval: vm::RawVal| {
                        constants.binary_search(&cval).unwrap_or_else(|_err| {
                            constants.push(cval);
                            constants.len() - 1
                        }) as ConstPos
                    };
                    emit_array_init(self, bytecodes_dst, &mut add_const);
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

            instr => {
                unimplemented!("Instruction not implemented: {:?}", instr)
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
        log::debug!("iochannels: {:?}", self.program.iochannels);
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

    #[test]
    fn phi_multiword_emits_moverange_and_two_word_return() {
        use super::*;
        use crate::numeric;
        use crate::types::Type;

        let mut src = mir::Mir::default();
        let tuple_ty = Type::Tuple(vec![numeric!(), numeric!()]).into_id();

        let mut func = mir::Function::new(0, "dsp".to_symbol(), &[], vec![], None);
        func.return_type.get_or_init(|| tuple_ty);

        let cond = Arc::new(mir::Value::Register(0));
        let then_val = Arc::new(mir::Value::Register(1));
        let else_val = Arc::new(mir::Value::Register(2));
        let phi_val = Arc::new(mir::Value::Register(3));

        let mut entry = mir::Block::default();
        entry.0.push((cond.clone(), mir::Instruction::Float(1.0)));
        entry.0.push((
            Arc::new(mir::Value::None),
            mir::Instruction::JmpIf(cond.clone(), 1, 2, 3),
        ));

        let mut then_block = mir::Block::default();
        then_block
            .0
            .push((then_val.clone(), mir::Instruction::Alloc(tuple_ty)));

        let mut else_block = mir::Block::default();
        else_block
            .0
            .push((else_val.clone(), mir::Instruction::Alloc(tuple_ty)));

        let mut merge_block = mir::Block::default();
        merge_block.0.push((
            phi_val.clone(),
            mir::Instruction::Phi(then_val.clone(), else_val.clone()),
        ));
        merge_block.0.push((
            Arc::new(mir::Value::None),
            mir::Instruction::Return(phi_val.clone(), tuple_ty),
        ));

        func.body = vec![entry, then_block, else_block, merge_block];
        src.functions.push(func);

        let mut generator = ByteCodeGenerator::default();
        let program = generator.generate(src, Config::default());
        let proto = &program.global_fn_table[0].1;

        assert!(
            proto
                .bytecodes
                .iter()
                .any(|inst| matches!(inst, VmInstruction::MoveRange(_, _, 2))),
            "Expected MoveRange for two-word Phi value, got: {:?}",
            proto.bytecodes
        );
        assert!(
            proto
                .bytecodes
                .iter()
                .any(|inst| matches!(inst, VmInstruction::Return(_, 2))),
            "Expected two-word Return for tuple type, got: {:?}",
            proto.bytecodes
        );
    }
}
