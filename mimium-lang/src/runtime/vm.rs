use core::slice;
use slotmap::{DefaultKey, SlotMap};
use std::{cell::RefCell, cmp::Ordering, collections::HashMap, ops::Range, rc::Rc, sync::Arc};

pub mod builtin;
pub mod bytecode;
pub mod program;
mod ringbuffer;
pub use bytecode::*;
use ringbuffer::Ringbuffer;

use program::OpenUpValue;
pub use program::{FuncProto, Program};

use crate::{
    compiler::bytecodegen::ByteCodeGenerator,
    interner::{Symbol, TypeNodeId},
    types::{Type, TypeSize},
};
pub type RawVal = u64;
pub type ReturnCode = i64;

pub type ExtFunType = fn(&mut Machine) -> ReturnCode;
pub type ExtClsType = Rc<RefCell<dyn Fn(&mut Machine) -> ReturnCode>>;
pub type ExtFnInfo = (Symbol, ExtFunType, TypeNodeId);
pub type ExtClsInfo = (Symbol, ExtClsType, TypeNodeId);

#[derive(Debug, Default, PartialEq)]
struct StateStorage {
    pos: usize,
    rawdata: Vec<u64>,
}
impl StateStorage {
    fn resize(&mut self, size: usize) {
        self.rawdata.resize(size, 0)
    }
    fn get_state(&self, size: u64) -> &[RawVal] {
        unsafe {
            let head = self.rawdata.as_ptr().add(self.pos);
            slice::from_raw_parts(head, size as _)
        }
    }
    fn get_state_mut(&mut self, size: usize) -> &mut [RawVal] {
        unsafe {
            let head = self.rawdata.as_mut_ptr().add(self.pos);
            slice::from_raw_parts_mut(head, size as _)
        }
    }
    fn get_as_ringbuffer(&mut self, size_in_samples: u64) -> Ringbuffer<'_> {
        let data_head = unsafe { self.rawdata.as_mut_ptr().add(self.pos) };
        Ringbuffer::new(data_head, size_in_samples)
    }
    fn shift_pos(&mut self, offset: i16) {
        self.pos = (self.pos as i64 + offset as i64) as usize;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ClosureIdx(pub slotmap::DefaultKey);

#[derive(Debug, Clone, Default)]
struct StateStorageStack(Vec<ClosureIdx>);

impl StateStorageStack {
    pub fn push(&mut self, i: ClosureIdx) {
        self.0.push(i)
    }
    pub fn pop(&mut self) {
        let _ = self.0.pop();
    }
}

// Upvalues are used with Rc<RefCell<UpValue>> because it maybe shared between multiple closures
// Maybe it will be managed with some GC mechanism in the future.
#[derive(Debug, Clone, PartialEq)]
enum UpValue {
    Open(OpenUpValue),
    Closed(Vec<RawVal>, bool),
}
impl UpValue {
    pub fn is_closure(&self) -> bool {
        match self {
            UpValue::Open(OpenUpValue { is_closure, .. }) => *is_closure,
            UpValue::Closed(_, is_closure) => *is_closure,
        }
    }
}
type SharedUpValue = Rc<RefCell<UpValue>>;
impl From<OpenUpValue> for UpValue {
    fn from(value: OpenUpValue) -> Self {
        Self::Open(value)
    }
}

#[derive(Default)]
struct LocalUpValueMap(Vec<(Reg, SharedUpValue)>);

impl LocalUpValueMap {
    pub fn get_or_insert(&mut self, ov: OpenUpValue) -> SharedUpValue {
        let OpenUpValue { pos, .. } = ov;
        self.0
            .iter()
            .find_map(|(i2, v)| (pos == *i2 as _).then_some(v.clone()))
            .unwrap_or_else(|| {
                let v = Rc::new(RefCell::new(UpValue::Open(ov)));
                self.0.push((pos as Reg, v.clone()));
                v
            })
    }
}

#[derive(Debug, Default, PartialEq)]
//closure object dynamically allocated
pub struct Closure {
    pub fn_proto_pos: usize, //position of function prototype in global_ftable
    pub base_ptr: u64,       //base pointer to current closure, to calculate open upvalue
    pub is_closed: bool,
    pub refcount: u64,
    pub(self) upvalues: Vec<SharedUpValue>,
    state_storage: StateStorage,
}
impl Closure {
    pub(self) fn new(
        program: &Program,
        base_ptr: u64,
        fn_i: usize,
        upv_map: &mut LocalUpValueMap,
    ) -> Self {
        let fnproto = &program.global_fn_table[fn_i].1;
        let upvalues = fnproto
            .upindexes
            .iter()
            .map(|ov| upv_map.get_or_insert(*ov))
            .collect::<Vec<_>>();
        let mut state_storage = StateStorage::default();
        state_storage.resize(fnproto.state_size as usize);
        Self {
            fn_proto_pos: fn_i,
            upvalues,
            is_closed: false,
            refcount: 1,
            base_ptr,
            state_storage,
        }
    }
}

pub type ClosureStorage = SlotMap<DefaultKey, Closure>;
pub fn drop_closure(storage: &mut ClosureStorage, id: ClosureIdx) {
    let cls = storage.get_mut(id.0).unwrap();
    cls.refcount -= 1;
    if cls.refcount == 0 {
        let c_cls = storage
            .get_mut(id.0)
            .unwrap()
            .upvalues
            .iter()
            .map(|v| {
                let v = v.borrow();
                if let UpValue::Closed(v, _) = &v as &UpValue {
                    let cls_i = Machine::get_as::<ClosureIdx>(v[0]);
                    Some(cls_i)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        c_cls.iter().filter_map(|i| *i).for_each(|clsi| {
            drop_closure(storage, clsi);
        });
        storage.remove(id.0);
    }
}

#[derive(Clone, Copy)]
enum RawValType {
    Float,
    Int,
    UInt,
}
impl Default for RawValType {
    fn default() -> Self {
        RawValType::Int
    }
}
enum ExtFnIdx {
    Fun(usize),
    Cls(usize),
}
pub struct Machine {
    // program will be modified while its execution, e.g., higher-order external closure creates its wrapper.
    pub prog: Program,
    stack: Vec<RawVal>,
    base_pointer: u64,
    pub closures: ClosureStorage,
    pub ext_fun_table: Vec<(Symbol, ExtFunType)>,
    pub ext_cls_table: Vec<(Symbol, ExtClsType)>,
    fn_map: HashMap<usize, ExtFnIdx>, //index from fntable index of program to it of machine.
    // cls_map: HashMap<usize, usize>, //index from fntable index of program to it of machine.
    global_states: StateStorage,
    states_stack: StateStorageStack,
    delaysizes_pos_stack: Vec<usize>,
    global_vals: Vec<RawVal>,
    debug_stacktype: Vec<RawValType>,
}

macro_rules! binop {
    ($op:tt,$t:ty, $dst:expr,$src1:expr,$src2:expr,$self:ident) => {
        {
        $self.set_stacktype($dst as i64, RawValType::Float);
        $self.set_stack($dst as i64, Self::to_value::<$t>(
            Self::get_as::<$t>($self.get_stack($src1 as i64))
        $op Self::get_as::<$t>($self.get_stack($src2 as i64))))
    }
    };
}
macro_rules! binop_bool {
    ($op:tt, $dst:expr,$src1:expr,$src2:expr,$self:ident) => {
        {
        $self.set_stacktype($dst as i64, RawValType::Float);
        let bres:bool =
            Self::get_as::<f64>($self.get_stack($src1 as i64))
        $op Self::get_as::<f64>($self.get_stack($src2 as i64));
        let fres = if bres{
            1.0f64
        }else{
            0.0f64
        };
        $self.set_stack($dst as i64,Self::to_value::<f64>(fres))
    }
    };
}
macro_rules! binop_bool_compose {//for and&or
    ($op:tt, $dst:expr,$src1:expr,$src2:expr,$self:ident) => {
        {
        $self.set_stacktype($dst as i64, RawValType::Float);
        let bres:bool =
            Self::get_as::<f64>($self.get_stack($src1 as i64))>0.0
        $op Self::get_as::<f64>($self.get_stack($src2 as i64))>0.0;
        let fres = if bres{ 1.0f64 }else{ 0.0f64 };
        $self.set_stack($dst as i64,Self::to_value::<f64>(fres))
    }
    };
}
macro_rules! binopmethod {
    ($op:ident,$t:ty, $dst:expr,$src1:expr,$src2:expr,$self:ident) => {{
        $self.set_stacktype($dst as i64, RawValType::Float);
        $self.set_stack(
            $dst as i64,
            Self::to_value::<$t>(
                Self::get_as::<$t>($self.get_stack($src1 as i64))
                    .$op(Self::get_as::<$t>($self.get_stack($src2 as i64))),
            ),
        )
    }};
}
macro_rules! uniop {
    ($op:tt,$t:ty, $dst:expr,$src:expr,$self:ident) => {
        $self.set_stack($dst as i64,
            Self::to_value::<$t>(
            $op Self::get_as::<$t>($self.get_stack($src as i64))))
    };
}
macro_rules! uniop_bool {
    ($op:tt, $dst:expr,$src:expr,$self:ident) => {{
        let bres: bool = $op(Self::get_as::<f64>($self.get_stack($src as i64)) > 0.0);
        let fres = if bres { 1.0f64 } else { 0.0f64 };
        $self.set_stack($dst as i64, Self::to_value::<f64>(fres))
    }};
}
macro_rules! uniopmethod {
    ($op:tt,$t:ty, $dst:expr,$src:expr,$self:ident) => {{
        $self.set_stack(
            $dst as i64,
            Self::to_value::<$t>(Self::get_as::<$t>($self.get_stack($src as i64)).$op()),
        )
    }};
}

fn set_vec<T>(vec: &mut Vec<T>, i: usize, value: T)
where
    T: Clone + std::default::Default,
{
    match i.cmp(&vec.len()) {
        Ordering::Less => vec[i] = value,
        Ordering::Equal => vec.push(value),
        Ordering::Greater => {
            vec.resize(i, T::default());
            vec.push(value);
        }
    }
}
fn set_vec_range<T>(vec: &mut Vec<T>, i: usize, values: &[T])
where
    T: std::fmt::Debug + Copy + std::default::Default,
{
    //do not use copy_from_slice  or extend_from_slice because the ptr range may overwrap,
    // and copy_from_slice use ptr::copy_nonoverwrapping internally.
    // vec[range].copy_from_slice(values)
    let start = i;
    let end = i + values.len();
    if end > vec.len() {
        vec.resize(i, T::default());
    }
    match start.cmp(&vec.len()) {
        Ordering::Less => {
            let range = i..(i + values.len());
            for (v, i) in values.iter().zip(range.into_iter()) {
                vec[i] = *v;
            }
        }
        Ordering::Equal => values.iter().for_each(|v| vec.push(*v)),
        Ordering::Greater => values.iter().for_each(|v| vec.push(*v)),
    }
}

impl Machine {
    pub fn new<'a>(
        prog: Program,
        extfns: impl Iterator<Item = ExtFnInfo>,
        extcls: impl Iterator<Item = ExtClsInfo>,
    ) -> Self {
        let mut res = Self {
            prog,
            stack: vec![],
            base_pointer: 0,
            closures: Default::default(),
            ext_fun_table: vec![],
            ext_cls_table: vec![],
            fn_map: HashMap::new(),
            // cls_map: HashMap::new(),
            global_states: Default::default(),
            states_stack: Default::default(),
            delaysizes_pos_stack: vec![0],
            global_vals: vec![],
            debug_stacktype: vec![RawValType::Int; 255],
        };
        extfns.for_each(|(name, f, _)| {
            let _ = res.install_extern_fn(name, f);
        });
        extcls.for_each(|(name, f, _)| {
            let _ = res.install_extern_cls(name, f);
        });
        res.link_functions();
        res
    }
    pub fn clear_stack(&mut self) {
        self.stack.fill(0);
    }
    pub fn get_stack(&self, offset: i64) -> RawVal {
        // unsafe {
        //     *self
        //         .stack
        //         .get_unchecked((self.base_pointer + offset as u64) as usize)
        // }
        self.get_stack_range(offset, 1).1[0]
    }
    pub fn get_stack_range(&self, offset: i64, word_size: TypeSize) -> (Range<usize>, &[RawVal]) {
        let addr_start = self.base_pointer as usize + offset as usize;
        let addr_end = addr_start + word_size as usize;
        let start = self.stack.as_slice().as_ptr();
        let slice = unsafe {
            // w/ unstable feature
            // let (_,snd) = self.stack.as_slice().split_at_unchecked(offset as usize);
            // snd.split_at_unchecked(n as usize)
            let vstart = start.add(addr_start);
            slice::from_raw_parts(vstart, word_size as usize)
        };
        (addr_start..addr_end, slice)
    }
    pub fn get_stack_range_mut(
        &mut self,
        offset: i64,
        word_size: TypeSize,
    ) -> (Range<usize>, &mut [RawVal]) {
        let addr_start = self.base_pointer as usize + offset as usize;
        let addr_end = addr_start + word_size as usize;
        let start = self.stack.as_mut_ptr();
        let slice = unsafe {
            // w/ unstable feature
            // let (_,snd) = self.stack.as_slice().split_at_unchecked(offset as usize);
            // snd.split_at_unchecked(n as usize)
            let vstart = start.add(addr_start);
            slice::from_raw_parts_mut(vstart, word_size as usize)
        };
        (addr_start..addr_end, slice)
    }
    pub fn set_stack(&mut self, offset: i64, v: RawVal) {
        self.set_stack_range(offset, &[v])
    }
    pub fn set_stack_range(&mut self, offset: i64, vs: &[RawVal]) {
        // debug_assert!(!v.is_null());
        // debug_assert!(v.is_aligned());
        // let vs = unsafe { slice::from_raw_parts(v, size) };
        set_vec_range(
            &mut self.stack,
            (self.base_pointer as i64 + offset) as usize,
            vs,
        )
    }
    fn move_stack_range(&mut self, offset: i64, srcrange: Range<usize>) {
        let dest = (self.base_pointer as i64 + offset) as usize;
        if srcrange.end > self.stack.len() {
            self.stack.resize(srcrange.end, 0);
        }
        let dest_end = dest + (srcrange.end - srcrange.start);
        if dest_end > self.stack.len() {
            self.stack.resize(dest_end, 0);
        }
        self.stack.copy_within(srcrange, dest)
    }
    fn set_stacktype(&mut self, offset: i64, t: RawValType) {
        // set_vec(
        //     &mut self.debug_stacktype,
        //     (self.base_pointer as i64 + offset) as usize,
        //     t,
        // );
    }
    pub fn get_top_n(&self, n: usize) -> &[RawVal] {
        let len = self.stack.len();
        &self.stack[(len - n)..]
    }
    fn get_upvalue_offset(upper_base: usize, offset: OpenUpValue) -> usize {
        upper_base + offset.pos
    }
    pub fn get_open_upvalue(
        &self,
        upper_base: usize,
        ov: OpenUpValue,
    ) -> (Range<usize>, &[RawVal]) {
        let OpenUpValue { size, .. } = ov;
        // log::trace!("upper base:{}, upvalue:{}", upper_base, offset);
        let abs_pos = Self::get_upvalue_offset(upper_base, ov);
        let end = abs_pos + size as usize;
        let slice = unsafe {
            let vstart = self.stack.as_slice().as_ptr().add(abs_pos);
            slice::from_raw_parts(vstart, size as usize)
        };
        (abs_pos..end, slice)
    }
    pub fn get_closure(&self, idx: ClosureIdx) -> &Closure {
        debug_assert!(
            self.closures.contains_key(idx.0),
            "Invalid Closure Id referred"
        );
        unsafe { self.closures.get_unchecked(idx.0) }
    }
    pub(crate) fn get_closure_mut(&mut self, idx: ClosureIdx) -> &mut Closure {
        debug_assert!(
            self.closures.contains_key(idx.0),
            "Invalid Closure Id referred"
        );
        unsafe { self.closures.get_unchecked_mut(idx.0) }
    }
    fn get_current_state(&mut self) -> &mut StateStorage {
        if self.states_stack.0.is_empty() {
            &mut self.global_states
        } else {
            let idx = unsafe { self.states_stack.0.last().unwrap_unchecked() };
            &mut self.get_closure_mut(*idx).state_storage
        }
    }
    fn return_general(&mut self, iret: Reg, nret: Reg) -> &[u64] {
        let base = self.base_pointer as usize;
        let iret_abs = base + iret as usize;
        self.stack
            .copy_within(iret_abs..(iret_abs + nret as usize), base - 1);
        // clean up temporary variables to ensure that `nret`
        // at the top of the stack is the return value
        self.stack.truncate(base - 1 + nret as usize);
        let res_slice = self.stack.split_at(base).1;
        res_slice
    }

    pub fn get_as<T>(v: RawVal) -> T {
        unsafe { std::mem::transmute_copy::<RawVal, T>(&v) }
    }
    pub fn get_as_array<T>(v: &[RawVal]) -> &[T] {
        unsafe { std::mem::transmute::<&[RawVal], &[T]>(v) }
    }
    pub fn to_value<T>(v: T) -> RawVal {
        assert_eq!(std::mem::size_of::<T>(), 8);
        unsafe { std::mem::transmute_copy::<T, RawVal>(&v) }
    }
    fn call_function<F>(
        &mut self,
        func_pos: u8,
        _nargs: u8,
        nret_req: u8,
        mut action: F,
    ) -> ReturnCode
    where
        F: FnMut(&mut Self) -> ReturnCode,
    {
        let offset = (func_pos + 1) as u64;
        self.delaysizes_pos_stack.push(0);
        self.base_pointer += offset;
        let nret = action(self);

        if nret_req > nret as u8 {
            panic!("invalid number of return value required.");
        }
        // shrink stack so as to match with number of return values
        self.stack
            .truncate((self.base_pointer as i64 + nret_req as i64) as usize);
        self.base_pointer -= offset;
        self.delaysizes_pos_stack.pop();
        nret
    }
    fn allocate_closure(&mut self, fn_i: usize, upv_map: &mut LocalUpValueMap) -> ClosureIdx {
        let idx = self
            .closures
            .insert(Closure::new(&self.prog, self.base_pointer, fn_i, upv_map));
        ClosureIdx(idx)
    }
    /// This API is used for defining higher-order external function that returns some external rust closure.
    /// Because the native closure cannot be called with CallCls directly, the vm appends an additional function the program,
    /// that wraps external closure call with an internal closure.
    pub fn wrap_extern_cls(&mut self, extcls: ExtClsInfo) -> ClosureIdx {
        let (name, f, t) = extcls;

        self.prog.ext_fun_table.push((name, t));
        let prog_funid = self.prog.ext_fun_table.len() - 1;
        self.ext_cls_table.push((name, f));
        let vm_clsid = self.ext_cls_table.len() - 1;
        self.fn_map.insert(prog_funid, ExtFnIdx::Cls(vm_clsid));
        let (bytecodes, nargs, nret) = if let Type::Function(args, ret, _) = t.to_type() {
            let mut wrap_bytecode = Vec::<Instruction>::new();
            // todo: decouple bytecode generator dependency
            let asizes = args.into_iter().map(ByteCodeGenerator::word_size_for_type);
            let nargs = asizes.clone().sum();
            // if there are 2 arguments of float for instance, base pointer should be 2
            let base = nargs;
            let nret = ByteCodeGenerator::word_size_for_type(ret);
            wrap_bytecode.push(Instruction::MoveConst(base as _, 0));
            let _ = asizes.fold(0u8, |acc, size| {
                //copy arguments for ext closure call
                wrap_bytecode.push(Instruction::MoveRange(base + acc + 1, acc, size as _));
                acc + size
            });
            wrap_bytecode.extend_from_slice(&[
                Instruction::CallExtFun(base, nargs, nret as _),
                Instruction::Return(base, nret as _),
            ]);
            (wrap_bytecode, nargs, nret)
        } else {
            panic!("non-function type called for wrapping external closure");
        };
        let newfunc = FuncProto {
            nparam: nargs as _,
            nret: nret as _,
            bytecodes,
            constants: vec![prog_funid as _],
            ..Default::default()
        };
        self.prog.global_fn_table.push((name, newfunc));
        let fn_i = self.prog.global_fn_table.len() - 1;
        let mut cls = Closure::new(
            &self.prog,
            self.base_pointer,
            fn_i,
            &mut LocalUpValueMap(vec![]),
        );
        // wrapper closure will not be released automatically.
        cls.is_closed = true;
        let idx = self.closures.insert(cls);
        ClosureIdx(idx)
    }
    fn close_upvalues(&mut self, src: Reg) {
        let clsidx = Self::get_as::<ClosureIdx>(self.get_stack(src as _));

        let clsidxs = self
            .get_closure(clsidx)
            .upvalues
            .iter()
            .map(|upv| {
                let upv = &mut *upv.borrow_mut();
                match upv {
                    UpValue::Open(ov) => {
                        let (_range, ov_raw) =
                            self.get_open_upvalue(self.base_pointer as usize, *ov);
                        let is_closure = ov.is_closure;
                        *upv = UpValue::Closed(ov_raw.to_vec(), is_closure);
                        is_closure.then_some(Self::get_as::<ClosureIdx>(ov_raw[0]))
                    }
                    UpValue::Closed(v, is_closure) => {
                        is_closure.then_some(Self::get_as::<ClosureIdx>(v[0]))
                    }
                }
            })
            .collect::<Vec<_>>();
        clsidxs.iter().for_each(|i| {
            if let Some(ci) = i {
                let cls = self.get_closure_mut(*ci);
                cls.refcount += 1;
            }
        });
        let cls = self.get_closure_mut(clsidx);
        cls.is_closed = true;
    }
    fn release_open_closures(&mut self, local_closures: &[ClosureIdx]) {
        for clsidx in local_closures.iter() {
            let cls = self.get_closure(*clsidx);
            if !cls.is_closed {
                // log::debug!("release {:?}", clsidx);
                drop_closure(&mut self.closures, *clsidx)
            }
        }
    }
    fn get_fnproto(&self, func_i: usize) -> &FuncProto {
        &self.prog.global_fn_table[func_i].1
    }
    /// Execute function, return retcode.
    pub fn execute(&mut self, func_i: usize, cls_i: Option<ClosureIdx>) -> ReturnCode {
        let mut local_closures: Vec<ClosureIdx> = vec![];
        let mut upv_map = LocalUpValueMap::default();
        let mut pcounter = 0;
        // if cfg!(test) {
        //     log::trace!("{:?}", func);
        // }

        loop {
            // if cfg!(debug_assertions) && log::max_level() >= log::Level::Trace {
            //     let mut line = String::new();
            //     line += &format!("{: <20} {}", func.bytecodes[pcounter], ": [");
            //     for i in 0..self.stack.len() {
            //         if i == self.base_pointer as usize {
            //             line += "!";
            //         }
            //         line += &match self.debug_stacktype[i] {
            //             RawValType::Float => format!("{0:.5}f", Self::get_as::<f64>(self.stack[i])),
            //             RawValType::Int => format!("{0:.5}i", Self::get_as::<i64>(self.stack[i])),
            //             RawValType::UInt => format!("{0:.5}u", Self::get_as::<u64>(self.stack[i])),
            //         };
            //         if i < self.stack.len() - 1 {
            //             line += ",";
            //         }
            //     }
            //     line += "]";
            //     log::trace!("{line}");
            // }
            let mut increment = 1;
            match self.get_fnproto(func_i).bytecodes[pcounter] {
                Instruction::Move(dst, src) => {
                    self.set_stack(dst as i64, self.get_stack(src as i64));
                }
                Instruction::MoveConst(dst, pos) => {
                    self.set_stack(dst as i64, self.get_fnproto(func_i).constants[pos as usize]);
                }
                Instruction::MoveImmF(dst, v) => {
                    self.set_stack(dst as i64, Self::to_value(Into::<f64>::into(v)));
                }
                Instruction::MoveRange(dst, src, n) => {
                    let (range, _slice) = self.get_stack_range(src as _, n);
                    self.move_stack_range(dst as i64, range);
                }
                Instruction::CallCls(func, nargs, nret_req) => {
                    let addr = self.get_stack(func as i64);
                    let cls_i = Self::get_as::<ClosureIdx>(addr);
                    let cls = self.get_closure(cls_i);
                    let pos_of_f = cls.fn_proto_pos;
                    self.states_stack.push(cls_i);
                    self.call_function(func, nargs, nret_req, move |machine| {
                        machine.execute(pos_of_f, Some(cls_i))
                    });
                    self.states_stack.pop();
                }
                Instruction::Call(func, nargs, nret_req) => {
                    let pos_of_f = Self::get_as::<usize>(self.get_stack(func as i64));
                    self.call_function(func, nargs, nret_req, move |machine| {
                        machine.execute(pos_of_f, None)
                    });
                }
                Instruction::CallExtFun(func, nargs, nret_req) => {
                    let ext_fn_idx = self.get_stack(func as i64) as usize;
                    let fidx = self.fn_map.get(&ext_fn_idx).unwrap();
                    let nret = match fidx {
                        ExtFnIdx::Fun(fi) => {
                            let f = self.ext_fun_table[*fi].1;
                            self.call_function(func, nargs, nret_req, f)
                        }
                        ExtFnIdx::Cls(ci) => {
                            let (_name, cls) = &self.ext_cls_table[*ci];
                            let cls = cls.clone();
                            self.call_function(func, nargs, nret_req, move |machine| {
                                cls.borrow()(machine)
                            })
                        }
                    };

                    // return
                    let base = self.base_pointer as usize;
                    let iret = base + func as usize + 1;
                    self.stack
                        .copy_within(iret..(iret + nret as usize), base + func as usize);
                    self.stack.truncate(base + func as usize + nret as usize);
                }
                Instruction::Closure(dst, fn_index) => {
                    let fn_proto_pos = self.get_stack(fn_index as i64) as usize;
                    let vaddr = self.allocate_closure(fn_proto_pos, &mut upv_map);
                    local_closures.push(vaddr);
                    self.set_stack(dst as i64, Self::to_value(vaddr));
                }
                Instruction::Close(src) => {
                    self.close_upvalues(src);
                }
                Instruction::Return0 => {
                    self.stack.truncate((self.base_pointer - 1) as usize);
                    self.release_open_closures(&local_closures);
                    return 0;
                }
                Instruction::Return(iret, nret) => {
                    let _ = self.return_general(iret, nret);
                    self.release_open_closures(&local_closures);
                    return nret.into();
                }
                Instruction::GetUpValue(dst, index, _size) => {
                    {
                        let up_i = cls_i.unwrap();
                        let cls = self.get_closure(up_i);
                        let upvalues = &cls.upvalues;
                        let rv = &upvalues[index as usize];
                        let vs = match &*rv.borrow() {
                            UpValue::Open(i) => {
                                let upper_base = cls.base_ptr as usize;
                                let (_range, rawv) = self.get_open_upvalue(upper_base, *i);
                                // log::trace!("open {}", unsafe {
                                //     std::mem::transmute::<u64, f64>(rawv[0])
                                // });
                                // assert_eq!(rawv.len(), size as usize);
                                let rawv: &[RawVal] = unsafe { std::mem::transmute(rawv) };
                                rawv
                            }
                            UpValue::Closed(rawval, _) => {
                                //force borrow because closure cell and stack never collisions
                                let rawv: &[RawVal] =
                                    unsafe { std::mem::transmute(rawval.as_slice()) };
                                rawv
                                //
                            }
                        };
                        self.set_stack_range(dst as i64, vs);
                    };
                }
                Instruction::SetUpValue(index, src, size) => {
                    let up_i = cls_i.unwrap();
                    let cls = self.get_closure(up_i);
                    let upper_base = cls.base_ptr as usize;
                    let upvalues = &cls.upvalues;
                    let (_range, v) = self.get_stack_range(src as i64, size);
                    let rv = &mut *upvalues[index as usize].borrow_mut();
                    match rv {
                        UpValue::Open(OpenUpValue {
                            pos: ref i,
                            ref mut size,
                            ..
                        }) => {
                            let (range, _v) = self.get_stack_range(src as i64, *size);
                            let dest = upper_base + *i;
                            unsafe {
                                //force borrow because closure cell and stack never collisions
                                let dst = slice::from_raw_parts_mut(
                                    std::mem::transmute::<*const RawVal, *mut RawVal>(
                                        self.stack.as_ptr(),
                                    ),
                                    self.stack.len(),
                                );
                                dst.copy_within(range, dest);
                            }
                        }
                        UpValue::Closed(ref mut uv, _) => {
                            uv.as_mut_slice().copy_from_slice(v);
                        }
                    };
                }
                Instruction::GetGlobal(dst, gid, size) => {
                    let gvs = unsafe {
                        let vstart = self.global_vals.as_ptr().offset(gid as _);
                        debug_assert!(!vstart.is_null());
                        // debug_assert!(vstart.is_aligned());
                        slice::from_raw_parts(vstart, size as _)
                    };
                    self.set_stack_range(dst as i64, gvs)
                }
                Instruction::SetGlobal(gid, src, size) => {
                    let gvs = unsafe {
                        let vstart = self.global_vals.as_mut_ptr().offset(gid as _);
                        debug_assert!(!vstart.is_null());
                        // debug_assert!(vstart.is_aligned());
                        slice::from_raw_parts_mut(vstart, size as _)
                    };
                    let (_, slice) = self.get_stack_range(src as i64, size);
                    gvs.copy_from_slice(slice);
                }
                Instruction::Jmp(offset) => {
                    // -1 is for the offset in last increment
                    increment = offset;
                }
                Instruction::JmpIfNeg(cond, offset) => {
                    let cond_v = self.get_stack(cond as i64);
                    if Self::get_as::<f64>(cond_v) <= 0.0 {
                        increment = offset;
                    }
                }
                Instruction::AddF(dst, src1, src2) => binop!(+,f64,dst,src1,src2,self),
                Instruction::SubF(dst, src1, src2) => {
                    binop!(-,f64,dst,src1,src2,self)
                }
                Instruction::MulF(dst, src1, src2) => binop!(*,f64,dst,src1,src2,self),
                Instruction::DivF(dst, src1, src2) => binop!(/,f64,dst,src1,src2,self),
                Instruction::ModF(dst, src1, src2) => binop!(%,f64,dst,src1,src2,self),
                Instruction::NegF(dst, src) => uniop!(-,f64,dst,src,self),
                Instruction::AbsF(dst, src) => uniopmethod!(abs, f64, dst, src, self),
                Instruction::SqrtF(dst, src) => uniopmethod!(sqrt, f64, dst, src, self),
                Instruction::SinF(dst, src) => uniopmethod!(sin, f64, dst, src, self),
                Instruction::CosF(dst, src) => uniopmethod!(cos, f64, dst, src, self),
                Instruction::PowF(dst, src1, src2) => {
                    binopmethod!(powf, f64, dst, src1, src2, self)
                }
                Instruction::LogF(dst, src1, src2) => binopmethod!(log, f64, dst, src1, src2, self),
                Instruction::AddI(dst, src1, src2) => binop!(+,i64,dst,src1,src2,self),
                Instruction::SubI(dst, src1, src2) => binop!(-,i64,dst,src1,src2,self),
                Instruction::MulI(dst, src1, src2) => binop!(*,i64,dst,src1,src2,self),
                Instruction::DivI(dst, src1, src2) => binop!(/,i64,dst,src1,src2,self),
                Instruction::ModI(dst, src1, src2) => binop!(%,i64,dst,src1,src2,self),
                Instruction::NegI(dst, src) => uniop!(-,i64,dst,src,self),
                Instruction::AbsI(dst, src) => uniopmethod!(abs, i64, dst, src, self),
                Instruction::PowI(dst, lhs, rhs) => binop!(^,i64,dst,lhs,rhs,self),
                Instruction::LogI(_, _, _) => todo!(),
                Instruction::Not(dst, src) => uniop_bool!(!, dst, src, self),
                Instruction::Eq(dst, src1, src2) => binop_bool!(==,dst,src1,src2,self),
                Instruction::Ne(dst, src1, src2) => binop_bool!(!=,dst,src1,src2,self),
                Instruction::Gt(dst, src1, src2) => binop_bool!(>,dst,src1,src2,self),
                Instruction::Ge(dst, src1, src2) => binop_bool!(>=,dst,src1,src2,self),
                Instruction::Lt(dst, src1, src2) => binop_bool!(<,dst,src1,src2,self),
                Instruction::Le(dst, src1, src2) => binop_bool!(<=,dst,src1,src2,self),
                Instruction::And(dst, src1, src2) => binop_bool_compose!(&&,dst,src1,src2,self),
                Instruction::Or(dst, src1, src2) => binop_bool_compose!(||,dst,src1,src2,self),
                Instruction::CastFtoI(dst, src) => self.set_stack(
                    dst as i64,
                    Self::to_value::<i64>(Self::get_as::<f64>(self.get_stack(src as i64)) as i64),
                ),
                Instruction::CastItoF(dst, src) => self.set_stack(
                    dst as i64,
                    Self::to_value::<f64>(Self::get_as::<i64>(self.get_stack(src as i64)) as f64),
                ),
                Instruction::CastItoB(dst, src) => self.set_stack(
                    dst as i64,
                    Self::to_value::<bool>(Self::get_as::<i64>(self.get_stack(src as i64)) != 0),
                ),
                Instruction::GetState(dst, size) => {
                    //force borrow because state storage and stack never collisions
                    let v: &[RawVal] = unsafe {
                        std::mem::transmute(self.get_current_state().get_state(size as _))
                    };
                    self.set_stack_range(dst as i64, v);
                }
                Instruction::SetState(src, size) => {
                    let vs = {
                        let (_range, v) = self.get_stack_range(src as i64, size as _);
                        unsafe { std::mem::transmute::<&[RawVal], &[RawVal]>(v) }
                    };
                    let dst = self.get_current_state().get_state_mut(size as _);
                    dst.copy_from_slice(vs);
                }
                Instruction::ShiftStatePos(v) => self.get_current_state().shift_pos(v),
                Instruction::Delay(dst, src, time) => {
                    let i = self.get_stack(src as i64);
                    let t = self.get_stack(time as i64);
                    let delaysize_i =
                        unsafe { self.delaysizes_pos_stack.last().unwrap_unchecked() };

                    let size_in_samples = unsafe {
                        *self
                            .get_fnproto(func_i)
                            .delay_sizes
                            .get_unchecked(*delaysize_i)
                    };
                    let mut ringbuf = self.get_current_state().get_as_ringbuffer(size_in_samples);

                    let res = ringbuf.process(i, t);
                    self.set_stack(dst as i64, res);
                }
                Instruction::Mem(dst, src) => {
                    let s = self.get_stack(src as i64);
                    let ptr = self.get_current_state().get_state_mut(1);
                    let v = Self::to_value(ptr[0]);
                    self.set_stack(dst as i64, v);
                    let ptr = self.get_current_state().get_state_mut(1);
                    ptr[0] = s;
                }
                Instruction::Dummy => {
                    unreachable!()
                }
            }
            pcounter = (pcounter as i64 + increment as i64) as usize;
        }
    }
    pub fn install_extern_fn(&mut self, name: Symbol, f: ExtFunType) -> usize {
        self.ext_fun_table.push((name, f));
        self.ext_fun_table.len() - 1
    }
    pub fn install_extern_cls(&mut self, name: Symbol, f: ExtClsType) -> usize {
        self.ext_cls_table.push((name, f));
        self.ext_cls_table.len() - 1
    }

    fn link_functions(&mut self) {
        //link external functions
        self.global_vals = self.prog.global_vals.clone();
        self.prog
            .ext_fun_table
            .iter_mut()
            .enumerate()
            .for_each(|(i, (name, _ty))| {
                if let Some((j, _)) = self
                    .ext_fun_table
                    .iter()
                    .enumerate()
                    .find(|(_j, (fname, _fn))| name == fname)
                {
                    let _ = self.fn_map.insert(i, ExtFnIdx::Fun(j));
                } else if let Some((j, _)) = self
                    .ext_cls_table
                    .iter()
                    .enumerate()
                    .find(|(_j, (fname, _fn))| name == fname)
                {
                    let _ = self.fn_map.insert(i, ExtFnIdx::Cls(j));
                } else {
                    panic!("external function {} cannot be found", name);
                }
            });
    }
    pub fn execute_idx(&mut self, idx: usize) -> ReturnCode {
        let (_name, func) = &self.prog.global_fn_table[idx];
        if !func.bytecodes.is_empty() {
            self.global_states.resize(func.state_size as usize);
            // 0 is always base pointer to the main function
            if self.stack.len() > 0 {
                self.stack[0] = 0;
            }
            self.base_pointer = 1;
            self.execute(idx, None)
        } else {
            0
        }
    }
    pub fn execute_entry(&mut self, entry: &Symbol) -> ReturnCode {
        if let Some(idx) = self.prog.get_fun_index(entry) {
            self.execute_idx(idx)
        } else {
            -1
        }
    }
    pub fn execute_main(&mut self) -> ReturnCode {
        //internal function table 0 is always mimium_main
        self.global_states
            .resize(self.prog.global_fn_table[0].1.state_size as usize);
        // 0 is always base pointer to the main function
        self.base_pointer += 1;
        self.execute(0, None)
    }
}

#[cfg(test)]
mod test;
