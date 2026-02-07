use core::slice;
use slotmap::{DefaultKey, SlotMap};
use std::{cell::RefCell, cmp::Ordering, collections::HashMap, ops::Range, rc::Rc};
pub mod bytecode;
pub mod heap;
pub mod program;
mod ringbuffer;
pub use bytecode::*;
use ringbuffer::Ringbuffer;

use program::OpenUpValue;
pub use program::{FuncProto, Program};

use crate::{
    compiler::bytecodegen::ByteCodeGenerator,
    interner::Symbol,
    plugin::{ExtClsInfo, ExtClsType, ExtFunInfo, ExtFunType, MachineFunction},
    runtime::vm::program::WordSize,
    types::{Type, TypeSize},
};
pub type RawVal = u64;
pub type ReturnCode = i64;

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
    fn push_pos(&mut self, offset: StateOffset) {
        self.pos = (self.pos as u64 + (std::convert::Into::<u64>::into(offset))) as usize;
    }
    fn pop_pos(&mut self, offset: StateOffset) {
        self.pos = (self.pos as u64 - (std::convert::Into::<u64>::into(offset))) as usize;
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

#[derive(Debug, Clone, Default)]
pub(crate) struct ArrayHeap {
    elem_word_size: u64,
    data: Vec<RawVal>,
}
impl ArrayHeap {
    pub fn get_length_array(&self) -> u64 {
        self.data.len() as u64 / self.elem_word_size
    }
    pub fn get_elem_word_size(&self) -> u64 {
        self.elem_word_size
    }
    pub fn get_data(&self) -> &[RawVal] {
        &self.data
    }
    pub fn get_data_mut(&mut self) -> &mut [RawVal] {
        &mut self.data
    }
}
#[derive(Debug, Clone, Default)]
pub(crate) struct ArrayStorage {
    data: SlotMap<DefaultKey, ArrayHeap>,
}
pub(crate) type ArrayIdx = slotmap::DefaultKey;
impl ArrayStorage {
    pub fn alloc_array(&mut self, len: u64, elem_size: u64) -> RawVal {
        let array = ArrayHeap {
            elem_word_size: elem_size,
            data: vec![0u64; (len * elem_size) as usize],
        };
        let key = self.data.insert(array);
        debug_assert!(
            std::mem::size_of::<ArrayIdx>() == 8,
            "ArrayIdx size must be 8 bytes"
        );
        unsafe { std::mem::transmute_copy::<ArrayIdx, RawVal>(&key) }
    }
    pub fn get_array(&self, id: RawVal) -> &ArrayHeap {
        let key: ArrayIdx = unsafe { std::mem::transmute_copy::<RawVal, ArrayIdx>(&id) };
        self.data.get(key).expect("Invalid ArrayIdx")
    }
    pub fn get_array_mut(&mut self, id: RawVal) -> &mut ArrayHeap {
        let key: ArrayIdx = unsafe { std::mem::transmute_copy::<RawVal, ArrayIdx>(&id) };
        self.data.get_mut(key).expect("Invalid ArrayIdx")
    }
}
// Upvalues are used with Rc<RefCell<UpValue>> because it maybe shared between multiple closures
// Maybe it will be managed with some GC mechanism in the future.
#[derive(Debug, Clone, PartialEq)]
enum UpValue {
    Open(OpenUpValue),
    Closed(Vec<RawVal>, bool),
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
        state_storage.resize(fnproto.state_skeleton.total_size() as usize);
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

#[derive(Clone, Copy, Default)]
enum RawValType {
    Float,
    #[default]
    Int,
    // UInt,
}

#[derive(Clone, Copy)]
enum ExtFnIdx {
    Fun(usize),
    Cls(usize),
}
/// The virtual machine that executes mimium bytecode programs.
///
/// A [`Machine`] holds the compiled [`Program`], value stack and all live
/// closures. External functions and closures installed from plugins are also
/// managed here.
pub struct Machine {
    // program will be modified while its execution, e.g., higher-order external closure creates its wrapper.
    pub prog: Program,
    stack: Vec<RawVal>,
    base_pointer: u64,
    pub closures: ClosureStorage, // TODO: Will be replaced by heap in later phases
    pub heap: heap::HeapStorage,  // New unified heap storage
    pub ext_fun_table: Vec<(Symbol, ExtFunType)>,
    pub ext_cls_table: Vec<(Symbol, ExtClsType)>,
    pub arrays: ArrayStorage,
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
        let bres: bool = $op(matches!(
            Self::get_as::<f64>($self.get_stack($src as i64)).partial_cmp(&0.0),
            Some(std::cmp::Ordering::Greater)
        ));
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
    /// Drop a closure by decrementing its reference count.
    /// When refcount reaches 0, recursively drops captured closures and removes the closure.
    pub fn drop_closure(&mut self, id: ClosureIdx) {
        let cls = self.closures.get_mut(id.0).unwrap();
        cls.refcount -= 1;
        if cls.refcount == 0 {
            let refs = self
                .closures
                .get_mut(id.0)
                .unwrap()
                .upvalues
                .iter()
                .map(|v| {
                    let v = v.borrow();
                    match &*v {
                        UpValue::Closed(data, true) => {
                            // Closure-typed upvalue: value is a HeapIdx
                            let heap_idx = Self::get_as::<heap::HeapIdx>(data[0]);
                            self.heap.get(heap_idx).and_then(|obj| {
                                (!obj.data.is_empty())
                                    .then_some((heap_idx, Self::get_as::<ClosureIdx>(obj.data[0])))
                            })
                        }
                        _ => None,
                    }
                })
                .collect::<Vec<_>>();
            refs.iter().filter_map(|i| *i).for_each(|(heap_idx, clsi)| {
                self.drop_closure(clsi);
                heap::heap_release(&mut self.heap, heap_idx);
            });
            self.closures.remove(id.0);
        }
    }

    /// Create a new VM from a compiled [`Program`] and external functions.
    pub fn new(
        prog: Program,
        extfns: impl Iterator<Item = ExtFunInfo>,
        extcls: impl Iterator<Item = Box<dyn MachineFunction>>,
    ) -> Self {
        let mut res = Self {
            prog,
            stack: vec![],
            base_pointer: 0,
            closures: Default::default(),
            heap: Default::default(),
            ext_fun_table: vec![],
            ext_cls_table: vec![],
            fn_map: HashMap::new(),
            // cls_map: HashMap::new(),
            arrays: ArrayStorage::default(),
            global_states: Default::default(),
            states_stack: Default::default(),
            delaysizes_pos_stack: vec![0],
            global_vals: vec![],
            debug_stacktype: vec![RawValType::Int; 255],
        };
        extfns.for_each(|ExtFunInfo { name, fun, .. }| {
            let _ = res.install_extern_fn(name, fun);
        });
        extcls.for_each(|machine_function| {
            let _ = res.install_extern_cls(machine_function.get_name(), machine_function.get_fn());
        });
        res.link_functions();
        res
    }
    /// Create a new VM instance with the new program, preserving the current state as possible.
    pub fn new_resume(&self, prog: Program) -> Self {
        let mut new_vm = Self {
            prog,
            stack: vec![],
            base_pointer: 0,
            closures: Default::default(),
            heap: Default::default(),
            ext_fun_table: vec![],
            ext_cls_table: vec![],
            fn_map: HashMap::new(),
            // cls_map: HashMap::new(),
            arrays: ArrayStorage::default(),
            global_states: Default::default(),
            states_stack: Default::default(),
            delaysizes_pos_stack: vec![0],
            global_vals: vec![],
            debug_stacktype: vec![RawValType::Int; 255],
        };
        //expect there are no change changes in external function use for now

        new_vm.ext_fun_table = self.ext_fun_table.clone();
        new_vm.ext_cls_table = self.ext_cls_table.clone();
        new_vm.global_vals = self.global_vals.clone();
        new_vm.arrays = self.arrays.clone();

        let new_state = state_tree::update_state_storage(
            &self.global_states.rawdata,
            self.prog
                .get_dsp_state_skeleton()
                .cloned()
                .expect("dsp function not found"),
            new_vm
                .prog
                .get_dsp_state_skeleton()
                .cloned()
                .expect("dsp function not found"),
        );
        match new_state {
            Ok(Some(s)) => {
                new_vm.global_states.rawdata = s;
            }
            Ok(None) => {
                log::info!("No state structure change detected. Just copies buffer");
                new_vm.global_states.rawdata = self.global_states.rawdata.clone();
            }
            Err(e) => {
                log::error!("Failed to migrate global state: {e}");
            }
        }
        new_vm.link_functions();
        new_vm.execute_main();
        new_vm
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
    /// Extract the [`ClosureIdx`] stored inside a heap-allocated closure object.
    ///
    /// During the migration period the heap object wraps a single `ClosureIdx`
    /// in `data[0]`.  External callers (e.g. the scheduler plugin) can use this
    /// to obtain the underlying closure index from a `HeapIdx` value that lives
    /// on the VM stack.
    pub fn get_closure_idx_from_heap(&self, heap_idx: heap::HeapIdx) -> ClosureIdx {
        let heap_obj = self.heap.get(heap_idx).expect("Invalid HeapIdx");
        Self::get_as::<ClosureIdx>(heap_obj.data[0])
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

        (self.stack.split_at(base).1) as _
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
            panic!("invalid number of return value {nret_req} required but accepts only {nret}.");
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

    /// Allocate a closure on the heap storage (Phase 4 implementation)
    /// Returns a HeapIdx that can be stored in a register
    fn allocate_heap_closure(
        &mut self,
        fn_i: usize,
        upv_map: &mut LocalUpValueMap,
    ) -> heap::HeapIdx {
        // For now, create a traditional closure and store its index in the heap
        // TODO: Eventually migrate to storing closure data directly in heap
        let closure_idx = self.allocate_closure(fn_i, upv_map);

        // Create a heap object containing the ClosureIdx
        // Layout: [closure_idx_as_raw_val]
        let heap_obj = heap::HeapObject::with_data(vec![Self::to_value(closure_idx)]);
        let heap_idx = self.heap.insert(heap_obj);

        log::trace!(
            "allocate_heap_closure: fn_i={fn_i}, heap_idx={heap_idx:?}, closure_idx={closure_idx:?}"
        );
        heap_idx
    }

    /// Release a heap-based closure.
    ///
    /// Decrements the HeapObject reference count via `heap_release`.
    /// If the underlying closure has not escaped (`is_closed == false`),
    /// also drops the closure via the normal refcount mechanism.
    fn release_heap_closure(&mut self, heap_idx: heap::HeapIdx) {
        log::trace!("release_heap_closure: heap_idx={heap_idx:?}");

        // Extract the ClosureIdx before we do anything that mutably borrows heap.
        let maybe_closure = self.heap.get(heap_idx).and_then(|obj| {
            (!obj.data.is_empty()).then_some(Self::get_as::<ClosureIdx>(obj.data[0]))
        });

        if let Some(closure_idx) = maybe_closure {
            if !self.get_closure(closure_idx).is_closed {
                self.drop_closure(closure_idx);
            }
        }

        // Always decrement the HeapObject refcount.
        // If refcount reaches 0 the wrapper is freed.
        // For escaped closures whose HeapIdx was CloneHeap'd before return,
        // the refcount will still be > 0 after this release.
        heap::heap_release(&mut self.heap, heap_idx);
    }

    /// Close upvalues of a heap-based closure (does not release the heap object)
    fn close_heap_upvalues(&mut self, heap_idx: heap::HeapIdx) {
        log::trace!("close_heap_upvalues: heap_idx={heap_idx:?}");

        // Extract the ClosureIdx and close its upvalues
        if let Some(heap_obj) = self.heap.get(heap_idx) {
            if !heap_obj.data.is_empty() {
                let closure_idx = Self::get_as::<ClosureIdx>(heap_obj.data[0]);
                // Close upvalues directly by ClosureIdx without corrupting the stack
                self.close_upvalues_by_idx(closure_idx);
            }
        }
    }

    /// Release all heap-based closures tracked by the current scope.
    ///
    /// Each entry in `local_heap_closures` was either:
    /// - created by `MakeHeapClosure` (initial refcount=1), or
    /// - cloned by `CloneHeap` (refcount was incremented and entry added).
    ///
    /// Calling `release_heap_closure` decrements the refcount.  Objects whose
    /// refcount reaches 0 are freed; those that were `CloneHeap`'d before return
    /// will survive with a positive refcount.
    fn release_heap_closures(&mut self, local_heap_closures: &[heap::HeapIdx]) {
        for &heap_idx in local_heap_closures {
            self.release_heap_closure(heap_idx);
        }
    }

    /// This API is used for defining higher-order external function that returns some external rust closure.
    /// Because the native closure cannot be called with CallCls directly, the vm appends an additional function the program,
    /// that wraps external closure call with an internal closure.
    pub fn wrap_extern_cls(&mut self, extcls: ExtClsInfo) -> ClosureIdx {
        let ExtClsInfo { name, fun, ty } = extcls;

        self.prog.ext_fun_table.push((name.to_string(), ty));
        let prog_funid = self.prog.ext_fun_table.len() - 1;
        self.ext_cls_table.push((name, fun));
        let vm_clsid = self.ext_cls_table.len() - 1;
        self.fn_map.insert(prog_funid, ExtFnIdx::Cls(vm_clsid));
        let (bytecodes, nargs, nret) = if let Type::Function { arg, ret } = ty.to_type() {
            let mut wrap_bytecode = Vec::<Instruction>::new();
            // todo: decouple bytecode generator dependency
            let asize = ByteCodeGenerator::word_size_for_type(arg);
            // if there are 2 arguments of float for instance, base pointer should be 2
            let nargs = match arg.to_type() {
                Type::Tuple(args) => args.len(),
                Type::Record(fields) => fields.len(),
                _ => unreachable!("single argument should be 1 element record"),
            } as u8;
            let base = nargs as u8;
            let nret = ByteCodeGenerator::word_size_for_type(ret);
            wrap_bytecode.push(Instruction::MoveConst(base, 0));
            wrap_bytecode.push(Instruction::MoveRange(base + 1, 0, asize));

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
        self.prog.global_fn_table.push((name.to_string(), newfunc));
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
        self.close_upvalues_by_idx(clsidx);
    }
    /// Close all open upvalues of the given closure, copying stack values into
    /// the upvalue cells so the closure can outlive the current stack frame.
    fn close_upvalues_by_idx(&mut self, clsidx: ClosureIdx) {
        let closure_base_ptr = self.get_closure(clsidx).base_ptr as usize;

        // Collect (ClosureIdx-to-retain, Option<HeapIdx-to-retain>) pairs for
        // each upvalue that holds a closure reference.
        let refs = self
            .get_closure(clsidx)
            .upvalues
            .iter()
            .map(|upv| {
                let upv = &mut *upv.borrow_mut();
                match upv {
                    UpValue::Open(ov) => {
                        let (_range, ov_raw) = self.get_open_upvalue(closure_base_ptr, *ov);
                        let is_closure = ov.is_closure;
                        *upv = UpValue::Closed(ov_raw.to_vec(), is_closure);
                        if is_closure {
                            // The raw value is a HeapIdx wrapping a ClosureIdx
                            let heap_idx = Self::get_as::<heap::HeapIdx>(ov_raw[0]);
                            Some(heap_idx)
                        } else {
                            None
                        }
                    }
                    UpValue::Closed(v, is_closure) => {
                        if *is_closure {
                            Some(Self::get_as::<heap::HeapIdx>(v[0]))
                        } else {
                            None
                        }
                    }
                }
            })
            .collect::<Vec<_>>();
        // Retain each captured HeapObject and increment the underlying Closure refcount
        refs.iter().for_each(|heap_opt| {
            if let Some(heap_idx) = heap_opt {
                // Retain the HeapObject so it is not freed when its creator exits
                heap::heap_retain(&mut self.heap, *heap_idx);
                // Also bump the underlying Closure refcount for the existing drop_closure chain
                if let Some(heap_obj) = self.heap.get(*heap_idx) {
                    if !heap_obj.data.is_empty() {
                        let ci = Self::get_as::<ClosureIdx>(heap_obj.data[0]);
                        self.get_closure_mut(ci).refcount += 1;
                    }
                }
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
                self.drop_closure(*clsidx)
            }
        }
    }
    fn get_fnproto(&self, func_i: usize) -> &FuncProto {
        &self.prog.global_fn_table[func_i].1
    }
    /// Execute a function within the VM.
    ///
    /// `func_i` is an index into the program's function table and `cls_i` is an
    /// optional closure that provides the environment for the call.
    /// The returned [`ReturnCode`] is the number of values pushed on the stack
    /// as a result of the call.
    pub fn execute(&mut self, func_i: usize, cls_i: Option<ClosureIdx>) -> ReturnCode {
        let mut local_closures: Vec<ClosureIdx> = vec![];
        let mut local_heap_closures: Vec<heap::HeapIdx> = vec![];
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
                                cls.borrow_mut()(machine)
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
                // New heap-based instructions (Phase 4)
                Instruction::MakeHeapClosure(dst, fn_index, _size) => {
                    let fn_proto_pos = self.get_stack(fn_index as i64) as usize;
                    let heap_idx = self.allocate_heap_closure(fn_proto_pos, &mut upv_map);
                    local_heap_closures.push(heap_idx);
                    // Store the heap index (not closure index) in the register
                    self.set_stack(dst as i64, Self::to_value(heap_idx));
                }
                Instruction::CloseHeapClosure(src) => {
                    let heap_addr = self.get_stack(src as i64);
                    let heap_idx = Self::get_as::<heap::HeapIdx>(heap_addr);
                    self.close_heap_upvalues(heap_idx);
                }
                Instruction::CloneHeap(src) => {
                    let heap_addr = self.get_stack(src as i64);
                    let heap_idx = Self::get_as::<heap::HeapIdx>(heap_addr);
                    heap::heap_retain(&mut self.heap, heap_idx);
                }
                Instruction::BoxAlloc(dst, src, inner_size) => {
                    // Allocate a heap object and copy data from stack
                    let (_, src_data) = self.get_stack_range(src as i64, inner_size);
                    let data = src_data.to_vec();
                    let heap_idx = self.heap.insert(heap::HeapObject::with_data(data));
                    self.set_stack(dst as i64, Self::to_value(heap_idx));
                }
                Instruction::BoxLoad(dst, src, inner_size) => {
                    // Load data from heap to stack
                    let heap_addr = self.get_stack(src as i64);
                    let heap_idx = Self::get_as::<heap::HeapIdx>(heap_addr);
                    let heap_obj = self
                        .heap
                        .get(heap_idx)
                        .expect("BoxLoad: invalid heap index");
                    let data: Vec<u64> = heap_obj.data[..inner_size as usize].to_vec();
                    self.set_stack_range(dst as i64, &data);
                }
                Instruction::CallIndirect(func, nargs, nret_req) => {
                    // Get heap index from the register
                    let heap_addr = self.get_stack(func as i64);
                    let heap_idx = Self::get_as::<heap::HeapIdx>(heap_addr);

                    // Extract the ClosureIdx from the heap object
                    let heap_obj = self.heap.get(heap_idx).expect("Invalid heap index");
                    let cls_i = Self::get_as::<ClosureIdx>(heap_obj.data[0]);

                    let cls = self.get_closure(cls_i);
                    let pos_of_f = cls.fn_proto_pos;
                    self.states_stack.push(cls_i);
                    self.call_function(func, nargs, nret_req, move |machine| {
                        machine.execute(pos_of_f, Some(cls_i))
                    });
                    self.states_stack.pop();
                }
                Instruction::Return0 => {
                    self.stack.truncate((self.base_pointer - 1) as usize);
                    self.release_open_closures(&local_closures);
                    self.release_heap_closures(&local_heap_closures);
                    return 0;
                }
                Instruction::Return(iret, nret) => {
                    let _ = self.return_general(iret, nret);
                    self.release_open_closures(&local_closures);
                    self.release_heap_closures(&local_heap_closures);
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
                                let rawv: &[RawVal] = unsafe { std::mem::transmute(rawv) };
                                rawv
                            }
                            UpValue::Closed(rawval, _) => {
                                let rawv: &[RawVal] =
                                    unsafe { std::mem::transmute(rawval.as_slice()) };
                                rawv
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
                        UpValue::Open(OpenUpValue { pos: i, size, .. }) => {
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
                        UpValue::Closed(uv, _) => {
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
                    increment = offset;
                }
                Instruction::JmpIfNeg(cond, offset) => {
                    let cond_v = self.get_stack(cond as i64);
                    if Self::get_as::<f64>(cond_v) <= 0.0 {
                        increment = offset;
                    }
                }
                Instruction::JmpTable(scrut, table_idx) => {
                    let scrut_val = self.get_stack(scrut as i64);
                    // Scrutinee is always an integer: either a tag from union or cast from float
                    let val = Self::get_as::<i64>(scrut_val);
                    let fn_proto = self.get_fnproto(func_i);
                    debug_assert!(
                        !fn_proto.jump_tables.is_empty(),
                        "JmpTable instruction requires non-empty jump_tables"
                    );
                    let table = &fn_proto.jump_tables[table_idx as usize];
                    let idx = (val - table.min) as usize;
                    // Last element of offsets is the default for out-of-range values
                    let default_idx = table.offsets.len() - 1;
                    increment = table
                        .offsets
                        .get(idx)
                        .copied()
                        .unwrap_or(table.offsets[default_idx]);
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
                Instruction::LogF(dst, src) => uniopmethod!(ln, f64, dst, src, self),
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
                Instruction::AllocArray(dst, len, elem_size) => {
                    // Allocate an array of the given length and element size
                    let key = self.arrays.alloc_array(len as _, elem_size as _);
                    // Set the stack to point to the start of the new array
                    self.set_stack(dst as i64, key);
                }
                Instruction::GetArrayElem(dst, arr, idx) => {
                    // Get the array and index values
                    let array = self.get_stack(arr as i64);
                    let index = self.get_stack(idx as i64);
                    let index_val = Self::get_as::<f64>(index);
                    let adata = self.arrays.get_array(array);
                    let elem_word_size = adata.elem_word_size as usize;
                    let buffer = unsafe {
                        let address = adata
                            .data
                            .as_ptr()
                            .wrapping_add(index_val as usize * elem_word_size);
                        std::slice::from_raw_parts(address, elem_word_size)
                    };
                    set_vec_range(
                        &mut self.stack,
                        (self.base_pointer + dst as u64) as usize,
                        buffer,
                    );
                    // todo: implement automatic interpolation and out-of-bounds handling for primitive arrays.
                }
                Instruction::SetArrayElem(arr, idx, val) => {
                    // Get the array, index, and value
                    let array = self.get_stack(arr as i64);
                    let index = self.get_stack(idx as i64);
                    let index_val = Self::get_as::<f64>(index);
                    let index_int = index_val as usize;
                    let adata = self.arrays.get_array_mut(array);
                    let elem_word_size = adata.elem_word_size as usize;
                    let buffer = unsafe {
                        let address = adata
                            .data
                            .as_mut_ptr()
                            .wrapping_add(index_int * elem_word_size);
                        std::slice::from_raw_parts_mut(address, elem_word_size)
                    };
                    let (_range, buf_src) = self.get_stack_range(val as _, elem_word_size as _);
                    buffer.copy_from_slice(buf_src);
                }
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
                Instruction::PushStatePos(v) => self.get_current_state().push_pos(v),
                Instruction::PopStatePos(v) => self.get_current_state().pop_pos(v),
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
        let global_mem_size = self
            .prog
            .global_vals
            .iter()
            .map(|WordSize(size)| *size as usize)
            .sum();
        self.global_vals = vec![0; global_mem_size];
        self.prog
            .ext_fun_table
            .iter_mut()
            .enumerate()
            .for_each(|(i, (name, _ty))| {
                if let Some((j, _)) = self
                    .ext_fun_table
                    .iter()
                    .enumerate()
                    .find(|(_j, (fname, _fn))| name == fname.as_str())
                {
                    let _ = self.fn_map.insert(i, ExtFnIdx::Fun(j));
                } else if let Some((j, _)) = self
                    .ext_cls_table
                    .iter()
                    .enumerate()
                    .find(|(_j, (fname, _fn))| name == fname.as_str())
                {
                    let _ = self.fn_map.insert(i, ExtFnIdx::Cls(j));
                } else {
                    panic!("external function {name} cannot be found");
                }
            });
    }
    pub fn execute_idx(&mut self, idx: usize) -> ReturnCode {
        let (_name, func) = &self.prog.global_fn_table[idx];
        if !func.bytecodes.is_empty() {
            self.global_states
                .resize(func.state_skeleton.total_size() as usize);
            // 0 is always base pointer to the main function
            if !self.stack.is_empty() {
                self.stack[0] = 0;
            }
            self.base_pointer = 1;
            self.execute(idx, None)
        } else {
            0
        }
    }
    pub fn execute_entry(&mut self, entry: &str) -> ReturnCode {
        if let Some(idx) = self.prog.get_fun_index(entry) {
            self.execute_idx(idx)
        } else {
            -1
        }
    }
    pub fn execute_main(&mut self) -> ReturnCode {
        // 0 is always base pointer to the main function
        self.base_pointer += 1;
        self.execute(0, None)
    }
}

#[cfg(test)]
mod test;
