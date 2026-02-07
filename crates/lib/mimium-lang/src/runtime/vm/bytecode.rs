use crate::{interner::TypeNodeId, types::TypeSize, utils::half_float::HFloat};

pub type Reg = u8; // register position
pub type ConstPos = u16;
pub type GlobalPos = u8;
pub type Offset = i16;
pub type ShotrOffset = i8;
//24bit unsigned integer for shiftsate
pub type StateOffset = intx::U24;
/// Instructions for bytecode. Currently, each instructon has the 64 bit size(Tag, up to 3 bytes arguments.)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Instruction {
    /// Move Single Value, Destination, Source
    Move(Reg, Reg),
    /// Load Single Value from Constants. Destination, Source
    MoveConst(Reg, ConstPos),
    /// Load Immediate float from half precision. Destination, Value
    MoveImmF(Reg, HFloat),
    // Move the range of registers (e.g. tuple) Destination, Source, Wordsize
    MoveRange(Reg, Reg, TypeSize),
    /// Call to internal function
    /// Function Address,Nargs,Word Size of Return Value
    Call(Reg, u8, TypeSize),
    //call internal closure
    CallCls(Reg, u8, TypeSize),
    /// Call external rust functions or closure,
    /// Currently, The execution branches into the invocation of a raw function item(function pointer), or Rust Closure depending on the information on the VM.
    /// Previously there was another operation `CallExtCls` for Rust Closure Invocation separately but keeping distinction between raw function pointer and closure at the type checking and program generation stage, makes compiler's design complex thus it was removed.
    /// The distincion may also be resolved statically at linking time.
    /// Function Address,Nargs,Nret
    CallExtFun(Reg, u8, TypeSize),
    /// Create new closure. Destination, index of inner function prototype in global function table.
    Closure(Reg, Reg),
    /// register of the closure to be closed. other local closures will be released with this instruction.
    Close(Reg),

    // New heap-based closure instructions (Phase 3)
    /// Create a closure on the heap. Destination, Function Index, Closure Size
    /// The closure data is allocated on the heap and a HeapIdx is stored in the destination register.
    MakeHeapClosure(Reg, Reg, TypeSize),
    /// Close upvalues of a heap-based closure. Address Register (HeapIdx)
    CloseHeapClosure(Reg),
    /// Increment reference count of a heap object and register it for release at scope exit.
    /// Used for call-by-value cloning of heap-allocated closures.
    CloneHeap(Reg),
    /// Call a closure indirectly through heap storage. Function register (HeapIdx), nargs, nret
    CallIndirect(Reg, u8, TypeSize),

    /// Box a value by allocating it on the heap.
    /// BoxAlloc(destination_for_HeapIdx, source_value, inner_word_size)
    /// Allocates a heap object of inner_word_size, copies from source, stores HeapIdx in destination.
    BoxAlloc(Reg, Reg, TypeSize),
    /// Unbox a heap-allocated value by loading from heap.
    /// BoxLoad(destination, source_HeapIdx, inner_word_size)
    /// Reads inner_word_size words from heap and stores to destination registers.
    BoxLoad(Reg, Reg, TypeSize),
    /// Increment the reference count of a boxed heap object.
    /// BoxClone(heap_ptr_reg)
    BoxClone(Reg),
    /// Decrement the reference count of a boxed heap object and free if count reaches 0.
    /// BoxRelease(heap_ptr_reg)
    BoxRelease(Reg),
    /// Write a value into an already-allocated heap object (destructive update).
    /// BoxStore(heap_ptr_reg, src_reg, inner_word_size)
    BoxStore(Reg, Reg, TypeSize),
    /// Clone all boxed references within a UserSum value.
    /// CloneUserSum(value_reg, value_size, type_id)
    CloneUserSum(Reg, TypeSize, TypeNodeId),

    /// destination,source, size
    GetUpValue(Reg, Reg, TypeSize),
    SetUpValue(Reg, Reg, TypeSize),

    /// destination,source
    GetGlobal(Reg, GlobalPos, TypeSize),
    SetGlobal(GlobalPos, Reg, TypeSize),
    /// Call internal state over time, destination,source
    GetState(Reg, TypeSize),
    SetState(Reg, TypeSize),
    PushStatePos(StateOffset),
    PopStatePos(StateOffset),

    /// Return from current function without return value.
    Return0,
    /// value start position, Nrets
    Return(Reg, TypeSize),
    //dst,src,time,idx
    Delay(Reg, Reg, Reg),
    Mem(Reg, Reg),

    /// jump to instruction over the offset.
    Jmp(Offset),
    /// jump to instruction over the offset if the value in the first argument was negative.
    JmpIfNeg(Reg, Offset),
    /// Jump table for switch/match expressions.
    /// (scrutinee_reg, table_index)
    /// Looks up the scrutinee value in the jump table and jumps to the corresponding offset.
    /// If not found, jumps to the default offset stored in the jump table.
    JmpTable(Reg, u8),

    /// Primitive Operations.
    /// Destination, Src1, Src2
    AddF(Reg, Reg, Reg),
    SubF(Reg, Reg, Reg),
    MulF(Reg, Reg, Reg),
    DivF(Reg, Reg, Reg),
    ModF(Reg, Reg, Reg),
    NegF(Reg, Reg),
    AbsF(Reg, Reg),
    SqrtF(Reg, Reg),
    SinF(Reg, Reg),
    CosF(Reg, Reg),
    PowF(Reg, Reg, Reg),
    LogF(Reg, Reg),

    // Primitive Operations for int
    AddI(Reg, Reg, Reg),
    SubI(Reg, Reg, Reg),
    MulI(Reg, Reg, Reg),
    DivI(Reg, Reg, Reg),
    ModI(Reg, Reg, Reg),
    NegI(Reg, Reg),
    AbsI(Reg, Reg),

    PowI(Reg, Reg, Reg),
    LogI(Reg, Reg, Reg),
    // primitive Operations for bool
    Not(Reg, Reg),
    Eq(Reg, Reg, Reg),
    Ne(Reg, Reg, Reg),
    Gt(Reg, Reg, Reg),
    Ge(Reg, Reg, Reg),
    Lt(Reg, Reg, Reg),
    Le(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),

    CastFtoI(Reg, Reg),
    CastItoF(Reg, Reg),
    CastItoB(Reg, Reg),
    /// Allocate new array in the heap.
    /// Destination, size of array, type size of each element
    AllocArray(Reg, Reg, TypeSize),
    /// Get array element: destination, array, index
    GetArrayElem(Reg, Reg, Reg),
    /// Set array element. Because the array is immutable, this instruction is mostly used for initialization.
    /// array, index, value.
    SetArrayElem(Reg, Reg, Reg),

    /// Dummy instruction for testing
    Dummy,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Return0 => write!(f, "ret0"),
            Instruction::Jmp(dst) => write!(f, "{:<10} {}", "jmp", dst),
            Instruction::GetState(dst, size) => write!(f, "{:<10} {} {}", "getstate", dst, size),
            Instruction::SetState(src, size) => write!(f, "{:<10} {} {}", "setstate", src, size),
            Instruction::PushStatePos(v) => write!(f, "{:<10} {}", "pushsttpos", v),
            Instruction::PopStatePos(v) => write!(f, "{:<10} {}", "popsttpos", v),
            Instruction::Move(dst, src) => write!(f, "{:<10} {} {}", "mov", dst, src),
            Instruction::MoveConst(dst, num) => write!(f, "{:<10} {} {}", "movc", dst, num),
            Instruction::MoveImmF(dst, v) => write!(f, "{:<10} {} {}", "movimmF", dst, v),
            Instruction::MoveRange(dst, src, n) => {
                write!(
                    f,
                    "{:<10} {}-{} {}-{}",
                    "mov",
                    dst,
                    dst + n - 1,
                    src,
                    src + n - 1
                )
            }
            Instruction::Closure(dst, src) => {
                write!(f, "{:<10} {} {}", "closure", dst, src)
            }
            Instruction::Close(src) => {
                write!(f, "{:<10} {}", "close", src)
            }
            // New heap-based instructions (Phase 3)
            Instruction::MakeHeapClosure(dst, fn_idx, size) => {
                write!(f, "{:<10} {} {} {}", "mkheapcls", dst, fn_idx, size)
            }
            Instruction::CloseHeapClosure(addr) => {
                write!(f, "{:<10} {}", "clsheapcls", addr)
            }
            Instruction::CloneHeap(addr) => {
                write!(f, "{:<10} {}", "cloneheap", addr)
            }
            Instruction::CallIndirect(func, nargs, nret_req) => {
                write!(f, "{:<10} {} {} {}", "callind", func, nargs, nret_req)
            }
            Instruction::BoxAlloc(dst, src, size) => {
                write!(f, "{:<10} {} {} {}", "boxalloc", dst, src, size)
            }
            Instruction::BoxLoad(dst, src, size) => {
                write!(f, "{:<10} {} {} {}", "boxload", dst, src, size)
            }
            Instruction::BoxClone(src) => {
                write!(f, "{:<10} {}", "boxclone", src)
            }
            Instruction::BoxRelease(src) => {
                write!(f, "{:<10} {}", "boxrelease", src)
            }
            Instruction::BoxStore(dst, src, size) => {
                write!(f, "{:<10} {} {} {}", "boxstore", dst, src, size)
            }
            Instruction::CloneUserSum(src, size, ty) => {
                write!(f, "{:<10} {} {} type:{}", "clone_usersum", src, size, ty.to_type())
            }
            Instruction::Delay(dst, src, time) => {
                write!(f, "{:<10} {} {} {}", "delay", dst, src, time)
            }
            Instruction::Mem(dst, src) => {
                write!(f, "{:<10} {} {}", "mem", dst, src)
            }
            Instruction::Return(iret, nret) => write!(f, "{:<10} {} {}", "ret", iret, nret),
            Instruction::GetUpValue(dst, srcup, size) => {
                write!(f, "{:<10} {} {} {}", "getupv", dst, srcup, size)
            }
            Instruction::SetUpValue(dstup, src, size) => {
                write!(f, "{:<10} {} {} {}", "setupv", dstup, src, size)
            }
            Instruction::GetGlobal(dst, src, size) => {
                write!(f, "{:<10} {} {} {}", "getglobal", dst, src, size)
            }
            Instruction::SetGlobal(dst, src, size) => {
                write!(f, "{:<10} {} {} {}", "setglobal", dst, src, size)
            }
            Instruction::JmpIfNeg(dst, cond) => write!(f, "{:<10} {} {}", "jmpifneg", dst, cond),
            Instruction::JmpTable(scrut, table_idx) => {
                write!(f, "{:<10} {} {}", "jmptable", scrut, table_idx)
            }
            Instruction::AbsF(dst, src) => write!(f, "{:<10} {} {}", "absf", dst, src),
            Instruction::NegF(dst, src) => write!(f, "{:<10} {} {}", "negf", dst, src),
            Instruction::SinF(dst, src) => write!(f, "{:<10} {} {}", "sin", dst, src),
            Instruction::CosF(dst, src) => write!(f, "{:<10} {} {}", "cos", dst, src),
            Instruction::SqrtF(dst, src) => write!(f, "{:<10} {} {}", "sqrt", dst, src),
            Instruction::LogF(dst, src) => write!(f, "{:<10} {} {} ", "logf", dst, src),
            Instruction::AbsI(dst, src) => write!(f, "{:<10} {} {}", "abs", dst, src),
            Instruction::NegI(dst, src) => write!(f, "{:<10} {} {}", "neg", dst, src),
            Instruction::Not(dst, src) => write!(f, "{:<10} {} {}", "not", dst, src),
            Instruction::CastFtoI(dst, src) => write!(f, "{:<10} {} {}", "f2i", dst, src),
            Instruction::CastItoF(dst, src) => write!(f, "{:<10} {} {}", "i2f", dst, src),
            Instruction::CastItoB(dst, src) => write!(f, "{:<10} {} {}", "i2b", dst, src),
            Instruction::Call(func, nargs, nret_req) => {
                write!(f, "{:<10} {} {} {}", "call", func, nargs, nret_req)
            }
            Instruction::CallCls(func, nargs, nret_req) => {
                write!(f, "{:<10} {} {} {}", "callcls", func, nargs, nret_req)
            }
            Instruction::CallExtFun(func, nargs, nret_req) => {
                write!(f, "{:<10} {} {} {}", "callext", func, nargs, nret_req)
            }
            Instruction::LogI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "logi", dst, lhs, rhs),
            Instruction::PowI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "powi", dst, lhs, rhs),
            Instruction::AddF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "addf", dst, lhs, rhs),
            Instruction::SubF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "subf", dst, lhs, rhs),
            Instruction::MulF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "mulf", dst, lhs, rhs),
            Instruction::DivF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "divf", dst, lhs, rhs),
            Instruction::ModF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "modf", dst, lhs, rhs),
            Instruction::PowF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "powf", dst, lhs, rhs),
            Instruction::AddI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "add", dst, lhs, rhs),
            Instruction::SubI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "sub", dst, lhs, rhs),
            Instruction::MulI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "mul", dst, lhs, rhs),
            Instruction::DivI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "div", dst, lhs, rhs),
            Instruction::ModI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "mod", dst, lhs, rhs),
            Instruction::Eq(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "eq", dst, lhs, rhs),
            Instruction::Ne(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "ne", dst, lhs, rhs),
            Instruction::Gt(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "gt", dst, lhs, rhs),
            Instruction::Ge(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "ge", dst, lhs, rhs),
            Instruction::Lt(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "le", dst, lhs, rhs),
            Instruction::Le(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "lt", dst, lhs, rhs),
            Instruction::And(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "and", dst, lhs, rhs),
            Instruction::Or(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "or", dst, lhs, rhs),
            Instruction::AllocArray(dst, size, typesize) => {
                write!(f, "{:<10} {} {} {}", "array", dst, size, typesize)
            }
            Instruction::GetArrayElem(dst, arr, idx) => {
                write!(f, "{:<10} {} {} {}", "getarray", dst, arr, idx)
            }
            Instruction::SetArrayElem(arr, idx, val) => {
                write!(f, "{:<10} {} {} {}", "setarray", arr, idx, val)
            }
            Instruction::Dummy => write!(f, "dummy"),
        }
    }
}

#[cfg(test)]
#[test]
fn ensure_bytecode_size() {
    let size = std::mem::size_of::<Instruction>();
    assert_eq!(4, size);
}
