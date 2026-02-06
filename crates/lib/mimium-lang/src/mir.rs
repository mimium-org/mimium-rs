// Mid-level intermediate representation that is more like imperative form than hir.
use crate::{
    compiler::IoChannelInfo,
    interner::{Symbol, TypeNodeId},
    types::{PType, RecordTypeField, Type, TypeSize},
};
use std::{cell::OnceCell, path::PathBuf, sync::Arc};
// Import StateTreeSkeleton for function state information
use state_tree::tree::{SizedType, StateTreeSkeleton};

pub mod print;

// #[derive(Debug, Clone, PartialEq)]
// pub struct Global(VPtr);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Argument(pub Symbol, pub TypeNodeId);

pub type VReg = u64;
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Value {
    Global(VPtr),
    // Argument(usize, Arc<Argument>), //index,
    Argument(usize),
    /// holds SSA index(position in infinite registers)
    Register(VReg),
    State(VPtr),
    // idx of the function in the program
    Function(usize),
    /// native function (Rust function item or closure)
    ExtFunction(Symbol, TypeNodeId),
    /// Constructor with payload: (name, tag_index, sum_type)
    Constructor(Symbol, usize, TypeNodeId),
    /// internal state
    None,
}

pub type VPtr = Arc<Value>;
type Bbindex = u64;
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Uinteger(u64),
    Integer(i64),
    //constant float
    Float(f64),
    String(Symbol),
    // allocate memory from stack depending on the size
    Alloc(TypeNodeId),
    // load value to register from the pointer type
    Load(VPtr, TypeNodeId),
    // Store value to stack(destination,source, type)
    Store(VPtr, VPtr, TypeNodeId),
    // Instruction for computing destination address like LLVM's GetElementPtr.
    // This instruction does no actual computation on runtime.
    GetElement {
        value: VPtr,
        ty: TypeNodeId, // type of the composite value like tuple or struct
        tuple_offset: u64,
    },
    // call function, arguments, type of return value
    Call(VPtr, Vec<(VPtr, TypeNodeId)>, TypeNodeId),
    CallCls(VPtr, Vec<(VPtr, TypeNodeId)>, TypeNodeId),
    GetGlobal(VPtr, TypeNodeId),
    SetGlobal(VPtr, VPtr, TypeNodeId),
    // make closure with upindexes
    Closure(VPtr),
    //closes upvalues of specific closure. Always inserted right before Return instruction.
    CloseUpValues(VPtr, TypeNodeId),

    // New heap-based closure instructions (Phase 2)
    /// Create a closure and allocate it on the heap
    /// MakeClosure(fn_proto, closure_size_in_words)
    /// Returns a HeapIdx that references the allocated closure
    MakeClosure {
        fn_proto: VPtr, // Function prototype index
        size: u64,      // Total size in words (for heap allocation)
    },
    /// Close upvalues of a heap-based closure (like CloseUpValues but for heap closures)
    /// CloseHeapClosure(heap_addr)
    /// This closes upvalues when the closure escapes its defining scope
    CloseHeapClosure(VPtr),
    /// Increment the reference count of a heap-allocated object.
    /// Inserted whenever a heap object value is duplicated (call-by-value cloning).
    /// At runtime this calls `heap_retain` and registers the object for release
    /// when the current scope exits.
    CloneHeap(VPtr),
    /// Call a closure indirectly through heap storage
    /// CallIndirect(heap_addr, args, return_type)
    CallIndirect(VPtr, Vec<(VPtr, TypeNodeId)>, TypeNodeId),

    //label to funcproto  and localvar offset?
    GetUpValue(u64, TypeNodeId),
    SetUpValue(u64, VPtr, TypeNodeId),
    //internal state: feed and delay
    PushStateOffset(u64),
    PopStateOffset(u64),
    //load internal state to register(destination)
    GetState(TypeNodeId),

    //condition,  basic block index for then statement, else statement, and merge block
    JmpIf(VPtr, Bbindex, Bbindex, Bbindex),
    // basic block index (for return statement)
    Jmp(i16),
    //merge
    Phi(VPtr, VPtr),
    /// Multi-way branch for match expressions
    /// Switch(scrutinee, cases, default_block, merge_block)
    /// cases: Vec<(literal_value_as_i64, block_index)>
    Switch {
        scrutinee: VPtr,
        /// Each case is (literal_value as i64, block_index)
        cases: Vec<(i64, Bbindex)>,
        /// Default block for non-exhaustive matches; None if exhaustive
        default_block: Option<Bbindex>,
        merge_block: Bbindex,
    },
    /// Phi for switch - merges values from multiple branches
    PhiSwitch(Vec<VPtr>),

    /// Tagged Union operations for sum types
    /// Wrap a value into a tagged union: (tag, value, union_type)
    TaggedUnionWrap {
        tag: u64,
        value: VPtr,
        union_type: TypeNodeId,
    },
    /// Extract tag from a tagged union
    TaggedUnionGetTag(VPtr),
    /// Extract value from a tagged union (assuming correct tag)
    TaggedUnionGetValue(VPtr, TypeNodeId),

    Return(VPtr, TypeNodeId),
    //value to update state
    ReturnFeed(VPtr, TypeNodeId),

    Delay(u64, VPtr, VPtr),
    Mem(VPtr),

    // Primitive Operations
    AddF(VPtr, VPtr),
    SubF(VPtr, VPtr),
    MulF(VPtr, VPtr),
    DivF(VPtr, VPtr),
    ModF(VPtr, VPtr),
    NegF(VPtr),
    AbsF(VPtr),
    SinF(VPtr),
    CosF(VPtr),
    PowF(VPtr, VPtr),
    LogF(VPtr),
    SqrtF(VPtr),

    // Primitive Operations for int
    AddI(VPtr, VPtr),
    SubI(VPtr, VPtr),
    MulI(VPtr, VPtr),
    DivI(VPtr, VPtr),
    ModI(VPtr, VPtr),
    NegI(VPtr),
    AbsI(VPtr),

    PowI(VPtr),
    LogI(VPtr, VPtr),
    // primitive Operations for bool
    Not(VPtr),
    Eq(VPtr, VPtr),
    Ne(VPtr, VPtr),
    Gt(VPtr, VPtr),
    Ge(VPtr, VPtr),
    Lt(VPtr, VPtr),
    Le(VPtr, VPtr),
    And(VPtr, VPtr),
    Or(VPtr, VPtr),

    CastFtoI(VPtr),
    CastItoF(VPtr),
    CastItoB(VPtr),

    /// Array Literal. Array in mimium is basically variable-length and immutable.
    /// So VM may allocate array in speacial heap area, or treat it as just as like static data in the program.
    /// MIR does not distinguish how the vm treats the array.
    Array(Vec<VPtr>, TypeNodeId), // array literal, values and type of the array
    GetArrayElem(VPtr, VPtr, TypeNodeId), // get array element, at specific index , type of the element
    SetArrayElem(VPtr, VPtr, VPtr, TypeNodeId), // set array element at index
    Error,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Block(pub Vec<(VPtr, Instruction)>);

impl Block {
    pub fn get_total_stateskeleton(
        &self,
        functions: &[Function],
    ) -> Vec<StateTreeSkeleton<StateType>> {
        let mut children = vec![];
        for (_v, instr) in &self.0 {
            match instr {
                Instruction::Delay(len, _, _) => {
                    children.push(Box::new(StateTreeSkeleton::Delay { len: *len }))
                }
                Instruction::Mem(_) => {
                    children.push(Box::new(StateTreeSkeleton::Mem(StateType(1))))
                }
                Instruction::ReturnFeed(_, ty) => {
                    children.push(Box::new(StateTreeSkeleton::Feed(StateType::from(*ty))))
                }
                Instruction::Call(v, _, _) => {
                    if let Value::Function(idx) = **v {
                        let func = &functions[idx];
                        if func.is_stateful() {
                            children.push(Box::new(func.state_skeleton.clone()))
                        }
                    }
                }
                _ => {}
            }
        }
        children.into_iter().map(|e| *e).collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UpIndex {
    Local(usize),   // index of local variables in upper functions
    Upvalue(usize), // index of upvalues in upper functions
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct OpenUpValue {
    pub pos: usize,
    pub size: TypeSize,
    pub is_closure: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StateType(pub u64);
impl state_tree::tree::SizedType for StateType {
    fn word_size(&self) -> u64 {
        self.0
    }
}
impl From<TypeNodeId> for StateType {
    fn from(t: TypeNodeId) -> Self {
        match t.to_type() {
            Type::Primitive(PType::Unit) => StateType(0),
            Type::Primitive(PType::Numeric) | Type::Function { .. } => StateType(1),
            Type::Record(fields) => StateType(
                fields
                    .iter()
                    .map(|RecordTypeField { ty, .. }| ty.word_size())
                    .sum(),
            ),
            Type::Tuple(elems) => StateType(elems.iter().map(|ty| ty.word_size()).sum()),
            Type::Array(_elem_ty) => StateType(1),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub index: usize,
    pub label: Symbol,
    pub args: Vec<Argument>,
    // pub argtypes: Vec<TypeNodeId>,
    pub return_type: OnceCell<TypeNodeId>, // TODO: None is the state when the type is not inferred yet.
    pub upindexes: Vec<Arc<Value>>,
    pub upperfn_i: Option<usize>,
    pub body: Vec<Block>,
    /// StateTree skeleton information for this function's state layout
    pub state_skeleton: StateTreeSkeleton<StateType>,
}

impl Function {
    pub fn new(
        index: usize,
        name: Symbol,
        args: &[Argument],
        // argtypes: &[TypeNodeId],
        state_skeleton: Vec<StateTreeSkeleton<StateType>>,
        upperfn_i: Option<usize>,
    ) -> Self {
        let state_boxed = state_skeleton.into_iter().map(Box::new).collect();
        Self {
            index,
            label: name,
            args: args.to_vec(),
            // argtypes: argtypes.to_vec(),
            return_type: OnceCell::new(),
            upindexes: vec![],
            upperfn_i,
            body: vec![Block::default()],
            state_skeleton: StateTreeSkeleton::FnCall(state_boxed),
        }
    }
    pub fn add_new_basicblock(&mut self) -> usize {
        self.body.push(Block(vec![]));
        self.body.len() - 1
    }
    pub fn get_argtypes(&self) -> Vec<TypeNodeId> {
        self.args.iter().map(|a| a.1).collect()
    }
    pub fn get_or_insert_upvalue(&mut self, v: &Arc<Value>) -> usize {
        self.upindexes
            .iter()
            .position(|vt| v == vt)
            .unwrap_or_else(|| {
                self.upindexes.push(v.clone());
                self.upindexes.len() - 1
            })
    }
    pub fn push_state_skeleton(&mut self, skeleton: StateTreeSkeleton<StateType>) {
        if let StateTreeSkeleton::FnCall(children) = &mut self.state_skeleton {
            children.push(Box::new(skeleton))
        } else {
            panic!("State skeleton for function must be FnCall type");
        }
    }
    pub fn is_stateful(&self) -> bool {
        if let StateTreeSkeleton::FnCall(children) = &self.state_skeleton {
            !children.is_empty()
        } else {
            panic!("State skeleton for function must be FnCall type");
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Mir {
    pub functions: Vec<Function>,
    pub file_path: Option<PathBuf>,
}

impl Mir {
    pub fn new(file_path: Option<PathBuf>) -> Self {
        Self {
            file_path,
            ..Default::default()
        }
    }
    pub fn get_dsp_iochannels(&self) -> Option<IoChannelInfo> {
        self.functions
            .iter()
            .find(|f| f.label.as_str() == "dsp")
            .and_then(|f| {
                // log::info!("input_type:{:?} output_type:{:?}", f.get_argtypes().as_slice(), f.return_type.get().as_ref());
                let input = match f.get_argtypes().as_slice() {
                    [] => Some(0),
                    [t] => t.to_type().get_iochannel_count(),
                    _ => None,
                };
                let output = f
                    .return_type
                    .get()
                    .and_then(|t| t.to_type().get_iochannel_count());
                input.and_then(|input| output.map(|output| IoChannelInfo { input, output }))
            })
    }

    /// Get the StateTreeSkeleton for a specific function by name
    pub fn get_function_state_skeleton(
        &self,
        function_name: &str,
    ) -> Option<&StateTreeSkeleton<StateType>> {
        self.functions.iter().find_map(|f| {
            if f.label.as_str() == function_name {
                Some(&f.state_skeleton)
            } else {
                None
            }
        })
    }

    /// Get the StateTreeSkeleton for the dsp function (commonly used for audio processing)
    pub fn get_dsp_state_skeleton(&self) -> Option<&StateTreeSkeleton<StateType>> {
        self.get_function_state_skeleton("dsp")
    }
}
