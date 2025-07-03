// Mid-level intermediate representation that is more like imperative form than hir.
use crate::{
    compiler::IoChannelInfo,
    interner::{Symbol, TypeNodeId},
    types::TypeSize,
};
use std::{cell::OnceCell, sync::Arc};

pub mod print;

// #[derive(Debug, Clone, PartialEq)]
// pub struct Global(VPtr);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Argument(pub Symbol, pub TypeNodeId);

pub type VReg = u64;
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Value {
    Global(VPtr),
    Argument(usize, Arc<Argument>), //index,
    /// holds SSA index(position in infinite registers)
    Register(VReg),
    State(VPtr),
    // idx of the function in the program
    Function(usize),
    /// native function (Rust function item or closure)
    ExtFunction(Symbol, TypeNodeId),
    /// internal state
    None,
}

pub type VPtr = Arc<Value>;

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
    //label to funcproto  and localvar offset?
    GetUpValue(u64, TypeNodeId),
    SetUpValue(u64, VPtr, TypeNodeId),
    //internal state: feed and delay
    PushStateOffset(Vec<StateSize>),
    PopStateOffset(Vec<StateSize>),
    //load internal state to register(destination)
    GetState(TypeNodeId),

    //condition,  basic block index for then statement, else statement, and merge block
    JmpIf(VPtr, u64, u64, u64),
    // basic block index (for return statement)
    Jmp(i16),
    //merge
    Phi(VPtr, VPtr),

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
pub struct Function {
    pub index: usize,
    pub label: Symbol,
    pub args: Vec<Arc<Value>>,
    pub argtypes: Vec<TypeNodeId>,
    pub return_type: OnceCell<TypeNodeId>, // TODO: None is the state when the type is not inferred yet.
    pub upindexes: Vec<Arc<Value>>,
    pub upperfn_i: Option<usize>,
    pub body: Vec<Block>,
    pub state_sizes: Vec<StateSize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StateSize {
    pub size: u64,
    pub ty: TypeNodeId,
}

impl Function {
    pub fn new(
        index: usize,
        name: Symbol,
        args: &[VPtr],
        argtypes: &[TypeNodeId],
        upperfn_i: Option<usize>,
    ) -> Self {
        Self {
            index,
            label: name,
            args: args.to_vec(),
            argtypes: argtypes.to_vec(),
            return_type: OnceCell::new(),
            upindexes: vec![],
            upperfn_i,
            body: vec![Block::default()],
            state_sizes: vec![],
        }
    }
    pub fn add_new_basicblock(&mut self) -> usize {
        self.body.push(Block(vec![]));
        self.body.len() - 1
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
}

#[derive(Debug, Clone, Default)]
pub struct Mir {
    pub functions: Vec<Function>,
    pub file_path: Option<Symbol>,
}

impl Mir {
    pub fn new(file_path: Option<Symbol>) -> Self {
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
                let input = match f.argtypes.as_slice() {
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
}
