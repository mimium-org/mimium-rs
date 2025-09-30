use super::{ConstPos, Instruction, RawVal};
use crate::compiler::IoChannelInfo;
use crate::interner::{Symbol, ToSymbol, TypeNodeId};
use crate::mir;
pub use mir::OpenUpValue;
use state_tree::tree::StateTreeSkeleton;

/// Function prototype definition in the bytecode program.

#[derive(Debug, Clone, PartialEq)]
pub struct FuncProto {
    pub nparam: usize,
    pub nret: usize,
    pub upindexes: Vec<OpenUpValue>,
    pub bytecodes: Vec<Instruction>,
    pub constants: Vec<RawVal>,
    pub state_size: u64,
    pub delay_sizes: Vec<u64>,
    /// StateTree skeleton information inherited from MIR for this function's state layout
    pub state_skeleton: StateTreeSkeleton<mir::StateType>,
}

impl Default for FuncProto {
    fn default() -> Self {
        Self {
            nparam: 0,
            nret: 0,
            upindexes: Vec::new(),
            bytecodes: Vec::new(),
            constants: Vec::new(),
            state_size: 0,
            delay_sizes: Vec::new(),
            state_skeleton: StateTreeSkeleton::FnCall(vec![]), // Initialize as empty FnCall
        }
    }
}
impl FuncProto {
    pub fn new(nparam: usize, nret: usize) -> Self {
        Self {
            nparam,
            nret,
            state_skeleton: StateTreeSkeleton::FnCall(vec![]), // Initialize as empty FnCall
            ..Default::default()
        }
    }
    /// Adds new constant to the program. Returns index in the array.
    pub fn add_new_constant(&mut self, cval: RawVal) -> ConstPos {
        self.constants.binary_search(&cval).unwrap_or_else(|_err| {
            self.constants.push(cval);
            self.constants.len() - 1
        }) as _
    }
}

/// Complete bytecode programs.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Program {
    pub global_fn_table: Vec<(Symbol, FuncProto)>,
    pub ext_fun_table: Vec<(Symbol, TypeNodeId)>,
    pub global_vals: Vec<RawVal>,
    pub strings: Vec<Symbol>,
    pub file_path: Option<Symbol>,
    pub iochannels: Option<IoChannelInfo>,
    //hold absolute index of dsp function because the symbol interner can't be used in the audio thread
    pub dsp_index: Option<usize>,
}
impl Program {
    pub fn get_fun_index(&self, name: &Symbol) -> Option<usize> {
        self.global_fn_table
            .iter()
            .position(|(label, _f)| label == name)
    }
    pub fn get_dsp_fn(&self) -> Option<&FuncProto> {
        self.get_fun_index(&"dsp".to_symbol())
            .and_then(|idx| self.global_fn_table.get(idx).map(|(_, f)| f))
    }

    /// Get the StateTreeSkeleton for the dsp function (commonly used for audio processing)
    pub fn get_dsp_state_skeleton(&self) -> Option<&StateTreeSkeleton<mir::StateType>> {
        let dsp_i = self.dsp_index?;
        self.global_fn_table
            .get(dsp_i)
            .map(|(_, f)| &f.state_skeleton)
    }
    pub fn add_new_str(&mut self, s: Symbol) -> usize {
        self.strings
            .iter()
            .position(|&c| s == c)
            .unwrap_or_else(|| {
                self.strings.push(s);
                self.strings.len() - 1
            })
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fns in self.global_fn_table.iter() {
            let _ = writeln!(f, "{}", fns.0);
            let _ = writeln!(f, "nparams:{} nret: {}", fns.1.nparam, fns.1.nret);
            let _ = write!(f, "upindexes: {:?}  ", fns.1.upindexes);
            let _ = writeln!(f, "state_size: {}  ", fns.1.state_size);
            let _ = writeln!(f, "state_skeleton: {:?}", fns.1.state_skeleton);
            let _ = writeln!(f, "constants:  {:?}", fns.1.constants);
            let _ = writeln!(f, "instructions:");
            for inst in fns.1.bytecodes.iter() {
                let _ = writeln!(f, "  {}", inst);
            }
        }
        let _ = write!(
            f,
            "ext_fun:\n{:?}\n",
            self.ext_fun_table
                .iter()
                .fold("".to_string(), |s, (f, _)| if s.is_empty() {
                    format!("{f}")
                } else {
                    format!("{s}, {f}")
                })
        );
        let _ = write!(f, "globals:\n{:?}", self.global_vals);
        writeln!(
            f,
            "strings:  {:?}",
            self.strings
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        )
    }
}
