use std::path::PathBuf;

use super::{ConstPos, Instruction, RawVal};
use crate::compiler::IoChannelInfo;
use crate::interner::TypeNodeId;
use crate::mir;
pub use mir::OpenUpValue;
use state_tree::tree::StateTreeSkeleton;

/// Function prototype definition in the bytecode program.

/// Jump table for switch/match expressions.
/// Uses a dense array for O(1) lookup when case values are within range.
/// The last element of `offsets` is the default offset for out-of-range values.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct JumpTable {
    /// Minimum case value (used to calculate array index: index = value - min)
    pub min: i64,
    /// Dense array of offsets. Index = (value - min).
    /// The last element is the default offset for values outside [min, min + len - 2].
    pub offsets: Vec<i16>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncProto {
    pub nparam: usize,
    pub nret: usize,
    pub upindexes: Vec<OpenUpValue>,
    pub bytecodes: Vec<Instruction>,
    pub constants: Vec<RawVal>,
    pub delay_sizes: Vec<u64>,
    /// Jump tables for switch/match expressions.
    pub jump_tables: Vec<JumpTable>,
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
            delay_sizes: Vec::new(),
            jump_tables: Vec::new(),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WordSize(pub u64);
/// Complete bytecode programs.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Program {
    pub global_fn_table: Vec<(String, FuncProto)>,
    pub ext_fun_table: Vec<(String, TypeNodeId)>,
    pub global_vals: Vec<WordSize>,
    pub strings: Vec<String>,
    pub file_path: Option<PathBuf>,
    pub iochannels: Option<IoChannelInfo>,
    //hold absolute index of dsp function because the symbol interner can't be used in the audio thread
    pub dsp_index: Option<usize>,
    /// Type table for CloneUserSum instruction.
    /// Maps u8 index to TypeNodeId to keep instruction size within 64 bits.
    pub type_table: Vec<TypeNodeId>,
}
impl Program {
    pub fn get_fun_index(&self, name: &str) -> Option<usize> {
        self.global_fn_table
            .iter()
            .position(|(label, _f)| label.as_str() == name)
    }
    pub fn get_dsp_fn(&self) -> Option<&FuncProto> {
        self.get_fun_index("dsp")
            .and_then(|idx| self.global_fn_table.get(idx).map(|(_, f)| f))
    }

    /// Get the StateTreeSkeleton for the dsp function (commonly used for audio processing)
    pub fn get_dsp_state_skeleton(&self) -> Option<&StateTreeSkeleton<mir::StateType>> {
        let dsp_i = self.dsp_index?;
        self.global_fn_table
            .get(dsp_i)
            .map(|(_, f)| &f.state_skeleton)
    }

    /// Add a type to the type table and return its index.
    /// If the type already exists, returns the existing index.
    /// Returns None if the type table is full (max 256 entries).
    pub fn add_type_to_table(&mut self, ty: TypeNodeId) -> Option<u8> {
        // Check if type already exists
        if let Some(idx) = self.type_table.iter().position(|&t| t == ty) {
            return Some(idx as u8);
        }
        // Add new type if table is not full
        if self.type_table.len() < 256 {
            self.type_table.push(ty);
            Some((self.type_table.len() - 1) as u8)
        } else {
            None // Type table overflow
        }
    }

    /// Get TypeNodeId from type table by index.
    pub fn get_type_from_table(&self, idx: u8) -> Option<TypeNodeId> {
        self.type_table.get(idx as usize).copied()
    }
    pub fn add_new_str(&mut self, s: String) -> usize {
        self.strings
            .iter()
            .position(|c| s == *c)
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
            let _ = writeln!(f, "state_skeleton: {:?}", fns.1.state_skeleton);
            let _ = writeln!(f, "constants:  {:?}", fns.1.constants);
            let _ = writeln!(f, "jump_tables: {:?}", fns.1.jump_tables);
            let _ = writeln!(f, "instructions:");
            for inst in fns.1.bytecodes.iter() {
                let _ = writeln!(f, "  {inst}");
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
