// WASM backend code generator
//
// This module generates WebAssembly modules from MIR (Mid-level Intermediate Representation).
// It uses the wasm-encoder crate to emit WASM bytecode and relies on RuntimePrimitives
// for heap, state, and array operations through imported host functions.
//
// Architecture:
// - MIR operations -> WASM instructions
// - Runtime operations (heap, state, arrays) -> imported host functions
// - Closures and higher-order functions -> WASM function references and tables
// - Memory layout: linear memory for stack and runtime primitive data

use crate::interner::Symbol;
use crate::mir::{self, Mir, VPtr};
use crate::runtime::primitives::WordSize;
use crate::types::{PType, Type};
use std::collections::HashMap;
use std::sync::Arc;
use wasm_encoder::{
    CodeSection, DataSection, ElementSection, EntityType, ExportSection, Function, FunctionSection,
    ImportSection, MemArg, MemorySection, MemoryType, Module, TableSection, TableType, TypeSection,
    ValType,
};

/// Runtime primitive function indices for calling imports
#[derive(Debug, Clone, Default)]
struct RuntimeFunctionIndices {
    heap_alloc: u32,
    heap_retain: u32,
    heap_release: u32,
    heap_load: u32,
    heap_store: u32,
    box_alloc: u32,
    box_load: u32,
    box_clone: u32,
    box_release: u32,
    box_store: u32,
    usersum_clone: u32,
    usersum_release: u32,
    closure_make: u32,
    closure_close: u32,
    closure_call: u32,
    state_push: u32,
    state_pop: u32,
    state_get: u32,
    state_set: u32,
    state_delay: u32,
    state_mem: u32,
    array_alloc: u32,
    array_get_elem: u32,
    array_set_elem: u32,
    runtime_get_now: u32,
    runtime_get_samplerate: u32,
    // Math function imports (from "math" module)
    math_sin: u32,
    math_cos: u32,
    math_pow: u32,
    math_log: u32,
}

/// WASM code generator state
pub struct WasmGenerator {
    /// Type table (WASM function types)
    type_section: TypeSection,
    /// Import section (runtime primitives and external functions)
    import_section: ImportSection,
    /// Function section (function type indices)
    function_section: FunctionSection,
    /// Memory section (linear memory)
    memory_section: MemorySection,
    /// Table section (function table for indirect calls)
    table_section: TableSection,
    /// Code section (function bodies)
    code_section: CodeSection,
    /// Export section (exported functions)
    export_section: ExportSection,
    /// Data section (static data and constants)
    data_section: DataSection,
    /// Element section (function table for indirect calls)
    element_section: ElementSection,

    /// MIR program
    mir: Arc<Mir>,
    /// Current function index (accounts for imported functions)
    current_fn_idx: u32,
    /// Number of imported runtime functions (used as offset for MIR function indices)
    num_imports: u32,
    /// Local variable register mapping
    registers: HashMap<Arc<mir::Value>, u32>,
    /// Function name to WASM function index mapping
    fn_name_to_idx: HashMap<Symbol, u32>,
    /// Register type tracking: maps VReg to its WASM type (I64 or F64)
    /// Used to correctly map registers to typed local variables
    register_types: HashMap<mir::VReg, wasm_encoder::ValType>,
    /// Register constant value tracking: maps VReg to constant value if it holds one
    /// Used for function indices stored in registers
    register_constants: HashMap<mir::VReg, u64>,
    /// Number of arguments for the current function being generated.
    /// WASM function parameters occupy the first local variable slots,
    /// so register-to-local mappings must be offset by this count.
    current_num_args: u32,
    /// Number of i64 local slots for the current function.
    /// Used as offset base for f64 locals: f64 reg `r` maps to local `num_args + num_i64_locals + r`.
    current_num_i64_locals: u32,
    /// Linear memory offset counter for Alloc instructions.
    /// Each Alloc reserves a unique range in linear memory for local variables.
    alloc_offset: u32,

    /// Base address for state exchange temporary memory region.
    /// Used by GetState/ReturnFeed to transfer data to/from the runtime state storage.
    state_temp_base: u32,

    /// Global variable memory offsets: maps global VPtr to linear memory address.
    global_offsets: HashMap<VPtr, u32>,
    /// Next available offset for global variable allocation.
    next_global_offset: u32,

    /// Runtime primitive function indices
    rt: RuntimeFunctionIndices,
}

impl WasmGenerator {
    /// Create a new WASM generator for the given MIR
    pub fn new(mir: Arc<Mir>) -> Self {
        let mut generator = Self {
            type_section: TypeSection::new(),
            import_section: ImportSection::new(),
            function_section: FunctionSection::new(),
            memory_section: MemorySection::new(),
            table_section: TableSection::new(),
            code_section: CodeSection::new(),
            export_section: ExportSection::new(),
            data_section: DataSection::new(),
            element_section: ElementSection::new(),
            mir,
            current_fn_idx: 0,
            num_imports: 0,
            registers: HashMap::new(),
            fn_name_to_idx: HashMap::new(),
            register_types: HashMap::new(),
            register_constants: HashMap::new(),
            current_num_args: 0,
            current_num_i64_locals: 0,
            alloc_offset: 1024,
            state_temp_base: 512, // Reserve 512..1024 for state exchange
            global_offsets: HashMap::new(),
            next_global_offset: 256, // Reserve 256..512 for globals
            // Runtime primitive indices will be set by setup_runtime_imports
            rt: RuntimeFunctionIndices::default(),
        };

        // Setup runtime primitive imports and memory/table
        generator.setup_runtime_imports();
        generator.setup_math_imports();
        generator.setup_memory_and_table();

        // Save number of imported functions for use in function index calculations
        generator.num_imports = generator.current_fn_idx;

        generator
    }

    /// Setup import section for RuntimePrimitives host functions
    fn setup_runtime_imports(&mut self) {
        // Each runtime primitive becomes an imported function from "runtime" module
        // All use i32 for pointers, Word (i64), WordSize (i32), and references

        let mut type_idx = 0u32;

        // Type 0: (i32) -> i64  for heap_alloc
        self.type_section
            .ty()
            .function(vec![ValType::I32], vec![ValType::I64]);
        self.rt.heap_alloc = self.add_import("heap_alloc", type_idx);
        type_idx += 1;

        // Type 1: (i64) -> ()  for heap_retain, heap_release, box_clone, box_release, closure_close
        self.type_section.ty().function(vec![ValType::I64], vec![]);
        self.rt.heap_retain = self.add_import("heap_retain", type_idx);
        self.rt.heap_release = self.add_import("heap_release", type_idx);
        self.rt.box_clone = self.add_import("box_clone", type_idx);
        self.rt.box_release = self.add_import("box_release", type_idx);
        self.rt.closure_close = self.add_import("closure_close", type_idx);
        type_idx += 1;

        // Type 2: (i32, i64, i32) -> ()  for heap_load, box_load
        self.type_section
            .ty()
            .function(vec![ValType::I32, ValType::I64, ValType::I32], vec![]);
        self.rt.heap_load = self.add_import("heap_load", type_idx);
        self.rt.box_load = self.add_import("box_load", type_idx);
        type_idx += 1;

        // Type 3: (i64, i32, i32) -> ()  for heap_store, box_store
        self.type_section
            .ty()
            .function(vec![ValType::I64, ValType::I32, ValType::I32], vec![]);
        self.rt.heap_store = self.add_import("heap_store", type_idx);
        self.rt.box_store = self.add_import("box_store", type_idx);
        type_idx += 1;

        // Type 4: (i32, i32) -> i64  for box_alloc
        self.type_section
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![ValType::I64]);
        self.rt.box_alloc = self.add_import("box_alloc", type_idx);
        type_idx += 1;

        // Type 5: (i32, i32, i32) -> ()  for usersum_clone, usersum_release
        self.type_section
            .ty()
            .function(vec![ValType::I32, ValType::I32, ValType::I32], vec![]);
        self.rt.usersum_clone = self.add_import("usersum_clone", type_idx);
        self.rt.usersum_release = self.add_import("usersum_release", type_idx);
        type_idx += 1;

        // Type 6: (i64, i32, i32) -> i64  for closure_make
        self.type_section.ty().function(
            vec![ValType::I64, ValType::I32, ValType::I32],
            vec![ValType::I64],
        );
        self.rt.closure_make = self.add_import("closure_make", type_idx);
        type_idx += 1;

        // Type 7: (i64, i32, i32, i32, i32) -> ()  for closure_call
        self.type_section.ty().function(
            vec![
                ValType::I64,
                ValType::I32,
                ValType::I32,
                ValType::I32,
                ValType::I32,
            ],
            vec![],
        );
        self.rt.closure_call = self.add_import("closure_call", type_idx);
        type_idx += 1;

        // Type 8: (i64) -> ()  for state_push, state_pop
        self.type_section.ty().function(vec![ValType::I64], vec![]);
        self.rt.state_push = self.add_import("state_push", type_idx);
        self.rt.state_pop = self.add_import("state_pop", type_idx);
        type_idx += 1;

        // Type 9: (i32, i32) -> ()  for state_get, state_set
        self.type_section
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![]);
        self.rt.state_get = self.add_import("state_get", type_idx);
        self.rt.state_set = self.add_import("state_set", type_idx);
        type_idx += 1;

        // Type 10: (i32, i32, i32, i64) -> ()  for state_delay
        self.type_section.ty().function(
            vec![ValType::I32, ValType::I32, ValType::I32, ValType::I64],
            vec![],
        );
        self.rt.state_delay = self.add_import("state_delay", type_idx);
        type_idx += 1;

        // Type 11: (i32, i32) -> ()  for state_mem
        self.type_section
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![]);
        self.rt.state_mem = self.add_import("state_mem", type_idx);
        type_idx += 1;

        // Type 12: (i64, i32) -> i64  for array_alloc
        self.type_section
            .ty()
            .function(vec![ValType::I64, ValType::I32], vec![ValType::I64]);
        self.rt.array_alloc = self.add_import("array_alloc", type_idx);
        type_idx += 1;

        // Type 13: (i32, i64, i64, i32) -> ()  for array_get_elem
        self.type_section.ty().function(
            vec![ValType::I32, ValType::I64, ValType::I64, ValType::I32],
            vec![],
        );
        self.rt.array_get_elem = self.add_import("array_get_elem", type_idx);
        type_idx += 1;

        // Type 14: (i64, i64, i32, i32) -> ()  for array_set_elem
        self.type_section.ty().function(
            vec![ValType::I64, ValType::I64, ValType::I32, ValType::I32],
            vec![],
        );
        self.rt.array_set_elem = self.add_import("array_set_elem", type_idx);
        type_idx += 1;

        // Type 15: () -> f64  for runtime_get_now, runtime_get_samplerate
        // These return f64 directly since mimium's `number` type is f64
        self.type_section.ty().function(vec![], vec![ValType::F64]);
        self.rt.runtime_get_now = self.add_import("runtime_get_now", type_idx);
        self.rt.runtime_get_samplerate = self.add_import("runtime_get_samplerate", type_idx);
    }

    /// Helper to add an import and increment function index, returns the function index
    fn add_import(&mut self, name: &str, type_idx: u32) -> u32 {
        self.add_import_from("runtime", name, type_idx)
    }

    /// Helper to add an import from a specific module
    fn add_import_from(&mut self, module: &str, name: &str, type_idx: u32) -> u32 {
        self.import_section
            .import(module, name, EntityType::Function(type_idx));
        let fn_idx = self.current_fn_idx;
        self.current_fn_idx += 1;
        fn_idx
    }

    /// Setup math function imports (sin, cos, pow, log)
    fn setup_math_imports(&mut self) {
        // Type: (f64) -> f64 for sin, cos, log
        let type_idx_f64_f64 = self.type_section.len();
        self.type_section
            .ty()
            .function(vec![ValType::F64], vec![ValType::F64]);
        self.rt.math_sin = self.add_import_from("math", "sin", type_idx_f64_f64);
        self.rt.math_cos = self.add_import_from("math", "cos", type_idx_f64_f64);
        self.rt.math_log = self.add_import_from("math", "log", type_idx_f64_f64);

        // Type: (f64, f64) -> f64 for pow
        let type_idx_f64_f64_f64 = self.type_section.len();
        self.type_section
            .ty()
            .function(vec![ValType::F64, ValType::F64], vec![ValType::F64]);
        self.rt.math_pow = self.add_import_from("math", "pow", type_idx_f64_f64_f64);
    }

    /// Setup memory and table sections
    fn setup_memory_and_table(&mut self) {
        // Linear memory: 1 initial page (64KB), max 1024 pages (64MB)
        // This is used for stack, runtime primitives data, and constants
        self.memory_section.memory(MemoryType {
            minimum: 1,
            maximum: Some(1024),
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        // Function table for indirect calls (closures)
        // Start with 128 slots, max 4096
        self.table_section.table(TableType {
            element_type: wasm_encoder::RefType::FUNCREF,
            minimum: 128,
            maximum: Some(4096),
            table64: false,
            shared: false,
        });
    }

    /// Generate a WASM module from MIR
    pub fn generate(&mut self) -> Result<Vec<u8>, String> {
        // Phase 1: Process MIR functions and generate WASM function declarations
        self.process_mir_functions()?;

        // Phase 2: Generate function bodies
        self.generate_function_bodies()?;

        // Phase 3: Export functions
        self.export_functions()?;

        // Phase 4: Build and encode the module
        let module = self.build_module();
        Ok(module.finish())
    }

    /// Process MIR functions and setup WASM function declarations
    fn process_mir_functions(&mut self) -> Result<(), String> {
        // Clone the functions vector to avoid borrow checker issues
        let functions = self.mir.functions.clone();

        // Iterate over MIR functions
        for func in &functions {
            // Get argument types
            let param_types: Vec<ValType> = func
                .args
                .iter()
                .map(|arg| Self::type_to_valtype(&arg.1.to_type()))
                .collect();

            // Get return type
            let return_types: Vec<ValType> = func
                .return_type
                .get()
                .and_then(|ty| {
                    let type_ref = ty.to_type();
                    // Unit type should have no return value in WASM
                    if matches!(type_ref, Type::Primitive(PType::Unit)) {
                        None
                    } else {
                        Some(vec![Self::type_to_valtype(&type_ref)])
                    }
                })
                .unwrap_or_default();

            // Add function type to type section
            let type_idx = self.type_section.len();
            self.type_section.ty().function(param_types, return_types);

            // Add function to function section (links to type index)
            self.function_section.function(type_idx);

            // Track function name to index mapping
            // Note: current_fn_idx already accounts for imported functions
            self.fn_name_to_idx.insert(func.label, self.current_fn_idx);
            self.current_fn_idx += 1;
        }

        Ok(())
    }

    /// Generate WASM function bodies from MIR
    fn generate_function_bodies(&mut self) -> Result<(), String> {
        use wasm_encoder::Instruction as W;

        // Clone the functions vector to avoid borrow checker issues
        let functions = self.mir.functions.clone();

        // For each MIR function, generate WASM function body
        for func in &functions {
            // Reset register mapping and type tracking for each function
            self.registers.clear();
            self.register_types.clear();
            self.register_constants.clear();
            self.current_num_args = func.args.len() as u32;

            // Scan function body to determine register types and constants
            self.analyze_register_types(func);

            // Compute required local counts from actual register usage
            let (max_i64_reg, max_f64_reg) = self.compute_max_register_indices();
            let num_i64_locals = max_i64_reg + 1;
            let num_f64_locals = max_f64_reg + 1;
            self.current_num_i64_locals = num_i64_locals;

            // WASM local layout: [args(0..n)] [i64 regs(n..n+num_i64)] [f64 regs(n+num_i64..)]
            let mut locals = Vec::new();
            if num_i64_locals > 0 {
                locals.push((num_i64_locals, ValType::I64));
            }
            if num_f64_locals > 0 {
                locals.push((num_f64_locals, ValType::F64));
            }

            // Create a new WASM function
            let mut wasm_func = Function::new(locals);

            // Translate basic blocks with structured control flow awareness
            self.emit_function_body(func, &mut wasm_func);

            // Every WASM function body must end with an End instruction
            // (functions are implicit blocks in WASM)
            wasm_func.instruction(&W::End);

            // Add function to code section
            self.code_section.function(&wasm_func);
        }

        Ok(())
    }

    /// Emit a function body handling structured control flow (if/else from JmpIf/Phi)
    fn emit_function_body(&mut self, func: &mir::Function, wasm_func: &mut Function) {
        use mir::Instruction as I;
        use wasm_encoder::Instruction as W;

        let blocks = &func.body;
        let mut processed = vec![false; blocks.len()];

        for block_idx in 0..blocks.len() {
            if processed[block_idx] {
                continue;
            }
            processed[block_idx] = true;

            let block = &blocks[block_idx];
            for (dest, instr) in &block.0 {
                match instr {
                    I::JmpIf(cond, then_bb, else_bb, merge_bb) => {
                        let then_idx = *then_bb as usize;
                        let else_idx = *else_bb as usize;
                        let merge_idx = *merge_bb as usize;

                        // Find Phi in merge block to determine result type and inputs
                        let phi_info = Self::find_phi_in_block(&blocks[merge_idx]);

                        // Load condition (i64) and convert to i32 for WASM if
                        self.emit_value_load(cond, wasm_func);
                        wasm_func.instruction(&W::I32WrapI64);

                        // Determine if block type from Phi presence
                        let block_type = if let Some((_, ref phi_dest)) = phi_info {
                            let reg_type = if let mir::Value::Register(reg_idx) = phi_dest.as_ref()
                            {
                                self.register_types
                                    .get(reg_idx)
                                    .copied()
                                    .unwrap_or(ValType::F64)
                            } else {
                                ValType::F64
                            };
                            wasm_encoder::BlockType::Result(reg_type)
                        } else {
                            wasm_encoder::BlockType::Empty
                        };
                        wasm_func.instruction(&W::If(block_type));

                        // Emit then-branch block
                        processed[then_idx] = true;
                        self.emit_branch_block(
                            &blocks[then_idx],
                            phi_info.as_ref().map(|(inputs, _)| &inputs.0),
                            wasm_func,
                        );

                        wasm_func.instruction(&W::Else);

                        // Emit else-branch block
                        processed[else_idx] = true;
                        self.emit_branch_block(
                            &blocks[else_idx],
                            phi_info.as_ref().map(|(inputs, _)| &inputs.1),
                            wasm_func,
                        );

                        wasm_func.instruction(&W::End);

                        // Emit merge block (skip Phi, which is on the stack from if/else)
                        processed[merge_idx] = true;
                        self.emit_merge_block(&blocks[merge_idx], phi_info.is_some(), wasm_func);
                    }
                    _ => {
                        self.translate_instruction_with_dest(dest.as_ref(), instr, wasm_func);
                    }
                }
            }
        }
    }

    /// Find the Phi instruction in a merge block.
    /// Returns `Some(((then_input, else_input), dest_vptr))` if found.
    fn find_phi_in_block(block: &mir::Block) -> Option<((VPtr, VPtr), VPtr)> {
        for (dest, instr) in &block.0 {
            if let mir::Instruction::Phi(v1, v2) = instr {
                return Some(((v1.clone(), v2.clone()), dest.clone()));
            }
        }
        None
    }

    /// Emit a branch block's instructions, then load the Phi input onto the stack
    fn emit_branch_block(
        &mut self,
        block: &mir::Block,
        phi_input: Option<&VPtr>,
        func: &mut Function,
    ) {
        for (dest, instr) in &block.0 {
            self.translate_instruction_with_dest(dest.as_ref(), instr, func);
        }
        // Push the phi input to stay on the stack as the if/else result
        if let Some(input) = phi_input {
            self.emit_value_load(input, func);
        }
    }

    /// Emit a merge block, handling the Phi at the top (already on stack from if/else)
    fn emit_merge_block(&mut self, block: &mir::Block, skip_phi: bool, func: &mut Function) {
        use wasm_encoder::Instruction as W;

        for (dest, instr) in &block.0 {
            if skip_phi && matches!(instr, mir::Instruction::Phi(_, _)) {
                // Phi value is already on the stack from if/else; store to dest
                if let mir::Value::Register(reg_idx) = dest.as_ref() {
                    let reg_type = self
                        .register_types
                        .get(reg_idx)
                        .copied()
                        .unwrap_or(ValType::F64);
                    let local_idx = match reg_type {
                        ValType::I64 => self.current_num_args + *reg_idx as u32,
                        ValType::F64 => {
                            self.current_num_args + self.current_num_i64_locals + *reg_idx as u32
                        }
                        _ => self.current_num_args + self.current_num_i64_locals + *reg_idx as u32,
                    };
                    func.instruction(&W::LocalSet(local_idx));
                }
            } else {
                self.translate_instruction_with_dest(dest.as_ref(), instr, func);
            }
        }
    }

    /// Analyze MIR function body to determine register types
    /// This scans all instructions and records each register's ValType (I64 or F64)
    fn analyze_register_types(&mut self, func: &mir::Function) {
        use mir::Instruction as I;
        use wasm_encoder::ValType;

        for block in &func.body {
            for (dest, instr) in &block.0 {
                if let mir::Value::Register(reg_idx) = dest.as_ref() {
                    // Track register type
                    let reg_type = match instr {
                        // Integer types -> I64
                        I::Uinteger(val) => {
                            // Also track the constant value for function indices
                            self.register_constants.insert(*reg_idx, *val);
                            ValType::I64
                        }
                        I::Integer(_) => ValType::I64,
                        // Float type -> F64
                        I::Float(_) => ValType::F64,
                        // Arithmetic operations - use operand types
                        I::AddF(..)
                        | I::SubF(..)
                        | I::MulF(..)
                        | I::DivF(..)
                        | I::ModF(..)
                        | I::PowF(..) => ValType::F64,
                        I::AddI(..)
                        | I::SubI(..)
                        | I::MulI(..)
                        | I::DivI(..)
                        | I::ModI(..)
                        | I::PowI(..) => ValType::I64,
                        // Comparison operations -> I32 in WASM, but we'll treat as I64
                        I::Eq(..) | I::Ne(..) | I::Le(..) | I::Lt(..) | I::Ge(..) | I::Gt(..) => {
                            ValType::I64
                        }
                        // Logical operations -> I64
                        I::And(..) | I::Or(..) | I::Not(..) => ValType::I64,
                        // Load - depends on type annotation
                        I::Load(_, ty) => {
                            // Use type_to_valtype for consistent handling
                            // (e.g., single-field records unwrap to their field type)
                            Self::type_to_valtype(&ty.to_type())
                        }
                        // Function calls and memory operations
                        I::Call(_, _, ret_ty)
                        | I::CallIndirect(_, _, ret_ty)
                        | I::CallCls(_, _, ret_ty) => {
                            // Use type_to_valtype for consistent handling
                            Self::type_to_valtype(&ret_ty.to_type())
                        }
                        // Memory allocation returns pointers (i64)
                        I::Alloc(_) => ValType::I64,
                        // GetElement returns pointer (i64)
                        I::GetElement { .. } => ValType::I64,
                        // Phi inherits type from its inputs
                        I::Phi(v1, _) => {
                            if let mir::Value::Register(r) = v1.as_ref() {
                                self.register_types.get(r).copied().unwrap_or(ValType::F64)
                            } else {
                                ValType::F64
                            }
                        }
                        // Type casts
                        I::CastFtoI(_) | I::CastItoB(_) => ValType::I64,
                        I::CastItoF(_) => ValType::F64,
                        // Unary float ops
                        I::NegF(..)
                        | I::AbsF(..)
                        | I::SqrtF(..)
                        | I::SinF(..)
                        | I::CosF(..)
                        | I::LogF(..) => ValType::F64,
                        I::NegI(..) | I::AbsI(..) => ValType::I64,
                        // State operations: GetState returns an address (pointer)
                        I::GetState(_) => ValType::I64,
                        // GetGlobal loads the actual value
                        I::GetGlobal(_, ty) => Self::type_to_valtype(&ty.to_type()),
                        // GetUpValue loads the actual value
                        I::GetUpValue(_, ty) => Self::type_to_valtype(&ty.to_type()),
                        // ReturnFeed is handled separately (is_return)
                        I::ReturnFeed(_, _) => ValType::F64,
                        // Default to F64 for unknown instructions
                        _ => ValType::F64,
                    };
                    self.register_types.insert(*reg_idx, reg_type);
                }
            }
        }
    }

    /// Compute the maximum register index for i64 and f64 types.
    /// Returns (max_i64_index + 1, max_f64_index + 1), i.e. the count of locals needed.
    fn compute_max_register_indices(&self) -> (u32, u32) {
        let mut max_i64: u32 = 0;
        let mut max_f64: u32 = 0;
        for (&reg_idx, &val_type) in &self.register_types {
            let idx = reg_idx as u32 + 1; // +1 to convert index to count
            match val_type {
                ValType::I64 => max_i64 = max_i64.max(idx),
                ValType::F64 => max_f64 = max_f64.max(idx),
                _ => max_f64 = max_f64.max(idx),
            }
        }
        (max_i64, max_f64)
    }

    /// Translate a single MIR instruction with destination to WASM
    fn translate_instruction_with_dest(
        &mut self,
        dest: &mir::Value,
        instr: &mir::Instruction,
        func: &mut Function,
    ) {
        use mir::Instruction as I;
        use wasm_encoder::Instruction as W;

        // Return instructions (both Return and ReturnFeed) are special - they leave the value on the stack
        // for the function to return. We should not store the value to a destination register or drop it.
        let is_return = matches!(instr, I::Return(_, _) | I::ReturnFeed(_, _));

        // Generate the instruction (pushes result to stack)
        self.translate_instruction(instr, func);

        // Only handle destination for non-return instructions that produce a value
        if !is_return && Self::instruction_produces_value(instr) {
            // Store result to destination register if specified
            match dest {
                mir::Value::Register(reg_idx) => {
                    // Map register to correct local variable based on type
                    // WASM local layout: [args(0..n)] [i64 regs(n..n+32)] [f64 regs(n+32..n+64)]
                    let reg_type = self
                        .register_types
                        .get(reg_idx)
                        .copied()
                        .unwrap_or(ValType::F64);
                    let local_idx = match reg_type {
                        ValType::I64 => self.current_num_args + *reg_idx as u32,
                        ValType::F64 => {
                            self.current_num_args + self.current_num_i64_locals + *reg_idx as u32
                        }
                        _ => self.current_num_args + self.current_num_i64_locals + *reg_idx as u32,
                    };
                    func.instruction(&W::LocalSet(local_idx));
                }
                mir::Value::None => {
                    // No destination, result is discarded (pop it from stack)
                    func.instruction(&W::Drop);
                }
                _ => {
                    // Other destination types not yet handled
                    // TODO: Handle Global, State, etc.
                }
            }
        }
    }

    /// Returns true if the instruction produces a value on the WASM stack.
    /// Non-producing instructions (stores, state ops, releases) do not leave
    /// a value on the stack, so the caller should not attempt to LocalSet or Drop.
    fn instruction_produces_value(instr: &mir::Instruction) -> bool {
        use mir::Instruction as I;
        !matches!(
            instr,
            I::Store(..)
                | I::PushStateOffset(..)
                | I::PopStateOffset(..)
                | I::BoxRelease { .. }
                | I::BoxClone { .. }
                | I::BoxStore { .. }
                | I::CloseHeapClosure(..)
                | I::CloneHeap(..)
                | I::CloneUserSum { .. }
                | I::ReleaseUserSum { .. }
                | I::CloseUpValues(..)
                | I::SetUpValue(..)
                | I::SetGlobal(..)
                | I::SetArrayElem(..)
                | I::Delay(..)
                | I::Mem(..)
                | I::Return(..)
                | I::ReturnFeed(..)
                | I::JmpIf(..)
                | I::Jmp(..)
                | I::Phi(..)
                | I::PhiSwitch(..)
        )
    }

    /// Export all functions and memory
    fn export_functions(&mut self) -> Result<(), String> {
        // Clone the functions vector to avoid borrow checker issues
        let functions = self.mir.functions.clone();

        // Track the dsp function index if it exists
        let mut dsp_idx = None;

        // Export all public functions
        for func in &functions {
            let fn_idx = *self
                .fn_name_to_idx
                .get(&func.label)
                .ok_or_else(|| format!("Function {:?} not found in mapping", func.label))?;

            // Check if this is the dsp function
            if func.label.as_str() == "dsp" {
                dsp_idx = Some(fn_idx);
            }

            // Use the function name from the symbol interner
            let fn_name = format!("fn_{fn_idx}");
            self.export_section
                .export(&fn_name, wasm_encoder::ExportKind::Func, fn_idx);
        }

        // Export the dsp function with its canonical name if it exists
        if let Some(idx) = dsp_idx {
            self.export_section
                .export("dsp", wasm_encoder::ExportKind::Func, idx);
        }

        // Export memory so the host can access it
        self.export_section
            .export("memory", wasm_encoder::ExportKind::Memory, 0);

        Ok(())
    }

    /// Translate a single MIR instruction to WASM instructions
    fn translate_instruction(&mut self, instr: &mir::Instruction, func: &mut Function) {
        use mir::Instruction as I;
        use wasm_encoder::Instruction as W;

        match instr {
            // Constants
            I::Float(f) => {
                func.instruction(&W::F64Const(*f));
            }
            I::Integer(i) => {
                func.instruction(&W::I64Const(*i));
            }
            I::Uinteger(u) => {
                func.instruction(&W::I64Const(*u as i64));
            }

            // Arithmetic operations (f64)
            I::AddF(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Add);
            }
            I::SubF(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Sub);
            }
            I::MulF(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Mul);
            }
            I::DivF(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Div);
            }
            I::NegF(a) => {
                self.emit_value_load(a, func);
                func.instruction(&W::F64Neg);
            }
            I::AbsF(a) => {
                self.emit_value_load(a, func);
                func.instruction(&W::F64Abs);
            }
            I::SqrtF(a) => {
                self.emit_value_load(a, func);
                func.instruction(&W::F64Sqrt);
            }
            I::SinF(a) => {
                self.emit_value_load(a, func);
                func.instruction(&W::Call(self.rt.math_sin));
            }
            I::CosF(a) => {
                self.emit_value_load(a, func);
                func.instruction(&W::Call(self.rt.math_cos));
            }
            I::LogF(a) => {
                self.emit_value_load(a, func);
                func.instruction(&W::Call(self.rt.math_log));
            }
            I::PowF(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::Call(self.rt.math_pow));
            }
            I::ModF(a, b) => {
                // WASM has no native f64 remainder; compute a - trunc(a/b) * b
                self.emit_value_load(a, func);
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Div);
                func.instruction(&W::F64Trunc);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Mul);
                func.instruction(&W::F64Sub);
            }

            // Integer arithmetic operations
            I::AddI(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::I64Add);
            }
            I::SubI(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::I64Sub);
            }
            I::MulI(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::I64Mul);
            }
            I::DivI(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::I64DivS);
            }
            I::ModI(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::I64RemS);
            }
            I::NegI(a) => {
                // -x = 0 - x
                func.instruction(&W::I64Const(0));
                self.emit_value_load(a, func);
                func.instruction(&W::I64Sub);
            }
            I::AbsI(a) => {
                // abs(x) = if x < 0 then -x else x
                // Use: (x ^ (x >> 63)) - (x >> 63)
                self.emit_value_load(a, func);
                self.emit_value_load(a, func);
                func.instruction(&W::I64Const(63));
                func.instruction(&W::I64ShrS);
                func.instruction(&W::I64Xor);
                self.emit_value_load(a, func);
                func.instruction(&W::I64Const(63));
                func.instruction(&W::I64ShrS);
                func.instruction(&W::I64Sub);
            }

            // Logical operations
            I::And(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::I64And);
            }
            I::Or(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::I64Or);
            }
            I::Not(a) => {
                self.emit_value_load(a, func);
                func.instruction(&W::I64Eqz);
                // I64Eqz produces i32, extend to i64
                func.instruction(&W::I64ExtendI32U);
            }

            // Type cast operations
            I::CastFtoI(a) => {
                self.emit_value_load(a, func);
                func.instruction(&W::I64TruncF64S);
            }
            I::CastItoF(a) => {
                self.emit_value_load(a, func);
                func.instruction(&W::F64ConvertI64S);
            }
            I::CastItoB(a) => {
                // i64 -> bool (i64): non-zero = 1, zero = 0
                self.emit_value_load(a, func);
                func.instruction(&W::I64Const(0));
                func.instruction(&W::I64Ne);
                func.instruction(&W::I64ExtendI32U);
            }

            // Boolean operations (results are i32 in WASM, extended to i64 for register storage)
            I::Eq(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Eq);
                func.instruction(&W::I64ExtendI32U);
            }
            I::Ne(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Ne);
                func.instruction(&W::I64ExtendI32U);
            }
            I::Lt(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Lt);
                func.instruction(&W::I64ExtendI32U);
            }
            I::Le(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Le);
                func.instruction(&W::I64ExtendI32U);
            }
            I::Gt(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Gt);
                func.instruction(&W::I64ExtendI32U);
            }
            I::Ge(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Ge);
                func.instruction(&W::I64ExtendI32U);
            }

            // Heap operations - Box operations
            I::BoxAlloc {
                value: _,
                inner_type,
            } => {
                // box_alloc(src_ptr: i32, size_words: i32) -> i64
                // TODO: Allocate temp memory for value, get pointer
                // For now, pass placeholder pointer (0) and size
                let size = inner_type.word_size() as i32;
                func.instruction(&W::I32Const(0)); // src_ptr placeholder
                func.instruction(&W::I32Const(size));
                func.instruction(&W::Call(self.rt.box_alloc));
            }

            I::BoxLoad { ptr, inner_type } => {
                // box_load(dst_ptr: i32, obj: i64, size_words: i32)
                let size = inner_type.word_size() as i32;
                func.instruction(&W::I32Const(0)); // dst_ptr placeholder
                self.emit_value_load(ptr, func);
                func.instruction(&W::I32Const(size));
                func.instruction(&W::Call(self.rt.box_load));
            }

            I::BoxClone { ptr } => {
                // box_clone(obj: i64)
                self.emit_value_load(ptr, func);
                func.instruction(&W::Call(self.rt.box_clone));
            }

            I::BoxRelease { ptr, inner_type: _ } => {
                // box_release(obj: i64)
                self.emit_value_load(ptr, func);
                func.instruction(&W::Call(self.rt.box_release));
            }

            I::BoxStore {
                ptr,
                value: _,
                inner_type,
            } => {
                // box_store(obj: i64, src_ptr: i32, size_words: i32)
                let size = inner_type.word_size() as i32;
                self.emit_value_load(ptr, func);
                func.instruction(&W::I32Const(0)); // src_ptr placeholder
                func.instruction(&W::I32Const(size));
                func.instruction(&W::Call(self.rt.box_store));
            }

            // Heap operations - Closure operations
            I::MakeClosure { fn_proto, size } => {
                // closure_make(fn_index: i64, upvalue_count: i32, state_size: i32) -> i64
                self.emit_value_load(fn_proto, func);
                func.instruction(&W::I32Const(*size as i32));
                func.instruction(&W::I32Const(0)); // state_size placeholder
                func.instruction(&W::Call(self.rt.closure_make));
            }

            I::CloseHeapClosure(ptr) => {
                // closure_close(obj: i64)
                self.emit_value_load(ptr, func);
                func.instruction(&W::Call(self.rt.closure_close));
            }

            I::CloneHeap(ptr) => {
                // heap_retain(obj: i64)
                self.emit_value_load(ptr, func);
                func.instruction(&W::Call(self.rt.heap_retain));
            }

            // Heap operations - UserSum operations
            I::CloneUserSum { value: _, ty } => {
                // usersum_clone(value_ptr: i32, size_words: i32, type_id: i32)
                let size = ty.word_size() as i32;
                let type_id = 0i32; // TODO: Get actual type_id from type table
                func.instruction(&W::I32Const(0)); // value_ptr placeholder
                func.instruction(&W::I32Const(size));
                func.instruction(&W::I32Const(type_id));
                func.instruction(&W::Call(self.rt.usersum_clone));
            }

            I::ReleaseUserSum { value: _, ty } => {
                // usersum_release(value_ptr: i32, size_words: i32, type_id: i32)
                let size = ty.word_size() as i32;
                let type_id = 0i32; // TODO: Get actual type_id from type table
                func.instruction(&W::I32Const(0)); // value_ptr placeholder
                func.instruction(&W::I32Const(size));
                func.instruction(&W::I32Const(type_id));
                func.instruction(&W::Call(self.rt.usersum_release));
            }

            // State operations
            I::PushStateOffset(offset) => {
                // state_push(offset: i64)
                func.instruction(&W::I64Const(*offset as i64));
                func.instruction(&W::Call(self.rt.state_push));
            }

            I::PopStateOffset(offset) => {
                // state_pop(offset: i64)
                func.instruction(&W::I64Const(*offset as i64));
                func.instruction(&W::Call(self.rt.state_pop));
            }

            I::GetState(ty) => {
                // Allocate a temp region in linear memory for state exchange
                let size = ty.word_size() as i32;
                let size_bytes = (size as u32).max(1) * 8;
                let temp_addr = self.state_temp_base;
                self.state_temp_base += size_bytes;

                // Call state_get(dst_ptr: i32, size_words: i32) to fill temp memory
                func.instruction(&W::I32Const(temp_addr as i32));
                func.instruction(&W::I32Const(size));
                func.instruction(&W::Call(self.rt.state_get));

                // Push the temp address as i64 so subsequent Load can read from it
                func.instruction(&W::I64Const(temp_addr as i64));
            }

            I::ReturnFeed(value, ty) => {
                // Store new state value to temp memory, then call state_set
                let size = ty.word_size() as i32;
                let size_bytes = (size as u32).max(1) * 8;
                let temp_addr = self.state_temp_base;
                self.state_temp_base += size_bytes;

                // Store the value to temp memory for state_set to read
                let memarg = MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };
                func.instruction(&W::I32Const(temp_addr as i32));
                self.emit_value_load(value, func);
                match Self::type_to_valtype(&ty.to_type()) {
                    ValType::F64 => func.instruction(&W::F64Store(memarg)),
                    _ => func.instruction(&W::I64Store(memarg)),
                };

                // Call state_set(src_ptr: i32, size_words: i32) to persist state
                func.instruction(&W::I32Const(temp_addr as i32));
                func.instruction(&W::I32Const(size));
                func.instruction(&W::Call(self.rt.state_set));

                // Push the return value onto the stack (ReturnFeed acts as Return)
                self.emit_value_load(value, func);
            }

            I::Delay(len, _input, _time) => {
                // state_delay(dst_ptr: i32, src_ptr: i32, time_ptr: i32, max_len: i64)
                func.instruction(&W::I32Const(0)); // dst_ptr placeholder
                func.instruction(&W::I32Const(0)); // src_ptr placeholder
                func.instruction(&W::I32Const(0)); // time_ptr placeholder
                func.instruction(&W::I64Const(*len as i64));
                func.instruction(&W::Call(self.rt.state_delay));
            }

            I::Mem(_value) => {
                // state_mem(dst_ptr: i32, src_ptr: i32)
                func.instruction(&W::I32Const(0)); // dst_ptr placeholder
                func.instruction(&W::I32Const(0)); // src_ptr placeholder
                func.instruction(&W::Call(self.rt.state_mem));
            }

            // Array operations
            I::Array(values, ty) => {
                // array_alloc(len: i64, elem_size_words: i32) -> i64
                // Then array_set_elem for each value
                let elem_ty = match ty.to_type() {
                    Type::Array(elem) => elem,
                    _ => *ty, // fallback
                };
                let elem_size = elem_ty.word_size() as i32;
                let len = values.len() as i64;

                func.instruction(&W::I64Const(len));
                func.instruction(&W::I32Const(elem_size));
                func.instruction(&W::Call(self.rt.array_alloc));

                // TODO: Store array ref and use array_set_elem to populate
                // For now, just leave the array ref on stack
            }

            I::GetArrayElem(array, index, elem_ty) => {
                // array_get_elem(dst_ptr: i32, arr: i64, index: i64, elem_size_words: i32)
                let elem_size = elem_ty.word_size() as i32;
                func.instruction(&W::I32Const(0)); // dst_ptr placeholder
                self.emit_value_load(array, func);
                self.emit_value_load(index, func);
                func.instruction(&W::I32Const(elem_size));
                func.instruction(&W::Call(self.rt.array_get_elem));
            }

            I::SetArrayElem(array, index, _value, elem_ty) => {
                // array_set_elem(arr: i64, index: i64, src_ptr: i32, elem_size_words: i32)
                let elem_size = elem_ty.word_size() as i32;
                self.emit_value_load(array, func);
                self.emit_value_load(index, func);
                func.instruction(&W::I32Const(0)); // src_ptr placeholder
                func.instruction(&W::I32Const(elem_size));
                func.instruction(&W::Call(self.rt.array_set_elem));
            }

            // Memory operations
            I::Alloc(ty) => {
                // Allocate space in linear memory and return the address as I64
                let size_bytes = (ty.word_size() as u32).max(1) * 8;
                func.instruction(&W::I64Const(self.alloc_offset as i64));
                self.alloc_offset += size_bytes;
            }

            I::Load(ptr, ty) => {
                // Load a value from a pointer
                match ptr.as_ref() {
                    mir::Value::Argument(_) => {
                        // Function arguments are direct values (not memory pointers)
                        self.emit_value_load(ptr, func);
                    }
                    _ => {
                        // Register or other: treat as a linear memory address
                        self.emit_value_load(ptr, func);
                        func.instruction(&W::I32WrapI64);
                        let memarg = MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        };
                        match Self::type_to_valtype(&ty.to_type()) {
                            ValType::F64 => {
                                func.instruction(&W::F64Load(memarg));
                            }
                            _ => {
                                func.instruction(&W::I64Load(memarg));
                            }
                        }
                    }
                }
            }

            I::Store(dst, src, ty) => {
                // Store a value to a memory address
                // Stack order for f64.store/i64.store: [i32_addr, value]
                self.emit_value_load(dst, func);
                func.instruction(&W::I32WrapI64);
                self.emit_value_load(src, func);
                let memarg = MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };
                match Self::type_to_valtype(&ty.to_type()) {
                    ValType::F64 => {
                        func.instruction(&W::F64Store(memarg));
                    }
                    _ => {
                        func.instruction(&W::I64Store(memarg));
                    }
                }
            }

            I::GetElement {
                value,
                ty: _,
                tuple_offset,
            } => {
                // Compute address of a field within a composite type
                self.emit_value_load(value, func);
                if *tuple_offset > 0 {
                    let offset_bytes = (*tuple_offset * 8) as i64;
                    func.instruction(&W::I64Const(offset_bytes));
                    func.instruction(&W::I64Add);
                }
            }

            // Function calls
            I::Call(fn_ptr, args, _ret_ty) => {
                // Direct function call
                // Load all arguments onto stack
                for (arg, _ty) in args {
                    self.emit_value_load(arg, func);
                }
                // Get function index and call
                let wasm_fn_idx = if let mir::Value::Function(fn_idx) = **fn_ptr {
                    // Direct function value
                    fn_idx as u32 + self.num_imports
                } else if let mir::Value::Register(reg_idx) = **fn_ptr {
                    // Function index stored in register (from Uinteger instruction)
                    // Check if we tracked a constant value for this register
                    if let Some(const_val) = self.register_constants.get(&reg_idx) {
                        *const_val as u32 + self.num_imports
                    } else {
                        // No constant value tracked - this is a runtime function pointer
                        // TODO: Implement call_indirect for true indirect calls
                        // For now, emit a placeholder
                        eprintln!("Warning: Indirect call through register without constant value");
                        self.current_fn_idx // placeholder
                    }
                } else {
                    // Other value types
                    // TODO: Handle other cases
                    self.current_fn_idx // placeholder
                };
                func.instruction(&W::Call(wasm_fn_idx));
            }

            I::CallIndirect(closure_ptr, args, ret_ty) => {
                // Check if this is an external function call or a closure call
                match closure_ptr.as_ref() {
                    mir::Value::ExtFunction(name, _fn_ty) => {
                        // External function call - map to runtime imports
                        // Load arguments first
                        for (arg, _ty) in args {
                            self.emit_value_load(arg, func);
                        }
                        if let Some(import_idx) = self.resolve_ext_function(name) {
                            func.instruction(&W::Call(import_idx));
                        } else {
                            // Unknown external function - push placeholder
                            eprintln!("Warning: Unknown external function: {}", name.as_str());
                            match Self::type_to_valtype(&ret_ty.to_type()) {
                                ValType::F64 => func.instruction(&W::F64Const(0.0)),
                                ValType::I64 => func.instruction(&W::I64Const(0)),
                                _ => func.instruction(&W::I32Const(0)),
                            };
                        }
                    }
                    _ => {
                        // Closure call through heap object
                        // closure_call(obj: i64, args_ptr: i32, nargs_words: i32, ret_ptr: i32, nret_words: i32)
                        let nargs_words = args
                            .iter()
                            .map(|(_, ty)| ty.word_size() as u64)
                            .sum::<u64>() as i32;
                        let nret_words = ret_ty.word_size() as i32;

                        self.emit_value_load(closure_ptr, func);
                        func.instruction(&W::I32Const(0)); // args_ptr placeholder
                        func.instruction(&W::I32Const(nargs_words));
                        func.instruction(&W::I32Const(0)); // ret_ptr placeholder
                        func.instruction(&W::I32Const(nret_words));
                        func.instruction(&W::Call(self.rt.closure_call));
                    }
                }
            }

            // Control flow
            I::Return(value, ty) => {
                // In WASM, functions implicitly return the value on the stack when they reach 'end'
                let is_unit = matches!(ty.to_type(), Type::Primitive(PType::Unit));

                if !is_unit {
                    self.emit_value_load(value, func);
                }
            }

            // String constants
            I::String(_sym) => {
                // String constant - return pointer to string in data section
                // TODO: Allocate strings in data section and return address
                func.instruction(&W::I32Const(0)); // placeholder pointer
            }

            // Global variable access
            I::GetGlobal(global_ptr, ty) => {
                // Load global value from linear memory
                let offset = self.get_or_alloc_global_offset(global_ptr);
                func.instruction(&W::I32Const(offset as i32));
                let memarg = MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };
                match Self::type_to_valtype(&ty.to_type()) {
                    ValType::F64 => func.instruction(&W::F64Load(memarg)),
                    _ => func.instruction(&W::I64Load(memarg)),
                };
            }

            I::SetGlobal(global_ptr, value, ty) => {
                // Store value to global's linear memory location
                let offset = self.get_or_alloc_global_offset(global_ptr);
                func.instruction(&W::I32Const(offset as i32));
                self.emit_value_load(value, func);
                let memarg = MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };
                match Self::type_to_valtype(&ty.to_type()) {
                    ValType::F64 => func.instruction(&W::F64Store(memarg)),
                    _ => func.instruction(&W::I64Store(memarg)),
                };
            }

            // Upvalue access (closure captured variables)
            I::GetUpValue(_idx, ty) => {
                // TODO: Implement proper upvalue access through closure heap object
                // For now, push a default value
                match Self::type_to_valtype(&ty.to_type()) {
                    ValType::F64 => func.instruction(&W::F64Const(0.0)),
                    ValType::I64 => func.instruction(&W::I64Const(0)),
                    _ => func.instruction(&W::I32Const(0)),
                };
            }

            I::SetUpValue(_idx, _value, _ty) => {
                // TODO: Implement proper upvalue store through closure heap object
                // Currently a no-op
            }

            // Closure (old-style, non-heap)
            I::Closure(_idx_cell) => {
                // TODO: Implement closure creation
                func.instruction(&W::I64Const(0)); // placeholder
            }

            I::CloseUpValues(_src, _ty) => {
                // Close upvalues - no-op in current WASM implementation
            }

            // CallCls (closure call variant)
            I::CallCls(fn_ptr, args, ret_ty) => {
                // Same handling as CallIndirect: check for ExtFunction
                match fn_ptr.as_ref() {
                    mir::Value::ExtFunction(name, _fn_ty) => {
                        for (arg, _ty) in args {
                            self.emit_value_load(arg, func);
                        }
                        if let Some(import_idx) = self.resolve_ext_function(name) {
                            func.instruction(&W::Call(import_idx));
                        } else {
                            match Self::type_to_valtype(&ret_ty.to_type()) {
                                ValType::F64 => func.instruction(&W::F64Const(0.0)),
                                ValType::I64 => func.instruction(&W::I64Const(0)),
                                _ => func.instruction(&W::I32Const(0)),
                            };
                        }
                    }
                    _ => {
                        // Closure call through register
                        let nargs_words = args
                            .iter()
                            .map(|(_, ty)| ty.word_size() as u64)
                            .sum::<u64>() as i32;
                        let nret_words = ret_ty.word_size() as i32;
                        self.emit_value_load(fn_ptr, func);
                        func.instruction(&W::I32Const(0)); // args_ptr placeholder
                        func.instruction(&W::I32Const(nargs_words));
                        func.instruction(&W::I32Const(0)); // ret_ptr placeholder
                        func.instruction(&W::I32Const(nret_words));
                        func.instruction(&W::Call(self.rt.closure_call));
                    }
                }
            }

            // Placeholder for unimplemented instructions
            _ => {
                // Placeholder for unimplemented instructions
                // In a real implementation, log or error here
            }
        }
    }

    /// Helper: Emit WASM instructions to load a value onto the stack
    fn emit_value_load(&mut self, value: &VPtr, func: &mut Function) {
        use mir::Value as V;
        use wasm_encoder::Instruction as W;
        use wasm_encoder::ValType;

        match value.as_ref() {
            V::Register(reg_idx) => {
                // Map register to correct local variable based on type
                // WASM local layout: [args(0..n)] [i64 regs(n..n+32)] [f64 regs(n+32..n+64)]
                let reg_type = self
                    .register_types
                    .get(reg_idx)
                    .copied()
                    .unwrap_or(ValType::F64);
                let local_idx = match reg_type {
                    ValType::I64 => self.current_num_args + *reg_idx as u32,
                    ValType::F64 => {
                        self.current_num_args + self.current_num_i64_locals + *reg_idx as u32
                    }
                    _ => self.current_num_args + self.current_num_i64_locals + *reg_idx as u32,
                };
                func.instruction(&W::LocalGet(local_idx));
            }
            V::Argument(arg_idx) => {
                // Arguments are in the first local slots
                func.instruction(&W::LocalGet(*arg_idx as u32));
            }
            V::Global(_global_val) => {
                // Load global variable
                // TODO: Implement global variable access
                // For now, placeholder
                func.instruction(&W::F64Const(0.0));
            }
            V::Function(func_idx) => {
                // Function index as i32
                func.instruction(&W::I32Const(*func_idx as i32));
            }
            V::ExtFunction(name, _ty) => {
                // External function reference - push its import index
                if let Some(import_idx) = self.resolve_ext_function(name) {
                    func.instruction(&W::I32Const(import_idx as i32));
                } else {
                    func.instruction(&W::I32Const(0)); // Unknown ext function
                }
            }
            V::State(_inner) => {
                // State reference - placeholder
                func.instruction(&W::I64Const(0));
            }
            V::Constructor(_name, _tag, _ty) => {
                // Constructor tag
                func.instruction(&W::I64Const(0)); // placeholder
            }
            _ => {
                // Placeholder for other value types (None, etc.)
                func.instruction(&W::F64Const(0.0));
            }
        }
    }

    /// Build the final WASM module
    fn build_module(&self) -> Module {
        let mut module = Module::new();

        // Add all sections in proper order
        module.section(&self.type_section);
        module.section(&self.import_section);
        module.section(&self.function_section);
        module.section(&self.table_section);
        module.section(&self.memory_section);
        module.section(&self.export_section);
        module.section(&self.element_section);
        module.section(&self.code_section);
        module.section(&self.data_section);

        module
    }

    /// Helper: Get or allocate a register for a MIR value
    #[allow(dead_code)]
    fn get_or_alloc_register(&mut self, value: &Arc<mir::Value>) -> u32 {
        if let Some(&reg) = self.registers.get(value) {
            reg
        } else {
            let reg = self.registers.len() as u32;
            self.registers.insert(value.clone(), reg);
            reg
        }
    }

    /// Resolve an external function name to a WASM import index.
    /// Returns None for unknown external functions.
    fn resolve_ext_function(&self, name: &Symbol) -> Option<u32> {
        match name.as_str() {
            "_mimium_getsamplerate" => Some(self.rt.runtime_get_samplerate),
            "_mimium_getnow" => Some(self.rt.runtime_get_now),
            _ => None,
        }
    }

    /// Allocate a linear memory offset for a global variable, or return existing one.
    fn get_or_alloc_global_offset(&mut self, global: &VPtr) -> u32 {
        if let Some(&offset) = self.global_offsets.get(global) {
            offset
        } else {
            let offset = self.next_global_offset;
            self.next_global_offset += 8; // 8 bytes per global slot
            self.global_offsets.insert(global.clone(), offset);
            offset
        }
    }

    /// Helper: Map mimium type to WASM ValType
    #[allow(dead_code)]
    fn type_to_valtype(ty: &Type) -> ValType {
        match ty {
            Type::Primitive(PType::Numeric) => ValType::F64,
            Type::Primitive(PType::Int) => ValType::I64,
            Type::Primitive(PType::String) => ValType::I32, // pointer to string in linear memory
            Type::Primitive(PType::Unit) => ValType::I32,   // placeholder
            Type::Function { .. } => ValType::I32,          // function reference or index
            Type::Record(fields) if fields.len() == 1 => {
                // Single-field record: unwrap to field type
                Self::type_to_valtype(&fields[0].ty.to_type())
            }
            Type::Tuple(_) | Type::Record(_) => ValType::I32, // pointer to aggregate
            Type::Array(_) => ValType::I32,                   // array reference
            Type::Union(_) | Type::UserSum { .. } => ValType::I32, // sum type tag + payload
            Type::Ref(_) => ValType::I32,                     // pointer reference
            _ => ValType::F64,                                // default to f64
        }
    }

    /// Helper: Calculate size of a type in words
    #[allow(dead_code)]
    fn type_size(_ty: &Type) -> WordSize {
        // TODO: Implement proper type size calculation
        1
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ExecContext;

    #[test]
    fn test_wasmgen_create() {
        let mir = Arc::new(Mir::default());
        let generator = WasmGenerator::new(mir);
        // Verify runtime primitive imports are set up
        assert!(generator.current_fn_idx > 20); // At least 25 runtime functions imported
        assert!(generator.rt.heap_alloc < generator.current_fn_idx);
        assert!(generator.rt.array_alloc < generator.current_fn_idx);
        assert!(generator.rt.state_push < generator.current_fn_idx);
    }

    #[test]
    fn test_wasmgen_generate_empty() {
        let mir = Arc::new(Mir::default());
        let mut generator = WasmGenerator::new(mir);
        let result = generator.generate();
        assert!(
            result.is_ok(),
            "Should generate valid WASM module for empty MIR"
        );
        let wasm_bytes = result.unwrap();
        assert!(!wasm_bytes.is_empty(), "WASM module should not be empty");
        // Basic WASM module header check
        assert_eq!(&wasm_bytes[0..4], &[0x00, 0x61, 0x73, 0x6d]); // "\0asm" magic
        assert_eq!(&wasm_bytes[4..8], &[0x01, 0x00, 0x00, 0x00]); // version 1
    }

    /// Test that Return instructions don't store to destination register
    /// This was a bug where MIR generates `reg(dest) := ret value`, causing
    /// wasmgen to load the return value onto the stack and then store it to
    /// a destination register, leaving the stack empty when the function End
    /// instruction expected a return value.
    ///
    /// Fix: Check if instruction is Return or ReturnFeed and skip destination
    /// handling in translate_instruction_with_dest().
    #[test]
    fn test_wasmgen_simple_return() {
        let src = "fn dsp() -> float { 0.5 }";

        let mut ctx = ExecContext::new(std::iter::empty(), None, crate::Config::default());
        ctx.prepare_compiler();
        let mir = ctx
            .get_compiler()
            .unwrap()
            .emit_mir(src)
            .expect("MIR generate failed");

        let mut generator = WasmGenerator::new(Arc::new(mir));
        let wasm_bytes = generator.generate().expect("WASM generation failed");

        // Write to tmp directory for inspection
        use std::path::PathBuf;
        let mut wasm_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        wasm_path.pop(); // Remove mimium-lang
        wasm_path.pop(); // Remove lib
        wasm_path.pop(); // Remove crates
        wasm_path.push("tmp");
        wasm_path.push("test_simple_return_from_test.wasm");

        std::fs::write(&wasm_path, &wasm_bytes).expect("Failed to write WASM file");

        eprintln!(
            "Generated WASM file: {:?} ({} bytes)",
            wasm_path,
            wasm_bytes.len()
        );

        // Validate with wasmtime - this was failing before the fix
        let engine = wasmtime::Engine::default();
        let result = wasmtime::Module::new(&engine, &wasm_bytes);
        assert!(
            result.is_ok(),
            "WASM validation should pass for simple return function. Error: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_wasmgen_function_call() {
        let src = r#"
fn helper(x: float) -> float { x * 2.0 }
fn dsp() -> float { helper(21.0) }
"#;

        let mut ctx = ExecContext::new(std::iter::empty(), None, crate::Config::default());
        ctx.prepare_compiler();
        let mir = ctx
            .get_compiler()
            .unwrap()
            .emit_mir(src)
            .expect("MIR generate failed");

        let mut generator = WasmGenerator::new(Arc::new(mir));
        let wasm_bytes = generator.generate().expect("WASM generation failed");

        // Write to tmp directory for inspection
        use std::path::PathBuf;
        let mut wasm_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        wasm_path.pop(); // Remove mimium-lang
        wasm_path.pop(); // Remove lib
        wasm_path.pop(); // Remove crates
        wasm_path.push("tmp");
        wasm_path.push("test_function_call_from_test.wasm");

        std::fs::write(&wasm_path, &wasm_bytes).expect("Failed to write WASM file");

        eprintln!(
            "Generated function call WASM: {:?} ({} bytes)",
            wasm_path,
            wasm_bytes.len()
        );

        // Validate with wasmtime
        let engine = wasmtime::Engine::default();
        let result = wasmtime::Module::new(&engine, &wasm_bytes);
        assert!(
            result.is_ok(),
            "WASM validation should pass for function call. Error: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_wasmgen_regenerate_test_file() {
        let src = r#"
fn dsp() -> float {
    42.0
}
"#;

        let mut ctx = ExecContext::new(std::iter::empty(), None, crate::Config::default());
        ctx.prepare_compiler();
        let mir = ctx
            .get_compiler()
            .unwrap()
            .emit_mir(src)
            .expect("MIR generate failed");

        let mut generator = WasmGenerator::new(Arc::new(mir));
        let wasm_bytes = generator.generate().expect("WASM generation failed");

        // Write to tmp directory (3 levels up from crates/lib/mimium-lang)
        use std::path::PathBuf;
        let mut wasm_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        wasm_path.pop(); // Remove mimium-lang
        wasm_path.pop(); // Remove lib
        wasm_path.pop(); // Remove crates
        wasm_path.push("tmp");
        wasm_path.push("test_integration.wasm");

        std::fs::write(&wasm_path, &wasm_bytes).expect("Failed to write WASM file");

        eprintln!(
            "Generated WASM file: {:?} ({} bytes)",
            wasm_path,
            wasm_bytes.len()
        );
    }

    /// Helper: Compile source to WASM and validate with wasmtime
    fn compile_and_validate(src: &str, test_name: &str) -> Vec<u8> {
        let mut ctx = ExecContext::new(std::iter::empty(), None, crate::Config::default());
        ctx.prepare_compiler();
        let mir = ctx
            .get_compiler()
            .unwrap()
            .emit_mir(src)
            .expect("MIR generation failed");

        let mut generator = WasmGenerator::new(Arc::new(mir));
        let wasm_bytes = generator.generate().expect("WASM generation failed");

        // Write to tmp for inspection
        use std::path::PathBuf;
        let mut wasm_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        wasm_path.pop();
        wasm_path.pop();
        wasm_path.pop();
        wasm_path.push("tmp");
        wasm_path.push(format!("{test_name}.wasm"));
        std::fs::write(&wasm_path, &wasm_bytes).expect("Failed to write WASM file");

        let engine = wasmtime::Engine::default();
        let result = wasmtime::Module::new(&engine, &wasm_bytes);
        assert!(
            result.is_ok(),
            "[{test_name}] WASM validation failed: {:?}",
            result.err()
        );
        wasm_bytes
    }

    /// Test arithmetic chain: multiple operations on floats
    #[test]
    fn test_wasmgen_arithmetic_chain() {
        let src = r#"
fn dsp() -> float {
    let x = 3.0;
    let y = x + 1.0;
    let z = y * 2.0 - x;
    z / 2.0
}
"#;
        compile_and_validate(src, "test_arithmetic_chain");
    }

    /// Test nested function calls
    #[test]
    fn test_wasmgen_nested_calls() {
        let src = r#"
fn double(x: float) -> float { x * 2.0 }
fn add_one(x: float) -> float { x + 1.0 }
fn dsp() -> float { add_one(double(10.0)) }
"#;
        compile_and_validate(src, "test_nested_calls");
    }

    /// Test multi-argument function
    #[test]
    fn test_wasmgen_multi_arg() {
        let src = r#"
fn lerp(a: float, b: float, t: float) -> float {
    a + (b - a) * t
}
fn dsp() -> float { lerp(0.0, 10.0, 0.5) }
"#;
        compile_and_validate(src, "test_multi_arg");
    }

    /// Test function returning expression with negation
    #[test]
    fn test_wasmgen_negation() {
        let src = r#"
fn neg(x: float) -> float { -x }
fn dsp() -> float { neg(42.0) }
"#;
        compile_and_validate(src, "test_negation");
    }

    /// Test conditional (if/else) expression with JmpIf/Phi
    #[test]
    fn test_wasmgen_conditional() {
        let src = r#"
fn abs_val(x: float) -> float {
    if (x < 0.0) {
        0.0 - x
    } else {
        x
    }
}
fn dsp() -> float { abs_val(0.0 - 42.0) }
"#;
        compile_and_validate(src, "test_conditional");
    }

    /// Test let bindings with intermediate computations
    #[test]
    fn test_wasmgen_let_bindings() {
        let src = r#"
fn clamp(x: float, lo: float, hi: float) -> float {
    let v = if (x < lo) { lo } else { x };
    if (v > hi) { hi } else { v }
}
fn dsp() -> float { clamp(1.5, 0.0, 1.0) }
"#;
        compile_and_validate(src, "test_let_bindings");
    }

    /// Test stateful function (counter using `self`)
    /// MIR pattern: GetState -> Load -> compute -> ReturnFeed
    #[test]
    fn test_wasmgen_stateful_counter() {
        let src = r#"
fn counter() -> float {
    self + 1.0
}
"#;
        compile_and_validate(src, "test_stateful_counter");
    }

    /// Test external function call: samplerate
    /// MIR pattern: call_indirect extfun _mimium_getsamplerate
    #[test]
    fn test_wasmgen_samplerate_call() {
        let src = r#"
fn get_sr() -> float {
    samplerate
}
"#;
        compile_and_validate(src, "test_samplerate_call");
    }

    /// Test external function call: now
    #[test]
    fn test_wasmgen_now_call() {
        let src = r#"
fn get_time() -> float {
    now
}
"#;
        compile_and_validate(src, "test_now_call");
    }

    /// Test stateful phasor function (phase accumulator with samplerate)
    #[test]
    fn test_wasmgen_phasor() {
        let src = r#"
fn phasor(freq: float) -> float {
    (self + freq / samplerate) % 1.0
}
"#;
        compile_and_validate(src, "test_phasor");
    }

    /// Test full sinewave DSP pipeline
    #[test]
    fn test_wasmgen_sinewave() {
        let src = r#"
fn phasor(freq: float) -> float {
    (self + freq / samplerate) % 1.0
}
fn dsp() -> float {
    sin(phasor(440.0) * 3.14159265 * 2.0) * 0.5
}
"#;
        compile_and_validate(src, "test_sinewave");
    }

    /// Test global variable (let binding at top level)
    #[test]
    fn test_wasmgen_global_variable() {
        let src = r#"
let PI = 3.14159265
fn dsp() -> float {
    sin(440.0 * PI * 2.0)
}
"#;
        compile_and_validate(src, "test_global_variable");
    }

    /// Test global variable used through function call
    #[test]
    fn test_wasmgen_global_in_function() {
        let src = r#"
let GAIN = 0.5
fn apply_gain(x: float) -> float { x * GAIN }
fn dsp() -> float { apply_gain(1.0) }
"#;
        compile_and_validate(src, "test_global_in_function");
    }
}
