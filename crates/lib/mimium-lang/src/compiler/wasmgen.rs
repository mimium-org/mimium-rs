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

use crate::interner::{Symbol, TypeNodeId};
use crate::mir::{self, Mir, VPtr};
use crate::runtime::primitives::WordSize;
use crate::types::{PType, Type};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use wasm_encoder::{
    CodeSection, DataSection, ElementSection, EntityType, ExportSection, Function, FunctionSection,
    GlobalSection, ImportSection, MemArg, MemorySection, MemoryType, Module, TableSection,
    TableType, TypeSection, ValType,
};

/// Plugin function import indices
#[derive(Debug, Clone, Default)]
struct PluginFunctionIndices {
    /// Map: function name -> WASM import index
    functions: HashMap<Symbol, u32>,
}

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
    closure_state_push: u32,
    closure_state_pop: u32,
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
    // Builtin function imports (from "builtin" module)
    builtin_probeln: u32,
    builtin_probe: u32,
    builtin_length_array: u32,
    builtin_split_head: u32,
    builtin_split_tail: u32,
}

/// Linear memory layout manager
///
/// Manages the layout of WASM linear memory regions:
/// - 0..256: Reserved for future use
/// - 256..512: Global variable storage
/// - 512..1024: State exchange temporary region
/// - 1024..: Dynamic allocation region
#[derive(Debug, Clone)]
struct MemoryLayout {
    /// Current offset for dynamic allocations (Alloc instructions)
    alloc_offset: u32,
    /// Base address for state exchange temporary memory region
    state_temp_base: u32,
    /// Global variable memory offsets: maps global VPtr to linear memory address
    global_offsets: HashMap<VPtr, u32>,
    /// Next available offset for global variable allocation
    next_global_offset: u32,
}

impl Default for MemoryLayout {
    fn default() -> Self {
        Self {
            alloc_offset: 1024,   // Start dynamic allocation after reserved regions
            state_temp_base: 512, // Reserve 512..1024 for state exchange
            global_offsets: HashMap::new(),
            next_global_offset: 256, // Reserve 256..512 for globals
        }
    }
}

impl MemoryLayout {
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
    /// Global section (WASM global variables)
    global_section: GlobalSection,

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
    /// Argument types for the current function being generated.
    /// Used by infer_value_type to determine the WASM type of function arguments.
    current_arg_types: Vec<wasm_encoder::ValType>,
    /// Maps MIR argument index to (WASM param start index, word count).
    /// When tuple args are flattened, a single MIR arg maps to multiple WASM params.
    current_arg_map: Vec<(u32, u32)>,
    /// Number of i64 local slots for the current function.
    /// Used as offset base for f64 locals: f64 reg `r` maps to local `num_args + num_i64_locals + r`.
    current_num_i64_locals: u32,

    /// Linear memory layout manager
    mem_layout: MemoryLayout,

    /// Runtime primitive function indices
    rt: RuntimeFunctionIndices,
    /// Plugin function import indices
    plugin_fns: PluginFunctionIndices,

    /// Registers produced by Alloc instructions, mapped to their content word size.
    /// word_size == 1 means single-word (dereference when capturing),
    /// word_size > 1 means multi-word like tuple (store pointer when capturing).
    alloc_registers: HashMap<mir::VReg, u8>,
    /// Registers produced by GetElement instructions, mapped to the element's ValType.
    /// These registers hold pointers to elements but are often used as values in the MIR.
    getelement_registers: HashMap<mir::VReg, wasm_encoder::ValType>,
    /// Maps MIR function index to its WASM type section index
    fn_type_indices: Vec<u32>,
    /// Local variable index for saving/restoring closure self pointer
    closure_save_local: u32,
    /// Local variable index for storing base address during runtime allocation (i32)
    alloc_base_local: u32,
    /// WASM global index for the runtime bump allocator pointer
    alloc_ptr_global: u32,
    /// Local variable index for saving alloc pointer at entry-function start (i32)
    alloc_ptr_save_local: u32,
    /// Whether the current function being generated is an entry point (dsp or _mimium_global).
    /// Entry functions save/restore the alloc pointer to prevent unbounded memory growth.
    is_entry_function: bool,
    /// Maps function signatures (params, results) to type section indices for call_indirect
    call_type_cache: HashMap<(Vec<wasm_encoder::ValType>, Vec<wasm_encoder::ValType>), u32>,
    /// Per-function upvalue indirection info.
    /// Maps MIR function index to a boolean vec: `true` at position `i` means upvalue `i`
    /// stores an alloc **pointer** (not the dereferenced value), so `GetUpValue`/`SetUpValue`
    /// must go through one extra level of indirection.
    /// This is necessary because in WASM we combine closure creation and close into one step:
    /// all closures are immediately "closed" and upvalues from alloc cells are shared by
    /// reference rather than copied by value.
    indirect_upvalues: HashMap<usize, Vec<bool>>,
    /// MIR function index of the function currently being compiled in `generate_function_bodies`.
    current_mir_fn_idx: usize,
}

/// Context for emitting control flow blocks
struct BlockEmitContext<'a> {
    blocks: &'a [mir::Block],
    processed: HashSet<usize>,
}

impl<'a> BlockEmitContext<'a> {
    fn new(blocks: &'a [mir::Block]) -> Self {
        Self {
            blocks,
            processed: HashSet::new(),
        }
    }

    fn mark_processed(&mut self, idx: usize) {
        self.processed.insert(idx);
    }

    fn is_processed(&self, idx: usize) -> bool {
        self.processed.contains(&idx)
    }
}

/// Context for emitting a Switch instruction
struct SwitchContext<'a> {
    scrutinee: &'a VPtr,
    cases: &'a [(i64, u64)],
    default_block: Option<u64>,
    merge_block: u64,
}

impl WasmGenerator {
    /// Create a new WASM generator for the given MIR.
    ///
    /// `ext_fns` should contain the complete list of external function type
    /// information gathered from all plugin sources (system plugins, dynamic
    /// plugins, etc.).  The generator uses this to set up the WASM import
    /// declarations that the runtime will later satisfy with host trampolines.
    pub fn new(mir: Arc<Mir>, ext_fns: &[crate::plugin::ExtFunTypeInfo]) -> Self {
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
            global_section: GlobalSection::new(),
            mir,
            current_fn_idx: 0,
            num_imports: 0,
            registers: HashMap::new(),
            fn_name_to_idx: HashMap::new(),
            register_types: HashMap::new(),
            register_constants: HashMap::new(),
            current_num_args: 0,
            current_arg_types: Vec::new(),
            current_arg_map: Vec::new(),
            current_num_i64_locals: 0,
            mem_layout: MemoryLayout::default(),
            // Runtime primitive indices will be set by setup_runtime_imports
            rt: RuntimeFunctionIndices::default(),
            plugin_fns: PluginFunctionIndices::default(),
            alloc_registers: HashMap::new(),
            getelement_registers: HashMap::new(),
            fn_type_indices: Vec::new(),
            closure_save_local: 0,
            alloc_base_local: 0,
            alloc_ptr_global: 0,
            alloc_ptr_save_local: 0,
            is_entry_function: false,
            call_type_cache: HashMap::new(),
            indirect_upvalues: HashMap::new(),
            current_mir_fn_idx: 0,
        };

        // Setup runtime primitive imports and memory/table
        generator.setup_runtime_imports();
        generator.setup_math_imports();
        generator.setup_builtin_imports();
        generator.setup_plugin_imports(ext_fns);
        generator.setup_memory_and_table();

        // Save number of imported functions for use in function index calculations
        generator.num_imports = generator.current_fn_idx;

        generator
    }

    /// Create a generator with no plugin imports.
    ///
    /// Shorthand used by tests and callers that do not need plugin support.
    pub fn new_without_plugins(mir: Arc<Mir>) -> Self {
        Self::new(mir, &[])
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

        // Type 8: (i64, i64) -> ()  for closure_state_push(closure_addr, state_size)
        self.type_section
            .ty()
            .function(vec![ValType::I64, ValType::I64], vec![]);
        self.rt.closure_state_push = self.add_import("closure_state_push", type_idx);
        type_idx += 1;

        // Type 9: () -> ()  for closure_state_pop
        self.type_section.ty().function(vec![], vec![]);
        self.rt.closure_state_pop = self.add_import("closure_state_pop", type_idx);
        type_idx += 1;

        // Type 10: (i64) -> ()  for state_push, state_pop
        self.type_section.ty().function(vec![ValType::I64], vec![]);
        self.rt.state_push = self.add_import("state_push", type_idx);
        self.rt.state_pop = self.add_import("state_pop", type_idx);
        type_idx += 1;

        // Type 11: (i32, i32) -> ()  for state_get, state_set
        self.type_section
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![]);
        self.rt.state_get = self.add_import("state_get", type_idx);
        self.rt.state_set = self.add_import("state_set", type_idx);
        type_idx += 1;

        // Type 10: (f64, f64, i64) -> f64  for state_delay
        self.type_section.ty().function(
            vec![ValType::F64, ValType::F64, ValType::I64],
            vec![ValType::F64],
        );
        self.rt.state_delay = self.add_import("state_delay", type_idx);
        type_idx += 1;

        // Type 11: (f64) -> f64  for state_mem
        self.type_section
            .ty()
            .function(vec![ValType::F64], vec![ValType::F64]);
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

    /// Setup builtin function imports (probeln, probe, length_array, split_head, split_tail)
    fn setup_builtin_imports(&mut self) {
        // Type: (f64) -> f64 for probeln, probe
        let type_idx_f64_f64 = self.type_section.len();
        self.type_section
            .ty()
            .function(vec![ValType::F64], vec![ValType::F64]);
        self.rt.builtin_probeln = self.add_import_from("builtin", "probeln", type_idx_f64_f64);
        self.rt.builtin_probe = self.add_import_from("builtin", "probe", type_idx_f64_f64);

        // Type: (i64) -> f64 for length_array (takes array handle, returns count as f64)
        let type_idx_i64_f64 = self.type_section.len();
        self.type_section
            .ty()
            .function(vec![ValType::I64], vec![ValType::F64]);
        self.rt.builtin_length_array =
            self.add_import_from("builtin", "length_array", type_idx_i64_f64);

        // Type: (i64, i32) -> () for split_head / split_tail
        //   arg1: array handle (i64),  arg2: destination pointer (i32)
        //   Writes result tuple to linear memory at dst_ptr.
        let type_idx_split = self.type_section.len();
        self.type_section
            .ty()
            .function(vec![ValType::I64, ValType::I32], vec![]);
        self.rt.builtin_split_head = self.add_import_from("builtin", "split_head", type_idx_split);
        self.rt.builtin_split_tail = self.add_import_from("builtin", "split_tail", type_idx_split);
    }

    /// Setup plugin function imports from dynamically loaded plugins
    /// Setup plugin function imports.
    ///
    /// Registers a WASM import for every runtime-stage external function
    /// described in `ext_fns`.  This covers both system plugins (like
    /// `GuiToolPlugin`) and dynamically loaded ones.
    #[cfg(not(target_arch = "wasm32"))]
    fn setup_plugin_imports(&mut self, ext_fns: &[crate::plugin::ExtFunTypeInfo]) {
        for type_info in ext_fns {
            let name = type_info.name;
            let fn_ty = type_info.ty.to_type();

            let Type::Function { arg, ret } = fn_ty else {
                log::warn!(
                    "Plugin function {} has non-function type: {:?}",
                    name.as_str(),
                    fn_ty
                );
                continue;
            };

            // Flatten tuple arguments into separate WASM parameters
            let param_types: Vec<ValType> = match arg.to_type() {
                Type::Tuple(elems) => elems
                    .iter()
                    .map(|t| Self::type_to_valtype(&t.to_type()))
                    .collect(),
                arg_ty => vec![Self::type_to_valtype(&arg_ty)],
            };
            // Strip Unit return type to match process_mir_functions behavior:
            // Unit-returning functions have no WASM return values.
            let return_types: Vec<ValType> =
                if matches!(ret.to_type(), Type::Primitive(PType::Unit)) {
                    vec![]
                } else {
                    vec![Self::type_to_valtype(&ret.to_type())]
                };

            let type_idx = self.type_section.len();
            self.type_section
                .ty()
                .function(param_types.clone(), return_types);

            let fn_idx = self.add_import_from("plugin", name.as_str(), type_idx);
            self.plugin_fns.functions.insert(name, fn_idx);

            log::debug!(
                "Added plugin import: {} ({} params)",
                name.as_str(),
                param_types.len()
            );
        }
    }

    #[cfg(target_arch = "wasm32")]
    fn setup_plugin_imports(&mut self, _ext_fns: &[crate::plugin::ExtFunTypeInfo]) {
        // Browser WASM does not use the native plugin mechanism.
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

        // Phase 1.5: Populate the function table for indirect calls
        self.setup_function_table();

        // Phase 2: Generate function bodies
        self.generate_function_bodies()?;

        // Phase 2.5: Generate the closure-execution trampoline exported as
        // `_mimium_exec_closure_void`.  The host (scheduler, etc.) calls this
        // to invoke a `() -> ()` closure stored in linear memory by address.
        self.generate_exec_closure_trampoline();

        // Phase 3: Export functions
        self.export_functions()?;

        // Phase 3.5: Setup runtime allocator global
        // The alloc_ptr_global starts at the final alloc_offset value,
        // which is past all compile-time static allocations.
        self.global_section.global(
            wasm_encoder::GlobalType {
                val_type: wasm_encoder::ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(self.mem_layout.alloc_offset as i32),
        );
        self.alloc_ptr_global = 0; // first (and only) global

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
            // Infer argument types from function body usage.
            // This resolves Unknown argument types that type inference
            // didn't propagate back to the MIR function signature.
            let inferred_arg_types = Self::infer_argument_types(func);

            // Flatten argument types: tuple/record args expand to individual params.
            // This matches the MIR calling convention where tuple args are passed
            // as separate words at call sites.
            let param_types: Vec<ValType> = func
                .args
                .iter()
                .enumerate()
                .flat_map(|(arg_idx, arg)| {
                    let mut flat = Self::flatten_type_to_valtypes(&arg.1.to_type());
                    // Override with body-inferred types when the declared type is
                    // insufficient (e.g., {tup:unknown} that should be 3 F64 params).
                    if let Some(inferred) = inferred_arg_types.get(&arg_idx) {
                        if inferred.len() > flat.len() {
                            // Multi-word inferred type replaces under-specified declaration
                            flat = inferred.clone();
                        } else if flat.len() == 1 && inferred.len() == 1 && flat[0] != inferred[0] {
                            flat[0] = inferred[0];
                        }
                    }
                    flat
                })
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

            // Track type index for call_indirect
            self.fn_type_indices.push(type_idx);

            // Track function name to index mapping
            // Note: current_fn_idx already accounts for imported functions
            // Only register the first occurrence of each function name (avoid duplicate mappings from multiple includes)
            if !self.fn_name_to_idx.contains_key(&func.label) {
                self.fn_name_to_idx.insert(func.label, self.current_fn_idx);
                log::debug!(
                    "Registered function {} at index {}",
                    func.label.as_str(),
                    self.current_fn_idx
                );
            } else {
                log::debug!(
                    "Skipping duplicate mapping for function {} (already mapped to index {})",
                    func.label.as_str(),
                    self.fn_name_to_idx[&func.label]
                );
            }
            self.current_fn_idx += 1;
        }

        Ok(())
    }

    /// Populate the WASM function table with all MIR functions for indirect calls.
    /// Table index i maps to WASM function (num_imports + i).
    fn setup_function_table(&mut self) {
        let num_fns = self.mir.functions.len() as u32;
        if num_fns == 0 {
            return;
        }
        let func_indices: Vec<u32> = (0..num_fns).map(|i| i + self.num_imports).collect();
        self.element_section.active(
            Some(0),
            &wasm_encoder::ConstExpr::i32_const(0),
            wasm_encoder::Elements::Functions(std::borrow::Cow::Borrowed(&func_indices)),
        );
    }

    /// Get or create a WASM function type index for indirect calls.
    /// Caches signatures to avoid duplicating type entries.
    fn get_or_create_call_type(
        &mut self,
        params: Vec<wasm_encoder::ValType>,
        results: Vec<wasm_encoder::ValType>,
    ) -> u32 {
        let key = (params.clone(), results.clone());
        if let Some(&idx) = self.call_type_cache.get(&key) {
            return idx;
        }
        let type_idx = self.type_section.len();
        self.type_section.ty().function(params, results);
        self.call_type_cache.insert(key, type_idx);
        type_idx
    }

    /// Generate WASM function bodies from MIR
    fn generate_function_bodies(&mut self) -> Result<(), String> {
        use wasm_encoder::Instruction as W;

        // Clone the functions vector to avoid borrow checker issues
        let functions = self.mir.functions.clone();

        // For each MIR function, generate WASM function body
        for (mir_fn_idx, func) in functions.iter().enumerate() {
            self.current_mir_fn_idx = mir_fn_idx;

            // Reset register mapping and type tracking for each function
            self.registers.clear();
            self.register_types.clear();
            self.register_constants.clear();

            // Infer argument types from body usage (same analysis as in process_mir_functions)
            let inferred_arg_types = Self::infer_argument_types(func);

            // Build flattened argument mapping.
            // MIR args are flattened when tuple/record types are passed:
            // each element becomes a separate WASM parameter.
            let mut arg_map = Vec::new();
            let mut flat_arg_types = Vec::new();
            let mut wasm_param_idx: u32 = 0;
            for (arg_idx, arg) in func.args.iter().enumerate() {
                let mut flat_types = Self::flatten_type_to_valtypes(&arg.1.to_type());
                // Override with body-inferred types when the declared type is
                // insufficient (e.g., {tup:unknown} that should be 3 F64 params).
                if let Some(inferred) = inferred_arg_types.get(&arg_idx) {
                    if inferred.len() > flat_types.len() {
                        // Multi-word inferred type replaces under-specified declaration
                        flat_types = inferred.clone();
                    } else if flat_types.len() == 1
                        && inferred.len() == 1
                        && flat_types[0] != inferred[0]
                    {
                        flat_types[0] = inferred[0];
                    }
                }
                let word_count = flat_types.len() as u32;
                arg_map.push((wasm_param_idx, word_count));
                flat_arg_types.extend(flat_types);
                wasm_param_idx += word_count;
            }
            self.current_num_args = wasm_param_idx;
            self.current_arg_types = flat_arg_types;
            self.current_arg_map = arg_map;

            // Scan function body to determine register types and constants
            self.analyze_register_types(func);

            // Compute required local counts from actual register usage
            let (max_i64_reg, max_f64_reg) = self.compute_max_register_indices();
            let num_i64_locals = max_i64_reg + 1;
            let num_f64_locals = max_f64_reg + 1;
            self.current_num_i64_locals = num_i64_locals;

            // WASM local layout: [args(0..n)] [i64 regs(n..n+num_i64)] [f64 regs(n+num_i64..)] [closure_save: i64]
            let mut locals = Vec::new();
            if num_i64_locals > 0 {
                locals.push((num_i64_locals, ValType::I64));
            }
            if num_f64_locals > 0 {
                locals.push((num_f64_locals, ValType::F64));
            }
            // Extra i64 local for saving/restoring closure self pointer during indirect calls
            locals.push((1, ValType::I64));
            self.closure_save_local = self.current_num_args + num_i64_locals + num_f64_locals;
            // Extra i32 local for runtime allocator base address (used by MakeClosure)
            locals.push((1, ValType::I32));
            self.alloc_base_local = self.closure_save_local + 1;

            // Detect entry-point functions that need alloc pointer save/restore.
            // Only `dsp` resets the allocator each call — `_mimium_global` runs
            // once and its allocations (closures stored in globals) must persist.
            let is_entry = func.label.as_str() == "dsp";
            self.is_entry_function = is_entry;

            // Extra i32 local for saving alloc pointer at entry function start
            locals.push((1, ValType::I32));
            self.alloc_ptr_save_local = self.alloc_base_local + 1;

            // Create a new WASM function
            let mut wasm_func = Function::new(locals);

            // Entry functions save the alloc pointer at start and restore before return,
            // preventing unbounded memory growth from per-sample Alloc instructions.
            if is_entry {
                wasm_func.instruction(&W::GlobalGet(self.alloc_ptr_global));
                wasm_func.instruction(&W::LocalSet(self.alloc_ptr_save_local));
            }

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
        let mut ctx = BlockEmitContext::new(blocks);

        for block_idx in 0..blocks.len() {
            if ctx.is_processed(block_idx) {
                continue;
            }
            ctx.mark_processed(block_idx);

            let block = &blocks[block_idx];
            for (dest, instr) in &block.0 {
                match instr {
                    I::JmpIf(cond, then_bb, else_bb, merge_bb) => {
                        let then_idx = *then_bb as usize;
                        let else_idx = *else_bb as usize;
                        let merge_idx = *merge_bb as usize;

                        // Find Phi in merge block to determine result type and inputs
                        let phi_info = Self::find_phi_in_block(&blocks[merge_idx]);

                        // Load condition and convert to i32 for WASM if instruction.
                        // The native VM uses JmpIfNeg which jumps to else when cond <= 0.0,
                        // so the then-branch is taken when cond > 0.0.
                        self.emit_value_load(cond, wasm_func);
                        if self.infer_value_type(cond) == ValType::F64 {
                            // cond > 0.0 means then-branch (matching VM's JmpIfNeg semantics)
                            wasm_func.instruction(&W::F64Const(0.0));
                            wasm_func.instruction(&W::F64Gt);
                        } else {
                            // For i64: cond > 0 means then-branch
                            wasm_func.instruction(&W::I64Const(0));
                            wasm_func.instruction(&W::I64GtS);
                        }

                        // Determine if block type from Phi presence
                        let (block_type, phi_result_type) =
                            if let Some((_, ref phi_dest)) = phi_info {
                                let reg_type =
                                    if let mir::Value::Register(reg_idx) = phi_dest.as_ref() {
                                        self.register_types
                                            .get(reg_idx)
                                            .copied()
                                            .unwrap_or(ValType::F64)
                                    } else {
                                        ValType::F64
                                    };
                                (wasm_encoder::BlockType::Result(reg_type), Some(reg_type))
                            } else {
                                (wasm_encoder::BlockType::Empty, None)
                            };
                        wasm_func.instruction(&W::If(block_type));

                        // Emit then-branch block
                        ctx.mark_processed(then_idx);
                        self.emit_branch_block(
                            &ctx.blocks[then_idx],
                            &mut ctx,
                            phi_info.as_ref().map(|(inputs, _)| &inputs.0),
                            phi_result_type,
                            wasm_func,
                        );

                        wasm_func.instruction(&W::Else);

                        // Emit else-branch block
                        ctx.mark_processed(else_idx);
                        self.emit_branch_block(
                            &ctx.blocks[else_idx],
                            &mut ctx,
                            phi_info.as_ref().map(|(inputs, _)| &inputs.1),
                            phi_result_type,
                            wasm_func,
                        );

                        wasm_func.instruction(&W::End);

                        // Emit merge block (skip Phi, which is on the stack from if/else)
                        ctx.mark_processed(merge_idx);
                        self.emit_merge_block(
                            &ctx.blocks[merge_idx],
                            phi_info.is_some(),
                            wasm_func,
                        );
                    }
                    I::Switch {
                        scrutinee,
                        cases,
                        default_block,
                        merge_block,
                    } => {
                        self.emit_switch(
                            SwitchContext {
                                scrutinee,
                                cases,
                                default_block: *default_block,
                                merge_block: *merge_block,
                            },
                            &mut ctx,
                            wasm_func,
                        );
                    }
                    _ => {
                        self.translate_instruction_with_dest(dest.as_ref(), instr, wasm_func);
                    }
                }
            }
        }
    }

    /// Emit a Switch instruction as a nested if/else chain.
    /// Extracted as a method so it can be called recursively for nested switches.
    fn emit_switch(
        &mut self,
        switch_ctx: SwitchContext,
        ctx: &mut BlockEmitContext,
        wasm_func: &mut Function,
    ) {
        use wasm_encoder::Instruction as W;

        let merge_idx = switch_ctx.merge_block as usize;

        // Find PhiSwitch in merge block
        let phi_switch_info = Self::find_phi_switch_in_block(&ctx.blocks[merge_idx]);

        // Determine result type from PhiSwitch
        let (block_type, phi_result_type) = if let Some((_, ref phi_dest)) = phi_switch_info {
            let reg_type = if let mir::Value::Register(reg_idx) = phi_dest.as_ref() {
                self.register_types
                    .get(reg_idx)
                    .copied()
                    .unwrap_or(ValType::F64)
            } else {
                ValType::F64
            };
            (wasm_encoder::BlockType::Result(reg_type), Some(reg_type))
        } else {
            (wasm_encoder::BlockType::Empty, None)
        };

        let num_cases = switch_ctx.cases.len();
        let phi_inputs = phi_switch_info
            .as_ref()
            .map(|(inputs, _)| inputs.clone())
            .unwrap_or_default();

        for (i, (case_val, case_bb)) in switch_ctx.cases.iter().enumerate() {
            let case_idx = *case_bb as usize;

            // Load scrutinee and compare with case value
            self.emit_value_load(switch_ctx.scrutinee, wasm_func);
            wasm_func.instruction(&W::I64Const(*case_val));
            wasm_func.instruction(&W::I64Eq);
            wasm_func.instruction(&W::If(block_type));

            // Emit case block with nested control flow support
            ctx.mark_processed(case_idx);
            self.emit_block_instructions(&ctx.blocks[case_idx], ctx, wasm_func);

            // Push phi input for this case
            if let Some(input) = phi_inputs.get(i) {
                if let Some(expected) = phi_result_type {
                    self.emit_value_load_typed(input, expected, wasm_func);
                } else {
                    self.emit_value_load(input, wasm_func);
                }
            }

            wasm_func.instruction(&W::Else);
        }

        // Default or last else
        if let Some(default_bb) = switch_ctx.default_block {
            let default_idx = default_bb as usize;
            ctx.mark_processed(default_idx);
            self.emit_block_instructions(&ctx.blocks[default_idx], ctx, wasm_func);

            if let Some(input) = phi_inputs.get(num_cases) {
                if let Some(expected) = phi_result_type {
                    self.emit_value_load_typed(input, expected, wasm_func);
                } else {
                    self.emit_value_load(input, wasm_func);
                }
            }
        } else if !phi_inputs.is_empty() {
            if let Some(last) = phi_inputs.last() {
                if let Some(expected) = phi_result_type {
                    self.emit_value_load_typed(last, expected, wasm_func);
                } else {
                    self.emit_value_load(last, wasm_func);
                }
            }
        }

        // Close all the nested if/else blocks
        for _ in 0..num_cases {
            wasm_func.instruction(&W::End);
        }

        // Emit merge block (skip PhiSwitch)
        ctx.mark_processed(merge_idx);
        self.emit_merge_block_switch(&ctx.blocks[merge_idx], phi_switch_info.is_some(), wasm_func);
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

    /// Find the PhiSwitch instruction in a merge block.
    /// Returns `Some((inputs_vec, dest_vptr))` if found.
    fn find_phi_switch_in_block(block: &mir::Block) -> Option<(Vec<VPtr>, VPtr)> {
        for (dest, instr) in &block.0 {
            if let mir::Instruction::PhiSwitch(inputs) = instr {
                return Some((inputs.clone(), dest.clone()));
            }
        }
        None
    }

    /// Emit a branch block's instructions, then load the Phi input onto the stack.
    /// Handles nested JmpIf instructions by recursively emitting inner if/else structures.
    fn emit_branch_block(
        &mut self,
        block: &mir::Block,
        ctx: &mut BlockEmitContext,
        phi_input: Option<&VPtr>,
        expected_phi_type: Option<ValType>,
        func: &mut Function,
    ) {
        use mir::Instruction as I;
        use wasm_encoder::Instruction as W;

        for (dest, instr) in &block.0 {
            match instr {
                I::JmpIf(cond, then_bb, else_bb, merge_bb) => {
                    let then_idx = *then_bb as usize;
                    let else_idx = *else_bb as usize;
                    let merge_idx = *merge_bb as usize;

                    let phi_info = Self::find_phi_in_block(&ctx.blocks[merge_idx]);

                    self.emit_value_load(cond, func);
                    if self.infer_value_type(cond) == ValType::F64 {
                        func.instruction(&W::F64Const(0.0));
                        func.instruction(&W::F64Gt);
                    } else {
                        func.instruction(&W::I64Const(0));
                        func.instruction(&W::I64GtS);
                    }

                    let (block_type, inner_phi_type) = if let Some((_, ref phi_dest)) = phi_info {
                        let reg_type = if let mir::Value::Register(reg_idx) = phi_dest.as_ref() {
                            self.register_types
                                .get(reg_idx)
                                .copied()
                                .unwrap_or(ValType::F64)
                        } else {
                            ValType::F64
                        };
                        (wasm_encoder::BlockType::Result(reg_type), Some(reg_type))
                    } else {
                        (wasm_encoder::BlockType::Empty, None)
                    };
                    func.instruction(&W::If(block_type));

                    ctx.mark_processed(then_idx);
                    self.emit_branch_block(
                        &ctx.blocks[then_idx],
                        ctx,
                        phi_info.as_ref().map(|(inputs, _)| &inputs.0),
                        inner_phi_type,
                        func,
                    );

                    func.instruction(&W::Else);

                    ctx.mark_processed(else_idx);
                    self.emit_branch_block(
                        &ctx.blocks[else_idx],
                        ctx,
                        phi_info.as_ref().map(|(inputs, _)| &inputs.1),
                        inner_phi_type,
                        func,
                    );

                    func.instruction(&W::End);

                    ctx.mark_processed(merge_idx);
                    self.emit_merge_block(&ctx.blocks[merge_idx], phi_info.is_some(), func);
                }
                I::Switch {
                    scrutinee,
                    cases,
                    default_block,
                    merge_block,
                } => {
                    self.emit_switch(
                        SwitchContext {
                            scrutinee,
                            cases,
                            default_block: *default_block,
                            merge_block: *merge_block,
                        },
                        ctx,
                        func,
                    );
                }
                _ => {
                    self.translate_instruction_with_dest(dest.as_ref(), instr, func);
                }
            }
        }
        // Push the phi input to stay on the stack as the if/else result
        if let Some(input) = phi_input {
            if let Some(expected) = expected_phi_type {
                self.emit_value_load_typed(input, expected, func);
            } else {
                self.emit_value_load(input, func);
            }
        }
    }

    /// Emit all instructions in a block, handling nested control flow (JmpIf, Switch).
    /// This is used by Switch case block emission to allow nesting.
    fn emit_block_instructions(
        &mut self,
        block: &mir::Block,
        ctx: &mut BlockEmitContext,
        func: &mut Function,
    ) {
        use mir::Instruction as I;
        use wasm_encoder::Instruction as W;

        for (dest, instr) in &block.0 {
            match instr {
                I::JmpIf(cond, then_bb, else_bb, merge_bb) => {
                    let then_idx = *then_bb as usize;
                    let else_idx = *else_bb as usize;
                    let merge_idx = *merge_bb as usize;

                    let phi_info = Self::find_phi_in_block(&ctx.blocks[merge_idx]);

                    self.emit_value_load(cond, func);
                    if self.infer_value_type(cond) == ValType::F64 {
                        func.instruction(&W::F64Const(0.0));
                        func.instruction(&W::F64Gt);
                    } else {
                        func.instruction(&W::I64Const(0));
                        func.instruction(&W::I64GtS);
                    }

                    let (block_type, inner_phi_type) = if let Some((_, ref phi_dest)) = phi_info {
                        let reg_type = if let mir::Value::Register(reg_idx) = phi_dest.as_ref() {
                            self.register_types
                                .get(reg_idx)
                                .copied()
                                .unwrap_or(ValType::F64)
                        } else {
                            ValType::F64
                        };
                        (wasm_encoder::BlockType::Result(reg_type), Some(reg_type))
                    } else {
                        (wasm_encoder::BlockType::Empty, None)
                    };
                    func.instruction(&W::If(block_type));

                    ctx.mark_processed(then_idx);
                    self.emit_branch_block(
                        &ctx.blocks[then_idx],
                        ctx,
                        phi_info.as_ref().map(|(inputs, _)| &inputs.0),
                        inner_phi_type,
                        func,
                    );

                    func.instruction(&W::Else);

                    ctx.mark_processed(else_idx);
                    self.emit_branch_block(
                        &ctx.blocks[else_idx],
                        ctx,
                        phi_info.as_ref().map(|(inputs, _)| &inputs.1),
                        inner_phi_type,
                        func,
                    );

                    func.instruction(&W::End);

                    ctx.mark_processed(merge_idx);
                    self.emit_merge_block(&ctx.blocks[merge_idx], phi_info.is_some(), func);
                }
                I::Switch {
                    scrutinee,
                    cases,
                    default_block,
                    merge_block,
                } => {
                    self.emit_switch(
                        SwitchContext {
                            scrutinee,
                            cases,
                            default_block: *default_block,
                            merge_block: *merge_block,
                        },
                        ctx,
                        func,
                    );
                }
                _ => {
                    self.translate_instruction_with_dest(dest.as_ref(), instr, func);
                }
            }
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

    /// Emit a merge block for Switch, handling PhiSwitch (already on stack from nested if/else)
    fn emit_merge_block_switch(
        &mut self,
        block: &mir::Block,
        skip_phi_switch: bool,
        func: &mut Function,
    ) {
        use wasm_encoder::Instruction as W;

        for (dest, instr) in &block.0 {
            if skip_phi_switch && matches!(instr, mir::Instruction::PhiSwitch(_)) {
                // PhiSwitch value is already on the stack from nested if/else; store to dest
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

    /// Infer the WASM ValType for function arguments by scanning how they are
    /// used in the function body. This resolves `Unknown` argument types that
    /// type inference didn't propagate back to the MIR function signature.
    ///
    /// Returns a map from argument index to inferred ValType.
    /// Infer argument types from function body usage.
    /// Returns a map from arg index to the flattened WASM types.
    /// For multi-word arguments (e.g., tuples loaded via Load), returns multiple ValTypes.
    fn infer_argument_types(
        func: &mir::Function,
    ) -> std::collections::HashMap<usize, Vec<ValType>> {
        use mir::Instruction as I;
        use mir::Value as V;

        let mut arg_types: std::collections::HashMap<usize, Vec<ValType>> =
            std::collections::HashMap::new();

        // Helper: if val is Argument(idx), record it with the given flat types.
        // Multi-word entries take priority over single-word ones because they
        // provide more precise information about the argument's actual shape.
        let mut record_arg = |val: &Arc<V>, flat: Vec<ValType>| {
            if let V::Argument(idx) = val.as_ref() {
                arg_types
                    .entry(*idx)
                    .and_modify(|existing| {
                        // Prefer the wider (multi-word) inference
                        if flat.len() > existing.len() {
                            *existing = flat.clone();
                        }
                    })
                    .or_insert(flat);
            }
        };

        for block in &func.body {
            for (_dest, instr) in &block.0 {
                match instr {
                    // Float arithmetic: operands must be F64
                    I::AddF(a, b)
                    | I::SubF(a, b)
                    | I::MulF(a, b)
                    | I::DivF(a, b)
                    | I::ModF(a, b)
                    | I::PowF(a, b) => {
                        record_arg(a, vec![ValType::F64]);
                        record_arg(b, vec![ValType::F64]);
                    }
                    I::NegF(a)
                    | I::AbsF(a)
                    | I::SqrtF(a)
                    | I::SinF(a)
                    | I::CosF(a)
                    | I::LogF(a) => {
                        record_arg(a, vec![ValType::F64]);
                    }
                    // Integer arithmetic: operands must be I64
                    I::AddI(a, b)
                    | I::SubI(a, b)
                    | I::MulI(a, b)
                    | I::DivI(a, b)
                    | I::ModI(a, b) => {
                        record_arg(a, vec![ValType::I64]);
                        record_arg(b, vec![ValType::I64]);
                    }
                    I::PowI(a) => {
                        record_arg(a, vec![ValType::I64]);
                    }
                    I::NegI(a) | I::AbsI(a) => {
                        record_arg(a, vec![ValType::I64]);
                    }
                    // Cast ops reveal operand types
                    I::CastFtoI(a) => {
                        record_arg(a, vec![ValType::F64]);
                    }
                    I::CastItoF(a) | I::CastItoB(a) => {
                        record_arg(a, vec![ValType::I64]);
                    }
                    // Tagged union operations: the union value is I64
                    I::TaggedUnionGetTag(a) | I::TaggedUnionGetValue(a, _) => {
                        record_arg(a, vec![ValType::I64]);
                    }
                    // GetElement: base is a pointer (I64)
                    I::GetElement { value, .. } => {
                        record_arg(value, vec![ValType::I64]);
                    }
                    // Store: address is I64, source type depends on stored type
                    I::Store(addr, src, ty) => {
                        record_arg(addr, vec![ValType::I64]);
                        let expected = Self::type_to_valtype(&ty.to_type());
                        record_arg(src, vec![expected]);
                    }
                    // Load: detect multi-word types to expand argument params
                    I::Load(val, ty) => {
                        let word_size = ty.word_size();
                        if word_size > 1 {
                            // Multi-word load reveals the actual aggregate shape
                            let flat = Self::flatten_type_to_valtypes(&ty.to_type());
                            record_arg(val, flat);
                        } else {
                            let expected = Self::type_to_valtype(&ty.to_type());
                            record_arg(val, vec![expected]);
                        }
                    }
                    // Call: argument types come from the Call's arg type annotations
                    I::Call(_, args, _) | I::CallIndirect(_, args, _) | I::CallCls(_, args, _) => {
                        for (arg_val, arg_ty) in args {
                            let expected = Self::type_to_valtype(&arg_ty.to_type());
                            record_arg(arg_val, vec![expected]);
                        }
                    }
                    // Return: the returned value should match the function's return type
                    I::Return(val, ty) => {
                        let expected = Self::type_to_valtype(&ty.to_type());
                        record_arg(val, vec![expected]);
                    }
                    _ => {}
                }
            }
        }
        arg_types
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
                        // Comparison operations -> f64 (0.0/1.0) in mimium
                        I::Eq(..) | I::Ne(..) | I::Le(..) | I::Lt(..) | I::Ge(..) | I::Gt(..) => {
                            ValType::F64
                        }
                        // Logical operations -> f64 (0.0/1.0) in mimium
                        I::And(..) | I::Or(..) | I::Not(..) => ValType::F64,
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
                        I::Alloc(type_id) => {
                            let ws = type_id.word_size();
                            self.alloc_registers.insert(*reg_idx, ws);
                            ValType::I64
                        }
                        // GetElement returns pointer (i64), but track it so consumers
                        // can dereference when using the result as a value.
                        I::GetElement {
                            ty, tuple_offset, ..
                        } => {
                            // Extract the element's own type from the composite type
                            let element_vtype = match ty.to_type() {
                                Type::Tuple(elems) => {
                                    let idx = *tuple_offset as usize;
                                    if idx < elems.len() {
                                        Self::type_to_valtype(&elems[idx].to_type())
                                    } else {
                                        ValType::I64
                                    }
                                }
                                Type::Record(fields) => {
                                    let idx = *tuple_offset as usize;
                                    if idx < fields.len() {
                                        Self::type_to_valtype(&fields[idx].ty.to_type())
                                    } else {
                                        ValType::I64
                                    }
                                }
                                _ => ValType::I64,
                            };
                            self.getelement_registers.insert(*reg_idx, element_vtype);
                            ValType::I64
                        }
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
                        // Delay and Mem return f64
                        I::Delay(..) => ValType::F64,
                        I::Mem(..) => ValType::F64,
                        // GetGlobal loads the actual value
                        I::GetGlobal(_, ty) => Self::type_to_valtype(&ty.to_type()),
                        // GetUpValue loads the actual value
                        I::GetUpValue(_, ty) => Self::type_to_valtype(&ty.to_type()),
                        // ReturnFeed is handled separately (is_return)
                        I::ReturnFeed(_, _) => ValType::F64,
                        // Tagged union operations
                        I::TaggedUnionWrap { union_type, .. } => {
                            Self::type_to_valtype(&union_type.to_type())
                        }
                        I::TaggedUnionGetTag(_) => ValType::I64,
                        I::TaggedUnionGetValue(_, _) => ValType::I64, // produces a pointer (address)
                        // PhiSwitch inherits type from its first input
                        I::PhiSwitch(inputs) => {
                            if let Some(first) = inputs.first() {
                                if let mir::Value::Register(r) = first.as_ref() {
                                    self.register_types.get(r).copied().unwrap_or(ValType::F64)
                                } else {
                                    ValType::F64
                                }
                            } else {
                                ValType::F64
                            }
                        }
                        // Switch does not produce a value
                        I::Switch { .. } => ValType::I64,
                        // Array literal produces a pointer
                        I::Array(..) => ValType::I64,
                        // String constant produces a pointer
                        I::String(_) => ValType::I64,
                        // Closure produces an i64 handle
                        I::Closure(_) => ValType::I64,
                        I::MakeClosure { .. } => ValType::I64,
                        // BoxAlloc produces an i64 handle
                        I::BoxAlloc { .. } => ValType::I64,
                        // BoxLoad type depends on inner type
                        I::BoxLoad { inner_type, .. } => {
                            Self::type_to_valtype(&inner_type.to_type())
                        }
                        // GetArrayElem type depends on elem type
                        I::GetArrayElem(_, _, elem_ty) => Self::type_to_valtype(&elem_ty.to_type()),
                        // Default to I64 for unknown/unhandled instructions.
                        // I64 is the safer default since it's the universal word type.
                        _ => ValType::I64,
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

        // Calls to Unit-returning functions produce no value on the WASM stack
        // (both MIR functions and plugin imports strip Unit return types).
        let is_void_call = matches!(
            instr,
            I::Call(_, _, ret_ty) | I::CallIndirect(_, _, ret_ty) | I::CallCls(_, _, ret_ty)
                if matches!(ret_ty.to_type(), Type::Primitive(PType::Unit))
        );

        // Only handle destination for non-return instructions that produce a value
        if !is_return && !is_void_call && Self::instruction_produces_value(instr) {
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
                | I::Return(..)
                | I::ReturnFeed(..)
                | I::JmpIf(..)
                | I::Jmp(..)
                | I::Phi(..)
                | I::PhiSwitch(..)
                | I::Switch { .. }
        )
    }

    /// Export all functions and memory
    fn export_functions(&mut self) -> Result<(), String> {
        // Clone the functions vector to avoid borrow checker issues
        let functions = self.mir.functions.clone();

        // Track the dsp function index if it exists
        let mut dsp_idx = None;
        let mut global_init_idx = None;
        // Track already exported function indices to avoid duplicates
        let mut exported_indices = std::collections::HashSet::new();

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
            // Check if this is the global initializer
            if func.label.as_str() == "_mimium_global" {
                global_init_idx = Some(fn_idx);
            }

            // Only export if not already exported
            if exported_indices.insert(fn_idx) {
                // Use the function name from the symbol interner
                let fn_name = format!("fn_{fn_idx}");
                self.export_section
                    .export(&fn_name, wasm_encoder::ExportKind::Func, fn_idx);
            }
        }

        // Export the dsp function with its canonical name if it exists
        if let Some(idx) = dsp_idx {
            self.export_section
                .export("dsp", wasm_encoder::ExportKind::Func, idx);
        }

        // Export the global initializer as "main" for the runtime to call
        if let Some(idx) = global_init_idx {
            self.export_section
                .export("main", wasm_encoder::ExportKind::Func, idx);
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
                self.emit_value_load_typed(a, ValType::F64, func);
                self.emit_value_load_typed(b, ValType::F64, func);
                func.instruction(&W::F64Add);
            }
            I::SubF(a, b) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                self.emit_value_load_typed(b, ValType::F64, func);
                func.instruction(&W::F64Sub);
            }
            I::MulF(a, b) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                self.emit_value_load_typed(b, ValType::F64, func);
                func.instruction(&W::F64Mul);
            }
            I::DivF(a, b) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                self.emit_value_load_typed(b, ValType::F64, func);
                func.instruction(&W::F64Div);
            }
            I::NegF(a) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                func.instruction(&W::F64Neg);
            }
            I::AbsF(a) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                func.instruction(&W::F64Abs);
            }
            I::SqrtF(a) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                func.instruction(&W::F64Sqrt);
            }
            I::SinF(a) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                func.instruction(&W::Call(self.rt.math_sin));
            }
            I::CosF(a) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                func.instruction(&W::Call(self.rt.math_cos));
            }
            I::LogF(a) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                func.instruction(&W::Call(self.rt.math_log));
            }
            I::PowF(a, b) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                self.emit_value_load_typed(b, ValType::F64, func);
                func.instruction(&W::Call(self.rt.math_pow));
            }
            I::ModF(a, b) => {
                // WASM has no native f64 remainder; compute a - trunc(a/b) * b
                self.emit_value_load_typed(a, ValType::F64, func);
                self.emit_value_load_typed(a, ValType::F64, func);
                self.emit_value_load_typed(b, ValType::F64, func);
                func.instruction(&W::F64Div);
                func.instruction(&W::F64Trunc);
                self.emit_value_load_typed(b, ValType::F64, func);
                func.instruction(&W::F64Mul);
                func.instruction(&W::F64Sub);
            }

            // Integer arithmetic operations
            I::AddI(a, b) => {
                self.emit_value_load_typed(a, ValType::I64, func);
                self.emit_value_load_typed(b, ValType::I64, func);
                func.instruction(&W::I64Add);
            }
            I::SubI(a, b) => {
                self.emit_value_load_typed(a, ValType::I64, func);
                self.emit_value_load_typed(b, ValType::I64, func);
                func.instruction(&W::I64Sub);
            }
            I::MulI(a, b) => {
                self.emit_value_load_typed(a, ValType::I64, func);
                self.emit_value_load_typed(b, ValType::I64, func);
                func.instruction(&W::I64Mul);
            }
            I::DivI(a, b) => {
                self.emit_value_load_typed(a, ValType::I64, func);
                self.emit_value_load_typed(b, ValType::I64, func);
                func.instruction(&W::I64DivS);
            }
            I::ModI(a, b) => {
                self.emit_value_load_typed(a, ValType::I64, func);
                self.emit_value_load_typed(b, ValType::I64, func);
                func.instruction(&W::I64RemS);
            }
            I::NegI(a) => {
                // -x = 0 - x
                func.instruction(&W::I64Const(0));
                self.emit_value_load_typed(a, ValType::I64, func);
                func.instruction(&W::I64Sub);
            }
            I::AbsI(a) => {
                // abs(x) = if x < 0 then -x else x
                // Use: (x ^ (x >> 63)) - (x >> 63)
                self.emit_value_load_typed(a, ValType::I64, func);
                self.emit_value_load_typed(a, ValType::I64, func);
                func.instruction(&W::I64Const(63));
                func.instruction(&W::I64ShrS);
                func.instruction(&W::I64Xor);
                self.emit_value_load_typed(a, ValType::I64, func);
                func.instruction(&W::I64Const(63));
                func.instruction(&W::I64ShrS);
                func.instruction(&W::I64Sub);
            }

            // Logical operations (produce f64 0.0/1.0)
            I::And(a, b) => {
                // Convert both operands to boolean, AND them, convert to f64
                self.emit_value_load_typed(a, ValType::F64, func);
                func.instruction(&W::F64Const(0.0));
                func.instruction(&W::F64Ne);
                self.emit_value_load_typed(b, ValType::F64, func);
                func.instruction(&W::F64Const(0.0));
                func.instruction(&W::F64Ne);
                func.instruction(&W::I32And);
                func.instruction(&W::F64ConvertI32U);
            }
            I::Or(a, b) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                func.instruction(&W::F64Const(0.0));
                func.instruction(&W::F64Ne);
                self.emit_value_load_typed(b, ValType::F64, func);
                func.instruction(&W::F64Const(0.0));
                func.instruction(&W::F64Ne);
                func.instruction(&W::I32Or);
                func.instruction(&W::F64ConvertI32U);
            }
            I::Not(a) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                func.instruction(&W::F64Const(0.0));
                func.instruction(&W::F64Eq);
                func.instruction(&W::F64ConvertI32U);
            }

            // Type cast operations
            I::CastFtoI(a) => {
                self.emit_value_load_typed(a, ValType::F64, func);
                func.instruction(&W::I64TruncF64S);
            }
            I::CastItoF(a) => {
                self.emit_value_load_typed(a, ValType::I64, func);
                func.instruction(&W::F64ConvertI64S);
            }
            I::CastItoB(a) => {
                // i64 -> bool (i64): non-zero = 1, zero = 0
                self.emit_value_load_typed(a, ValType::I64, func);
                func.instruction(&W::I64Const(0));
                func.instruction(&W::I64Ne);
                func.instruction(&W::I64ExtendI32U);
            }

            // Boolean operations (results are i32 in WASM, extended to i64 for register storage)
            // Operands can be f64 or i64; we check operand type to emit correct comparison.
            I::Eq(a, b) => {
                let op_type = self.infer_value_type(a);
                self.emit_value_load_typed(a, op_type, func);
                self.emit_value_load_typed(b, op_type, func);
                if op_type == ValType::F64 {
                    func.instruction(&W::F64Eq);
                } else {
                    func.instruction(&W::I64Eq);
                }
                func.instruction(&W::F64ConvertI32U);
            }
            I::Ne(a, b) => {
                let op_type = self.infer_value_type(a);
                self.emit_value_load_typed(a, op_type, func);
                self.emit_value_load_typed(b, op_type, func);
                if op_type == ValType::F64 {
                    func.instruction(&W::F64Ne);
                } else {
                    func.instruction(&W::I64Ne);
                }
                func.instruction(&W::F64ConvertI32U);
            }
            I::Lt(a, b) => {
                let op_type = self.infer_value_type(a);
                self.emit_value_load_typed(a, op_type, func);
                self.emit_value_load_typed(b, op_type, func);
                if op_type == ValType::F64 {
                    func.instruction(&W::F64Lt);
                } else {
                    func.instruction(&W::I64LtS);
                }
                func.instruction(&W::F64ConvertI32U);
            }
            I::Le(a, b) => {
                let op_type = self.infer_value_type(a);
                self.emit_value_load_typed(a, op_type, func);
                self.emit_value_load_typed(b, op_type, func);
                if op_type == ValType::F64 {
                    func.instruction(&W::F64Le);
                } else {
                    func.instruction(&W::I64LeS);
                }
                func.instruction(&W::F64ConvertI32U);
            }
            I::Gt(a, b) => {
                let op_type = self.infer_value_type(a);
                self.emit_value_load_typed(a, op_type, func);
                self.emit_value_load_typed(b, op_type, func);
                if op_type == ValType::F64 {
                    func.instruction(&W::F64Gt);
                } else {
                    func.instruction(&W::I64GtS);
                }
                func.instruction(&W::F64ConvertI32U);
            }
            I::Ge(a, b) => {
                let op_type = self.infer_value_type(a);
                self.emit_value_load_typed(a, op_type, func);
                self.emit_value_load_typed(b, op_type, func);
                if op_type == ValType::F64 {
                    func.instruction(&W::F64Ge);
                } else {
                    func.instruction(&W::I64GeS);
                }
                func.instruction(&W::F64ConvertI32U);
            }

            // Heap operations - Box operations
            I::BoxAlloc { value, inner_type } => {
                // box_alloc(src_ptr: i32, size_words: i32) -> i64
                // Write value to temp memory, then pass its address
                let size = inner_type.word_size() as u32;
                let size_words = size.max(1);
                let temp_addr = self.mem_layout.alloc_offset;
                self.mem_layout.alloc_offset += size_words * 8;

                // Write value to temp memory
                let val_type = self.infer_value_type(value);
                if size_words > 1 && val_type == ValType::I64 {
                    // Multi-word: copy word-by-word from value pointer
                    for w in 0..size_words {
                        func.instruction(&W::I32Const((temp_addr + w * 8) as i32));
                        self.emit_value_load(value, func);
                        func.instruction(&W::I32WrapI64);
                        func.instruction(&W::I64Load(MemArg {
                            offset: (w * 8) as u64,
                            align: 3,
                            memory_index: 0,
                        }));
                        func.instruction(&W::I64Store(MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));
                    }
                } else {
                    // Single-word: store directly
                    func.instruction(&W::I32Const(temp_addr as i32));
                    self.emit_value_load(value, func);
                    match val_type {
                        ValType::F64 => {
                            func.instruction(&W::I64ReinterpretF64);
                            func.instruction(&W::I64Store(MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));
                        }
                        _ => {
                            func.instruction(&W::I64Store(MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));
                        }
                    };
                }

                func.instruction(&W::I32Const(temp_addr as i32)); // src_ptr
                func.instruction(&W::I32Const(size_words as i32));
                func.instruction(&W::Call(self.rt.box_alloc));
            }

            I::BoxLoad { ptr, inner_type } => {
                // box_load(dst_ptr: i32, obj: i64, size_words: i32)
                // This writes to dst_ptr but returns void.
                // We allocate temp memory and read from it after the call.
                let size = inner_type.word_size() as i32;
                let size_bytes = (size as u32).max(1) * 8;
                let temp_addr = self.mem_layout.alloc_offset;
                self.mem_layout.alloc_offset += size_bytes;

                func.instruction(&W::I32Const(temp_addr as i32)); // dst_ptr
                self.emit_value_load_deref(ptr, ValType::I64, func); // obj: HeapIdx
                func.instruction(&W::I32Const(size));
                func.instruction(&W::Call(self.rt.box_load));

                if inner_type.word_size() > 1 {
                    // Multi-word: return pointer to temp data
                    func.instruction(&W::I64Const(temp_addr as i64));
                } else {
                    // Single-word: read the loaded value from temp memory
                    func.instruction(&W::I32Const(temp_addr as i32));
                    let memarg = MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    };
                    match Self::type_to_valtype(&inner_type.to_type()) {
                        ValType::F64 => func.instruction(&W::F64Load(memarg)),
                        _ => func.instruction(&W::I64Load(memarg)),
                    };
                }
            }

            I::BoxClone { ptr } => {
                // box_clone(obj: i64)
                self.emit_value_load_deref(ptr, ValType::I64, func);
                func.instruction(&W::Call(self.rt.box_clone));
            }

            I::BoxRelease { ptr, inner_type: _ } => {
                // box_release(obj: i64)
                self.emit_value_load_deref(ptr, ValType::I64, func);
                func.instruction(&W::Call(self.rt.box_release));
            }

            I::BoxStore {
                ptr,
                value,
                inner_type,
            } => {
                // box_store(obj: i64, src_ptr: i32, size_words: i32)
                let size = inner_type.word_size() as u32;
                let size_words = size.max(1);
                let temp_addr = self.mem_layout.alloc_offset;
                self.mem_layout.alloc_offset += size_words * 8;

                // Write new value to temp memory
                let val_type = self.infer_value_type(value);
                if size_words > 1 && val_type == ValType::I64 {
                    // Multi-word: copy word-by-word from value pointer
                    for w in 0..size_words {
                        func.instruction(&W::I32Const((temp_addr + w * 8) as i32));
                        self.emit_value_load(value, func);
                        func.instruction(&W::I32WrapI64);
                        func.instruction(&W::I64Load(MemArg {
                            offset: (w * 8) as u64,
                            align: 3,
                            memory_index: 0,
                        }));
                        func.instruction(&W::I64Store(MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));
                    }
                } else {
                    // Single-word: store directly
                    func.instruction(&W::I32Const(temp_addr as i32));
                    self.emit_value_load(value, func);
                    match val_type {
                        ValType::F64 => {
                            func.instruction(&W::I64ReinterpretF64);
                            func.instruction(&W::I64Store(MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));
                        }
                        _ => {
                            func.instruction(&W::I64Store(MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));
                        }
                    };
                }

                self.emit_value_load_deref(ptr, ValType::I64, func); // obj: HeapIdx
                func.instruction(&W::I32Const(temp_addr as i32)); // src_ptr
                func.instruction(&W::I32Const(size_words as i32));
                func.instruction(&W::Call(self.rt.box_store));
            }

            // Heap operations - Closure operations
            I::MakeClosure { fn_proto, size: _ } => {
                // Allocate closure in linear memory: [fn_table_idx: i64] [upval0: i64] ... [upvalN: i64]
                // Uses runtime bump allocator so each recursive invocation gets a unique address.
                let mir_fn_idx = match fn_proto.as_ref() {
                    mir::Value::Function(idx) => *idx,
                    mir::Value::Register(reg_idx) => {
                        self.register_constants.get(reg_idx).copied().unwrap_or(0) as usize
                    }
                    _ => 0,
                };
                let mir = self.mir.clone();
                let upindexes = if mir_fn_idx < mir.functions.len() {
                    mir.functions[mir_fn_idx].upindexes.clone()
                } else {
                    vec![]
                };
                let num_upvalues = upindexes.len();
                let closure_size_bytes = ((1 + num_upvalues) as u32) * 8;

                // Runtime allocation: base address saved in alloc_base_local
                self.emit_runtime_alloc(closure_size_bytes, func);

                // Store function table index at base (table idx = MIR fn idx)
                func.instruction(&W::LocalGet(self.alloc_base_local));
                func.instruction(&W::I64Const(mir_fn_idx as i64));
                func.instruction(&W::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));

                // Capture upvalues into the closure.
                //
                // In WASM we cannot reference the WASM value stack from
                // outside the function, so closures are immediately "closed":
                // upvalues originating from single-word `alloc` cells store
                // the **alloc pointer** (not the dereferenced value).  This
                // gives all closures sharing the same alloc cell a live
                // reference to the same mutable storage — essential for
                // `letrec` self-references and shared mutable state.
                //
                // Multi-word allocs (tuples, etc.) already store pointers
                // (the alloc address IS the data address) and need no extra
                // indirection at GetUpValue time.
                //
                // `GetUpValue` / `SetUpValue` check `indirect_upvalues` to
                // decide whether an extra dereference is needed.
                let mut is_indirect = vec![false; upindexes.len()];
                for (i, upindex) in upindexes.iter().enumerate() {
                    let upval_byte_offset = ((1 + i) as u32) * 8;
                    // Push address: base + offset
                    func.instruction(&W::LocalGet(self.alloc_base_local));
                    func.instruction(&W::I32Const(upval_byte_offset as i32));
                    func.instruction(&W::I32Add);
                    match upindex.as_ref() {
                        mir::Value::Register(reg_idx)
                            if self.alloc_registers.get(reg_idx).copied().unwrap_or(0) == 1 =>
                        {
                            // Single-word alloc: store the alloc pointer so the
                            // closure shares the mutable cell.  GetUpValue will
                            // dereference through the pointer.
                            is_indirect[i] = true;
                            self.emit_value_load(upindex, func);
                        }
                        mir::Value::Register(reg_idx)
                            if self.alloc_registers.contains_key(reg_idx) =>
                        {
                            // Multi-word alloc (tuple etc.): the alloc address
                            // IS the data address; store as-is, no indirection.
                            self.emit_value_load(upindex, func);
                        }
                        _ => {
                            // Direct value (non-alloc register, argument, etc.)
                            let val_type = self.infer_value_type(upindex);
                            self.emit_value_load(upindex, func);
                            if val_type == ValType::F64 {
                                func.instruction(&W::I64ReinterpretF64);
                            }
                        }
                    }
                    func.instruction(&W::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                self.indirect_upvalues.insert(mir_fn_idx, is_indirect);

                // Push closure address as the result (i64)
                func.instruction(&W::LocalGet(self.alloc_base_local));
                func.instruction(&W::I64ExtendI32U);
            }

            I::CloseHeapClosure(_ptr) => {
                // No-op in WASM: upvalues are already in linear memory
            }

            I::CloneHeap(_ptr) => {
                // No-op in WASM: linear memory closures don't need refcounting
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
                let temp_addr = self.mem_layout.state_temp_base;
                self.mem_layout.state_temp_base += size_bytes;

                // Call state_get(dst_ptr: i32, size_words: i32) to fill temp memory
                func.instruction(&W::I32Const(temp_addr as i32));
                func.instruction(&W::I32Const(size));
                func.instruction(&W::Call(self.rt.state_get));

                // Push the temp address as i64 so subsequent Load can read from it
                func.instruction(&W::I64Const(temp_addr as i64));
            }

            I::ReturnFeed(value, ty) => {
                // Persist state value, then return it.
                let size = ty.word_size() as i32;

                if size <= 1 {
                    // Single-word value: store to temp memory, then state_set from temp
                    let size_bytes = 8u32;
                    let temp_addr = self.mem_layout.state_temp_base;
                    self.mem_layout.state_temp_base += size_bytes;

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
                } else {
                    // Multi-word value: value is a pointer to linear memory data.
                    // Pass the pointer directly to state_set (data is already in memory).
                    self.emit_value_load(value, func);
                    func.instruction(&W::I32WrapI64); // convert i64 ptr to i32
                    func.instruction(&W::I32Const(size));
                    func.instruction(&W::Call(self.rt.state_set));
                }

                // Entry functions restore the alloc pointer before returning.
                if self.is_entry_function {
                    func.instruction(&W::LocalGet(self.alloc_ptr_save_local));
                    func.instruction(&W::GlobalSet(self.alloc_ptr_global));
                }

                // Push the return value onto the stack (ReturnFeed acts as Return)
                self.emit_value_load(value, func);
            }

            I::Delay(len, input, time) => {
                // state_delay(input: f64, time: f64, max_len: i64) -> f64
                self.emit_value_load_typed(input, ValType::F64, func);
                self.emit_value_load_typed(time, ValType::F64, func);
                func.instruction(&W::I64Const(*len as i64));
                func.instruction(&W::Call(self.rt.state_delay));
            }

            I::Mem(value) => {
                // state_mem(input: f64) -> f64
                self.emit_value_load_typed(value, ValType::F64, func);
                func.instruction(&W::Call(self.rt.state_mem));
            }

            // Array operations
            I::Array(values, ty) => {
                // array_alloc(src_ptr: i64, size_words: i32) -> i64
                // Reads `size_words` words from linear memory at `src_ptr` and creates an array.
                let elem_ty = match ty.to_type() {
                    Type::Array(elem) => elem,
                    _ => *ty, // fallback
                };
                let elem_size = elem_ty.word_size() as u32;
                let total_words = values.len() as u32 * elem_size.max(1);
                let total_bytes = total_words * 8;

                // Allocate temp memory region for element data
                let temp_addr = self.mem_layout.alloc_offset;
                self.mem_layout.alloc_offset += total_bytes;

                // Write each element's data to linear memory
                for (i, val) in values.iter().enumerate() {
                    let base_offset = temp_addr + (i as u32) * elem_size.max(1) * 8;
                    if elem_size > 1 {
                        // Multi-word element (e.g. tuple): copy data word-by-word
                        // from the pointer held by the value register.
                        for w in 0..elem_size {
                            func.instruction(&W::I32Const((base_offset + w * 8) as i32));
                            self.emit_value_load(val, func);
                            func.instruction(&W::I32WrapI64);
                            func.instruction(&W::I64Load(MemArg {
                                offset: (w * 8) as u64,
                                align: 3,
                                memory_index: 0,
                            }));
                            func.instruction(&W::I64Store(MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));
                        }
                    } else {
                        // Single-word element: store directly
                        func.instruction(&W::I32Const(base_offset as i32));
                        let elem_valtype = Self::type_to_valtype(&elem_ty.to_type());
                        self.emit_value_load_typed(val, elem_valtype, func);
                        match elem_valtype {
                            ValType::F64 => func.instruction(&W::F64Store(MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            })),
                            _ => func.instruction(&W::I64Store(MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            })),
                        };
                    }
                }

                // Call array_alloc(src_ptr, total_words)
                func.instruction(&W::I64Const(temp_addr as i64)); // src_ptr
                func.instruction(&W::I32Const(total_words as i32)); // size_words
                func.instruction(&W::Call(self.rt.array_alloc));
            }

            I::GetArrayElem(array, index, elem_ty) => {
                // array_get_elem(dst_ptr: i32, arr: i64, index: i64, elem_size_words: i32)
                // This writes to dst_ptr but returns void.
                // We allocate temp memory and read from it after the call.
                let elem_size = elem_ty.word_size() as i32;
                let size_bytes = (elem_size as u32).max(1) * 8;
                let temp_addr = self.mem_layout.alloc_offset;
                self.mem_layout.alloc_offset += size_bytes;

                func.instruction(&W::I32Const(temp_addr as i32)); // dst_ptr
                self.emit_value_load_typed(array, ValType::I64, func); // array handle
                self.emit_value_load_as_numeric_i64(index, func); // index: use numeric truncation, not bit reinterpret
                func.instruction(&W::I32Const(elem_size));
                func.instruction(&W::Call(self.rt.array_get_elem));

                if elem_size > 1 {
                    // Multi-word element: return pointer to temp memory.
                    // Subsequent GetElement/Load will offset from this address.
                    func.instruction(&W::I64Const(temp_addr as i64));
                } else {
                    // Single-word element: load the value from temp memory
                    func.instruction(&W::I32Const(temp_addr as i32));
                    let memarg = MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    };
                    match Self::type_to_valtype(&elem_ty.to_type()) {
                        ValType::F64 => func.instruction(&W::F64Load(memarg)),
                        _ => func.instruction(&W::I64Load(memarg)),
                    };
                }
            }

            I::SetArrayElem(array, index, _value, elem_ty) => {
                // array_set_elem(arr: i64, index: i64, src_ptr: i32, elem_size_words: i32)
                let elem_size = elem_ty.word_size() as i32;
                self.emit_value_load_typed(array, ValType::I64, func);
                self.emit_value_load_as_numeric_i64(index, func);
                func.instruction(&W::I32Const(0)); // src_ptr placeholder
                func.instruction(&W::I32Const(elem_size));
                func.instruction(&W::Call(self.rt.array_set_elem));
            }

            // Memory operations
            I::Alloc(ty) => {
                // Allocate space in linear memory using the runtime bump allocator.
                // This ensures each function invocation (including recursive ones)
                // gets a unique address for its local allocations.
                let size_bytes = (ty.word_size() as u32).max(1) * 8;
                self.emit_runtime_alloc(size_bytes, func);
                func.instruction(&W::LocalGet(self.alloc_base_local));
                func.instruction(&W::I64ExtendI32U);
            }

            I::Load(ptr, ty) => {
                // Load a value from a pointer
                let is_scalar = ty.word_size() <= 1;
                match ptr.as_ref() {
                    mir::Value::Argument(arg_idx) => {
                        let is_multi_word = self
                            .current_arg_map
                            .get(*arg_idx)
                            .is_some_and(|&(_, wc)| wc > 1);
                        if is_multi_word {
                            // Multi-word arg was materialized to a pointer by emit_value_load;
                            // if we're loading a scalar from the tuple, dereference the pointer.
                            if is_scalar {
                                self.emit_value_load(ptr, func);
                                func.instruction(&W::I32WrapI64);
                                let memarg = MemArg {
                                    offset: 0,
                                    align: 3,
                                    memory_index: 0,
                                };
                                match Self::type_to_valtype(&ty.to_type()) {
                                    ValType::F64 => func.instruction(&W::F64Load(memarg)),
                                    _ => func.instruction(&W::I64Load(memarg)),
                                };
                            } else {
                                // Loading the entire tuple — return the pointer as-is
                                self.emit_value_load(ptr, func);
                            }
                        } else {
                            // Single-word arg: the value is direct
                            self.emit_value_load(ptr, func);
                        }
                    }
                    _ => {
                        if is_scalar {
                            // Scalar: dereference the pointer in linear memory
                            self.emit_value_load(ptr, func);
                            func.instruction(&W::I32WrapI64);
                            let memarg = MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            };
                            let expected_type = Self::type_to_valtype(&ty.to_type());
                            match expected_type {
                                ValType::F64 => {
                                    func.instruction(&W::F64Load(memarg));
                                }
                                _ => {
                                    func.instruction(&W::I64Load(memarg));
                                }
                            }
                        } else {
                            // Multi-word type: the address IS the value (pointer to aggregate).
                            // Don't dereference; GetElement will offset from this address.
                            self.emit_value_load(ptr, func);
                        }
                    }
                }
            }

            I::Store(dst, src, ty) => {
                // Store a value to a memory address
                let word_size = ty.word_size() as u32;
                if word_size > 1 {
                    // Multi-word store: copy each word from src address to dst address
                    // Both src and dst are pointers (I64) to linear memory.
                    for i in 0..word_size {
                        let byte_offset = (i * 8) as u64;
                        // Push destination address (i32) for the store
                        self.emit_value_load(dst, func);
                        func.instruction(&W::I32WrapI64);
                        // Load source word from src address + offset
                        self.emit_value_load(src, func);
                        func.instruction(&W::I32WrapI64);
                        let load_memarg = MemArg {
                            offset: byte_offset,
                            align: 3,
                            memory_index: 0,
                        };
                        func.instruction(&W::I64Load(load_memarg));
                        // Store to dst address + offset
                        let store_memarg = MemArg {
                            offset: byte_offset,
                            align: 3,
                            memory_index: 0,
                        };
                        func.instruction(&W::I64Store(store_memarg));
                    }
                } else {
                    // Single-word store
                    // Stack order for f64.store/i64.store: [i32_addr, value]
                    let expected_vtype = Self::type_to_valtype(&ty.to_type());
                    self.emit_value_load(dst, func);
                    func.instruction(&W::I32WrapI64);
                    self.emit_value_load_typed(src, expected_vtype, func);
                    let memarg = MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    };
                    match expected_vtype {
                        ValType::F64 => {
                            func.instruction(&W::F64Store(memarg));
                        }
                        _ => {
                            func.instruction(&W::I64Store(memarg));
                        }
                    }
                }
            }

            I::GetElement {
                value,
                ty,
                tuple_offset,
            } => {
                // Compute address of a field within a composite type.
                // The byte offset must account for the actual word sizes of
                // all preceding elements. We use word_size() (not
                // flatten_type_to_valtypes) because memory layout follows
                // word_size: e.g. a UserSum is 2 words (tag + payload) in
                // memory even though it flattens to a single I64 pointer
                // when passed as a function parameter.
                self.emit_value_load(value, func);
                if *tuple_offset > 0 {
                    let composite_ty = ty.to_type();
                    let offset_bytes = match &composite_ty {
                        Type::Tuple(elems) => {
                            let word_offset: u64 = elems[0..(*tuple_offset as usize)]
                                .iter()
                                .map(|e| e.word_size() as u64)
                                .sum();
                            (word_offset * 8) as i64
                        }
                        Type::Record(fields) => {
                            let word_offset: u64 = fields[0..(*tuple_offset as usize)]
                                .iter()
                                .map(|f| f.ty.word_size() as u64)
                                .sum();
                            (word_offset * 8) as i64
                        }
                        _ => (*tuple_offset * 8) as i64,
                    };
                    func.instruction(&W::I64Const(offset_bytes));
                    func.instruction(&W::I64Add);
                }
            }

            // Function calls
            I::Call(fn_ptr, args, ret_ty) => {
                // Check if this is an ext function with multi-word return type
                // (e.g. split_head/split_tail returning a tuple).
                // Such functions use a dest-pointer calling convention:
                // the codegen allocates temp space, passes it as an extra arg,
                // and the host writes the result there.
                if let mir::Value::ExtFunction(name, _fn_ty) = fn_ptr.as_ref() {
                    let ret_words = ret_ty.word_size() as u32;
                    if ret_words > 1 {
                        // Multi-word return: dest-pointer convention
                        let temp_addr = self.mem_layout.alloc_offset;
                        self.mem_layout.alloc_offset += ret_words * 8;

                        if let Some(import_idx) = self.resolve_ext_function(name) {
                            // Load normal args
                            for (arg, ty) in args {
                                let expected = Self::type_to_valtype(&ty.to_type());
                                self.emit_value_load_typed(arg, expected, func);
                            }
                            // Push dest pointer as extra argument
                            func.instruction(&W::I32Const(temp_addr as i32));
                            func.instruction(&W::Call(import_idx));
                        } else {
                            eprintln!(
                                "Warning: Unknown ext function (multi-word): {}",
                                name.as_str()
                            );
                            // Don't load arguments for unknown functions
                        }
                        // Push temp address as I64 — this is the tuple pointer result
                        func.instruction(&W::I64Const(temp_addr as i64));
                    } else {
                        // Single-word return ExtFunction: standard call
                        if let Some(import_idx) = self.resolve_ext_function(name) {
                            // Load arguments and make the call
                            for (arg, ty) in args {
                                let expected = Self::type_to_valtype(&ty.to_type());
                                self.emit_value_load_typed(arg, expected, func);
                            }
                            func.instruction(&W::Call(import_idx));
                        } else {
                            // Unknown external function - don't load args, just push placeholder
                            eprintln!(
                                "Warning: Unknown external function in Call: {}",
                                name.as_str()
                            );
                            // Push placeholder value (skip for Unit returns)
                            if !matches!(ret_ty.to_type(), Type::Primitive(PType::Unit)) {
                                match Self::type_to_valtype(&ret_ty.to_type()) {
                                    ValType::F64 => func.instruction(&W::F64Const(0.0)),
                                    ValType::I64 => func.instruction(&W::I64Const(0)),
                                    _ => func.instruction(&W::I32Const(0)),
                                };
                            }
                        }
                    }
                } else {
                    // Non-ext function call: load args and call by index.
                    // Direct calls share the caller's state storage, navigated
                    // by MIR's PushStateOffset/PopStateOffset instructions.
                    // Only closure calls (CallCls/CallIndirect) switch state context.
                    let wasm_fn_idx = match fn_ptr.as_ref() {
                        mir::Value::Function(fn_idx) => {
                            let wasm_idx = *fn_idx as u32 + self.num_imports;
                            log::debug!("Calling function idx={fn_idx}");
                            wasm_idx
                        }
                        mir::Value::Register(reg_idx) => {
                            if let Some(const_val) = self.register_constants.get(reg_idx) {
                                let fn_idx = *const_val as usize;
                                let wasm_idx = *const_val as u32 + self.num_imports;
                                log::debug!(
                                    "Calling function (via register) idx={fn_idx}"
                                );
                                wasm_idx
                            } else {
                                eprintln!(
                                    "Warning: Indirect call through register without constant value"
                                );
                                self.current_fn_idx
                            }
                        }
                        _ => self.current_fn_idx,
                    };

                    // Load arguments with tuple flattening and make the call
                    self.emit_call_args_flattened(args, func);
                    func.instruction(&W::Call(wasm_fn_idx));
                }
            }

            I::CallIndirect(closure_ptr, args, ret_ty) => {
                // Check if this is an external function call or a closure call
                match closure_ptr.as_ref() {
                    mir::Value::ExtFunction(name, _fn_ty) => {
                        // External function call - map to runtime imports
                        if let Some(import_idx) = self.resolve_ext_function(name) {
                            // Load arguments first with type coercion
                            for (arg, ty) in args {
                                let expected = Self::type_to_valtype(&ty.to_type());
                                self.emit_value_load_typed(arg, expected, func);
                            }
                            func.instruction(&W::Call(import_idx));
                        } else {
                            // Unknown external function - don't load args, just push placeholder
                            eprintln!("Warning: Unknown external function: {}", name.as_str());
                            if !matches!(ret_ty.to_type(), Type::Primitive(PType::Unit)) {
                                match Self::type_to_valtype(&ret_ty.to_type()) {
                                    ValType::F64 => func.instruction(&W::F64Const(0.0)),
                                    ValType::I64 => func.instruction(&W::I64Const(0)),
                                    _ => func.instruction(&W::I32Const(0)),
                                };
                            }
                        }
                    }
                    _ => {
                        // Closure call via WASM call_indirect through the function table
                        let memarg = MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        };

                        // Compute state size for the callee closure
                        let state_size = self
                            .resolve_mir_fn_idx(closure_ptr.as_ref())
                            .map(|idx| self.get_mir_fn_state_size(idx))
                            .unwrap_or(64); // conservative default for dynamic calls

                        // Push closure state onto the state stack
                        self.emit_closure_state_push(closure_ptr, state_size, func);

                        // Save current closure_self_ptr to local
                        func.instruction(&W::I32Const(0)); // CLOSURE_SELF_PTR_ADDR
                        func.instruction(&W::I64Load(memarg));
                        func.instruction(&W::LocalSet(self.closure_save_local));

                        // Set closure_self_ptr to the new closure
                        func.instruction(&W::I32Const(0)); // CLOSURE_SELF_PTR_ADDR
                        self.emit_value_load(closure_ptr, func);
                        func.instruction(&W::I64Store(memarg));

                        // Push arguments with tuple flattening
                        self.emit_call_args_flattened(args, func);

                        // Load function table index from closure[0]
                        self.emit_value_load(closure_ptr, func);
                        func.instruction(&W::I32WrapI64);
                        func.instruction(&W::I64Load(memarg));
                        func.instruction(&W::I32WrapI64); // table index must be i32

                        // Compute call_indirect type from the call site signature
                        let param_types: Vec<ValType> = args
                            .iter()
                            .flat_map(|(_, ty)| Self::flatten_type_to_valtypes(&ty.to_type()))
                            .collect();
                        let return_types: Vec<ValType> = {
                            let ty = ret_ty.to_type();
                            if matches!(ty, Type::Primitive(PType::Unit)) {
                                vec![]
                            } else {
                                vec![Self::type_to_valtype(&ty)]
                            }
                        };
                        let type_idx = self.get_or_create_call_type(param_types, return_types);
                        func.instruction(&W::CallIndirect {
                            type_index: type_idx,
                            table_index: 0,
                        });

                        // Restore the previous closure_self_ptr
                        func.instruction(&W::I32Const(0)); // CLOSURE_SELF_PTR_ADDR
                        func.instruction(&W::LocalGet(self.closure_save_local));
                        func.instruction(&W::I64Store(memarg));

                        // Pop closure state from the state stack
                        self.emit_closure_state_pop(func);
                    }
                }
            }

            // Control flow
            I::Return(value, ty) => {
                // In WASM, functions implicitly return the value on the stack when they reach 'end'
                let is_unit = matches!(ty.to_type(), Type::Primitive(PType::Unit));

                // Entry functions restore the alloc pointer before returning.
                if self.is_entry_function {
                    func.instruction(&W::LocalGet(self.alloc_ptr_save_local));
                    func.instruction(&W::GlobalSet(self.alloc_ptr_global));
                }

                if !is_unit {
                    let expected = Self::type_to_valtype(&ty.to_type());
                    self.emit_value_load_typed(value, expected, func);
                }
            }

            // String constants
            I::String(_sym) => {
                // String constant - return pointer to string in data section as i64
                // TODO: Allocate strings in data section and return address
                func.instruction(&W::I64Const(0)); // placeholder pointer
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
                let expected_vtype = Self::type_to_valtype(&ty.to_type());
                // Check if the value register comes from GetElement (holds a pointer,
                // not the value itself). In such cases, dereference the pointer.
                let is_getelement = matches!(value.as_ref(), mir::Value::Register(r) if self.getelement_registers.contains_key(r));
                func.instruction(&W::I32Const(offset as i32));
                if is_getelement {
                    if ty.word_size() > 1 {
                        // Multi-word aggregate: GetElement produced a pointer
                        // to the sub-aggregate in the parent's allocation.
                        // Store the pointer as-is without dereferencing.
                        self.emit_value_load(value, func);
                    } else {
                        // Scalar element: GetElement produced a pointer to
                        // where the scalar lives. Dereference to get the value.
                        self.emit_value_load(value, func);
                        func.instruction(&W::I32WrapI64);
                        let deref_memarg = MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        };
                        match expected_vtype {
                            ValType::F64 => func.instruction(&W::F64Load(deref_memarg)),
                            _ => func.instruction(&W::I64Load(deref_memarg)),
                        };
                    }
                } else {
                    let actual_vtype = self.infer_value_type(value);
                    if actual_vtype == ValType::I64 && expected_vtype == ValType::F64 {
                        // Value is a pointer (e.g., from alloc), dereference to get f64
                        self.emit_value_load(value, func);
                        func.instruction(&W::I32WrapI64);
                        let deref_memarg = MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        };
                        func.instruction(&W::F64Load(deref_memarg));
                    } else if actual_vtype == ValType::F64 && expected_vtype == ValType::I64 {
                        // Value is f64 but destination expects i64 - reinterpret bits
                        self.emit_value_load(value, func);
                        func.instruction(&W::I64ReinterpretF64);
                    } else {
                        self.emit_value_load(value, func);
                    }
                }
                let memarg = MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };
                match expected_vtype {
                    ValType::F64 => func.instruction(&W::F64Store(memarg)),
                    _ => func.instruction(&W::I64Store(memarg)),
                };
            }

            // Upvalue access (closure captured variables)
            I::GetUpValue(idx, ty) => {
                // Load closure_self_ptr from fixed memory location (addr 0)
                let memarg = MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };
                func.instruction(&W::I32Const(0)); // CLOSURE_SELF_PTR_ADDR
                func.instruction(&W::I64Load(memarg));
                // Add offset: skip fn_index word (8 bytes) + idx * 8
                let byte_offset = 8 + (*idx as i64) * 8;
                func.instruction(&W::I64Const(byte_offset));
                func.instruction(&W::I64Add);
                func.instruction(&W::I32WrapI64);
                // Load the raw slot value (always i64 in the closure struct)
                func.instruction(&W::I64Load(memarg));

                // If this upvalue is indirect (alloc pointer), dereference once
                // more to reach the actual value stored in the alloc cell.
                let is_indirect = self
                    .indirect_upvalues
                    .get(&self.current_mir_fn_idx)
                    .and_then(|v| v.get(*idx as usize))
                    .copied()
                    .unwrap_or(false);
                if is_indirect {
                    // The slot holds an alloc cell address; load through it.
                    func.instruction(&W::I32WrapI64);
                    func.instruction(&W::I64Load(memarg));
                }

                // Final type conversion
                if Self::type_to_valtype(&ty.to_type()) == ValType::F64 {
                    func.instruction(&W::F64ReinterpretI64);
                }
            }

            I::SetUpValue(idx, value, ty) => {
                // Store to upvalue slot in the current closure.
                let memarg = MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };

                // Check if this upvalue is indirect (alloc pointer).
                let is_indirect = self
                    .indirect_upvalues
                    .get(&self.current_mir_fn_idx)
                    .and_then(|v| v.get(*idx as usize))
                    .copied()
                    .unwrap_or(false);

                // Compute the store target address.
                // closure_self_ptr + 8 + idx * 8
                func.instruction(&W::I32Const(0)); // CLOSURE_SELF_PTR_ADDR
                func.instruction(&W::I64Load(memarg));
                let byte_offset = 8 + (*idx as i64) * 8;
                func.instruction(&W::I64Const(byte_offset));
                func.instruction(&W::I64Add);
                func.instruction(&W::I32WrapI64);

                if is_indirect {
                    // The slot holds an alloc cell address; load the pointer,
                    // then store the value through it.
                    func.instruction(&W::I64Load(memarg));
                    func.instruction(&W::I32WrapI64);
                }

                // Push value (always as i64 for uniform storage)
                let expected = Self::type_to_valtype(&ty.to_type());
                self.emit_value_load_typed(value, expected, func);
                if expected == ValType::F64 {
                    func.instruction(&W::I64ReinterpretF64);
                }
                func.instruction(&W::I64Store(memarg));
            }

            // Closure (old-style, non-heap) - same mechanism as MakeClosure
            I::Closure(fn_ptr) => {
                // Resolve target MIR function index
                let mir_fn_idx = match fn_ptr.as_ref() {
                    mir::Value::Function(idx) => *idx,
                    mir::Value::Register(reg_idx) => {
                        self.register_constants.get(reg_idx).copied().unwrap_or(0) as usize
                    }
                    _ => 0,
                };
                let mir = self.mir.clone();
                let upindexes = if mir_fn_idx < mir.functions.len() {
                    mir.functions[mir_fn_idx].upindexes.clone()
                } else {
                    vec![]
                };
                let num_upvalues = upindexes.len();
                let closure_size_bytes = ((1 + num_upvalues) as u32) * 8;

                // Runtime allocation
                self.emit_runtime_alloc(closure_size_bytes, func);

                // Store function table index
                func.instruction(&W::LocalGet(self.alloc_base_local));
                func.instruction(&W::I64Const(mir_fn_idx as i64));
                func.instruction(&W::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));

                // Capture upvalues
                for (i, upindex) in upindexes.iter().enumerate() {
                    let upval_byte_offset = ((1 + i) as u32) * 8;
                    func.instruction(&W::LocalGet(self.alloc_base_local));
                    func.instruction(&W::I32Const(upval_byte_offset as i32));
                    func.instruction(&W::I32Add);
                    match upindex.as_ref() {
                        mir::Value::Register(reg_idx)
                            if self.alloc_registers.get(reg_idx).copied().unwrap_or(0) == 1 =>
                        {
                            // Single-word alloc: dereference
                            self.emit_value_load(upindex, func);
                            func.instruction(&W::I32WrapI64);
                            func.instruction(&W::I64Load(MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));
                        }
                        mir::Value::Register(reg_idx)
                            if self.alloc_registers.contains_key(reg_idx) =>
                        {
                            // Multi-word alloc (tuple etc.): store pointer as-is
                            self.emit_value_load(upindex, func);
                        }
                        _ => {
                            let val_type = self.infer_value_type(upindex);
                            self.emit_value_load(upindex, func);
                            if val_type == ValType::F64 {
                                func.instruction(&W::I64ReinterpretF64);
                            }
                        }
                    }
                    func.instruction(&W::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }

                // Push closure address as result (i64)
                func.instruction(&W::LocalGet(self.alloc_base_local));
                func.instruction(&W::I64ExtendI32U);
            }

            I::CloseUpValues(_src, _ty) => {
                // No-op in WASM: upvalues are already in linear memory
            }

            // CallCls (closure call variant)
            I::CallCls(fn_ptr, args, ret_ty) => {
                match fn_ptr.as_ref() {
                    mir::Value::ExtFunction(name, _fn_ty) => {
                        for (arg, ty) in args {
                            let expected = Self::type_to_valtype(&ty.to_type());
                            self.emit_value_load_typed(arg, expected, func);
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
                        // Closure call via WASM call_indirect (same as CallIndirect)
                        let memarg = MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        };

                        // Compute state size for the callee closure
                        let state_size = self
                            .resolve_mir_fn_idx(fn_ptr.as_ref())
                            .map(|idx| self.get_mir_fn_state_size(idx))
                            .unwrap_or(64); // conservative default

                        // Push closure state onto the state stack
                        self.emit_closure_state_push(fn_ptr, state_size, func);

                        // Save current closure_self_ptr
                        func.instruction(&W::I32Const(0));
                        func.instruction(&W::I64Load(memarg));
                        func.instruction(&W::LocalSet(self.closure_save_local));

                        // Set closure_self_ptr to the new closure
                        func.instruction(&W::I32Const(0));
                        self.emit_value_load(fn_ptr, func);
                        func.instruction(&W::I64Store(memarg));

                        // Push arguments with tuple flattening
                        self.emit_call_args_flattened(args, func);

                        // Load function table index from closure[0]
                        self.emit_value_load(fn_ptr, func);
                        func.instruction(&W::I32WrapI64);
                        func.instruction(&W::I64Load(memarg));
                        func.instruction(&W::I32WrapI64);

                        // Compute call_indirect type
                        let param_types: Vec<ValType> = args
                            .iter()
                            .flat_map(|(_, ty)| Self::flatten_type_to_valtypes(&ty.to_type()))
                            .collect();
                        let return_types: Vec<ValType> = {
                            let ty = ret_ty.to_type();
                            if matches!(ty, Type::Primitive(PType::Unit)) {
                                vec![]
                            } else {
                                vec![Self::type_to_valtype(&ty)]
                            }
                        };
                        let type_idx = self.get_or_create_call_type(param_types, return_types);
                        func.instruction(&W::CallIndirect {
                            type_index: type_idx,
                            table_index: 0,
                        });

                        // Restore closure_self_ptr
                        func.instruction(&W::I32Const(0));
                        func.instruction(&W::LocalGet(self.closure_save_local));
                        func.instruction(&W::I64Store(memarg));

                        // Pop closure state from the state stack
                        self.emit_closure_state_pop(func);
                    }
                }
            }

            // Placeholder for unimplemented instructions
            //
            // Tagged union operations
            I::TaggedUnionWrap {
                tag,
                value,
                union_type,
            } => {
                // Store tag and value inline into temp memory, return pointer.
                // Layout: [tag: i64] [value_word_0] [value_word_1] ...
                let size_bytes = (union_type.word_size() as u32).max(2) * 8;
                let temp_addr = self.mem_layout.alloc_offset;
                self.mem_layout.alloc_offset += size_bytes;

                // Store tag to first word
                let memarg = MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };
                func.instruction(&W::I32Const(temp_addr as i32));
                func.instruction(&W::I64Const(*tag as i64));
                func.instruction(&W::I64Store(memarg));

                // Determine variant payload word size from the union type and tag
                let tag_idx = *tag as usize;
                let variant_word_size: u32 = match union_type.to_type() {
                    Type::UserSum { variants, .. } => variants
                        .get(tag_idx)
                        .and_then(|(_, payload_ty)| *payload_ty)
                        .map(|t| t.word_size() as u32)
                        .unwrap_or(0),
                    Type::Union(variants) => variants
                        .get(tag_idx)
                        .map(|t| t.word_size() as u32)
                        .unwrap_or(1),
                    _ => 1,
                };

                if variant_word_size > 1 {
                    // Multi-word value: the value register holds a pointer to aggregate data.
                    // Copy each word inline into the union's value area.
                    for i in 0..variant_word_size {
                        func.instruction(&W::I32Const(temp_addr as i32));
                        self.emit_value_load(value, func);
                        func.instruction(&W::I32WrapI64);
                        func.instruction(&W::I64Load(MemArg {
                            offset: (i * 8) as u64,
                            align: 3,
                            memory_index: 0,
                        }));
                        func.instruction(&W::I64Store(MemArg {
                            offset: 8 + (i * 8) as u64,
                            align: 3,
                            memory_index: 0,
                        }));
                    }
                } else {
                    // Single-word value: store directly
                    let memarg_val = MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    };
                    func.instruction(&W::I32Const(temp_addr as i32));
                    self.emit_value_load(value, func);
                    match self.infer_value_type(value) {
                        ValType::F64 => func.instruction(&W::F64Store(memarg_val)),
                        _ => func.instruction(&W::I64Store(memarg_val)),
                    };
                }

                // Return pointer to the tagged union
                func.instruction(&W::I64Const(temp_addr as i64));
            }

            I::TaggedUnionGetTag(ptr) => {
                // Load tag (first word) from tagged union pointer
                self.emit_value_load(ptr, func);
                func.instruction(&W::I32WrapI64);
                let memarg = MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };
                func.instruction(&W::I64Load(memarg));
            }

            I::TaggedUnionGetValue(ptr, _ty) => {
                // Compute address of value field (ptr + 8), but do NOT load.
                // The subsequent I::Load instruction will dereference this address.
                self.emit_value_load(ptr, func);
                func.instruction(&W::I64Const(8));
                func.instruction(&W::I64Add);
            }

            _ => {
                // Unimplemented instructions - no-op
            }
        }
    }

    /// Helper: Load a scalar value, dereferencing if the source is a GetElement register.
    ///
    /// GetElement registers hold addresses in linear memory (not the value itself).
    /// When a consumer needs the actual value (e.g., BoxLoad needs the HeapIdx),
    /// this method dereferences the address to load the value.
    fn emit_value_load_deref(
        &mut self,
        value: &VPtr,
        expected: wasm_encoder::ValType,
        func: &mut Function,
    ) {
        use wasm_encoder::Instruction as W;
        self.emit_value_load(value, func);
        if let mir::Value::Register(reg_idx) = value.as_ref() {
            if self.getelement_registers.contains_key(reg_idx) {
                // GetElement register holds an address; dereference to get the actual value
                func.instruction(&W::I32WrapI64);
                let memarg = MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };
                match expected {
                    wasm_encoder::ValType::F64 => func.instruction(&W::F64Load(memarg)),
                    _ => func.instruction(&W::I64Load(memarg)),
                };
            }
        }
    }

    /// Helper: Emit a runtime bump allocation of `size_bytes` bytes.
    ///
    /// Reads the current allocator pointer from the WASM global,
    /// saves it to `alloc_base_local`, bumps the global, and returns
    /// the base address in `alloc_base_local` for subsequent use.
    ///
    /// After calling this, use `W::LocalGet(self.alloc_base_local)` to
    /// get the i32 base address for memory stores.
    fn emit_runtime_alloc(&self, size_bytes: u32, func: &mut Function) {
        use wasm_encoder::Instruction as W;
        // Save current alloc pointer
        func.instruction(&W::GlobalGet(self.alloc_ptr_global));
        func.instruction(&W::LocalSet(self.alloc_base_local));
        // Bump allocator
        func.instruction(&W::GlobalGet(self.alloc_ptr_global));
        func.instruction(&W::I32Const(size_bytes as i32));
        func.instruction(&W::I32Add);
        func.instruction(&W::GlobalSet(self.alloc_ptr_global));
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
                // Use the arg map to find the WASM param index
                if let Some(&(param_start, word_count)) =
                    self.current_arg_map.get(*arg_idx as usize)
                {
                    if word_count <= 1 {
                        // Single-word arg: just load the param
                        func.instruction(&W::LocalGet(param_start));
                    } else {
                        // Multi-word arg (tuple/record): store all params to temp memory
                        // and return the pointer so subsequent GetElement/Load can access it
                        let size_bytes = (word_count as u32) * 8;
                        let temp_addr = self.mem_layout.alloc_offset;
                        self.mem_layout.alloc_offset += size_bytes;

                        for i in 0..word_count {
                            let param_idx = param_start + i;
                            let param_type = self
                                .current_arg_types
                                .get(param_idx as usize)
                                .copied()
                                .unwrap_or(ValType::F64);
                            func.instruction(&W::I32Const((temp_addr + i * 8) as i32));
                            func.instruction(&W::LocalGet(param_idx));
                            let memarg = MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            };
                            match param_type {
                                ValType::F64 => func.instruction(&W::F64Store(memarg)),
                                _ => func.instruction(&W::I64Store(memarg)),
                            };
                        }
                        // Push the temp address as the aggregate pointer
                        func.instruction(&W::I64Const(temp_addr as i64));
                    }
                } else {
                    // Fallback: assume arg index maps directly
                    func.instruction(&W::LocalGet(*arg_idx as u32));
                }
            }
            V::Global(_global_val) => {
                // Load global variable
                // TODO: Implement global variable access
                // For now, use I64 placeholder for uniformity
                func.instruction(&W::I64Const(0));
            }
            V::Function(func_idx) => {
                // Function index as i64 (matching register type I64)
                func.instruction(&W::I64Const(*func_idx as i64));
            }
            V::ExtFunction(name, _ty) => {
                // External function reference - push its import index as i64
                if let Some(import_idx) = self.resolve_ext_function(name) {
                    func.instruction(&W::I64Const(import_idx as i64));
                } else {
                    func.instruction(&W::I64Const(0)); // Unknown ext function
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
            V::None => {
                // None value - push appropriate zero based on expected type context
                // Default to I64 since it's safer (can be reinterpreted as f64 if needed)
                func.instruction(&W::I64Const(0));
            }
            _ => {
                // Placeholder for other value types
                func.instruction(&W::I64Const(0));
            }
        }
    }

    /// Emit WASM instructions to load a value with type coercion.
    /// If the value's actual type (I64 pointer from GetElement) differs from the expected
    /// type (F64 value), a memory dereference is inserted automatically.
    fn emit_value_load_typed(&mut self, value: &VPtr, expected: ValType, func: &mut Function) {
        use wasm_encoder::Instruction as W;

        let actual = self.infer_value_type(value);
        self.emit_value_load(value, func);
        if actual == ValType::I64 && expected == ValType::F64 {
            // I64 values used where F64 is expected are pointers to memory
            // (e.g. GetElement results). Dereference to load the float value.
            let memarg = MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            };
            func.instruction(&W::I32WrapI64);
            func.instruction(&W::F64Load(memarg));
        } else if actual == ValType::F64 && expected == ValType::I64 {
            // Value is f64 but an i64 is expected; reinterpret bits
            // (preserves float bit pattern for storage in i64 word slots)
            func.instruction(&W::I64ReinterpretF64);
        }
    }

    /// Emit call arguments with proper tuple/record flattening.
    /// When a MIR argument has a multi-word type (tuple/record), its runtime
    /// value is a pointer to linear memory. This method dereferences the pointer
    /// and pushes each element onto the WASM operand stack individually,
    /// matching the flattened function signature produced by `process_mir_functions`.
    fn emit_call_args_flattened(&mut self, args: &[(VPtr, TypeNodeId)], func: &mut Function) {
        use wasm_encoder::Instruction as W;

        for (arg, ty) in args {
            let resolved = ty.to_type();
            let flat_types = Self::flatten_type_to_valtypes(&resolved);

            if flat_types.len() <= 1 {
                // Single-word arg: load with standard type coercion
                let expected = flat_types.first().copied().unwrap_or(ValType::I64);
                self.emit_value_load_typed(arg, expected, func);
            } else {
                // Multi-word arg (tuple/record): the runtime value is an i64
                // pointer to the data in linear memory. Dereference each
                // element individually to match the flattened WASM signature.
                for (i, vtype) in flat_types.iter().enumerate() {
                    self.emit_value_load(arg, func);
                    func.instruction(&W::I32WrapI64);
                    let load_memarg = MemArg {
                        offset: (i * 8) as u64,
                        align: 3,
                        memory_index: 0,
                    };
                    match vtype {
                        ValType::F64 => func.instruction(&W::F64Load(load_memarg)),
                        _ => func.instruction(&W::I64Load(load_memarg)),
                    };
                }
            }
        }
    }

    /// Load a value and convert to i64 using numeric truncation (not bit reinterpretation).
    /// Use this for values that represent numeric indices (array indices, etc.)
    /// where f64 1.0 should become i64 1, not i64 0x3FF0000000000000.
    fn emit_value_load_as_numeric_i64(&mut self, value: &VPtr, func: &mut Function) {
        use wasm_encoder::Instruction as W;
        let actual = self.infer_value_type(value);
        self.emit_value_load(value, func);
        if actual == ValType::F64 {
            func.instruction(&W::I64TruncSatF64S);
        }
    }

    /// Infer the WASM ValType of a MIR value.
    /// Used to select the correct comparison instruction (F64 vs I64).
    fn infer_value_type(&self, value: &VPtr) -> ValType {
        match value.as_ref() {
            mir::Value::Register(reg_idx) => self
                .register_types
                .get(reg_idx)
                .copied()
                .unwrap_or(ValType::F64),
            mir::Value::Argument(arg_idx) => {
                // Look up actual argument type from the current function's arg map.
                // For multi-word args (tuples), the materialized value is a pointer (I64).
                if let Some(&(param_start, word_count)) = self.current_arg_map.get(*arg_idx) {
                    if word_count > 1 {
                        ValType::I64 // Multi-word arg is materialized as a pointer
                    } else {
                        self.current_arg_types
                            .get(param_start as usize)
                            .copied()
                            .unwrap_or(ValType::F64)
                    }
                } else {
                    ValType::F64
                }
            }
            mir::Value::Function(_) | mir::Value::ExtFunction(_, _) => ValType::I64,
            // None, State, Global, Constructor all push I64 in emit_value_load,
            // so infer_value_type must be consistent and return I64.
            mir::Value::None
            | mir::Value::State(_)
            | mir::Value::Global(_)
            | mir::Value::Constructor(_, _, _) => ValType::I64,
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
        module.section(&self.global_section);
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
            "probeln" => Some(self.rt.builtin_probeln),
            "probe" => Some(self.rt.builtin_probe),
            "length_array" => Some(self.rt.builtin_length_array),
            "split_head" => Some(self.rt.builtin_split_head),
            "split_tail" => Some(self.rt.builtin_split_tail),
            _ => {
                // Check plugin functions
                self.plugin_fns.functions.get(name).copied()
            }
        }
    }

    /// Compute the state storage size (in words) for a MIR function.
    /// This is used to allocate per-closure state storage when emitting closure calls.
    fn get_mir_fn_state_size(&self, mir_fn_idx: usize) -> u64 {
        if mir_fn_idx < self.mir.functions.len() {
            self.mir.functions[mir_fn_idx].state_skeleton.total_size()
        } else {
            0
        }
    }

    /// Resolve MIR function index from a closure pointer value.
    /// Returns the MIR function index if resolvable at compile time, None otherwise.
    fn resolve_mir_fn_idx(&self, fn_ptr: &mir::Value) -> Option<usize> {
        match fn_ptr {
            mir::Value::Function(idx) => Some(*idx),
            mir::Value::Register(vreg) => self.register_constants.get(vreg).map(|&v| v as usize),
            _ => None,
        }
    }

    /// Emit closure_state_push host function call.
    /// Pushes the closure's state onto the state stack before calling it.
    fn emit_closure_state_push(
        &mut self,
        closure_ptr: &VPtr,
        state_size: u64,
        func: &mut Function,
    ) {
        use wasm_encoder::Instruction as W;
        // Push closure address (i64) and state size (i64)
        self.emit_value_load(closure_ptr, func);
        func.instruction(&W::I64Const(state_size as i64));
        func.instruction(&W::Call(self.rt.closure_state_push));
    }

    /// Emit closure_state_pop host function call.
    /// Pops the closure's state from the state stack after the call returns.
    fn emit_closure_state_pop(&self, func: &mut Function) {
        use wasm_encoder::Instruction as W;
        func.instruction(&W::Call(self.rt.closure_state_pop));
    }

    /// Generate an exported WASM trampoline function `_mimium_exec_closure_void`.
    ///
    /// This function takes a single `i64` argument (the closure address in
    /// linear memory) and executes it as a `() -> ()` closure via
    /// `call_indirect`.  The host scheduler calls this to fire scheduled
    /// tasks without needing to replicate the closure-call protocol on the
    /// host side.
    ///
    /// Generated WASM (pseudo):
    /// ```wasm
    /// (func $_mimium_exec_closure_void (param $closure_addr i64)
    ///   ;; closure_state_push(closure_addr, 64)
    ///   ;; save old self-ptr, set new self-ptr = closure_addr
    ///   ;; load fn_table_idx = mem[closure_addr as i32]
    ///   ;; call_indirect [] -> [] (type () -> ())
    ///   ;; restore old self-ptr
    ///   ;; closure_state_pop()
    /// )
    /// ```
    fn generate_exec_closure_trampoline(&mut self) {
        use wasm_encoder::Instruction as W;

        let memarg = wasm_encoder::MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        };

        // 1. Declare the function type: (i64) -> ()
        let type_idx = self.type_section.len();
        self.type_section
            .ty()
            .function(vec![ValType::I64], vec![]);

        // 2. Add to function section
        self.function_section.function(type_idx);
        let fn_idx = self.current_fn_idx;
        self.current_fn_idx += 1;

        // 3. Build function body
        //    Locals: local 0 = param (closure_addr: i64)
        //            local 1 = saved_self_ptr (i64)
        let mut func = Function::new([(1, ValType::I64)]);
        let param_closure_addr: u32 = 0;
        let local_saved_self_ptr: u32 = 1;

        // closure_state_push(closure_addr, 64)
        // 64 is a conservative default state size for dynamically dispatched closures
        func.instruction(&W::LocalGet(param_closure_addr));
        func.instruction(&W::I64Const(64));
        func.instruction(&W::Call(self.rt.closure_state_push));

        // Save current closure_self_ptr (at memory address 0) to local
        func.instruction(&W::I32Const(0));
        func.instruction(&W::I64Load(memarg));
        func.instruction(&W::LocalSet(local_saved_self_ptr));

        // Set closure_self_ptr = closure_addr
        func.instruction(&W::I32Const(0));
        func.instruction(&W::LocalGet(param_closure_addr));
        func.instruction(&W::I64Store(memarg));

        // Load fn_table_idx from closure[0]: i64 at closure_addr
        func.instruction(&W::LocalGet(param_closure_addr));
        func.instruction(&W::I32WrapI64);
        func.instruction(&W::I64Load(memarg));
        func.instruction(&W::I32WrapI64); // table index is i32

        // call_indirect with type () -> ()
        // In mimium's WASM codegen, Unit return types produce no WASM results
        // (the return is stripped in process_mir_functions), so the scheduled
        // `() -> ()` closures truly have WASM type `() -> ()`.
        let void_void_type = self.get_or_create_call_type(vec![], vec![]);
        func.instruction(&W::CallIndirect {
            type_index: void_void_type,
            table_index: 0,
        });

        // Restore the previous closure_self_ptr
        func.instruction(&W::I32Const(0));
        func.instruction(&W::LocalGet(local_saved_self_ptr));
        func.instruction(&W::I64Store(memarg));

        // closure_state_pop()
        func.instruction(&W::Call(self.rt.closure_state_pop));

        func.instruction(&W::End);
        self.code_section.function(&func);

        // 4. Export with canonical name
        self.export_section.export(
            "_mimium_exec_closure_void",
            wasm_encoder::ExportKind::Func,
            fn_idx,
        );
    }

    /// Allocate a linear memory offset for a global variable, or return existing one.
    fn get_or_alloc_global_offset(&mut self, global: &VPtr) -> u32 {
        self.mem_layout.get_or_alloc_global_offset(global)
    }

    /// Helper: Map mimium type to WASM ValType
    ///
    /// WASM locals are partitioned into two pools: I64 and F64.
    /// All non-float types (pointers, integers, function refs, etc.) map to I64,
    /// matching the VM's uniform Word (u64) representation.
    #[allow(dead_code)]
    fn type_to_valtype(ty: &Type) -> ValType {
        match ty {
            Type::Primitive(PType::Numeric) => ValType::F64,
            Type::Primitive(PType::Int) => ValType::I64,
            Type::Primitive(PType::String) => ValType::I64, // pointer to string in linear memory
            Type::Primitive(PType::Unit) => ValType::I64, // zero-sized, but use i64 for uniformity
            Type::Function { .. } => ValType::I64,        // function reference (index as i64)
            Type::Record(fields) if fields.len() == 1 => {
                // Single-field record: unwrap to field type
                Self::type_to_valtype(&fields[0].ty.to_type())
            }
            Type::Tuple(elems) if elems.len() == 1 => {
                // Single-element tuple: unwrap to element type
                Self::type_to_valtype(&elems[0].to_type())
            }
            Type::Tuple(_) | Type::Record(_) => ValType::I64, // pointer to aggregate
            Type::Array(_) => ValType::I64,                   // array reference
            Type::Union(_) | Type::UserSum { .. } => ValType::I64, // sum type tag + payload
            Type::Ref(_) => ValType::I64,                     // pointer reference
            Type::Boxed(_) => ValType::I64,                   // heap pointer
            Type::Code(_) => ValType::I64,                    // code reference
            // Intermediate type variable: try to resolve through the parent chain
            Type::Intermediate(cell) => {
                let tv = cell.read().unwrap();
                if let Some(parent) = &tv.parent {
                    Self::type_to_valtype(&parent.to_type())
                } else {
                    // Unresolved type variable  Edefault to F64 for numeric compatibility
                    ValType::F64
                }
            }
            // Unknown/Any/Failure/TypeAlias/TypeScheme: not fully resolved.
            // Default to F64 for backward compatibility with untyped numeric parameters.
            _ => ValType::F64,
        }
    }

    /// Helper: Calculate size of a type in words
    #[allow(dead_code)]
    fn type_size(_ty: &Type) -> WordSize {
        // TODO: Implement proper type size calculation
        1
    }

    /// Flatten a type into individual WASM ValTypes for function parameter expansion.
    /// Tuple and record types are recursively expanded into per-element types,
    /// matching the MIR calling convention where tuple args are passed as separate words.
    fn flatten_type_to_valtypes(ty: &Type) -> Vec<ValType> {
        match ty {
            Type::Primitive(PType::Numeric) => vec![ValType::F64],
            Type::Primitive(PType::Int) => vec![ValType::I64],
            Type::Primitive(PType::Unit) => vec![],
            // Resolve Intermediate type variables before flattening
            Type::Intermediate(cell) => {
                let tv = cell.read().unwrap();
                if let Some(parent) = &tv.parent {
                    Self::flatten_type_to_valtypes(&parent.to_type())
                } else {
                    vec![ValType::I64]
                }
            }
            Type::Tuple(elems) if elems.len() == 1 => {
                Self::flatten_type_to_valtypes(&elems[0].to_type())
            }
            Type::Tuple(elems) => elems
                .iter()
                .flat_map(|e| Self::flatten_type_to_valtypes(&e.to_type()))
                .collect(),
            Type::Record(fields) if fields.len() == 1 => {
                Self::flatten_type_to_valtypes(&fields[0].ty.to_type())
            }
            Type::Record(fields) => fields
                .iter()
                .flat_map(|f| Self::flatten_type_to_valtypes(&f.ty.to_type()))
                .collect(),
            _ => vec![Self::type_to_valtype(ty)],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ExecContext;

    /// Helper: Create a path to write WASM output file in tmp directory
    fn get_wasm_output_path(filename: &str) -> std::path::PathBuf {
        let mut wasm_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        wasm_path.pop(); // Remove mimium-lang
        wasm_path.pop(); // Remove lib
        wasm_path.pop(); // Remove crates
        wasm_path.push("tmp");
        std::fs::create_dir_all(&wasm_path).ok(); // Ensure tmp directory exists
        wasm_path.push(filename);
        wasm_path
    }

    /// Helper: Compile source code to MIR
    fn compile_to_mir(src: &str) -> Arc<Mir> {
        let mut ctx = ExecContext::new(std::iter::empty(), None, crate::Config::default());
        ctx.prepare_compiler();
        let mir = ctx
            .get_compiler()
            .unwrap()
            .emit_mir(src)
            .expect("MIR generation failed");
        Arc::new(mir)
    }

    #[test]
    fn test_wasmgen_create() {
        let mir = Arc::new(Mir::default());
        let generator = WasmGenerator::new_without_plugins(mir);
        // Verify runtime primitive imports are set up
        assert!(generator.current_fn_idx > 20); // At least 25 runtime functions imported
        assert!(generator.rt.heap_alloc < generator.current_fn_idx);
        assert!(generator.rt.array_alloc < generator.current_fn_idx);
        assert!(generator.rt.state_push < generator.current_fn_idx);
    }

    #[test]
    fn test_wasmgen_generate_empty() {
        let mir = Arc::new(Mir::default());
        let mut generator = WasmGenerator::new_without_plugins(mir);
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

        let mir = compile_to_mir(src);
        let mut generator = WasmGenerator::new_without_plugins(mir);
        let wasm_bytes = generator.generate().expect("WASM generation failed");

        // Write to tmp directory for inspection
        let wasm_path = get_wasm_output_path("test_simple_return_from_test.wasm");
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

        let mir = compile_to_mir(src);
        let mut generator = WasmGenerator::new_without_plugins(mir);
        let wasm_bytes = generator.generate().expect("WASM generation failed");

        // Write to tmp directory for inspection
        let wasm_path = get_wasm_output_path("test_function_call_from_test.wasm");
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

        let mir = compile_to_mir(src);
        let mut generator = WasmGenerator::new_without_plugins(mir);
        let wasm_bytes = generator.generate().expect("WASM generation failed");

        // Write to tmp directory (3 levels up from crates/lib/mimium-lang)
        let wasm_path = get_wasm_output_path("test_integration.wasm");
        std::fs::write(&wasm_path, &wasm_bytes).expect("Failed to write WASM file");

        eprintln!(
            "Generated WASM file: {:?} ({} bytes)",
            wasm_path,
            wasm_bytes.len()
        );
    }

    /// Helper: Compile source to WASM and validate with wasmtime
    fn compile_and_validate(src: &str, test_name: &str) -> Vec<u8> {
        let mir = compile_to_mir(src);
        let mut generator = WasmGenerator::new_without_plugins(mir);
        let wasm_bytes = generator.generate().expect("WASM generation failed");

        // Write to tmp for inspection
        let wasm_path = get_wasm_output_path(&format!("{test_name}.wasm"));
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
