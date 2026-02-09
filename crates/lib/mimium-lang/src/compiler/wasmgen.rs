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
    ImportSection, MemorySection, MemoryType, Module, TableSection, TableType, TypeSection,
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
    /// Local variable register mapping
    registers: HashMap<Arc<mir::Value>, u32>,
    /// Function name to WASM function index mapping
    fn_name_to_idx: HashMap<Symbol, u32>,

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
            registers: HashMap::new(),
            fn_name_to_idx: HashMap::new(),
            // Runtime primitive indices will be set by setup_runtime_imports
            rt: RuntimeFunctionIndices::default(),
        };

        // Setup runtime primitive imports and memory/table
        generator.setup_runtime_imports();
        generator.setup_memory_and_table();

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

        // Type 15: () -> i64  for runtime_get_now, runtime_get_samplerate
        self.type_section.ty().function(vec![], vec![ValType::I64]);
        self.rt.runtime_get_now = self.add_import("runtime_get_now", type_idx);
        self.rt.runtime_get_samplerate = self.add_import("runtime_get_samplerate", type_idx);
    }

    /// Helper to add an import and increment function index, returns the function index
    fn add_import(&mut self, name: &str, type_idx: u32) -> u32 {
        self.import_section
            .import("runtime", name, EntityType::Function(type_idx));
        let fn_idx = self.current_fn_idx;
        self.current_fn_idx += 1;
        fn_idx
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
                .map(|ty| vec![Self::type_to_valtype(&ty.to_type())])
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
        // Clone the functions vector to avoid borrow checker issues
        let functions = self.mir.functions.clone();

        // For each MIR function, generate WASM function body
        for func in &functions {
            // Reset register mapping for each function
            self.registers.clear();

            // Allocate local variables for both i64 and f64 types
            // Arguments are in slots 0..n, additional locals start from n
            let _num_args = func.args.len();

            // For simplicity, allocate both i64 and f64 locals
            // WASMのローカル変数宣言: 最初にi64、次にf64
            let locals = vec![
                (32, ValType::I64), // 32 i64 locals for integers/pointers
                (32, ValType::F64), // 32 f64 locals for floats
            ];

            // Create a new WASM function
            let mut wasm_func = Function::new(locals);

            // Translate each basic block
            for block in &func.body {
                for (dest, instr) in &block.0 {
                    self.translate_instruction_with_dest(dest.as_ref(), instr, &mut wasm_func);
                }
            }

            // Add function to code section
            self.code_section.function(&wasm_func);
        }

        Ok(())
    }

    /// Translate a single MIR instruction with destination to WASM
    fn translate_instruction_with_dest(
        &mut self,
        dest: &mir::Value,
        instr: &mir::Instruction,
        func: &mut Function,
    ) {
        use wasm_encoder::Instruction as W;

        // Generate the instruction (pushes result to stack)
        self.translate_instruction(instr, func);

        // Store result to destination register if specified
        match dest {
            mir::Value::Register(reg_idx) => {
                // For now, use f64 locals (offset 32) for all registers
                // TODO: Track register types properly
                let local_idx = 32 + reg_idx;
                func.instruction(&W::LocalSet(local_idx as u32));
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

    /// Export all functions
    fn export_functions(&mut self) -> Result<(), String> {
        // Clone the functions vector to avoid borrow checker issues
        let functions = self.mir.functions.clone();

        // Export all public functions
        for func in &functions {
            let fn_idx = *self
                .fn_name_to_idx
                .get(&func.label)
                .ok_or_else(|| format!("Function {:?} not found in mapping", func.label))?;

            // Use the function name from the symbol interner
            let fn_name = format!("fn_{fn_idx}");
            self.export_section
                .export(&fn_name, wasm_encoder::ExportKind::Func, fn_idx);
        }

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

            // Boolean operations
            I::Eq(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Eq);
            }
            I::Ne(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Ne);
            }
            I::Lt(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Lt);
            }
            I::Le(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Le);
            }
            I::Gt(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Gt);
            }
            I::Ge(a, b) => {
                self.emit_value_load(a, func);
                self.emit_value_load(b, func);
                func.instruction(&W::F64Ge);
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
                // state_get(dst_ptr: i32, size_words: i32)
                let size = ty.word_size() as i32;
                func.instruction(&W::I32Const(0)); // dst_ptr placeholder
                func.instruction(&W::I32Const(size));
                func.instruction(&W::Call(self.rt.state_get));
            }

            I::ReturnFeed(value, ty) => {
                // state_set(src_ptr: i32, size_words: i32) + return value
                let size = ty.word_size() as i32;
                func.instruction(&W::I32Const(0)); // src_ptr placeholder
                func.instruction(&W::I32Const(size));
                func.instruction(&W::Call(self.rt.state_set));
                // Also emit the return value
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
            I::Alloc(_ty) => {
                // Allocate local variable space
                // For now, this is a no-op in WASM as locals are pre-allocated
                // We just track the register allocation
            }

            I::Load(ptr, _ty) => {
                // Load value from pointer
                // In WASM backend, this translates to loading from linear memory
                // TODO: Implement proper memory load
                self.emit_value_load(ptr, func);
            }

            I::Store(_dst, src, _ty) => {
                // Store value to pointer
                // In WASM backend, this translates to storing to linear memory
                // TODO: Implement proper memory store
                self.emit_value_load(src, func);
                // For now, just drop the value (no actual store)
                func.instruction(&W::Drop);
            }

            // Function calls
            I::Call(fn_ptr, args, _ret_ty) => {
                // Direct function call
                // Load all arguments onto stack
                for (arg, _ty) in args {
                    self.emit_value_load(arg, func);
                }
                // Get function index and call
                if let mir::Value::Function(fn_idx) = **fn_ptr {
                    // Map from MIR function index to WASM function index
                    // Need to account for imported functions
                    let wasm_fn_idx = fn_idx as u32 + self.current_fn_idx;
                    func.instruction(&W::Call(wasm_fn_idx));
                } else {
                    // Indirect call through function pointer
                    self.emit_value_load(fn_ptr, func);
                    // TODO: Implement call_indirect with proper type signature
                }
            }

            I::CallIndirect(closure_ptr, args, ret_ty) => {
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

            // Control flow
            I::Return(value, _ty) => {
                // Return from function
                self.emit_value_load(value, func);
                func.instruction(&W::Return);
            }

            // String constants
            I::String(_sym) => {
                // String constant - return pointer to string in data section
                // TODO: Allocate strings in data section and return address
                func.instruction(&W::I32Const(0)); // placeholder pointer
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

        match value.as_ref() {
            V::Register(reg_idx) => {
                // Load from f64 local variable (offset 32)
                // TODO: Track register types properly
                let local_idx = 32 + reg_idx;
                func.instruction(&W::LocalGet(local_idx as u32));
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
            _ => {
                // Placeholder for other value types
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

    /// Helper: Map mimium type to WASM ValType
    #[allow(dead_code)]
    fn type_to_valtype(ty: &Type) -> ValType {
        match ty {
            Type::Primitive(PType::Numeric) => ValType::F64,
            Type::Primitive(PType::Int) => ValType::I64,
            Type::Primitive(PType::String) => ValType::I32, // pointer to string in linear memory
            Type::Primitive(PType::Unit) => ValType::I32,   // placeholder
            Type::Function { .. } => ValType::I32,          // function reference or index
            Type::Tuple(_) | Type::Record(_) => ValType::I32, // pointer to aggregate
            Type::Array(_) => ValType::I32,                 // array reference
            Type::Union(_) | Type::UserSum { .. } => ValType::I32, // sum type tag + payload
            Type::Ref(_) => ValType::I32,                   // pointer reference
            _ => ValType::F64,                              // default to f64
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
}
