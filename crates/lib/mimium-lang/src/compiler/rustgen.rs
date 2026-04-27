use std::collections::{BTreeMap, HashMap};
use std::fmt::Write;
use std::sync::Arc;

use crate::compiler::Config;
use crate::compiler::bytecodegen::SelfEvalMode;
use crate::interner::Symbol;
use crate::mir::{Function, Instruction, Mir, VPtr, VReg, Value};

pub struct RustGenerator {
    mir: Arc<Mir>,
    config: Config,
}

enum CallKind {
    FunctionHandle(String),
    Ext(Symbol),
}

impl RustGenerator {
    pub fn new(mir: Arc<Mir>, config: Config) -> Self {
        Self { mir, config }
    }

    pub fn generate(&self) -> Result<String, String> {
        let global_slots = self.collect_global_slots();
        let mut out = String::new();

        out.push_str(
            r#"pub type Word = u64;

fn f64_to_word(value: f64) -> Word { value.to_bits() }
fn word_to_f64(value: Word) -> f64 { f64::from_bits(value) }
fn i64_to_word(value: i64) -> Word { u64::from_ne_bytes(value.to_ne_bytes()) }
fn word_to_i64(value: Word) -> i64 { i64::from_ne_bytes(value.to_ne_bytes()) }
fn truthy(value: Word) -> bool { word_to_f64(value) > 0.0 }
const FUNCTION_HANDLE_TAG: Word = 1 << 63;
fn encode_function(index: usize) -> Word { FUNCTION_HANDLE_TAG | index as Word }
fn decode_function(handle: Word) -> Option<usize> {
    (handle & FUNCTION_HANDLE_TAG != 0).then_some((handle & !FUNCTION_HANDLE_TAG) as usize)
}

pub trait MimiumHost {
    fn call_ext(&mut self, name: &str, args: &[Word], ret_words: usize) -> Result<Vec<Word>, String>;

    fn current_time(&mut self) -> f64 {
        0.0
    }

    fn sample_rate(&mut self) -> f64 {
        48_000.0
    }
}

#[derive(Default)]
pub struct PanicHost;

impl MimiumHost for PanicHost {
    fn call_ext(&mut self, name: &str, _args: &[Word], _ret_words: usize) -> Result<Vec<Word>, String> {
        Err(format!("external function '{}' is not available in the generated Rust host", name))
    }
}

#[derive(Clone, Default)]
struct StateStorage {
    pos: usize,
    rawdata: Vec<Word>,
}

impl StateStorage {
    fn new(size: usize) -> Self {
        Self {
            pos: 0,
            rawdata: vec![0; size],
        }
    }

    fn ensure(&mut self, size: usize) {
        let needed = self.pos.saturating_add(size);
        if self.rawdata.len() < needed {
            self.rawdata.resize(needed, 0);
        }
    }

    fn push_pos(&mut self, offset: usize) {
        self.pos = self.pos.saturating_add(offset);
    }

    fn pop_pos(&mut self, offset: usize) {
        self.pos = self.pos.saturating_sub(offset);
    }

    fn get_state(&mut self, size: usize) -> Vec<Word> {
        self.ensure(size);
        self.rawdata[self.pos..self.pos + size].to_vec()
    }

    fn set_state(&mut self, src: &[Word], size: usize) {
        self.ensure(size);
        self.rawdata[self.pos..self.pos + size].copy_from_slice(&src[..size]);
    }

    fn mem(&mut self, src: Word) -> Word {
        self.ensure(1);
        let prev = self.rawdata[self.pos];
        self.rawdata[self.pos] = src;
        prev
    }

    fn delay(&mut self, input: Word, time_raw: Word, max_len: usize) -> Word {
        let total_words = max_len.saturating_add(2);
        self.ensure(total_words);
        if max_len == 0 {
            return 0;
        }

        let delay_samples = word_to_f64(time_raw)
            .clamp(0.0, max_len.saturating_sub(1) as f64) as usize;
        let read_slot = self.pos;
        let write_slot = self.pos + 1;
        let data_start = self.pos + 2;
        let write_idx = (self.rawdata[write_slot] as usize) % max_len;
        let read_idx = (write_idx + max_len - delay_samples) % max_len;
        let result = self.rawdata[data_start + read_idx];
        self.rawdata[data_start + write_idx] = input;
        self.rawdata[read_slot] = read_idx as u64;
        self.rawdata[write_slot] = ((write_idx + 1) % max_len) as u64;
        result
    }
}

#[derive(Clone, Default)]
struct Pointer {
    slot: usize,
    offset: usize,
}

#[derive(Default)]
struct MemoryStore {
    slots: Vec<Vec<Word>>,
    ptrs: Vec<Pointer>,
}

impl MemoryStore {
    fn alloc(&mut self, size: usize) -> Word {
        let slot = self.slots.len();
        self.slots.push(vec![0; size]);
        self.ptrs.push(Pointer { slot, offset: 0 });
        self.ptrs.len() as Word
    }

    fn ptr(&self, handle: Word) -> Result<&Pointer, String> {
        let index = handle
            .checked_sub(1)
            .ok_or_else(|| "invalid memory handle 0".to_string())? as usize;
        self.ptrs
            .get(index)
            .ok_or_else(|| format!("invalid memory handle {}", handle))
    }

    fn get_element(&mut self, base: Word, tuple_offset: usize) -> Result<Word, String> {
        let pointer = self.ptr(base)?.clone();
        self.ptrs.push(Pointer {
            slot: pointer.slot,
            offset: pointer.offset + tuple_offset,
        });
        Ok(self.ptrs.len() as Word)
    }

    fn load(&self, ptr: Word, size: usize) -> Result<Vec<Word>, String> {
        let pointer = self.ptr(ptr)?;
        let slot = self
            .slots
            .get(pointer.slot)
            .ok_or_else(|| format!("invalid memory slot {}", pointer.slot))?;
        let end = pointer.offset + size;
        if end > slot.len() {
            return Err(format!(
                "load out of bounds: offset={} size={} len={}",
                pointer.offset,
                size,
                slot.len()
            ));
        }
        Ok(slot[pointer.offset..end].to_vec())
    }

    fn store(&mut self, ptr: Word, src: &[Word], size: usize) -> Result<(), String> {
        let pointer = self.ptr(ptr)?.clone();
        let slot = self
            .slots
            .get_mut(pointer.slot)
            .ok_or_else(|| format!("invalid memory slot {}", pointer.slot))?;
        let end = pointer.offset + size;
        if end > slot.len() {
            return Err(format!(
                "store out of bounds: offset={} size={} len={}",
                pointer.offset,
                size,
                slot.len()
            ));
        }
        slot[pointer.offset..end].copy_from_slice(&src[..size]);
        Ok(())
    }
}

#[derive(Clone, Default)]
struct ArrayObject {
    elem_size_words: usize,
    data: Vec<Word>,
}

#[derive(Default)]
struct ArrayStorage {
    arrays: Vec<ArrayObject>,
}

impl ArrayStorage {
    fn alloc_array(&mut self, len: usize, elem_size_words: usize) -> Word {
        self.arrays.push(ArrayObject {
            elem_size_words,
            data: vec![0; len.saturating_mul(elem_size_words)],
        });
        self.arrays.len() as Word
    }

    fn get(&self, handle: Word) -> Result<&ArrayObject, String> {
        let index = handle
            .checked_sub(1)
            .ok_or_else(|| "invalid array handle 0".to_string())? as usize;
        self.arrays
            .get(index)
            .ok_or_else(|| format!("invalid array handle {}", handle))
    }

    fn get_mut(&mut self, handle: Word) -> Result<&mut ArrayObject, String> {
        let index = handle
            .checked_sub(1)
            .ok_or_else(|| "invalid array handle 0".to_string())? as usize;
        self.arrays
            .get_mut(index)
            .ok_or_else(|| format!("invalid array handle {}", handle))
    }
}

pub struct MimiumProgram<H: MimiumHost = PanicHost> {
    pub host: H,
    globals: Vec<Vec<Word>>,
    function_states: Vec<StateStorage>,
    arrays: ArrayStorage,
}

impl MimiumProgram<PanicHost> {
    pub fn new() -> Self {
        Self::with_host(PanicHost)
    }
}

"#,
        );

        writeln!(out, "impl<H: MimiumHost> MimiumProgram<H> {{").unwrap();
        writeln!(out, "    pub fn with_host(host: H) -> Self {{").unwrap();
        writeln!(out, "        Self {{").unwrap();
        writeln!(out, "            host,").unwrap();
        writeln!(out, "            globals: vec![").unwrap();
        for (_key, _value, size) in &global_slots {
            writeln!(out, "                vec![0; {size}],").unwrap();
        }
        writeln!(out, "            ],").unwrap();
        writeln!(out, "            function_states: vec![").unwrap();
        for func in &self.mir.functions {
            writeln!(
                out,
                "                StateStorage::new({}),",
                func.state_skeleton.total_size()
            )
            .unwrap();
        }
        writeln!(out, "            ],").unwrap();
        writeln!(out, "            arrays: ArrayStorage::default(),").unwrap();
        writeln!(out, "        }}").unwrap();
        writeln!(out, "    }}").unwrap();

        if let Some(dsp_index) = self.find_function_by_name("dsp") {
            writeln!(
                out,
                "    pub fn call_dsp(&mut self, args: &[Word]) -> Result<Vec<Word>, String> {{"
            )
            .unwrap();
            writeln!(
                out,
                "        let mut state = std::mem::take(&mut self.function_states[{dsp_index}]);"
            )
            .unwrap();
            writeln!(
                out,
                "        let result = self.func_{dsp_index}(&mut state, args);"
            )
            .unwrap();
            writeln!(out, "        self.function_states[{dsp_index}] = state;").unwrap();
            writeln!(out, "        result").unwrap();
            writeln!(out, "    }}").unwrap();
        }

        if let Some(main_index) = self.find_function_by_name("main") {
            writeln!(
                out,
                "    pub fn call_main(&mut self) -> Result<Vec<Word>, String> {{"
            )
            .unwrap();
            writeln!(
                out,
                "        let mut state = std::mem::take(&mut self.function_states[{main_index}]);"
            )
            .unwrap();
            writeln!(
                out,
                "        let result = self.func_{main_index}(&mut state, &[]);"
            )
            .unwrap();
            writeln!(out, "        self.function_states[{main_index}] = state;").unwrap();
            writeln!(out, "        result").unwrap();
            writeln!(out, "    }}").unwrap();
        }

        writeln!(out, "    fn call_function_handle(&mut self, handle: Word, state: &mut StateStorage, args: &[Word]) -> Result<Vec<Word>, String> {{").unwrap();
        writeln!(out, "        match decode_function(handle) {{").unwrap();
        for func in &self.mir.functions {
            writeln!(
                out,
                "            Some({}) => self.func_{}(state, args),",
                func.index, func.index
            )
            .unwrap();
        }
        writeln!(
            out,
            "            Some(index) => Err(format!(\"unknown function handle {{}}\", index)),"
        )
        .unwrap();
        writeln!(
            out,
            "            None => Err(format!(\"unsupported callable handle {{}}\", handle)),"
        )
        .unwrap();
        writeln!(out, "        }}").unwrap();
        writeln!(out, "    }}").unwrap();

        for func in &self.mir.functions {
            self.emit_function(&mut out, func, &global_slots)?;
        }

        writeln!(out, "}}").unwrap();
        Ok(out)
    }

    fn find_function_by_name(&self, name: &str) -> Option<usize> {
        self.mir
            .functions
            .iter()
            .find(|func| func.label.as_str() == name)
            .map(|func| func.index)
    }

    fn collect_global_slots(&self) -> Vec<(String, VPtr, usize)> {
        let mut slots = BTreeMap::<String, (VPtr, usize)>::new();
        for func in &self.mir.functions {
            for block in &func.body {
                for (_dst, instr) in &block.0 {
                    match instr {
                        Instruction::GetGlobal(global, ty)
                        | Instruction::SetGlobal(global, _, ty) => {
                            let key = format!("{global:?}");
                            let size = ty.word_size() as usize;
                            slots
                                .entry(key)
                                .and_modify(|(_, existing)| *existing = (*existing).max(size))
                                .or_insert_with(|| (global.clone(), size));
                        }
                        _ => {}
                    }
                }
            }
        }
        slots
            .into_iter()
            .map(|(key, (value, size))| (key, value, size))
            .collect()
    }

    fn emit_function(
        &self,
        out: &mut String,
        func: &Function,
        global_slots: &[(String, VPtr, usize)],
    ) -> Result<(), String> {
        let register_sizes = self.collect_register_sizes(func)?;
        let block_preds = self.collect_block_predecessors(func);

        writeln!(out, "    fn func_{}(&mut self, state: &mut StateStorage, args: &[Word]) -> Result<Vec<Word>, String> {{", func.index).unwrap();
        writeln!(out, "        let mut memory = MemoryStore::default();").unwrap();
        writeln!(out, "        let mut bb: usize = 0;").unwrap();
        writeln!(out, "        let mut pred_bb: usize = 0;").unwrap();
        writeln!(out, "        let mut arg_offset = 0usize;").unwrap();
        for (index, arg) in func.args.iter().enumerate() {
            let size = arg.1.word_size() as usize;
            writeln!(
                out,
                "        let arg_{index} = args[arg_offset..arg_offset + {size}].to_vec();"
            )
            .unwrap();
            writeln!(out, "        arg_offset += {size};").unwrap();
        }

        let mut regs: Vec<_> = register_sizes.into_iter().collect();
        regs.sort_by_key(|(reg, _size)| *reg);
        for (reg, size) in regs {
            writeln!(out, "        let mut reg_{reg} = vec![0; {size}];").unwrap();
        }

        writeln!(out, "        loop {{").unwrap();
        writeln!(out, "            match bb {{").unwrap();
        for (block_index, block) in func.body.iter().enumerate() {
            writeln!(out, "                {block_index} => {{").unwrap();
            for (dst, instr) in &block.0 {
                self.emit_instruction(
                    out,
                    func,
                    block_index,
                    dst,
                    instr,
                    global_slots,
                    &block_preds,
                )?;
            }
            writeln!(out, "                    return Err(format!(\"basic block {} in function {} fell through without terminator\"));", block_index, func.index).unwrap();
            writeln!(out, "                }}").unwrap();
        }
        writeln!(out, "                _ => return Err(format!(\"invalid basic block {} in function {}\", bb, {})),", func.index, func.index, func.index).unwrap();
        writeln!(out, "            }}").unwrap();
        writeln!(out, "        }}").unwrap();
        writeln!(out, "    }}").unwrap();
        Ok(())
    }

    fn collect_register_sizes(&self, func: &Function) -> Result<HashMap<VReg, usize>, String> {
        let mut sizes = HashMap::new();
        for block in &func.body {
            for (dst, instr) in &block.0 {
                if let Value::Register(reg) = dst.as_ref() {
                    let size = self.instruction_result_size(func, instr)?.max(1);
                    sizes.entry(*reg).or_insert(size);
                }
            }
        }
        Ok(sizes)
    }

    fn instruction_result_size(
        &self,
        func: &Function,
        instr: &Instruction,
    ) -> Result<usize, String> {
        Ok(match instr {
            Instruction::Uinteger(_)
            | Instruction::Integer(_)
            | Instruction::Float(_)
            | Instruction::GetElement { .. }
            | Instruction::Alloc(_)
            | Instruction::GetGlobal(_, _)
            | Instruction::GetState(_)
            | Instruction::Delay(_, _, _)
            | Instruction::Mem(_)
            | Instruction::AddF(_, _)
            | Instruction::SubF(_, _)
            | Instruction::MulF(_, _)
            | Instruction::DivF(_, _)
            | Instruction::ModF(_, _)
            | Instruction::NegF(_)
            | Instruction::AbsF(_)
            | Instruction::SinF(_)
            | Instruction::CosF(_)
            | Instruction::PowF(_, _)
            | Instruction::LogF(_)
            | Instruction::SqrtF(_)
            | Instruction::AddI(_, _)
            | Instruction::SubI(_, _)
            | Instruction::MulI(_, _)
            | Instruction::DivI(_, _)
            | Instruction::ModI(_, _)
            | Instruction::NegI(_)
            | Instruction::AbsI(_)
            | Instruction::Not(_)
            | Instruction::Eq(_, _)
            | Instruction::Ne(_, _)
            | Instruction::Gt(_, _)
            | Instruction::Ge(_, _)
            | Instruction::Lt(_, _)
            | Instruction::Le(_, _)
            | Instruction::And(_, _)
            | Instruction::Or(_, _)
            | Instruction::CastFtoI(_)
            | Instruction::CastItoF(_)
            | Instruction::CastItoB(_)
            | Instruction::Array(_, _) => 1,
            Instruction::Load(_, ty)
            | Instruction::Call(_, _, ty)
            | Instruction::GetArrayElem(_, _, ty)
            | Instruction::Return(_, ty)
            | Instruction::ReturnFeed(_, ty) => ty.word_size() as usize,
            Instruction::Phi(left, _) => self.value_size(func, left)?,
            Instruction::PhiSwitch(inputs) => {
                let Some(first) = inputs.first() else {
                    return Err("PhiSwitch without inputs is invalid".to_string());
                };
                self.value_size(func, first)?
            }
            Instruction::SetGlobal(_, _, _)
            | Instruction::Store(_, _, _)
            | Instruction::PushStateOffset(_)
            | Instruction::PopStateOffset(_)
            | Instruction::JmpIf(_, _, _, _)
            | Instruction::Jmp(_)
            | Instruction::Switch { .. }
            | Instruction::SetArrayElem(_, _, _, _)
            | Instruction::Closure(_)
            | Instruction::CallCls(_, _, _)
            | Instruction::String(_)
            | Instruction::CloseUpValues(_, _)
            | Instruction::MakeClosure { .. }
            | Instruction::CloseHeapClosure(_)
            | Instruction::CloneHeap(_)
            | Instruction::CallIndirect(_, _, _)
            | Instruction::GetUpValue(_, _)
            | Instruction::SetUpValue(_, _, _)
            | Instruction::TaggedUnionWrap { .. }
            | Instruction::TaggedUnionGetTag(_)
            | Instruction::TaggedUnionGetValue(_, _)
            | Instruction::BoxAlloc { .. }
            | Instruction::BoxLoad { .. }
            | Instruction::BoxClone { .. }
            | Instruction::BoxRelease { .. }
            | Instruction::BoxStore { .. }
            | Instruction::CloneUserSum { .. }
            | Instruction::ReleaseUserSum { .. }
            | Instruction::PowI(_)
            | Instruction::LogI(_, _)
            | Instruction::Error => 1,
        })
    }

    fn value_size(&self, func: &Function, value: &VPtr) -> Result<usize, String> {
        match value.as_ref() {
            Value::Argument(index) => func
                .args
                .get(*index)
                .map(|arg| arg.1.word_size() as usize)
                .ok_or_else(|| format!("invalid argument index {}", index)),
            Value::Register(reg) => {
                for block in &func.body {
                    for (dst, instr) in &block.0 {
                        if let Value::Register(dst_reg) = dst.as_ref() {
                            if dst_reg == reg {
                                return self.instruction_result_size(func, instr);
                            }
                        }
                    }
                }
                Err(format!("unknown register {}", reg))
            }
            Value::Function(_) | Value::ExtFunction(_, _) | Value::Global(_) | Value::State(_) => {
                Ok(1)
            }
            Value::Constructor(_, _, ty) => Ok(ty.word_size() as usize),
            Value::None => Ok(0),
        }
    }

    fn collect_block_predecessors(&self, func: &Function) -> Vec<Vec<usize>> {
        let mut preds = vec![Vec::new(); func.body.len()];
        for (block_index, block) in func.body.iter().enumerate() {
            for (_dst, instr) in &block.0 {
                match instr {
                    Instruction::JmpIf(_, then_bb, else_bb, merge_bb) => {
                        preds[*then_bb as usize].push(block_index);
                        preds[*else_bb as usize].push(block_index);
                        preds[*merge_bb as usize].push(*then_bb as usize);
                        preds[*merge_bb as usize].push(*else_bb as usize);
                    }
                    Instruction::Jmp(offset) => {
                        let target = (block_index as isize + *offset as isize) as usize;
                        preds[target].push(block_index);
                    }
                    Instruction::Switch {
                        cases,
                        default_block,
                        merge_block,
                        ..
                    } => {
                        for (_literal, case_bb) in cases {
                            preds[*case_bb as usize].push(block_index);
                            preds[*merge_block as usize].push(*case_bb as usize);
                        }
                        if let Some(default_bb) = default_block {
                            preds[*default_bb as usize].push(block_index);
                            preds[*merge_block as usize].push(*default_bb as usize);
                        }
                    }
                    _ => {}
                }
            }
        }
        preds
    }

    fn emit_instruction(
        &self,
        out: &mut String,
        func: &Function,
        block_index: usize,
        dst: &VPtr,
        instr: &Instruction,
        global_slots: &[(String, VPtr, usize)],
        block_preds: &[Vec<usize>],
    ) -> Result<(), String> {
        match instr {
            Instruction::Uinteger(value) => self.assign_scalar(out, dst, format!("{value}u64"))?,
            Instruction::Integer(value) => {
                self.assign_scalar(out, dst, format!("i64_to_word({value}i64)"))?
            }
            Instruction::Float(value) => {
                self.assign_scalar(out, dst, format!("f64_to_word({value:?}f64)"))?
            }
            Instruction::Alloc(ty) => {
                self.assign_scalar(out, dst, format!("memory.alloc({}usize)", ty.word_size()))?
            }
            Instruction::Load(ptr, ty) => {
                let dest = self.reg_name(dst)?;
                let ptr_expr = self.word0_expr(ptr)?;
                writeln!(
                    out,
                    "                    {dest} = memory.load({ptr_expr}, {}usize)?;",
                    ty.word_size()
                )
                .unwrap();
            }
            Instruction::Store(ptr, src, ty) => {
                let ptr_expr = self.word0_expr(ptr)?;
                let src_expr = self.value_vec_expr(src)?;
                writeln!(
                    out,
                    "                    memory.store({ptr_expr}, &{src_expr}, {}usize)?;",
                    ty.word_size()
                )
                .unwrap();
            }
            Instruction::GetElement {
                value,
                tuple_offset,
                ..
            } => {
                let dest = self.reg_name(dst)?;
                let base_expr = self.word0_expr(value)?;
                writeln!(
                    out,
                    "                    {dest}[0] = memory.get_element({base_expr}, {}usize)?;",
                    tuple_offset
                )
                .unwrap();
            }
            Instruction::Call(callee, args, _ret_ty) => {
                let dest = self.reg_name(dst)?;
                let call_kind = self.call_kind(callee)?;
                writeln!(out, "                    let mut call_args = Vec::new();").unwrap();
                for (arg, _ty) in args {
                    let expr = self.value_vec_expr(arg)?;
                    writeln!(
                        out,
                        "                    call_args.extend_from_slice(&{expr});"
                    )
                    .unwrap();
                }
                match call_kind {
                    CallKind::FunctionHandle(handle_expr) => {
                        writeln!(out, "                    {dest} = self.call_function_handle({handle_expr}, state, &call_args)?;").unwrap();
                    }
                    CallKind::Ext(symbol) => {
                        let name = format!("{:?}", symbol.as_str());
                        let ret_words = self.instruction_result_size(func, instr)?;
                        writeln!(out, "                    {dest} = self.host.call_ext({name}, &call_args, {ret_words})?;").unwrap();
                    }
                }
            }
            Instruction::GetGlobal(global, ty) => {
                let dest = self.reg_name(dst)?;
                let global_index = self.global_index(global_slots, global)?;
                writeln!(
                    out,
                    "                    {dest} = self.globals[{global_index}][..{}].to_vec();",
                    ty.word_size()
                )
                .unwrap();
            }
            Instruction::SetGlobal(global, src, ty) => {
                let global_index = self.global_index(global_slots, global)?;
                let src_expr = self.value_vec_expr(src)?;
                writeln!(out, "                    self.globals[{global_index}][..{}].copy_from_slice(&{src_expr}[..{}]);", ty.word_size(), ty.word_size()).unwrap();
            }
            Instruction::PushStateOffset(offset) => {
                writeln!(out, "                    state.push_pos({offset}usize);").unwrap();
            }
            Instruction::PopStateOffset(offset) => {
                writeln!(out, "                    state.pop_pos({offset}usize);").unwrap();
            }
            Instruction::GetState(ty) => {
                let dest = self.reg_name(dst)?;
                writeln!(
                    out,
                    "                    {dest} = state.get_state({}usize);",
                    ty.word_size()
                )
                .unwrap();
            }
            Instruction::JmpIf(cond, then_bb, else_bb, _merge_bb) => {
                let cond_expr = self.word0_expr(cond)?;
                writeln!(out, "                    pred_bb = {block_index};").unwrap();
                writeln!(out, "                    bb = if truthy({cond_expr}) {{ {}usize }} else {{ {}usize }};", then_bb, else_bb).unwrap();
                writeln!(out, "                    continue;").unwrap();
            }
            Instruction::Jmp(offset) => {
                writeln!(out, "                    pred_bb = {block_index};").unwrap();
                writeln!(
                    out,
                    "                    bb = (({block_index}isize) + ({}isize)) as usize;",
                    offset
                )
                .unwrap();
                writeln!(out, "                    continue;").unwrap();
            }
            Instruction::Phi(left, right) => {
                let preds = block_preds.get(block_index).cloned().unwrap_or_default();
                if preds.len() < 2 {
                    return Err(format!(
                        "Phi in block {block_index} does not have enough predecessors"
                    ));
                }
                let dest = self.reg_name(dst)?;
                let left_expr = self.value_vec_expr(left)?;
                let right_expr = self.value_vec_expr(right)?;
                writeln!(out, "                    if pred_bb == {}usize {{ {dest} = {left_expr}; }} else if pred_bb == {}usize {{ {dest} = {right_expr}; }} else {{ return Err(format!(\"phi predecessor mismatch in block {}: {}\", {}, pred_bb)); }}", preds[0], preds[1], block_index, "{}", block_index).unwrap();
            }
            Instruction::Switch {
                scrutinee,
                cases,
                default_block,
                ..
            } => {
                let scrutinee_expr = self.word0_expr(scrutinee)?;
                writeln!(
                    out,
                    "                    let scrutinee = word_to_i64({scrutinee_expr});"
                )
                .unwrap();
                writeln!(out, "                    pred_bb = {block_index};").unwrap();
                writeln!(out, "                    bb = match scrutinee {{").unwrap();
                for (literal, case_bb) in cases {
                    writeln!(
                        out,
                        "                        {literal} => {}usize,",
                        case_bb
                    )
                    .unwrap();
                }
                if let Some(default_bb) = default_block {
                    writeln!(out, "                        _ => {}usize,", default_bb).unwrap();
                } else {
                    writeln!(out, "                        _ => return Err(format!(\"non exhaustive switch in block {}\", {})),", block_index, block_index).unwrap();
                }
                writeln!(out, "                    }};").unwrap();
                writeln!(out, "                    continue;").unwrap();
            }
            Instruction::PhiSwitch(inputs) => {
                let preds = block_preds.get(block_index).cloned().unwrap_or_default();
                if preds.len() != inputs.len() {
                    return Err(format!(
                        "PhiSwitch predecessor count mismatch in block {block_index}: preds={} inputs={}",
                        preds.len(),
                        inputs.len()
                    ));
                }
                let dest = self.reg_name(dst)?;
                writeln!(out, "                    {dest} = match pred_bb {{").unwrap();
                for (pred, input) in preds.iter().zip(inputs.iter()) {
                    let expr = self.value_vec_expr(input)?;
                    writeln!(out, "                        {pred}usize => {expr},").unwrap();
                }
                writeln!(out, "                        _ => return Err(format!(\"phi switch predecessor mismatch in block {}\", {})),", block_index, block_index).unwrap();
                writeln!(out, "                    }};").unwrap();
            }
            Instruction::Return(value, ty) => {
                if matches!(value.as_ref(), Value::None) || ty.word_size() == 0 {
                    writeln!(out, "                    return Ok(Vec::new());").unwrap();
                } else {
                    let expr = self.value_vec_expr(value)?;
                    writeln!(out, "                    return Ok({expr});").unwrap();
                }
            }
            Instruction::ReturnFeed(value, ty) => {
                let expr = self.value_vec_expr(value)?;
                match self.config.self_eval_mode {
                    SelfEvalMode::SimpleState => {
                        writeln!(out, "                    let result = {expr};").unwrap();
                        writeln!(
                            out,
                            "                    state.set_state(&result, {}usize);",
                            ty.word_size()
                        )
                        .unwrap();
                        writeln!(out, "                    return Ok(result);").unwrap();
                    }
                    SelfEvalMode::ZeroAtInit => {
                        writeln!(
                            out,
                            "                    let previous = state.get_state({}usize);",
                            ty.word_size()
                        )
                        .unwrap();
                        writeln!(out, "                    let result = {expr};").unwrap();
                        writeln!(
                            out,
                            "                    state.set_state(&result, {}usize);",
                            ty.word_size()
                        )
                        .unwrap();
                        writeln!(out, "                    return Ok(previous);").unwrap();
                    }
                }
            }
            Instruction::Delay(max_len, src, time) => {
                let dest = self.reg_name(dst)?;
                let src_expr = self.word0_expr(src)?;
                let time_expr = self.word0_expr(time)?;
                writeln!(out, "                    {dest}[0] = state.delay({src_expr}, {time_expr}, {}usize);", max_len).unwrap();
            }
            Instruction::Mem(src) => {
                let dest = self.reg_name(dst)?;
                let src_expr = self.word0_expr(src)?;
                writeln!(
                    out,
                    "                    {dest}[0] = state.mem({src_expr});"
                )
                .unwrap();
            }
            Instruction::AddF(left, right) => {
                self.assign_float_binop(out, dst, left, right, "+")?
            }
            Instruction::SubF(left, right) => {
                self.assign_float_binop(out, dst, left, right, "-")?
            }
            Instruction::MulF(left, right) => {
                self.assign_float_binop(out, dst, left, right, "*")?
            }
            Instruction::DivF(left, right) => {
                self.assign_float_binop(out, dst, left, right, "/")?
            }
            Instruction::ModF(left, right) => {
                self.assign_float_binop(out, dst, left, right, "%")?
            }
            Instruction::PowF(left, right) => {
                self.assign_float_method2(out, dst, left, right, "powf")?
            }
            Instruction::NegF(value) => self.assign_float_unop(out, dst, value, "-")?,
            Instruction::AbsF(value) => self.assign_float_method1(out, dst, value, "abs")?,
            Instruction::SinF(value) => self.assign_float_method1(out, dst, value, "sin")?,
            Instruction::CosF(value) => self.assign_float_method1(out, dst, value, "cos")?,
            Instruction::LogF(value) => self.assign_float_method1(out, dst, value, "ln")?,
            Instruction::SqrtF(value) => self.assign_float_method1(out, dst, value, "sqrt")?,
            Instruction::AddI(left, right) => self.assign_int_binop(out, dst, left, right, "+")?,
            Instruction::SubI(left, right) => self.assign_int_binop(out, dst, left, right, "-")?,
            Instruction::MulI(left, right) => self.assign_int_binop(out, dst, left, right, "*")?,
            Instruction::DivI(left, right) => self.assign_int_binop(out, dst, left, right, "/")?,
            Instruction::ModI(left, right) => self.assign_int_binop(out, dst, left, right, "%")?,
            Instruction::NegI(value) => self.assign_int_unop(out, dst, value, "-")?,
            Instruction::AbsI(value) => self.assign_int_method1(out, dst, value, "abs")?,
            Instruction::Gt(left, right) => self.assign_cmp(out, dst, left, right, ">")?,
            Instruction::Ge(left, right) => self.assign_cmp(out, dst, left, right, ">=")?,
            Instruction::Lt(left, right) => self.assign_cmp(out, dst, left, right, "<")?,
            Instruction::Le(left, right) => self.assign_cmp(out, dst, left, right, "<=")?,
            Instruction::Eq(left, right) => self.assign_cmp(out, dst, left, right, "==")?,
            Instruction::Ne(left, right) => self.assign_cmp(out, dst, left, right, "!=")?,
            Instruction::And(left, right) => {
                self.assign_truthy_binop(out, dst, left, right, "&&")?
            }
            Instruction::Or(left, right) => {
                self.assign_truthy_binop(out, dst, left, right, "||")?
            }
            Instruction::Not(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.word0_expr(value)?;
                writeln!(out, "                    {dest}[0] = f64_to_word(if !truthy({expr}) {{ 1.0 }} else {{ 0.0 }});").unwrap();
            }
            Instruction::CastFtoI(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.word0_expr(value)?;
                writeln!(
                    out,
                    "                    {dest}[0] = i64_to_word(word_to_f64({expr}) as i64);"
                )
                .unwrap();
            }
            Instruction::CastItoF(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.word0_expr(value)?;
                writeln!(
                    out,
                    "                    {dest}[0] = f64_to_word(word_to_i64({expr}) as f64);"
                )
                .unwrap();
            }
            Instruction::CastItoB(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.word0_expr(value)?;
                writeln!(out, "                    {dest}[0] = f64_to_word(if word_to_i64({expr}) != 0 {{ 1.0 }} else {{ 0.0 }});").unwrap();
            }
            Instruction::Array(values, elem_ty) => {
                let dest = self.reg_name(dst)?;
                let elem_words = elem_ty.word_size() as usize;
                writeln!(
                    out,
                    "                    let array_handle = self.arrays.alloc_array({}, {}usize);",
                    values.len(),
                    elem_words
                )
                .unwrap();
                for (index, value) in values.iter().enumerate() {
                    let expr = self.value_vec_expr(value)?;
                    writeln!(out, "                    {{").unwrap();
                    writeln!(
                        out,
                        "                        let array = self.arrays.get_mut(array_handle)?;"
                    )
                    .unwrap();
                    writeln!(
                        out,
                        "                        let start = {index}usize * {}usize;",
                        elem_words
                    )
                    .unwrap();
                    writeln!(
                        out,
                        "                        let end = start + {}usize;",
                        elem_words
                    )
                    .unwrap();
                    writeln!(out, "                        array.data[start..end].copy_from_slice(&{expr}[..{}]);", elem_words).unwrap();
                    writeln!(out, "                    }}").unwrap();
                }
                writeln!(out, "                    {dest}[0] = array_handle;").unwrap();
            }
            Instruction::GetArrayElem(arr, idx, elem_ty) => {
                let dest = self.reg_name(dst)?;
                let arr_expr = self.word0_expr(arr)?;
                let idx_expr = self.word0_expr(idx)?;
                let elem_words = elem_ty.word_size() as usize;
                writeln!(out, "                    {{").unwrap();
                writeln!(
                    out,
                    "                        let array = self.arrays.get({arr_expr})?;"
                )
                .unwrap();
                writeln!(out, "                        let len = if array.elem_size_words == 0 {{ 0usize }} else {{ array.data.len() / array.elem_size_words }};").unwrap();
                writeln!(
                    out,
                    "                        let index_value = word_to_f64({idx_expr});"
                )
                .unwrap();
                writeln!(out, "                        let index = if len == 0 {{ 0usize }} else if !index_value.is_finite() {{ 0usize }} else {{ (index_value as i64).clamp(0, (len - 1) as i64) as usize }};").unwrap();
                writeln!(
                    out,
                    "                        if len == 0 {{ {dest}.fill(0); }} else {{"
                )
                .unwrap();
                writeln!(
                    out,
                    "                            let start = index * {}usize;",
                    elem_words
                )
                .unwrap();
                writeln!(
                    out,
                    "                            let end = start + {}usize;",
                    elem_words
                )
                .unwrap();
                writeln!(
                    out,
                    "                            {dest} = array.data[start..end].to_vec();"
                )
                .unwrap();
                writeln!(out, "                        }}").unwrap();
                writeln!(out, "                    }}").unwrap();
            }
            Instruction::SetArrayElem(arr, idx, value, elem_ty) => {
                let arr_expr = self.word0_expr(arr)?;
                let idx_expr = self.word0_expr(idx)?;
                let value_expr = self.value_vec_expr(value)?;
                let elem_words = elem_ty.word_size() as usize;
                writeln!(out, "                    {{").unwrap();
                writeln!(
                    out,
                    "                        let array = self.arrays.get_mut({arr_expr})?;"
                )
                .unwrap();
                writeln!(out, "                        let len = if array.elem_size_words == 0 {{ 0usize }} else {{ array.data.len() / array.elem_size_words }};").unwrap();
                writeln!(
                    out,
                    "                        let index_value = word_to_f64({idx_expr});"
                )
                .unwrap();
                writeln!(out, "                        let index = if len == 0 {{ 0usize }} else if !index_value.is_finite() {{ 0usize }} else {{ (index_value as i64).clamp(0, (len - 1) as i64) as usize }};").unwrap();
                writeln!(out, "                        if len != 0 {{").unwrap();
                writeln!(
                    out,
                    "                            let start = index * {}usize;",
                    elem_words
                )
                .unwrap();
                writeln!(
                    out,
                    "                            let end = start + {}usize;",
                    elem_words
                )
                .unwrap();
                writeln!(out, "                            array.data[start..end].copy_from_slice(&{value_expr}[..{}]);", elem_words).unwrap();
                writeln!(out, "                        }}").unwrap();
                writeln!(out, "                    }}").unwrap();
            }
            Instruction::Closure(_)
            | Instruction::CallCls(_, _, _)
            | Instruction::String(_)
            | Instruction::CloseUpValues(_, _)
            | Instruction::MakeClosure { .. }
            | Instruction::CloseHeapClosure(_)
            | Instruction::CloneHeap(_)
            | Instruction::CallIndirect(_, _, _)
            | Instruction::GetUpValue(_, _)
            | Instruction::SetUpValue(_, _, _)
            | Instruction::TaggedUnionWrap { .. }
            | Instruction::TaggedUnionGetTag(_)
            | Instruction::TaggedUnionGetValue(_, _)
            | Instruction::BoxAlloc { .. }
            | Instruction::BoxLoad { .. }
            | Instruction::BoxClone { .. }
            | Instruction::BoxRelease { .. }
            | Instruction::BoxStore { .. }
            | Instruction::CloneUserSum { .. }
            | Instruction::ReleaseUserSum { .. }
            | Instruction::PowI(_)
            | Instruction::LogI(_, _)
            | Instruction::Error => {
                return Err(format!(
                    "instruction {:?} is not supported by the initial Rust backend",
                    instr
                ));
            }
        }
        Ok(())
    }

    fn assign_scalar(&self, out: &mut String, dst: &VPtr, expr: String) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        writeln!(out, "                    {dest}[0] = {expr};").unwrap();
        Ok(())
    }

    fn assign_float_binop(
        &self,
        out: &mut String,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.word0_expr(left)?;
        let right_expr = self.word0_expr(right)?;
        writeln!(out, "                    {dest}[0] = f64_to_word(word_to_f64({left_expr}) {op} word_to_f64({right_expr}));").unwrap();
        Ok(())
    }

    fn assign_float_method1(
        &self,
        out: &mut String,
        dst: &VPtr,
        value: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.word0_expr(value)?;
        writeln!(
            out,
            "                    {dest}[0] = f64_to_word(word_to_f64({expr}).{method}());"
        )
        .unwrap();
        Ok(())
    }

    fn assign_float_method2(
        &self,
        out: &mut String,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.word0_expr(left)?;
        let right_expr = self.word0_expr(right)?;
        writeln!(out, "                    {dest}[0] = f64_to_word(word_to_f64({left_expr}).{method}(word_to_f64({right_expr})));").unwrap();
        Ok(())
    }

    fn assign_float_unop(
        &self,
        out: &mut String,
        dst: &VPtr,
        value: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.word0_expr(value)?;
        writeln!(
            out,
            "                    {dest}[0] = f64_to_word({op}word_to_f64({expr}));"
        )
        .unwrap();
        Ok(())
    }

    fn assign_int_binop(
        &self,
        out: &mut String,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.word0_expr(left)?;
        let right_expr = self.word0_expr(right)?;
        writeln!(out, "                    {dest}[0] = i64_to_word(word_to_i64({left_expr}) {op} word_to_i64({right_expr}));").unwrap();
        Ok(())
    }

    fn assign_int_unop(
        &self,
        out: &mut String,
        dst: &VPtr,
        value: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.word0_expr(value)?;
        writeln!(
            out,
            "                    {dest}[0] = i64_to_word({op}word_to_i64({expr}));"
        )
        .unwrap();
        Ok(())
    }

    fn assign_int_method1(
        &self,
        out: &mut String,
        dst: &VPtr,
        value: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.word0_expr(value)?;
        writeln!(
            out,
            "                    {dest}[0] = i64_to_word(word_to_i64({expr}).{method}());"
        )
        .unwrap();
        Ok(())
    }

    fn assign_cmp(
        &self,
        out: &mut String,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.word0_expr(left)?;
        let right_expr = self.word0_expr(right)?;
        writeln!(out, "                    {dest}[0] = f64_to_word(if word_to_f64({left_expr}) {op} word_to_f64({right_expr}) {{ 1.0 }} else {{ 0.0 }});").unwrap();
        Ok(())
    }

    fn assign_truthy_binop(
        &self,
        out: &mut String,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.word0_expr(left)?;
        let right_expr = self.word0_expr(right)?;
        writeln!(out, "                    {dest}[0] = f64_to_word(if truthy({left_expr}) {op} truthy({right_expr}) {{ 1.0 }} else {{ 0.0 }});").unwrap();
        Ok(())
    }

    fn call_kind(&self, callee: &VPtr) -> Result<CallKind, String> {
        match callee.as_ref() {
            Value::Function(index) => Ok(CallKind::FunctionHandle(format!(
                "encode_function({index})"
            ))),
            Value::ExtFunction(symbol, _) => Ok(CallKind::Ext(*symbol)),
            Value::Register(reg) => Ok(CallKind::FunctionHandle(format!("reg_{reg}[0]"))),
            Value::Argument(index) => Ok(CallKind::FunctionHandle(format!("arg_{index}[0]"))),
            _ => Err(format!(
                "unsupported callee value for initial Rust backend: {:?}",
                callee
            )),
        }
    }

    fn reg_name(&self, value: &VPtr) -> Result<String, String> {
        match value.as_ref() {
            Value::Register(reg) => Ok(format!("reg_{reg}")),
            _ => Err(format!("destination is not a register: {value:?}")),
        }
    }

    fn word0_expr(&self, value: &VPtr) -> Result<String, String> {
        match value.as_ref() {
            Value::Argument(index) => Ok(format!("arg_{index}[0]")),
            Value::Register(reg) => Ok(format!("reg_{reg}[0]")),
            Value::Function(index) => Ok(format!("encode_function({index})")),
            Value::None => Ok("0u64".to_string()),
            _ => Err(format!(
                "value is not representable as a single word in the initial Rust backend: {:?}",
                value
            )),
        }
    }

    fn value_vec_expr(&self, value: &VPtr) -> Result<String, String> {
        match value.as_ref() {
            Value::Argument(index) => Ok(format!("arg_{index}.clone()")),
            Value::Register(reg) => Ok(format!("reg_{reg}.clone()")),
            Value::Function(index) => Ok(format!("vec![encode_function({index})]")),
            Value::None => Ok("Vec::new()".to_string()),
            _ => Err(format!(
                "value is not representable as a word vector in the initial Rust backend: {:?}",
                value
            )),
        }
    }

    fn global_index(
        &self,
        global_slots: &[(String, VPtr, usize)],
        target: &VPtr,
    ) -> Result<usize, String> {
        global_slots
            .iter()
            .position(|(_key, value, _size)| value == target)
            .ok_or_else(|| format!("unknown global slot for {target:?}"))
    }
}
