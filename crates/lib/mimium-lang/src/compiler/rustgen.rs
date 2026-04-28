use std::collections::{BTreeMap, HashMap};
use std::io::Write;
use std::sync::Arc;

use crate::compiler::Config;
use crate::compiler::bytecodegen::SelfEvalMode;
use crate::interner::{Symbol, TypeNodeId};
use crate::mir::{Function, Instruction, Mir, VPtr, VReg, Value};
use crate::types::Type;

const PROGRAM_TEMPLATE: &str = include_str!("mimium_placeholder.rs.template");
const GLOBAL_SLOTS_MARKER: &str = "/*__GLOBAL_SLOTS__*/";
const FUNCTION_STATES_MARKER: &str = "/*__FUNCTION_STATES__*/";
const CALL_DSP_MARKER: &str = "/*__CALL_DSP__*/";
const CALL_MAIN_MARKER: &str = "/*__CALL_MAIN__*/";
const HANDLE_DISPATCH_MARKER: &str = "/*__HANDLE_DISPATCH__*/";
const FUNCTIONS_MARKER: &str = "/*__FUNCTIONS__*/";

pub struct RustGenerator {
    mir: Arc<Mir>,
    config: Config,
}

enum CallKind {
    FunctionHandle(String),
    Ext(Symbol),
}

struct CodeWriter<W> {
    inner: W,
    indent: usize,
}

impl<W: Write> CodeWriter<W> {
    const INDENT_WIDTH: usize = 4;

    fn with_indent(inner: W, indent: usize) -> Self {
        Self { inner, indent }
    }

    fn line(&mut self, line: impl AsRef<str>) -> Result<(), String> {
        self.write_indent()?;
        self.inner
            .write_all(line.as_ref().as_bytes())
            .map_err(|err| format!("failed to write generated Rust line: {err}"))?;
        self.inner
            .write_all(b"\n")
            .map_err(|err| format!("failed to terminate generated Rust line: {err}"))
    }

    fn blank_line(&mut self) -> Result<(), String> {
        self.inner
            .write_all(b"\n")
            .map_err(|err| format!("failed to write generated Rust blank line: {err}"))
    }

    fn indented<T>(
        &mut self,
        levels: usize,
        render: impl FnOnce(&mut Self) -> Result<T, String>,
    ) -> Result<T, String> {
        self.indent += levels;
        let result = render(self);
        self.indent -= levels;
        result
    }

    fn write_indent(&mut self) -> Result<(), String> {
        let padding = " ".repeat(self.indent * Self::INDENT_WIDTH);
        self.inner
            .write_all(padding.as_bytes())
            .map_err(|err| format!("failed to write generated Rust indentation: {err}"))
    }
}

impl RustGenerator {
    pub fn new(mir: Arc<Mir>, config: Config) -> Self {
        Self { mir, config }
    }

    pub fn generate(&self) -> Result<String, String> {
        let mut out = Vec::new();
        self.generate_to(&mut out)?;
        String::from_utf8(out).map_err(|err| format!("generated Rust was not valid UTF-8: {err}"))
    }

    pub fn generate_to<W: Write>(&self, mut out: W) -> Result<(), String> {
        let source = self.render_program()?;
        out.write_all(source.as_bytes())
            .map_err(|err| format!("failed to write generated Rust source: {err}"))
    }

    fn render_program(&self) -> Result<String, String> {
        let global_slots = self.collect_global_slots();
        let mut program = PROGRAM_TEMPLATE.to_string();

        self.replace_template_marker(
            &mut program,
            GLOBAL_SLOTS_MARKER,
            &self.render_section(4, |writer| self.emit_global_slots(writer, &global_slots))?,
        )?;
        self.replace_template_marker(
            &mut program,
            FUNCTION_STATES_MARKER,
            &self.render_section(4, |writer| self.emit_function_states(writer))?,
        )?;
        self.replace_template_marker(
            &mut program,
            CALL_DSP_MARKER,
            &self.render_section(1, |writer| self.emit_call_dsp(writer))?,
        )?;
        self.replace_template_marker(
            &mut program,
            CALL_MAIN_MARKER,
            &self.render_section(1, |writer| self.emit_call_main(writer))?,
        )?;
        self.replace_template_marker(
            &mut program,
            HANDLE_DISPATCH_MARKER,
            &self.render_section(3, |writer| self.emit_function_handle_dispatch(writer))?,
        )?;
        self.replace_template_marker(
            &mut program,
            FUNCTIONS_MARKER,
            &self.render_section(1, |writer| self.emit_functions(writer, &global_slots))?,
        )?;

        Ok(program)
    }

    fn render_section(
        &self,
        base_indent: usize,
        render: impl FnOnce(&mut CodeWriter<&mut Vec<u8>>) -> Result<(), String>,
    ) -> Result<String, String> {
        let mut buffer = Vec::new();
        {
            let mut writer = CodeWriter::with_indent(&mut buffer, base_indent);
            render(&mut writer)?;
        }
        String::from_utf8(buffer)
            .map_err(|err| format!("generated Rust section was not valid UTF-8: {err}"))
    }

    fn replace_template_marker(
        &self,
        template: &mut String,
        marker: &str,
        replacement: &str,
    ) -> Result<(), String> {
        if !template.contains(marker) {
            return Err(format!("missing Rust template marker: {marker}"));
        }
        *template = template.replace(marker, replacement);
        Ok(())
    }

    fn emit_global_slots<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        global_slots: &[(String, VPtr, usize)],
    ) -> Result<(), String> {
        global_slots
            .iter()
            .try_for_each(|(_key, _value, size)| writer.line(format!("vec![0; {size}],")))
    }

    fn emit_function_states<W: Write>(&self, writer: &mut CodeWriter<W>) -> Result<(), String> {
        self.mir.functions.iter().try_for_each(|func| {
            writer.line(format!(
                "StateStorage::new({}),",
                func.state_skeleton.total_size()
            ))
        })
    }

    fn emit_call_dsp<W: Write>(&self, writer: &mut CodeWriter<W>) -> Result<(), String> {
        let Some(dsp_func) = self.find_function("dsp") else {
            return Ok(());
        };
        let dsp_index = dsp_func.index;
        let dsp_return = dsp_func.return_type.get().copied();

        writer.line("pub fn call_dsp(&mut self, args: &[Word]) -> Result<Vec<Word>, String> {")?;
        writer.indented(1, |writer| {
            writer.line("let mut memory = std::mem::take(&mut self.memory);")?;
            writer.line(format!(
                "let result = self.call_function_handle_with_memory(encode_function({dsp_index}), args, &mut memory)?;"
            ))?;
            if let Some(ret_ty) = dsp_return.filter(|ty| self.is_deepcopy_aggregate_type(*ty)) {
                let size = ret_ty.word_size() as usize;
                writer.line(format!(
                    "let final_result = if result.is_empty() {{ Ok(Vec::new()) }} else {{ memory.load(result[0], {size}usize) }};"
                ))
                ?;
                writer.line("self.memory = memory;")?;
                writer.line("final_result")
            } else {
                writer.line("self.memory = memory;")?;
                writer.line("Ok(result)")
            }
        })?;
        writer.line("}")?;
        writer.blank_line()
    }

    fn emit_call_main<W: Write>(&self, writer: &mut CodeWriter<W>) -> Result<(), String> {
        let main_func = self.find_function("main").or_else(|| {
            (!self.collect_global_slots().is_empty())
                .then(|| self.mir.functions.first())
                .flatten()
        });
        let Some(main_func) = main_func else {
            return Ok(());
        };
        let main_index = main_func.index;
        let main_return = main_func.return_type.get().copied();

        writer.line("pub fn call_main(&mut self) -> Result<Vec<Word>, String> {")?;
        writer.indented(1, |writer| {
            writer.line("let mut memory = std::mem::take(&mut self.memory);")?;
            writer.line(format!(
                "let result = self.call_function_handle_with_memory(encode_function({main_index}), &[], &mut memory)?;"
            ))?;
            if let Some(ret_ty) = main_return.filter(|ty| self.is_deepcopy_aggregate_type(*ty)) {
                let size = ret_ty.word_size() as usize;
                writer.line(format!(
                    "let final_result = if result.is_empty() {{ Ok(Vec::new()) }} else {{ memory.load(result[0], {size}usize) }};"
                ))
                ?;
                writer.line("self.memory = memory;")?;
                writer.line("final_result")
            } else {
                writer.line("self.memory = memory;")?;
                writer.line("Ok(result)")
            }
        })?;
        writer.line("}")?;
        writer.blank_line()
    }

    fn emit_function_handle_dispatch<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
    ) -> Result<(), String> {
        self.mir.functions.iter().try_for_each(|func| {
            writer.line(format!("Some({}) => {{", func.index))?;
            writer.indented(1, |writer| {
                writer.line(format!(
                    "let mut state = self.take_call_state(handle, {})?;",
                    func.index
                ))?;
                writer.line(format!(
                    "let result = self.func_{}(&mut state, args, current_closure, memory);",
                    func.index
                ))?;
                writer.line(format!(
                    "self.restore_call_state(handle, {}, state)?;",
                    func.index
                ))?;
                writer.line("result")
            })?;
            writer.line("},")
        })
    }

    fn emit_functions<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        global_slots: &[(String, VPtr, usize)],
    ) -> Result<(), String> {
        for (index, func) in self.mir.functions.iter().enumerate() {
            if index != 0 {
                writer.blank_line()?;
            }
            self.emit_function(writer, func, global_slots)?;
        }
        Ok(())
    }

    fn find_function_by_name(&self, name: &str) -> Option<usize> {
        self.mir
            .functions
            .iter()
            .find(|func| func.label.as_str() == name)
            .map(|func| func.index)
    }

    fn find_function(&self, name: &str) -> Option<&Function> {
        self.mir
            .functions
            .iter()
            .find(|func| func.label.as_str() == name)
    }

    fn default_argument_functions(&self, target_index: usize) -> Vec<usize> {
        let prefix = format!("__default_{target_index}_");
        let mut defaults = self
            .mir
            .functions
            .iter()
            .filter(|func| func.label.as_str().starts_with(&prefix))
            .map(|func| func.index)
            .collect::<Vec<_>>();
        defaults.sort_unstable();
        defaults
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

    fn emit_function<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        global_slots: &[(String, VPtr, usize)],
    ) -> Result<(), String> {
        let register_sizes = self.collect_register_sizes(func)?;
        let block_preds = self.collect_block_predecessors(func);
        let fallthrough_edges = self.collect_fallthrough_edges(func);

        writer.line(format!(
            "fn func_{}(&mut self, state: &mut StateStorage, args: &[Word], current_closure: Option<Word>, memory: &mut MemoryStore) -> Result<Vec<Word>, String> {{",
            func.index
        ))?;
        writer.indented(1, |writer| {
            writer.line("let mut bb: usize = 0;")?;
            writer.line("let mut pred_bb: usize = 0;")?;
            writer.line("let mut arg_offset = 0usize;")?;

            for (index, arg) in func.args.iter().enumerate() {
                let size = arg.1.word_size() as usize;
                writer.line(format!(
                    "let arg_{index} = copy_words::<{size}>(&args[arg_offset..arg_offset + {size}])?;"
                ))?;
                writer.line(format!("arg_offset += {size};"))?;
            }

            let mut regs: Vec<_> = register_sizes.into_iter().collect();
            regs.sort_by_key(|(reg, _size)| *reg);
            for (reg, size) in regs {
                writer.line(format!("let mut reg_{reg} = [0u64; {size}];"))?;
            }

            writer.line("loop {")?;
            writer.indented(1, |writer| {
                writer.line("match bb {")?;
                writer.indented(1, |writer| {
                    for (block_index, block) in func.body.iter().enumerate() {
                        writer.line(format!("{block_index} => {{"))?;
                        writer.indented(1, |writer| {
                            for (dst, instr) in &block.0 {
                                self.emit_instruction(
                                    writer,
                                    func,
                                    block_index,
                                    dst,
                                    instr,
                                    global_slots,
                                    &block_preds,
                                )?;
                            }
                            if let Some((target_bb, pred_source)) =
                                fallthrough_edges.get(block_index).and_then(|target| *target)
                            {
                                writer.line(format!("pred_bb = {pred_source};"))?;
                                writer.line(format!("bb = {target_bb}usize;"))?;
                                writer.line("continue;")
                            } else {
                                writer.line(format!(
                                    "return Err(\"basic block {block_index} in function {} fell through without terminator\".to_string());",
                                    func.index
                                ))
                            }
                        })?;
                        writer.line("},")?;
                    }

                    writer.line(format!(
                        "_ => return Err(format!(\"invalid basic block {{}} in function {}\", bb)),",
                        func.index
                    ))
                })?;
                writer.line("}")
            })?;
            writer.line("}")
        })?;
        writer.line("}")
    }

    fn collect_register_sizes(&self, func: &Function) -> Result<HashMap<VReg, usize>, String> {
        let mut sizes = HashMap::new();
        for block in &func.body {
            for (dst, instr) in &block.0 {
                if let Value::Register(reg) = dst.as_ref() {
                    let size = self.instruction_result_size(func, instr)?;
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
            | Instruction::CallIndirect(_, _, ty) => self.runtime_value_size(*ty),
            Instruction::GetArrayElem(_, _, ty) => self.runtime_value_size(*ty),
            Instruction::Return(_, ty) | Instruction::ReturnFeed(_, ty) => ty.word_size() as usize,
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

    fn collect_fallthrough_edges(&self, func: &Function) -> Vec<Option<(usize, usize)>> {
        let mut edges = vec![None; func.body.len()];

        let mut fill_branch_region = |start: usize, end: usize, merge: usize| {
            let region_end = end.min(func.body.len());
            for edge in edges.iter_mut().take(region_end).skip(start) {
                *edge = Some((merge, start));
            }
        };

        for block in &func.body {
            for (_dst, instr) in &block.0 {
                match instr {
                    Instruction::JmpIf(_, then_bb, else_bb, merge_bb) => {
                        let merge = *merge_bb as usize;
                        let mut starts = [*then_bb as usize, *else_bb as usize];
                        starts.sort_unstable();
                        fill_branch_region(starts[0], starts[1], merge);
                        fill_branch_region(starts[1], merge, merge);
                    }
                    Instruction::Switch {
                        cases,
                        default_block,
                        merge_block,
                        ..
                    } => {
                        let merge = *merge_block as usize;
                        let mut starts = cases
                            .iter()
                            .map(|(_literal, case_bb)| *case_bb as usize)
                            .collect::<Vec<_>>();
                        if let Some(default_bb) = default_block {
                            starts.push(*default_bb as usize);
                        }
                        starts.sort_unstable();
                        starts.dedup();
                        for (index, start) in starts.iter().enumerate() {
                            let end = starts.get(index + 1).copied().unwrap_or(merge);
                            fill_branch_region(*start, end, merge);
                        }
                    }
                    _ => {}
                }
            }
        }
        edges
    }

    fn emit_instruction<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        block_index: usize,
        dst: &VPtr,
        instr: &Instruction,
        global_slots: &[(String, VPtr, usize)],
        block_preds: &[Vec<usize>],
    ) -> Result<(), String> {
        match instr {
            Instruction::Uinteger(value) => {
                self.assign_scalar(writer, dst, format!("{value}u64"))?
            }
            Instruction::Integer(value) => {
                self.assign_scalar(writer, dst, format!("i64_to_word({value}i64)"))?
            }
            Instruction::Float(value) => {
                self.assign_scalar(writer, dst, format!("f64_to_word({value:?}f64)"))?
            }
            Instruction::Alloc(ty) => self.assign_scalar(
                writer,
                dst,
                format!("memory.alloc({}usize)", ty.word_size()),
            )?,
            Instruction::Load(ptr, ty) => {
                if self.is_deepcopy_aggregate_type(*ty) {
                    let dest = self.reg_name(dst)?;
                    writer.line(format!(
                        "{dest}[0] = memory.alloc({}usize);",
                        ty.word_size()
                    ))?;
                    self.emit_deep_copy_value_into_ptr(writer, &format!("{dest}[0]"), ptr, *ty)?;
                } else {
                    let dest = self.reg_name(dst)?;
                    let ptr_expr = self.word0_expr(ptr)?;
                    writer.line(format!(
                        "{dest} = {};",
                        self.vec_to_array_expr(
                            format!("memory.load({ptr_expr}, {}usize)?", ty.word_size()),
                            self.runtime_value_size(*ty),
                        )
                    ))?;
                }
            }
            Instruction::Store(ptr, src, ty) => {
                let ptr_expr = self.word0_expr(ptr)?;
                if self.is_deepcopy_aggregate_type(*ty) {
                    self.emit_deep_copy_value_into_ptr(writer, &ptr_expr, src, *ty)?;
                } else {
                    let src_expr = self.context_value_slice_expr(func, src, *ty)?;
                    writer.line(format!(
                        "memory.store({ptr_expr}, {src_expr}, {}usize)?;",
                        ty.word_size()
                    ))?;
                }
            }
            Instruction::GetElement {
                value,
                tuple_offset,
                ..
            } => {
                let dest = self.reg_name(dst)?;
                let base_expr = self.word0_expr(value)?;
                let word_offset = self.getelement_word_offset(instr)?;
                writer.line(format!(
                    "{dest}[0] = memory.get_element({base_expr}, {}usize)?;",
                    word_offset
                ))?;
            }
            Instruction::Call(callee, args, ret_ty) => {
                let dest = self.reg_name(dst)?;
                let call_kind = self.call_kind(callee)?;
                let result_size = self.instruction_result_size(func, instr)?;
                let static_callee = self.resolve_function_index(func, callee);
                writer.line("let mut call_args = Vec::new();")?;
                for (arg, ty) in args {
                    let expr = self.boundary_value_slice_expr(func, arg, *ty)?;
                    writer.line(format!("call_args.extend_from_slice({expr});"))?;
                }
                if let Some(target_index) = static_callee {
                    let target_func = self
                        .mir
                        .functions
                        .iter()
                        .find(|candidate| candidate.index == target_index)
                        .ok_or_else(|| {
                            format!("missing target function for index {target_index}")
                        })?;
                    let provided_args = args.len();
                    let expected_args = target_func.args.len();
                    if provided_args < expected_args {
                        let default_functions = self.default_argument_functions(target_index);
                        let missing_args = expected_args - provided_args;
                        if default_functions.len() < missing_args {
                            return Err(format!(
                                "function {} expects {} args but only {} provided and {} defaults found",
                                target_func.label,
                                expected_args,
                                provided_args,
                                default_functions.len()
                            ));
                        }
                        for default_index in default_functions.into_iter().take(missing_args) {
                            writer.line(format!(
                                "call_args.extend_from_slice(&self.func_{default_index}(state, &[], None, memory)?);"
                            ))?;
                        }
                    }
                }
                match call_kind {
                    CallKind::FunctionHandle(handle_expr) => {
                        if let Some(target_index) = static_callee {
                            writer.line(format!(
                                "{dest} = {};",
                                self.vec_to_array_expr(
                                    format!(
                                        "self.func_{target_index}(state, &call_args, None, memory)?"
                                    ),
                                    result_size,
                                )
                            ))?;
                        } else {
                            writer.line(format!(
                                "{dest} = {};",
                                self.vec_to_array_expr(
                                    format!("self.call_function_handle_with_memory({handle_expr}, &call_args, memory)?"),
                                    result_size,
                                )
                            ))?;
                        }
                    }
                    CallKind::Ext(symbol) => {
                        let name = format!("{:?}", symbol.as_str());
                        let ret_words = ret_ty.word_size() as usize;
                        if self.is_deepcopy_aggregate_type(*ret_ty) {
                            writer.line(format!(
                                "let call_result = self.call_ext({name}, &call_args, {ret_words})?;"
                            ))?;
                            writer.line(format!("{dest}[0] = memory.alloc({ret_words}usize);"))?;
                            writer.line(format!(
                                "memory.store({dest}[0], &call_result, {ret_words}usize)?;"
                            ))?;
                        } else {
                            writer.line(format!(
                                "{dest} = {};",
                                self.vec_to_array_expr(
                                    format!("self.call_ext({name}, &call_args, {ret_words})?"),
                                    result_size,
                                )
                            ))?;
                        }
                    }
                }
            }
            Instruction::CallIndirect(callee, args, ret_ty) => {
                let runtime_expr = match callee.as_ref() {
                    Value::ExtFunction(symbol, _) if args.is_empty() && ret_ty.word_size() == 1 => {
                        match symbol.as_str() {
                            "_mimium_getnow" => {
                                Some("f64_to_word(self.host.current_time())".to_string())
                            }
                            "_mimium_getsamplerate" => {
                                Some("f64_to_word(self.host.sample_rate())".to_string())
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                };

                if let Some(expr) = runtime_expr {
                    self.assign_scalar(writer, dst, expr)?;
                } else {
                    let dest = self.reg_name(dst)?;
                    let result_size = self.instruction_result_size(func, instr)?;
                    let handle_expr = self.word0_expr(callee)?;
                    writer.line("let mut call_args = Vec::new();")?;
                    for (arg, ty) in args {
                        let expr = self.boundary_value_slice_expr(func, arg, *ty)?;
                        writer.line(format!("call_args.extend_from_slice({expr});"))?;
                    }
                    writer.line(format!(
                        "{dest} = {};",
                        self.vec_to_array_expr(
                            format!("self.call_function_handle_with_memory({handle_expr}, &call_args, memory)?"),
                            result_size,
                        )
                    ))?;
                }
            }
            Instruction::MakeClosure { fn_proto, .. } => {
                let dest = self.reg_name(dst)?;
                let handle_expr = self.word0_expr(fn_proto)?;
                let closure_function =
                    self.resolve_function_index(func, fn_proto).ok_or_else(|| {
                        format!("could not resolve closure target for {:?}", fn_proto)
                    })?;
                let closure_func = self
                    .mir
                    .functions
                    .iter()
                    .find(|candidate| candidate.index == closure_function)
                    .ok_or_else(|| format!("missing closure function {}", closure_function))?;

                writer.line("let mut closure_upvalues = Vec::new();")?;
                writer.line("let mut closure_indirect = Vec::new();")?;
                for upvalue in &closure_func.upindexes {
                    match upvalue.as_ref() {
                        Value::Register(reg) => {
                            if let Some(alloc_ty) = self.resolve_register_alloc_type(func, *reg) {
                                let alloc_size = alloc_ty.word_size() as usize;
                                let upvalue_expr = self.word0_expr(upvalue)?;
                                if alloc_size == 1 {
                                    if self.should_capture_alloc_by_value(alloc_ty) {
                                        writer.line(format!(
                                            "closure_upvalues.push(memory.load({upvalue_expr}, 1usize)?[0]);"
                                        ))?;
                                        writer.line("closure_indirect.push(false);")?;
                                    } else {
                                        writer.line(format!(
                                            "closure_upvalues.push({upvalue_expr});"
                                        ))?;
                                        writer.line("closure_indirect.push(true);")?;
                                    }
                                } else {
                                    writer.line(format!(
                                        "let captured_upvalue = memory.alloc({alloc_size}usize);"
                                    ))?;
                                    writer.line("{")?;
                                    writer.indented(1, |writer| {
                                        writer.line(format!(
                                            "let copied_words = memory.load({upvalue_expr}, {alloc_size}usize)?;"
                                        ))?;
                                        writer.line(format!(
                                            "memory.store(captured_upvalue, &copied_words, {alloc_size}usize)?;"
                                        ))
                                    })?;
                                    writer.line("}")?;
                                    writer.line("closure_upvalues.push(captured_upvalue);")?;
                                    writer.line("closure_indirect.push(true);")?;
                                }
                            } else if let Some(value_ty) =
                                self.resolve_register_value_type(func, *reg)
                            {
                                if self.is_deepcopy_aggregate_type(value_ty) {
                                    let upvalue_expr = self.word0_expr(upvalue)?;
                                    let value_size = value_ty.word_size() as usize;
                                    writer.line(format!(
                                        "let captured_upvalue = memory.alloc({value_size}usize);"
                                    ))?;
                                    writer.line("{")?;
                                    writer.indented(1, |writer| {
                                        writer.line(format!(
                                            "let copied_words = memory.load({upvalue_expr}, {value_size}usize)?;"
                                        ))?;
                                        writer.line(format!(
                                            "memory.store(captured_upvalue, &copied_words, {value_size}usize)?;"
                                        ))
                                    })?;
                                    writer.line("}")?;
                                    writer.line("closure_upvalues.push(captured_upvalue);")?;
                                    writer.line("closure_indirect.push(true);")?;
                                } else {
                                    let upvalue_expr = self.scalar_word_expr(func, upvalue)?;
                                    writer
                                        .line(format!("closure_upvalues.push({upvalue_expr});"))?;
                                    writer.line("closure_indirect.push(false);")?;
                                }
                            } else {
                                let upvalue_expr = self.scalar_word_expr(func, upvalue)?;
                                writer.line(format!("closure_upvalues.push({upvalue_expr});"))?;
                                writer.line("closure_indirect.push(false);")?;
                            }
                        }
                        Value::Argument(index) => {
                            let arg_ty =
                                func.args.get(*index).map(|arg| arg.1).ok_or_else(|| {
                                    format!("invalid upvalue argument index {}", index)
                                })?;
                            if self.is_deepcopy_aggregate_type(arg_ty) {
                                let value_size = arg_ty.word_size() as usize;
                                writer.line(format!(
                                    "let captured_upvalue = memory.alloc({value_size}usize);"
                                ))?;
                                writer.line(format!(
                                    "memory.store(captured_upvalue, &arg_{index}, {value_size}usize)?;"
                                ))?;
                                writer.line("closure_upvalues.push(captured_upvalue);")?;
                                writer.line("closure_indirect.push(true);")?;
                            } else {
                                let upvalue_expr = self.scalar_word_expr(func, upvalue)?;
                                writer.line(format!("closure_upvalues.push({upvalue_expr});"))?;
                                writer.line("closure_indirect.push(false);")?;
                            }
                        }
                        Value::Function(_) => {
                            let upvalue_expr = self.word0_expr(upvalue)?;
                            writer.line(format!("closure_upvalues.push({upvalue_expr});"))?;
                            writer.line("closure_indirect.push(false);")?;
                        }
                        _ => {
                            return Err(format!(
                                "unsupported upvalue for initial Rust backend: {:?}",
                                upvalue
                            ));
                        }
                    }
                }

                writer.line(format!(
                    "{dest}[0] = self.closures.alloc({handle_expr}, closure_upvalues, closure_indirect, {}usize)?;"
                    ,
                    closure_func.state_skeleton.total_size()
                ))?;
            }
            Instruction::CloneHeap(src) | Instruction::CloseHeapClosure(src) => {
                let src_expr = self.scalar_word_expr(func, src)?;
                self.assign_scalar(writer, dst, src_expr)?;
            }
            Instruction::GetUpValue(index, ty) => {
                let size = ty.word_size() as usize;
                writer.line("let closure_handle = current_closure.ok_or_else(|| \"missing closure context for GetUpValue\".to_string())?;")?;
                if self.is_deepcopy_aggregate_type(*ty) {
                    let dest = self.reg_name(dst)?;
                    writer.line(format!("{dest}[0] = memory.alloc({size}usize);"))?;
                    writer.line("{")?;
                    writer.indented(1, |writer| {
                        writer.line(format!(
                            "let copied_words = self.load_upvalue(closure_handle, {}usize, {size}usize, memory)?;",
                            *index as usize
                        ))?;
                        writer.line(format!(
                            "memory.store({dest}[0], &copied_words, {size}usize)?;"
                        ))
                    })?;
                    writer.line("}")?;
                } else {
                    let dest = self.reg_name(dst)?;
                    writer.line(format!(
                        "{dest} = {};",
                        self.vec_to_array_expr(
                            format!(
                                "self.load_upvalue(closure_handle, {}usize, {size}usize, memory)?",
                                *index as usize
                            ),
                            size,
                        )
                    ))?;
                }
            }
            Instruction::SetUpValue(index, src, ty) => {
                let dest = self.reg_name(dst)?;
                let src_expr = self.boundary_value_slice_expr(func, src, *ty)?;
                let src_array = self.context_value_array_expr(func, src, *ty)?;
                let size = ty.word_size() as usize;
                writer.line("let closure_handle = current_closure.ok_or_else(|| \"missing closure context for SetUpValue\".to_string())?;")?;
                writer.line(format!(
                    "self.store_upvalue(closure_handle, {}usize, {src_expr}, {size}usize, memory)?;",
                    *index as usize
                ))?;
                writer.line(format!("{dest} = {src_array};"))?;
            }
            Instruction::GetGlobal(global, ty) => {
                let global_index = self.global_index(global_slots, global)?;
                if self.is_deepcopy_aggregate_type(*ty) {
                    let dest = self.reg_name(dst)?;
                    writer.line(format!(
                        "{dest}[0] = memory.alloc({}usize);",
                        ty.word_size()
                    ))?;
                    writer.line(format!(
                        "memory.store({dest}[0], &self.globals[{global_index}][..{}], {}usize)?;",
                        ty.word_size(),
                        ty.word_size()
                    ))?;
                } else {
                    let dest = self.reg_name(dst)?;
                    writer.line(format!(
                        "{dest} = {};",
                        self.slice_to_array_expr(
                            format!("&self.globals[{global_index}][..{}]", ty.word_size()),
                            ty.word_size() as usize,
                        )
                    ))?;
                }
            }
            Instruction::SetGlobal(global, src, ty) => {
                let global_index = self.global_index(global_slots, global)?;
                if self.is_deepcopy_aggregate_type(*ty) {
                    let size = ty.word_size() as usize;
                    let src_expr = self.boundary_value_slice_expr(func, src, *ty)?;
                    writer.line(format!(
                        "self.globals[{global_index}][..{size}].copy_from_slice({src_expr});"
                    ))?;
                } else {
                    let src_expr = self.context_value_slice_expr(func, src, *ty)?;
                    writer.line(format!(
                        "self.globals[{global_index}][..{}].copy_from_slice({src_expr});",
                        ty.word_size()
                    ))?;
                }
            }
            Instruction::PushStateOffset(offset) => {
                writer.line(format!("state.push_pos({offset}usize);"))?;
            }
            Instruction::PopStateOffset(offset) => {
                writer.line(format!("state.pop_pos({offset}usize);"))?;
            }
            Instruction::GetState(ty) => {
                if self.is_deepcopy_aggregate_type(*ty) {
                    let dest = self.reg_name(dst)?;
                    let size = ty.word_size() as usize;
                    writer.line(format!("{dest}[0] = memory.alloc({size}usize);"))?;
                    writer.line(format!(
                        "memory.store({dest}[0], &state.get_state({size}usize), {size}usize)?;"
                    ))?;
                } else {
                    let dest = self.reg_name(dst)?;
                    writer.line(format!(
                        "{dest} = {};",
                        self.vec_to_array_expr(
                            format!("state.get_state({}usize)", ty.word_size()),
                            ty.word_size() as usize,
                        )
                    ))?;
                }
            }
            Instruction::JmpIf(cond, then_bb, else_bb, _merge_bb) => {
                let cond_expr = self.scalar_word_expr(func, cond)?;
                writer.line(format!("pred_bb = {block_index};"))?;
                writer.line(format!(
                    "bb = if truthy({cond_expr}) {{ {}usize }} else {{ {}usize }};",
                    then_bb, else_bb
                ))?;
                writer.line("continue;")?;
            }
            Instruction::Jmp(offset) => {
                writer.line(format!("pred_bb = {block_index};"))?;
                writer.line(format!(
                    "bb = (({block_index}isize) + ({}isize)) as usize;",
                    offset
                ))?;
                writer.line("continue;")?;
            }
            Instruction::Phi(left, right) => {
                let preds = block_preds.get(block_index).cloned().unwrap_or_default();
                if preds.len() < 2 {
                    return Err(format!(
                        "Phi in block {block_index} does not have enough predecessors"
                    ));
                }
                let dest = self.reg_name(dst)?;
                let left_expr = self.value_array_expr(left)?;
                let right_expr = self.value_array_expr(right)?;
                writer.line(format!("if pred_bb == {}usize {{", preds[0]))?;
                writer.indented(1, |writer| writer.line(format!("{dest} = {left_expr};")))?;
                writer.line(format!("}} else if pred_bb == {}usize {{", preds[1]))?;
                writer.indented(1, |writer| writer.line(format!("{dest} = {right_expr};")))?;
                writer.line("} else {")?;
                writer.indented(1, |writer| {
                    writer.line(format!(
                        "return Err(format!(\"phi predecessor mismatch in block {block_index}: {{}}\", pred_bb));"
                    ))
                })?;
                writer.line("}")?;
            }
            Instruction::Switch {
                scrutinee,
                cases,
                default_block,
                ..
            } => {
                let scrutinee_expr = self.scalar_word_expr(func, scrutinee)?;
                writer.line(format!("let scrutinee = word_to_i64({scrutinee_expr});"))?;
                writer.line(format!("pred_bb = {block_index};"))?;
                writer.line("bb = match scrutinee {")?;
                writer.indented(1, |writer| {
                    for (literal, case_bb) in cases {
                        writer.line(format!("{literal} => {}usize,", case_bb))?;
                    }
                    if let Some(default_bb) = default_block {
                        writer.line(format!("_ => {}usize,", default_bb))?;
                    } else {
                        writer.line(format!(
                            "_ => return Err(\"non exhaustive switch in block {block_index}\".to_string()),"
                        ))?;
                    }
                    Ok(())
                })?;
                writer.line("};")?;
                writer.line("continue;")?;
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
                writer.line(format!("{dest} = match pred_bb {{"))?;
                writer.indented(1, |writer| {
                    for (pred, input) in preds.iter().zip(inputs.iter()) {
                        let expr = self.value_array_expr(input)?;
                        writer.line(format!("{pred}usize => {expr},"))?;
                    }
                    writer.line(format!(
                        "_ => return Err(\"phi switch predecessor mismatch in block {block_index}\".to_string()),"
                    ))
                })?;
                writer.line("};")?;
            }
            Instruction::Return(value, ty) => {
                if matches!(value.as_ref(), Value::None) || ty.word_size() == 0 {
                    writer.line("return Ok(Vec::new());")?;
                } else {
                    let expr = self.context_value_vec_expr(func, value, *ty)?;
                    writer.line(format!("return Ok({expr});"))?;
                }
            }
            Instruction::ReturnFeed(value, ty) => {
                let expr = self.context_value_array_expr(func, value, *ty)?;
                match self.config.self_eval_mode {
                    SelfEvalMode::SimpleState => {
                        writer.line(format!("let result = {expr};"))?;
                        let state_expr = self.boundary_value_slice_expr(func, value, *ty)?;
                        writer.line(format!(
                            "state.set_state({state_expr}, {}usize);",
                            ty.word_size()
                        ))?;
                        writer.line("return Ok(result.to_vec());")?;
                    }
                    SelfEvalMode::ZeroAtInit => {
                        if self.is_deepcopy_aggregate_type(*ty) {
                            let size = ty.word_size() as usize;
                            writer.line(format!(
                                "let previous_words = state.get_state({size}usize);"
                            ))?;
                            writer.line(
                                "let previous_handle = memory.alloc(previous_words.len());",
                            )?;
                            writer.line("memory.store(previous_handle, &previous_words, previous_words.len())?;")?;
                        } else {
                            writer.line(format!(
                                "let previous = state.get_state({}usize);",
                                ty.word_size()
                            ))?;
                        }
                        writer.line(format!("let result = {expr};"))?;
                        let state_expr = self.boundary_value_slice_expr(func, value, *ty)?;
                        writer.line(format!(
                            "state.set_state({state_expr}, {}usize);",
                            ty.word_size()
                        ))?;
                        if self.is_deepcopy_aggregate_type(*ty) {
                            writer.line("return Ok(vec![previous_handle]);")?;
                        } else {
                            writer.line("return Ok(previous);")?;
                        }
                    }
                }
            }
            Instruction::Delay(max_len, src, time) => {
                let dest = self.reg_name(dst)?;
                let src_expr = self.scalar_word_expr(func, src)?;
                let time_expr = self.word0_expr(time)?;
                writer.line(format!(
                    "{dest}[0] = state.delay({src_expr}, {time_expr}, {}usize);",
                    max_len
                ))?;
            }
            Instruction::Mem(src) => {
                let dest = self.reg_name(dst)?;
                let src_expr = self.scalar_word_expr(func, src)?;
                writer.line(format!("{dest}[0] = state.mem({src_expr});"))?;
            }
            Instruction::AddF(left, right) => {
                self.assign_float_binop(writer, func, dst, left, right, "+")?
            }
            Instruction::SubF(left, right) => {
                self.assign_float_binop(writer, func, dst, left, right, "-")?
            }
            Instruction::MulF(left, right) => {
                self.assign_float_binop(writer, func, dst, left, right, "*")?
            }
            Instruction::DivF(left, right) => {
                self.assign_float_binop(writer, func, dst, left, right, "/")?
            }
            Instruction::ModF(left, right) => {
                self.assign_float_binop(writer, func, dst, left, right, "%")?
            }
            Instruction::PowF(left, right) => {
                self.assign_float_method2(writer, func, dst, left, right, "powf")?
            }
            Instruction::NegF(value) => self.assign_float_unop(writer, func, dst, value, "-")?,
            Instruction::AbsF(value) => {
                self.assign_float_method1(writer, func, dst, value, "abs")?
            }
            Instruction::SinF(value) => {
                self.assign_float_method1(writer, func, dst, value, "sin")?
            }
            Instruction::CosF(value) => {
                self.assign_float_method1(writer, func, dst, value, "cos")?
            }
            Instruction::LogF(value) => {
                self.assign_float_method1(writer, func, dst, value, "ln")?
            }
            Instruction::SqrtF(value) => {
                self.assign_float_method1(writer, func, dst, value, "sqrt")?
            }
            Instruction::AddI(left, right) => {
                self.assign_int_binop(writer, func, dst, left, right, "+")?
            }
            Instruction::SubI(left, right) => {
                self.assign_int_binop(writer, func, dst, left, right, "-")?
            }
            Instruction::MulI(left, right) => {
                self.assign_int_binop(writer, func, dst, left, right, "*")?
            }
            Instruction::DivI(left, right) => {
                self.assign_int_binop(writer, func, dst, left, right, "/")?
            }
            Instruction::ModI(left, right) => {
                self.assign_int_binop(writer, func, dst, left, right, "%")?
            }
            Instruction::NegI(value) => self.assign_int_unop(writer, func, dst, value, "-")?,
            Instruction::AbsI(value) => self.assign_int_method1(writer, func, dst, value, "abs")?,
            Instruction::Gt(left, right) => self.assign_cmp(writer, func, dst, left, right, ">")?,
            Instruction::Ge(left, right) => {
                self.assign_cmp(writer, func, dst, left, right, ">=")?
            }
            Instruction::Lt(left, right) => self.assign_cmp(writer, func, dst, left, right, "<")?,
            Instruction::Le(left, right) => {
                self.assign_cmp(writer, func, dst, left, right, "<=")?
            }
            Instruction::Eq(left, right) => {
                self.assign_cmp(writer, func, dst, left, right, "==")?
            }
            Instruction::Ne(left, right) => {
                self.assign_cmp(writer, func, dst, left, right, "!=")?
            }
            Instruction::And(left, right) => {
                self.assign_truthy_binop(writer, func, dst, left, right, "&&")?
            }
            Instruction::Or(left, right) => {
                self.assign_truthy_binop(writer, func, dst, left, right, "||")?
            }
            Instruction::Not(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.scalar_word_expr(func, value)?;
                writer.line(format!(
                    "{dest}[0] = f64_to_word(if !truthy({expr}) {{ 1.0 }} else {{ 0.0 }});"
                ))?;
            }
            Instruction::CastFtoI(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.scalar_word_expr(func, value)?;
                writer.line(format!(
                    "{dest}[0] = i64_to_word(word_to_f64({expr}) as i64);"
                ))?;
            }
            Instruction::CastItoF(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.scalar_word_expr(func, value)?;
                writer.line(format!(
                    "{dest}[0] = f64_to_word(word_to_i64({expr}) as f64);"
                ))?;
            }
            Instruction::CastItoB(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.scalar_word_expr(func, value)?;
                writer.line(format!(
                    "{dest}[0] = f64_to_word(if word_to_i64({expr}) != 0 {{ 1.0 }} else {{ 0.0 }});"
                ))?;
            }
            Instruction::Array(values, elem_ty) => {
                let dest = self.reg_name(dst)?;
                let elem_words = elem_ty.word_size() as usize;
                writer.line(format!(
                    "let array_handle = self.arrays.alloc_array({}, {}usize);",
                    values.len(),
                    elem_words
                ))?;
                for (index, value) in values.iter().enumerate() {
                    let expr = self.context_value_slice_expr(func, value, *elem_ty)?;
                    writer.line("{")?;
                    writer.indented(1, |writer| {
                        writer.line("let array = self.arrays.get_mut(array_handle)?;")?;
                        writer.line(format!("let start = {index}usize * {}usize;", elem_words))?;
                        writer.line(format!("let end = start + {}usize;", elem_words))?;
                        writer.line(format!("array.data[start..end].copy_from_slice({expr});"))
                    })?;
                    writer.line("}")?;
                }
                writer.line(format!("{dest}[0] = array_handle;"))?;
            }
            Instruction::GetArrayElem(arr, idx, elem_ty) => {
                let dest = self.reg_name(dst)?;
                let arr_expr = self.word0_expr(arr)?;
                let idx_expr = self.word0_expr(idx)?;
                let elem_words = elem_ty.word_size() as usize;
                writer.line("{")?;
                writer.indented(1, |writer| {
                    writer.line(format!("let array = self.arrays.get({arr_expr})?;"))?;
                    writer.line(
                        "let len = if array.elem_size_words == 0 { 0usize } else { array.data.len() / array.elem_size_words };",
                    )?;
                    writer.line(format!("let index_value = word_to_f64({idx_expr});"))?;
                    writer.line(
                        "let index = if len == 0 { 0usize } else if !index_value.is_finite() { 0usize } else { (index_value as i64).clamp(0, (len - 1) as i64) as usize };",
                    )?;
                    writer.line(format!("if len == 0 {{ {dest}.fill(0); }} else {{"))?;
                    writer.indented(1, |writer| {
                        writer.line(format!("let start = index * {}usize;", elem_words))?;
                        writer.line(format!("let end = start + {}usize;", elem_words))?;
                        if self.is_deepcopy_aggregate_type(*elem_ty) {
                            writer.line(format!("{dest}[0] = memory.alloc({elem_words}usize);"))?;
                            writer.line(format!(
                                "memory.store({dest}[0], &array.data[start..end], {elem_words}usize)?;"
                            ))
                        } else {
                            writer.line(format!(
                                "{dest} = {};",
                                self.slice_to_array_expr(
                                    format!("&array.data[start..end]"),
                                    elem_words,
                                )
                            ))
                        }
                    })?;
                    writer.line("}")
                })?;
                writer.line("}")?;
            }
            Instruction::SetArrayElem(arr, idx, value, elem_ty) => {
                let arr_expr = self.word0_expr(arr)?;
                let idx_expr = self.word0_expr(idx)?;
                let elem_words = elem_ty.word_size() as usize;
                let value_expr = self.context_value_slice_expr(func, value, *elem_ty)?;
                writer.line("{")?;
                writer.indented(1, |writer| {
                    writer.line(format!("let array = self.arrays.get_mut({arr_expr})?;"))?;
                    writer.line(
                        "let len = if array.elem_size_words == 0 { 0usize } else { array.data.len() / array.elem_size_words };",
                    )?;
                    writer.line(format!("let index_value = word_to_f64({idx_expr});"))?;
                    writer.line(
                        "let index = if len == 0 { 0usize } else if !index_value.is_finite() { 0usize } else { (index_value as i64).clamp(0, (len - 1) as i64) as usize };",
                    )?;
                    writer.line("if len != 0 {")?;
                    writer.indented(1, |writer| {
                        writer.line(format!("let start = index * {}usize;", elem_words))?;
                        writer.line(format!("let end = start + {}usize;", elem_words))?;
                        writer.line(format!("array.data[start..end].copy_from_slice({value_expr});"))
                    })?;
                    writer.line("}")
                })?;
                writer.line("}")?;
            }
            Instruction::String(symbol) => {
                self.assign_scalar(
                    writer,
                    dst,
                    format!("self.alloc_string({:?})", symbol.as_str()),
                )?;
            }
            Instruction::TaggedUnionWrap {
                tag,
                value,
                union_type,
            } => {
                let dest = self.reg_name(dst)?;
                let union_size = union_type.word_size() as usize;
                writer.line(format!("{dest}[0] = memory.alloc({union_size}usize);"))?;
                writer.line(format!(
                    "memory.store({dest}[0], &[i64_to_word({}i64)], 1usize)?;",
                    *tag as i64
                ))?;
                if let Some(payload_ty) =
                    self.union_variant_payload_type(*union_type, *tag as usize)
                {
                    if payload_ty.word_size() > 0 {
                        writer.line(format!(
                            "let union_value_ptr = memory.get_element({dest}[0], 1usize)?;"
                        ))?;
                        if self.is_deepcopy_aggregate_type(payload_ty) {
                            self.emit_deep_copy_value_into_ptr(
                                writer,
                                "union_value_ptr",
                                value,
                                payload_ty,
                            )?;
                        } else {
                            let value_expr =
                                self.context_value_slice_expr(func, value, payload_ty)?;
                            writer.line(format!(
                                "memory.store(union_value_ptr, {value_expr}, {}usize)?;",
                                payload_ty.word_size()
                            ))?;
                        }
                    }
                }
            }
            Instruction::TaggedUnionGetTag(value) => {
                let dest = self.reg_name(dst)?;
                let ptr_expr = self.word0_expr(value)?;
                writer.line(format!("{dest}[0] = memory.load({ptr_expr}, 1usize)?[0];"))?;
            }
            Instruction::TaggedUnionGetValue(value, ty) => {
                let dest = self.reg_name(dst)?;
                let ptr_expr = self.word0_expr(value)?;
                writer.line(format!(
                    "let union_value_ptr = memory.get_element({ptr_expr}, 1usize)?;"
                ))?;
                if self.is_deepcopy_aggregate_type(*ty) {
                    writer.line(format!("{dest}[0] = union_value_ptr;"))?;
                } else {
                    writer.line(format!(
                        "{dest} = {};",
                        self.vec_to_array_expr(
                            format!("memory.load(union_value_ptr, {}usize)?", ty.word_size()),
                            self.runtime_value_size(*ty),
                        )
                    ))?;
                }
            }
            Instruction::CloneUserSum { value, ty } => {
                let dest = self.reg_name(dst)?;
                let expr = self.context_value_array_expr(func, value, *ty)?;
                writer.line(format!("{dest} = {expr};"))?;
            }
            Instruction::ReleaseUserSum { .. } => {}
            Instruction::Closure(_)
            | Instruction::CallCls(_, _, _)
            | Instruction::CloseUpValues(_, _)
            | Instruction::BoxAlloc { .. }
            | Instruction::BoxLoad { .. }
            | Instruction::BoxClone { .. }
            | Instruction::BoxRelease { .. }
            | Instruction::BoxStore { .. }
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

    fn assign_scalar<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dst: &VPtr,
        expr: String,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        writer.line(format!("{dest}[0] = {expr};"))
    }

    fn assign_float_binop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.scalar_word_expr(func, left)?;
        let right_expr = self.scalar_word_expr(func, right)?;
        writer.line(format!(
            "{dest}[0] = f64_to_word(word_to_f64({left_expr}) {op} word_to_f64({right_expr}));"
        ))
    }

    fn assign_float_method1<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        value: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.scalar_word_expr(func, value)?;
        writer.line(format!(
            "{dest}[0] = f64_to_word(word_to_f64({expr}).{method}());"
        ))
    }

    fn assign_float_method2<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.scalar_word_expr(func, left)?;
        let right_expr = self.scalar_word_expr(func, right)?;
        writer.line(format!(
            "{dest}[0] = f64_to_word(word_to_f64({left_expr}).{method}(word_to_f64({right_expr})));"
        ))
    }

    fn assign_float_unop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        value: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.scalar_word_expr(func, value)?;
        writer.line(format!("{dest}[0] = f64_to_word({op}word_to_f64({expr}));"))
    }

    fn assign_int_binop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.scalar_word_expr(func, left)?;
        let right_expr = self.scalar_word_expr(func, right)?;
        writer.line(format!(
            "{dest}[0] = i64_to_word(word_to_i64({left_expr}) {op} word_to_i64({right_expr}));"
        ))
    }

    fn assign_int_unop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        value: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.scalar_word_expr(func, value)?;
        writer.line(format!("{dest}[0] = i64_to_word({op}word_to_i64({expr}));"))
    }

    fn assign_int_method1<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        value: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.scalar_word_expr(func, value)?;
        writer.line(format!(
            "{dest}[0] = i64_to_word(word_to_i64({expr}).{method}());"
        ))
    }

    fn assign_cmp<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.scalar_word_expr(func, left)?;
        let right_expr = self.scalar_word_expr(func, right)?;
        writer.line(format!(
            "{dest}[0] = f64_to_word(if word_to_f64({left_expr}) {op} word_to_f64({right_expr}) {{ 1.0 }} else {{ 0.0 }});"
        ))
    }

    fn assign_truthy_binop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.scalar_word_expr(func, left)?;
        let right_expr = self.scalar_word_expr(func, right)?;
        writer.line(format!(
            "{dest}[0] = f64_to_word(if truthy({left_expr}) {op} truthy({right_expr}) {{ 1.0 }} else {{ 0.0 }});"
        ))
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

    fn resolve_function_index(&self, func: &Function, callee: &VPtr) -> Option<usize> {
        match callee.as_ref() {
            Value::Function(index) => Some(*index),
            Value::Register(reg) => self.resolve_register_function_index(func, *reg),
            _ => None,
        }
    }

    fn resolve_register_function_index(&self, func: &Function, target_reg: VReg) -> Option<usize> {
        func.body.iter().find_map(|block| {
            block
                .0
                .iter()
                .find_map(|(dst, instr)| match (dst.as_ref(), instr) {
                    (Value::Register(reg), Instruction::Uinteger(value)) if *reg == target_reg => {
                        Some(*value as usize)
                    }
                    (Value::Register(reg), Instruction::Integer(value))
                        if *reg == target_reg && *value >= 0 =>
                    {
                        Some(*value as usize)
                    }
                    _ => None,
                })
        })
    }

    fn resolve_register_alloc_size(&self, func: &Function, target_reg: VReg) -> Option<usize> {
        self.resolve_register_alloc_type(func, target_reg)
            .map(|ty| ty.word_size() as usize)
    }

    fn resolve_register_alloc_type(&self, func: &Function, target_reg: VReg) -> Option<TypeNodeId> {
        func.body.iter().find_map(|block| {
            block
                .0
                .iter()
                .find_map(|(dst, instr)| match (dst.as_ref(), instr) {
                    (Value::Register(reg), Instruction::Alloc(ty)) if *reg == target_reg => {
                        Some(*ty)
                    }
                    _ => None,
                })
        })
    }

    fn resolve_register_value_type(&self, func: &Function, target_reg: VReg) -> Option<TypeNodeId> {
        func.body.iter().find_map(|block| {
            block
                .0
                .iter()
                .find_map(|(dst, instr)| match (dst.as_ref(), instr) {
                    (Value::Register(reg), Instruction::Alloc(ty)) if *reg == target_reg => {
                        Some(*ty)
                    }
                    (Value::Register(reg), Instruction::Load(_, ty)) if *reg == target_reg => {
                        Some(*ty)
                    }
                    (Value::Register(reg), Instruction::Call(_, _, ty)) if *reg == target_reg => {
                        Some(*ty)
                    }
                    (Value::Register(reg), Instruction::CallIndirect(_, _, ty))
                        if *reg == target_reg =>
                    {
                        Some(*ty)
                    }
                    (Value::Register(reg), Instruction::GetArrayElem(_, _, ty))
                        if *reg == target_reg =>
                    {
                        Some(*ty)
                    }
                    (Value::Register(reg), Instruction::GetGlobal(_, ty)) if *reg == target_reg => {
                        Some(*ty)
                    }
                    (Value::Register(reg), Instruction::GetState(ty)) if *reg == target_reg => {
                        Some(*ty)
                    }
                    (Value::Register(reg), Instruction::GetUpValue(_, ty))
                        if *reg == target_reg =>
                    {
                        Some(*ty)
                    }
                    (Value::Register(reg), Instruction::TaggedUnionWrap { union_type, .. })
                        if *reg == target_reg =>
                    {
                        Some(*union_type)
                    }
                    (Value::Register(reg), Instruction::TaggedUnionGetValue(_, ty))
                        if *reg == target_reg =>
                    {
                        Some(*ty)
                    }
                    _ => None,
                })
        })
    }

    fn should_capture_alloc_by_value(&self, ty: TypeNodeId) -> bool {
        matches!(
            ty.to_type(),
            Type::Function { .. }
                | Type::Array(_)
                | Type::Code(_)
                | Type::Boxed(_)
                | Type::Primitive(crate::types::PType::String)
        )
    }

    fn union_variant_payload_type(&self, union_type: TypeNodeId, tag: usize) -> Option<TypeNodeId> {
        match union_type.to_type() {
            Type::UserSum { variants, .. } => variants.get(tag).and_then(|(_, payload)| *payload),
            Type::Union(variants) => variants.get(tag).copied(),
            _ => None,
        }
    }

    fn resolve_register_getelement_type(
        &self,
        func: &Function,
        target_reg: VReg,
    ) -> Option<TypeNodeId> {
        func.body.iter().find_map(|block| {
            block
                .0
                .iter()
                .find_map(|(dst, instr)| match (dst.as_ref(), instr) {
                    (
                        Value::Register(reg),
                        Instruction::GetElement {
                            ty, tuple_offset, ..
                        },
                    ) if *reg == target_reg => match ty.to_type() {
                        Type::Tuple(elems) => elems.get(*tuple_offset as usize).copied(),
                        Type::Record(fields) => fields.get(*tuple_offset as usize).map(|f| f.ty),
                        _ => None,
                    },
                    _ => None,
                })
        })
    }

    fn getelement_word_offset(&self, instr: &Instruction) -> Result<usize, String> {
        match instr {
            Instruction::GetElement {
                ty, tuple_offset, ..
            } => match ty.to_type() {
                Type::Tuple(elems) => Ok(elems[..(*tuple_offset as usize)]
                    .iter()
                    .map(|elem| elem.word_size() as usize)
                    .sum()),
                Type::Record(fields) => Ok(fields[..(*tuple_offset as usize)]
                    .iter()
                    .map(|field| field.ty.word_size() as usize)
                    .sum()),
                other => Err(format!(
                    "GetElement expects tuple or record type, got {:?}",
                    other
                )),
            },
            other => Err(format!("expected GetElement instruction, got {:?}", other)),
        }
    }

    fn scalar_word_expr(&self, func: &Function, value: &VPtr) -> Result<String, String> {
        match value.as_ref() {
            Value::Register(reg)
                if self
                    .resolve_register_getelement_type(func, *reg)
                    .is_some_and(|ty| {
                        ty.word_size() == 1 && !self.is_deepcopy_aggregate_type(ty)
                    }) =>
            {
                Ok(format!("memory.load(reg_{reg}[0], 1usize)?[0]"))
            }
            _ => self.word0_expr(value),
        }
    }

    fn context_value_vec_expr(
        &self,
        func: &Function,
        value: &VPtr,
        ty: TypeNodeId,
    ) -> Result<String, String> {
        if ty.word_size() == 1 {
            let expr = self.scalar_word_expr(func, value)?;
            Ok(format!("vec![{expr}]"))
        } else {
            self.value_vec_expr(value)
        }
    }

    fn context_value_array_expr(
        &self,
        func: &Function,
        value: &VPtr,
        ty: TypeNodeId,
    ) -> Result<String, String> {
        if ty.word_size() == 1 {
            let expr = self.scalar_word_expr(func, value)?;
            Ok(format!("[{expr}]"))
        } else {
            self.value_array_expr(value)
        }
    }

    fn context_value_slice_expr(
        &self,
        func: &Function,
        value: &VPtr,
        ty: TypeNodeId,
    ) -> Result<String, String> {
        if self.is_deepcopy_aggregate_type(ty) {
            return match value.as_ref() {
                Value::Argument(index) => Ok(format!("&arg_{index}")),
                _ => {
                    let ptr_expr = self.word0_expr(value)?;
                    Ok(format!(
                        "&memory.load({ptr_expr}, {}usize)?",
                        ty.word_size()
                    ))
                }
            };
        }
        if ty.word_size() == 1 {
            let expr = self.scalar_word_expr(func, value)?;
            Ok(format!("&[{expr}]"))
        } else {
            self.value_slice_expr(value)
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
            Value::Argument(index) => Ok(format!("arg_{index}.to_vec()")),
            Value::Register(reg) => Ok(format!("reg_{reg}.to_vec()")),
            Value::Function(index) => Ok(format!("vec![encode_function({index})]")),
            Value::None => Ok("Vec::new()".to_string()),
            _ => Err(format!(
                "value is not representable as a word vector in the initial Rust backend: {:?}",
                value
            )),
        }
    }

    fn value_array_expr(&self, value: &VPtr) -> Result<String, String> {
        match value.as_ref() {
            Value::Argument(index) => Ok(format!("arg_{index}")),
            Value::Register(reg) => Ok(format!("reg_{reg}")),
            Value::Function(index) => Ok(format!("[encode_function({index})]")),
            Value::None => Ok("[0u64; 0]".to_string()),
            _ => Err(format!(
                "value is not representable as a fixed-size word array in the initial Rust backend: {:?}",
                value
            )),
        }
    }

    fn value_slice_expr(&self, value: &VPtr) -> Result<String, String> {
        match value.as_ref() {
            Value::Argument(index) => Ok(format!("&arg_{index}")),
            Value::Register(reg) => Ok(format!("&reg_{reg}")),
            Value::Function(index) => Ok(format!("&[encode_function({index})]")),
            Value::None => Ok("&[]".to_string()),
            _ => Err(format!(
                "value is not representable as a word slice in the initial Rust backend: {:?}",
                value
            )),
        }
    }

    fn boundary_value_slice_expr(
        &self,
        func: &Function,
        value: &VPtr,
        ty: TypeNodeId,
    ) -> Result<String, String> {
        if !self.is_deepcopy_aggregate_type(ty) {
            return self.context_value_slice_expr(func, value, ty);
        }
        match value.as_ref() {
            Value::Argument(index) => Ok(format!("&arg_{index}")),
            _ => {
                let ptr_expr = self.word0_expr(value)?;
                Ok(format!(
                    "&memory.load({ptr_expr}, {}usize)?",
                    ty.word_size()
                ))
            }
        }
    }

    fn runtime_value_size(&self, ty: TypeNodeId) -> usize {
        if ty.word_size() == 0 {
            0
        } else if self.is_deepcopy_aggregate_type(ty) {
            1
        } else {
            ty.word_size() as usize
        }
    }

    fn is_deepcopy_aggregate_type(&self, ty: TypeNodeId) -> bool {
        matches!(
            ty.to_type(),
            Type::Tuple(_) | Type::Record(_) | Type::Union(_) | Type::UserSum { .. }
        )
    }

    fn emit_deep_copy_value_into_ptr<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dest_ptr_expr: &str,
        src: &VPtr,
        ty: TypeNodeId,
    ) -> Result<(), String> {
        match src.as_ref() {
            Value::Argument(index) => {
                self.emit_copy_flat_argument_into_ptr(writer, dest_ptr_expr, *index, ty, 0)
            }
            _ => {
                let size = ty.word_size() as usize;
                let src_handle = self.word0_expr(src)?;
                writer.line("{")?;
                writer.indented(1, |writer| {
                    writer.line(format!(
                        "let copied_words = memory.load({src_handle}, {size}usize)?;"
                    ))?;
                    writer.line(format!(
                        "memory.store({dest_ptr_expr}, &copied_words, {size}usize)?;"
                    ))
                })?;
                writer.line("}")
            }
        }
    }

    fn emit_copy_flat_argument_into_ptr<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dest_ptr_expr: &str,
        arg_index: usize,
        ty: TypeNodeId,
        source_offset: usize,
    ) -> Result<(), String> {
        let size = ty.word_size() as usize;
        if size == 0 {
            return Ok(());
        }
        writer.line(format!(
            "memory.store({dest_ptr_expr}, &arg_{arg_index}[{source_offset}..{}], {size}usize)?;",
            source_offset + size
        ))
    }

    fn slice_to_array_expr(&self, slice_expr: String, size: usize) -> String {
        format!("copy_words::<{size}>({slice_expr})?")
    }

    fn vec_to_array_expr(&self, vec_expr: String, size: usize) -> String {
        format!("vec_to_words::<{size}>({vec_expr})?")
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
