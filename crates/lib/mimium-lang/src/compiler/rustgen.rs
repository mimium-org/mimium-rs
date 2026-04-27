use std::collections::{BTreeMap, HashMap};
use std::io::Write;
use std::sync::Arc;

use crate::compiler::Config;
use crate::compiler::bytecodegen::SelfEvalMode;
use crate::interner::Symbol;
use crate::mir::{Function, Instruction, Mir, VPtr, VReg, Value};

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
        let Some(dsp_index) = self.find_function_by_name("dsp") else {
            return Ok(());
        };

        writer.line("pub fn call_dsp(&mut self, args: &[Word]) -> Result<Vec<Word>, String> {")?;
        writer.indented(1, |writer| {
            writer.line(format!(
                "self.call_function_handle(encode_function({dsp_index}), args)"
            ))
        })?;
        writer.line("}")?;
        writer.blank_line()
    }

    fn emit_call_main<W: Write>(&self, writer: &mut CodeWriter<W>) -> Result<(), String> {
        let main_index = self.find_function_by_name("main").or_else(|| {
            (!self.collect_global_slots().is_empty())
                .then(|| self.mir.functions.first().map(|func| func.index))
                .flatten()
        });
        let Some(main_index) = main_index else {
            return Ok(());
        };

        writer.line("pub fn call_main(&mut self) -> Result<Vec<Word>, String> {")?;
        writer.indented(1, |writer| {
            writer.line(format!(
                "self.call_function_handle(encode_function({main_index}), &[])"
            ))
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
                    "let mut state = std::mem::take(&mut self.function_states[{}]);",
                    func.index
                ))?;
                writer.line(format!(
                    "let result = self.func_{}(&mut state, args);",
                    func.index
                ))?;
                writer.line(format!("self.function_states[{}] = state;", func.index))?;
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
        let fallthrough_targets = self.collect_fallthrough_targets(func);

        writer.line(format!(
            "fn func_{}(&mut self, state: &mut StateStorage, args: &[Word]) -> Result<Vec<Word>, String> {{",
            func.index
        ))?;
        writer.indented(1, |writer| {
            writer.line("let mut memory = MemoryStore::default();")?;
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
                            if let Some(target_bb) =
                                fallthrough_targets.get(block_index).and_then(|target| *target)
                            {
                                writer.line(format!("pred_bb = {block_index};"))?;
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

    fn collect_fallthrough_targets(&self, func: &Function) -> Vec<Option<usize>> {
        let mut targets = vec![None; func.body.len()];
        for block in &func.body {
            for (_dst, instr) in &block.0 {
                match instr {
                    Instruction::JmpIf(_, then_bb, else_bb, merge_bb) => {
                        targets[*then_bb as usize] = Some(*merge_bb as usize);
                        targets[*else_bb as usize] = Some(*merge_bb as usize);
                    }
                    Instruction::Switch {
                        cases,
                        default_block,
                        merge_block,
                        ..
                    } => {
                        for (_literal, case_bb) in cases {
                            targets[*case_bb as usize] = Some(*merge_block as usize);
                        }
                        if let Some(default_bb) = default_block {
                            targets[*default_bb as usize] = Some(*merge_block as usize);
                        }
                    }
                    _ => {}
                }
            }
        }
        targets
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
                let dest = self.reg_name(dst)?;
                let ptr_expr = self.word0_expr(ptr)?;
                writer.line(format!(
                    "{dest} = {};",
                    self.vec_to_array_expr(
                        format!("memory.load({ptr_expr}, {}usize)?", ty.word_size()),
                        ty.word_size() as usize,
                    )
                ))?;
            }
            Instruction::Store(ptr, src, ty) => {
                let ptr_expr = self.word0_expr(ptr)?;
                let src_expr = self.value_slice_expr(src)?;
                writer.line(format!(
                    "memory.store({ptr_expr}, {src_expr}, {}usize)?;",
                    ty.word_size()
                ))?;
            }
            Instruction::GetElement {
                value,
                tuple_offset,
                ..
            } => {
                let dest = self.reg_name(dst)?;
                let base_expr = self.word0_expr(value)?;
                writer.line(format!(
                    "{dest}[0] = memory.get_element({base_expr}, {}usize)?;",
                    tuple_offset
                ))?;
            }
            Instruction::Call(callee, args, _ret_ty) => {
                let dest = self.reg_name(dst)?;
                let call_kind = self.call_kind(callee)?;
                let result_size = self.instruction_result_size(func, instr)?;
                writer.line("let mut call_args = Vec::new();")?;
                for (arg, _ty) in args {
                    let expr = self.value_slice_expr(arg)?;
                    writer.line(format!("call_args.extend_from_slice({expr});"))?;
                }
                match call_kind {
                    CallKind::FunctionHandle(handle_expr) => {
                        writer.line(format!(
                            "{dest} = {};",
                            self.vec_to_array_expr(
                                format!("self.call_function_handle({handle_expr}, &call_args)?"),
                                result_size,
                            )
                        ))?;
                    }
                    CallKind::Ext(symbol) => {
                        let name = format!("{:?}", symbol.as_str());
                        let ret_words = self.instruction_result_size(func, instr)?;
                        writer.line(format!(
                            "{dest} = {};",
                            self.vec_to_array_expr(
                                format!("self.host.call_ext({name}, &call_args, {ret_words})?"),
                                result_size,
                            )
                        ))?;
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
                    return Err(format!(
                        "instruction {:?} is not supported by the initial Rust backend",
                        instr
                    ));
                }
            }
            Instruction::GetGlobal(global, ty) => {
                let dest = self.reg_name(dst)?;
                let global_index = self.global_index(global_slots, global)?;
                writer.line(format!(
                    "{dest} = {};",
                    self.slice_to_array_expr(
                        format!("&self.globals[{global_index}][..{}]", ty.word_size()),
                        ty.word_size() as usize,
                    )
                ))?;
            }
            Instruction::SetGlobal(global, src, ty) => {
                let global_index = self.global_index(global_slots, global)?;
                let src_expr = self.value_slice_expr(src)?;
                writer.line(format!(
                    "self.globals[{global_index}][..{}].copy_from_slice({src_expr});",
                    ty.word_size()
                ))?;
            }
            Instruction::PushStateOffset(offset) => {
                writer.line(format!("state.push_pos({offset}usize);"))?;
            }
            Instruction::PopStateOffset(offset) => {
                writer.line(format!("state.pop_pos({offset}usize);"))?;
            }
            Instruction::GetState(ty) => {
                let dest = self.reg_name(dst)?;
                writer.line(format!(
                    "{dest} = {};",
                    self.vec_to_array_expr(
                        format!("state.get_state({}usize)", ty.word_size()),
                        ty.word_size() as usize,
                    )
                ))?;
            }
            Instruction::JmpIf(cond, then_bb, else_bb, _merge_bb) => {
                let cond_expr = self.word0_expr(cond)?;
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
                let scrutinee_expr = self.word0_expr(scrutinee)?;
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
                    let expr = self.value_vec_expr(value)?;
                    writer.line(format!("return Ok({expr});"))?;
                }
            }
            Instruction::ReturnFeed(value, ty) => {
                let expr = self.value_array_expr(value)?;
                match self.config.self_eval_mode {
                    SelfEvalMode::SimpleState => {
                        writer.line(format!("let result = {expr};"))?;
                        writer.line(format!(
                            "state.set_state(&result, {}usize);",
                            ty.word_size()
                        ))?;
                        writer.line("return Ok(result.to_vec());")?;
                    }
                    SelfEvalMode::ZeroAtInit => {
                        writer.line(format!(
                            "let previous = state.get_state({}usize);",
                            ty.word_size()
                        ))?;
                        writer.line(format!("let result = {expr};"))?;
                        writer.line(format!(
                            "state.set_state(&result, {}usize);",
                            ty.word_size()
                        ))?;
                        writer.line("return Ok(previous);")?;
                    }
                }
            }
            Instruction::Delay(max_len, src, time) => {
                let dest = self.reg_name(dst)?;
                let src_expr = self.word0_expr(src)?;
                let time_expr = self.word0_expr(time)?;
                writer.line(format!(
                    "{dest}[0] = state.delay({src_expr}, {time_expr}, {}usize);",
                    max_len
                ))?;
            }
            Instruction::Mem(src) => {
                let dest = self.reg_name(dst)?;
                let src_expr = self.word0_expr(src)?;
                writer.line(format!("{dest}[0] = state.mem({src_expr});"))?;
            }
            Instruction::AddF(left, right) => {
                self.assign_float_binop(writer, dst, left, right, "+")?
            }
            Instruction::SubF(left, right) => {
                self.assign_float_binop(writer, dst, left, right, "-")?
            }
            Instruction::MulF(left, right) => {
                self.assign_float_binop(writer, dst, left, right, "*")?
            }
            Instruction::DivF(left, right) => {
                self.assign_float_binop(writer, dst, left, right, "/")?
            }
            Instruction::ModF(left, right) => {
                self.assign_float_binop(writer, dst, left, right, "%")?
            }
            Instruction::PowF(left, right) => {
                self.assign_float_method2(writer, dst, left, right, "powf")?
            }
            Instruction::NegF(value) => self.assign_float_unop(writer, dst, value, "-")?,
            Instruction::AbsF(value) => self.assign_float_method1(writer, dst, value, "abs")?,
            Instruction::SinF(value) => self.assign_float_method1(writer, dst, value, "sin")?,
            Instruction::CosF(value) => self.assign_float_method1(writer, dst, value, "cos")?,
            Instruction::LogF(value) => self.assign_float_method1(writer, dst, value, "ln")?,
            Instruction::SqrtF(value) => self.assign_float_method1(writer, dst, value, "sqrt")?,
            Instruction::AddI(left, right) => {
                self.assign_int_binop(writer, dst, left, right, "+")?
            }
            Instruction::SubI(left, right) => {
                self.assign_int_binop(writer, dst, left, right, "-")?
            }
            Instruction::MulI(left, right) => {
                self.assign_int_binop(writer, dst, left, right, "*")?
            }
            Instruction::DivI(left, right) => {
                self.assign_int_binop(writer, dst, left, right, "/")?
            }
            Instruction::ModI(left, right) => {
                self.assign_int_binop(writer, dst, left, right, "%")?
            }
            Instruction::NegI(value) => self.assign_int_unop(writer, dst, value, "-")?,
            Instruction::AbsI(value) => self.assign_int_method1(writer, dst, value, "abs")?,
            Instruction::Gt(left, right) => self.assign_cmp(writer, dst, left, right, ">")?,
            Instruction::Ge(left, right) => self.assign_cmp(writer, dst, left, right, ">=")?,
            Instruction::Lt(left, right) => self.assign_cmp(writer, dst, left, right, "<")?,
            Instruction::Le(left, right) => self.assign_cmp(writer, dst, left, right, "<=")?,
            Instruction::Eq(left, right) => self.assign_cmp(writer, dst, left, right, "==")?,
            Instruction::Ne(left, right) => self.assign_cmp(writer, dst, left, right, "!=")?,
            Instruction::And(left, right) => {
                self.assign_truthy_binop(writer, dst, left, right, "&&")?
            }
            Instruction::Or(left, right) => {
                self.assign_truthy_binop(writer, dst, left, right, "||")?
            }
            Instruction::Not(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.word0_expr(value)?;
                writer.line(format!(
                    "{dest}[0] = f64_to_word(if !truthy({expr}) {{ 1.0 }} else {{ 0.0 }});"
                ))?;
            }
            Instruction::CastFtoI(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.word0_expr(value)?;
                writer.line(format!(
                    "{dest}[0] = i64_to_word(word_to_f64({expr}) as i64);"
                ))?;
            }
            Instruction::CastItoF(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.word0_expr(value)?;
                writer.line(format!(
                    "{dest}[0] = f64_to_word(word_to_i64({expr}) as f64);"
                ))?;
            }
            Instruction::CastItoB(value) => {
                let dest = self.reg_name(dst)?;
                let expr = self.word0_expr(value)?;
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
                    let expr = self.value_slice_expr(value)?;
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
                        writer.line(format!(
                            "{dest} = {};",
                            self.slice_to_array_expr(
                                format!("&array.data[start..end]"),
                                elem_words,
                            )
                        ))
                    })?;
                    writer.line("}")
                })?;
                writer.line("}")?;
            }
            Instruction::SetArrayElem(arr, idx, value, elem_ty) => {
                let arr_expr = self.word0_expr(arr)?;
                let idx_expr = self.word0_expr(idx)?;
                let value_expr = self.value_slice_expr(value)?;
                let elem_words = elem_ty.word_size() as usize;
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
            Instruction::Closure(_)
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
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.word0_expr(left)?;
        let right_expr = self.word0_expr(right)?;
        writer.line(format!(
            "{dest}[0] = f64_to_word(word_to_f64({left_expr}) {op} word_to_f64({right_expr}));"
        ))
    }

    fn assign_float_method1<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dst: &VPtr,
        value: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.word0_expr(value)?;
        writer.line(format!(
            "{dest}[0] = f64_to_word(word_to_f64({expr}).{method}());"
        ))
    }

    fn assign_float_method2<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.word0_expr(left)?;
        let right_expr = self.word0_expr(right)?;
        writer.line(format!(
            "{dest}[0] = f64_to_word(word_to_f64({left_expr}).{method}(word_to_f64({right_expr})));"
        ))
    }

    fn assign_float_unop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dst: &VPtr,
        value: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.word0_expr(value)?;
        writer.line(format!("{dest}[0] = f64_to_word({op}word_to_f64({expr}));"))
    }

    fn assign_int_binop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.word0_expr(left)?;
        let right_expr = self.word0_expr(right)?;
        writer.line(format!(
            "{dest}[0] = i64_to_word(word_to_i64({left_expr}) {op} word_to_i64({right_expr}));"
        ))
    }

    fn assign_int_unop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dst: &VPtr,
        value: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.word0_expr(value)?;
        writer.line(format!("{dest}[0] = i64_to_word({op}word_to_i64({expr}));"))
    }

    fn assign_int_method1<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dst: &VPtr,
        value: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let expr = self.word0_expr(value)?;
        writer.line(format!(
            "{dest}[0] = i64_to_word(word_to_i64({expr}).{method}());"
        ))
    }

    fn assign_cmp<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.word0_expr(left)?;
        let right_expr = self.word0_expr(right)?;
        writer.line(format!(
            "{dest}[0] = f64_to_word(if word_to_f64({left_expr}) {op} word_to_f64({right_expr}) {{ 1.0 }} else {{ 0.0 }});"
        ))
    }

    fn assign_truthy_binop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        let left_expr = self.word0_expr(left)?;
        let right_expr = self.word0_expr(right)?;
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
