use std::collections::{BTreeMap, HashMap, HashSet};
use std::io::Write;
use std::sync::Arc;

use crate::compiler::Config;
use crate::compiler::bytecodegen::SelfEvalMode;
use crate::interner::{Symbol, TypeNodeId};
use crate::mir::{Block, Function, Instruction, Mir, VPtr, VReg, Value};
use crate::types::{PType, Type};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ScalarStorageKind {
    Word,
    I64,
    F64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RegisterStorage {
    Scalar(ScalarStorageKind),
    Words(usize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct StackPointer {
    root_reg: VReg,
    word_offset: usize,
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
        let dsp_dispatch = self.function_dispatch_name(dsp_func);
        let dsp_direct = self.function_direct_name(dsp_func);
        let dsp_return = dsp_func.return_type.get().copied();
        let dsp_uses_return_pointer =
            dsp_return.is_some_and(|ty| self.uses_direct_return_pointer(ty));
        let dsp_io_channels = self.mir.get_dsp_iochannels();
        let dsp_input_channels = dsp_io_channels.map_or(0usize, |info| info.input as usize);
        let dsp_output_channels = dsp_io_channels.map_or(0usize, |info| info.output as usize);

        writer.line("pub fn call_dsp(&mut self, args: &[Word]) -> Result<Vec<Word>, String> {")?;
        writer.indented(1, |writer| {
            writer.line("let previous_function_state = self.current_function_state;")?;
            writer.line(format!("self.current_function_state = Some({dsp_index});"))?;
            writer.line(format!("let result = self.{dsp_dispatch}(args);"))?;
            writer.line("self.current_function_state = previous_function_state;")?;
            self.emit_entrypoint_result(writer, "result", dsp_return)
        })?;
        writer.line("}")?;
        writer.blank_line()?;

        writer.line(
            "pub fn call_dsp_buffer(&mut self, input: &[mmmfloat], output: &mut [mmmfloat], frames: usize) -> Result<(), String> {",
        )?;
        writer.indented(1, |writer| {
            if dsp_io_channels.is_none() {
                writer.line(
                    "return Err(\"call_dsp_buffer requires dsp I/O types that can be flattened to audio channels\".to_string());",
                )?;
                return Ok(());
            }

            if dsp_input_channels == 0 {
                writer.line("if !input.is_empty() {")?;
                writer.indented(1, |writer| {
                    writer.line(
                        "return Err(format!(\"expected 0 input samples for {} dsp frames, got {}\", frames, input.len()));",
                    )
                })?;
                writer.line("}")?;
            } else {
                writer.line(format!(
                    "let expected_input_len = frames.saturating_mul({dsp_input_channels}usize);"
                ))?;
                writer.line("if input.len() != expected_input_len {")?;
                writer.indented(1, |writer| {
                    writer.line(
                        "return Err(format!(\"expected {} input samples for {} dsp frames, got {}\", expected_input_len, frames, input.len()));",
                    )
                })?;
                writer.line("}")?;
            }

            writer.line(format!(
                "let expected_output_len = frames.saturating_mul({dsp_output_channels}usize);"
            ))?;
            writer.line("if output.len() != expected_output_len {")?;
            writer.indented(1, |writer| {
                writer.line(
                    "return Err(format!(\"expected {} output samples for {} dsp frames, got {}\", expected_output_len, frames, output.len()));",
                )
            })?;
            writer.line("}")?;

            writer.line("let previous_function_state = self.current_function_state;")?;
            writer.line(format!("self.current_function_state = Some({dsp_index});"))?;
            writer.line("for frame in 0..frames {")?;
            writer.indented(1, |writer| {
                if dsp_output_channels > 0 {
                    writer.line(format!(
                        "let frame_output_start = frame * {dsp_output_channels}usize;"
                    ))?;
                }
                if dsp_func.args.is_empty() {
                    if dsp_output_channels == 0 {
                        writer.line(format!("self.{dsp_direct}();"))?;
                    } else {
                        if dsp_uses_return_pointer {
                            let size = dsp_return.map(|ty| ty.word_size() as usize).unwrap_or(0);
                            writer.line(format!("let mut result_words = [0u64; {size}];"))?;
                            writer.line(format!("self.{dsp_direct}(&mut result_words);"))?;
                            self.emit_buffer_output_assignments_from_words(
                                writer,
                                "output",
                                "frame_output_start",
                                "result_words",
                                dsp_return,
                            )?;
                        } else {
                            writer.line(format!("let result = self.{dsp_direct}();"))?;
                            self.emit_buffer_output_assignments(
                                writer,
                                "output",
                                "frame_output_start",
                                "result",
                                dsp_return,
                            )?;
                        }
                    }
                } else {
                    let input_ty = dsp_func
                        .args
                        .first()
                        .ok_or_else(|| "missing dsp input argument".to_string())?
                        .1;
                    writer.line(format!(
                        "let frame_input_start = frame * {dsp_input_channels}usize;"
                    ))?;
                    if dsp_output_channels == 0 {
                        writer.line(format!(
                            "self.{dsp_direct}({});",
                            self.abi_expr_from_f64_buffer_frame("input", "frame_input_start", input_ty)?
                        ))?;
                    } else {
                        let dsp_input_expr =
                            self.abi_expr_from_f64_buffer_frame("input", "frame_input_start", input_ty)?;
                        if dsp_uses_return_pointer {
                            let size = dsp_return.map(|ty| ty.word_size() as usize).unwrap_or(0);
                            writer.line(format!("let mut result_words = [0u64; {size}];"))?;
                            writer.line(format!(
                                "self.{dsp_direct}({}, &mut result_words);",
                                dsp_input_expr
                            ))?;
                            self.emit_buffer_output_assignments_from_words(
                                writer,
                                "output",
                                "frame_output_start",
                                "result_words",
                                dsp_return,
                            )?;
                        } else {
                            writer.line(format!("let result = self.{dsp_direct}({dsp_input_expr});"))?;
                            self.emit_buffer_output_assignments(
                                writer,
                                "output",
                                "frame_output_start",
                                "result",
                                dsp_return,
                            )?;
                        }
                    }
                }
                Ok(())
            })?;
            writer.line("}")?;
            writer.line("self.current_function_state = previous_function_state;")?;
            writer.line("Ok(())")
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
        let main_dispatch = self.function_dispatch_name(main_func);
        let main_return = main_func.return_type.get().copied();

        writer.line("pub fn call_main(&mut self) -> Result<Vec<Word>, String> {")?;
        writer.indented(1, |writer| {
            writer.line("let previous_function_state = self.current_function_state;")?;
            writer.line(format!("self.current_function_state = Some({main_index});"))?;
            writer.line(format!("let result = self.{main_dispatch}(&[]);"))?;
            writer.line("self.current_function_state = previous_function_state;")?;
            self.emit_entrypoint_result(writer, "result", main_return)
        })?;
        writer.line("}")?;
        writer.blank_line()
    }

    fn emit_entrypoint_result<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        result_expr: &str,
        return_ty: Option<TypeNodeId>,
    ) -> Result<(), String> {
        if let Some(ret_ty) = return_ty.filter(|ty| self.is_deepcopy_aggregate_type(*ty)) {
            if self.uses_direct_return_pointer(ret_ty) {
                writer.line(format!("Ok({result_expr})"))
            } else {
                let size = ret_ty.word_size() as usize;
                writer.line(format!(
                    "let final_result = if {result_expr}.is_empty() {{ Ok(Vec::new()) }} else {{ self.memory.load({result_expr}[0], {size}usize) }};"
                ))?;
                writer.line("final_result")
            }
        } else {
            writer.line(format!("Ok({result_expr})"))
        }
    }

    fn emit_buffer_output_assignments<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        output_expr: &str,
        frame_start_expr: &str,
        result_expr: &str,
        return_ty: Option<TypeNodeId>,
    ) -> Result<(), String> {
        let Some(ret_ty) = return_ty else {
            return Ok(());
        };

        self.abi_word_exprs(result_expr, ret_ty)?
            .into_iter()
            .enumerate()
            .try_for_each(|(index, word_expr)| {
                writer.line(format!(
                    "{output_expr}[{frame_start_expr} + {index}usize] = word_to_f64({word_expr});"
                ))
            })
    }

    fn emit_buffer_output_assignments_from_words<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        output_expr: &str,
        frame_start_expr: &str,
        result_words_expr: &str,
        return_ty: Option<TypeNodeId>,
    ) -> Result<(), String> {
        let Some(ret_ty) = return_ty else {
            return Ok(());
        };

        (0..ret_ty.word_size() as usize).try_for_each(|index| {
            writer.line(format!(
                "{output_expr}[{frame_start_expr} + {index}usize] = word_to_f64({result_words_expr}[{index}]);"
            ))
        })
    }

    fn abi_expr_from_f64_buffer_frame(
        &self,
        buffer_expr: &str,
        frame_start_expr: &str,
        ty: TypeNodeId,
    ) -> Result<String, String> {
        let mut offset = 0usize;
        self.abi_expr_from_f64_buffer_frame_at(buffer_expr, frame_start_expr, ty, &mut offset)
    }

    fn abi_expr_from_f64_buffer_frame_at(
        &self,
        buffer_expr: &str,
        frame_start_expr: &str,
        ty: TypeNodeId,
        offset: &mut usize,
    ) -> Result<String, String> {
        Ok(match ty.to_type() {
            Type::Primitive(crate::types::PType::Unit) => "()".to_string(),
            Type::Primitive(crate::types::PType::Numeric) => {
                let expr = format!("{buffer_expr}[{frame_start_expr} + {}usize]", *offset);
                *offset += 1;
                expr
            }
            Type::Primitive(crate::types::PType::Int) => {
                let expr = format!(
                    "({buffer_expr}[{frame_start_expr} + {}usize] as i64)",
                    *offset
                );
                *offset += 1;
                expr
            }
            Type::Tuple(elems) => self.abi_tuple_expr(
                elems
                    .iter()
                    .map(|elem| {
                        self.abi_expr_from_f64_buffer_frame_at(
                            buffer_expr,
                            frame_start_expr,
                            *elem,
                            offset,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Type::Record(fields) => self.abi_tuple_expr(
                fields
                    .iter()
                    .map(|field| {
                        self.abi_expr_from_f64_buffer_frame_at(
                            buffer_expr,
                            frame_start_expr,
                            field.ty,
                            offset,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            _ => {
                let expr = format!(
                    "f64_to_word({buffer_expr}[{frame_start_expr} + {}usize])",
                    *offset
                );
                *offset += 1;
                expr
            }
        })
    }

    fn emit_function_handle_dispatch<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
    ) -> Result<(), String> {
        self.mir.functions.iter().try_for_each(|func| {
            writer.line(format!("Some({}) => {{", func.index))?;
            writer.indented(1, |writer| {
                writer.line(format!(
                    "let result = self.{}(args);",
                    self.function_dispatch_name(func)
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
            self.emit_function_dispatch_wrapper(writer, func)?;
            writer.blank_line()?;
            self.emit_function(writer, func, global_slots)?;
        }
        Ok(())
    }

    fn emit_function_dispatch_wrapper<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
    ) -> Result<(), String> {
        let direct_name = self.function_direct_name(func);
        let wrapper_name = self.function_dispatch_name(func);
        let return_ty = self.function_return_type(func)?;
        let uses_return_pointer = self.uses_direct_return_pointer(return_ty);

        writer.line(format!(
            "fn {wrapper_name}(&mut self, args: &[Word]) -> Vec<Word> {{"
        ))?;
        writer.indented(1, |writer| {
            writer.line("let mut arg_offset = 0usize;")?;

            for (index, arg) in func.args.iter().enumerate() {
                let size = arg.1.word_size() as usize;
                if size == 0 {
                    writer.line(format!("let arg_{index}_value = ();"))?;
                } else {
                    writer.line(format!(
                        "let arg_{index}_words = copy_words::<{size}>(&args[arg_offset..arg_offset + {size}]).unwrap();"
                    ))?;
                    writer.line(format!("arg_offset += {size};"))?;
                    writer.line(format!(
                        "let arg_{index}_value = {};",
                        self.abi_expr_from_word_source(&format!("arg_{index}_words"), arg.1)?
                    ))?;
                }
            }

            let mut call_args = Vec::new();
            call_args.extend((0..func.args.len()).map(|index| format!("arg_{index}_value")));
            if uses_return_pointer {
                let size = return_ty.word_size() as usize;
                writer.line(format!(
                    "let mut {} = [0u64; {size}];",
                    self.direct_return_words_name()
                ))?;
                call_args.push(format!("&mut {}", self.direct_return_words_name()));
                writer.line(format!(
                    "self.{direct_name}({});",
                    call_args.join(", ")
                ))?;
                writer.line(format!("{}.to_vec()", self.direct_return_words_name()))
            } else {
                writer.line(format!(
                    "let result = self.{direct_name}({});",
                    call_args.join(", ")
                ))?;
                self.emit_pack_abi_result(writer, "result", return_ty)
            }
        })?;
        writer.line("}")
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
        let register_storage = self.collect_register_storage(func)?;
        let stack_pointers = self.collect_stack_pointers(func)?;
        let block_preds = self.collect_block_predecessors(func);
        let fallthrough_edges = self.collect_fallthrough_edges(func);
        let direct_name = self.function_direct_name(func);
        let return_ty = self.function_return_type(func)?;
        let uses_return_pointer = self.uses_direct_return_pointer(return_ty);
        let mut signature_args = vec!["&mut self".to_string()];
        for (index, arg) in func.args.iter().enumerate() {
            signature_args.push(format!("arg_{index}_value: {}", self.abi_type_expr(arg.1)?));
        }
        if uses_return_pointer {
            signature_args.push(format!(
                "{}: &mut [Word; {}]",
                self.direct_return_words_name(),
                return_ty.word_size() as usize
            ));
        }

        writer.line("#[inline(always)]")?;
        writer.line(format!(
            "fn {direct_name}({}) -> {} {{",
            signature_args.join(", "),
            self.direct_function_return_type_expr(return_ty)?
        ))?;
        writer.indented(1, |writer| {
            for (index, arg) in func.args.iter().enumerate() {
                writer.line(format!(
                    "let arg_{index} = {};",
                    self.abi_to_words_array_expr(&format!("arg_{index}_value"), arg.1)?
                ))?;
                if let RegisterStorage::Scalar(kind) = self.flat_value_storage_for_type(arg.1) {
                    let abi_kind = self
                        .scalar_storage_kind_for_type(arg.1)
                        .unwrap_or(ScalarStorageKind::Word);
                    writer.line(format!(
                        "let {} = {};",
                        self.argument_scalar_name(index),
                        self.convert_scalar_expr(format!("arg_{index}_value"), abi_kind, kind)
                    ))?;
                }
            }

            let mut regs: Vec<_> = register_storage.into_iter().collect();
            regs.sort_by_key(|(reg, _storage)| *reg);
            for (reg, storage) in regs {
                match storage {
                    RegisterStorage::Scalar(kind) => writer.line(format!(
                        "let mut reg_{reg}: {} = {};",
                        self.scalar_storage_rust_type(kind),
                        self.scalar_storage_default_expr(kind)
                    ))?,
                    RegisterStorage::Words(size) => {
                        writer.line(format!("let mut reg_{reg} = [0u64; {size}];"))?
                    }
                }
            }

            let mut stack_roots = stack_pointers
                .values()
                .map(|pointer| pointer.root_reg)
                .collect::<Vec<_>>();
            stack_roots.sort_unstable();
            stack_roots.dedup();
            for root_reg in stack_roots {
                let size = self.resolve_register_alloc_size(func, root_reg).ok_or_else(|| {
                    format!("missing alloc size for stackified register {root_reg}")
                })?;
                writer.line(format!("let mut {} = [0u64; {size}];", self.stack_alloc_name(root_reg)))?;
            }

            let body = self.render_section(0, |writer| {
                if self.can_emit_straight_line_function(func) {
                    for (dst, instr) in &func.body[0].0 {
                        self.emit_instruction(
                            writer,
                            func,
                            0,
                            dst,
                            instr,
                            global_slots,
                            &block_preds,
                            &stack_pointers,
                        )?;
                    }
                    if self.block_can_fall_through(&func.body[0]) {
                        writer.line(format!(
                            "return Err(\"basic block 0 in function {} fell through without terminator\".to_string());",
                            func.index
                        ))
                    } else {
                        Ok(())
                    }
                } else {
                    writer.line("let mut bb: usize = 0;")?;
                    writer.line("let mut pred_bb: usize = 0;")?;
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
                                            &stack_pointers,
                                        )?;
                                    }
                                    if let Some((target_bb, pred_source)) = fallthrough_edges
                                        .get(block_index)
                                        .and_then(|target| *target)
                                    {
                                        writer.line(format!("pred_bb = {pred_source};"))?;
                                        writer.line(format!("bb = {target_bb}usize;"))?;
                                        writer.line("continue;")
                                    } else if self.block_can_fall_through(block) {
                                        writer.line(format!(
                                            "return Err(\"basic block {block_index} in function {} fell through without terminator\".to_string());",
                                            func.index
                                        ))
                                    } else {
                                        Ok(())
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
                }
            })?;

            self.emit_infallible_generated_body(writer, &body)
        })?;
        writer.line("}")
    }

    fn emit_infallible_generated_body<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        body: &str,
    ) -> Result<(), String> {
        for line in self.rewrite_infallible_generated_body(body).lines() {
            if line.is_empty() {
                writer.blank_line()?;
            } else {
                writer.line(line)?;
            }
        }
        Ok(())
    }

    fn rewrite_infallible_generated_body(&self, body: &str) -> String {
        body.lines()
            .map(Self::rewrite_infallible_generated_line)
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn rewrite_infallible_generated_line(line: &str) -> String {
        let line = line.replace("memory.", "self.memory.");
        let line = line.replace('?', ".unwrap()");
        if let Some(rewritten) =
            Self::rewrite_generated_return(&line, "return Ok(", |prefix, inner, suffix| {
                format!("{prefix}return {inner}{suffix}")
            })
        {
            return rewritten;
        }
        if let Some(rewritten) =
            Self::rewrite_generated_return(&line, "return Err(", |prefix, inner, suffix| {
                format!("{prefix}panic!(\"{{}}\", {inner}){suffix}")
            })
        {
            return rewritten;
        }
        line
    }

    fn rewrite_generated_return(
        line: &str,
        marker: &str,
        build: impl FnOnce(&str, &str, &str) -> String,
    ) -> Option<String> {
        let start = line.find(marker)?;
        let prefix = &line[..start];
        let expr_start = start + marker.len();
        let mut depth = 1usize;
        let mut expr_end = None;

        for (offset, ch) in line[expr_start..].char_indices() {
            match ch {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        expr_end = Some(expr_start + offset);
                        break;
                    }
                }
                _ => {}
            }
        }

        let expr_end = expr_end?;
        let inner = &line[expr_start..expr_end];
        let suffix = &line[expr_end + 1..];
        Some(build(prefix, inner, suffix))
    }

    fn can_emit_straight_line_function(&self, func: &Function) -> bool {
        func.body.len() == 1
            && func.body[0].0.iter().all(|(_dst, instr)| {
                !matches!(
                    instr,
                    Instruction::JmpIf(_, _, _, _)
                        | Instruction::Jmp(_)
                        | Instruction::Switch { .. }
                        | Instruction::Phi(_, _)
                        | Instruction::PhiSwitch(_)
                )
            })
    }

    fn block_can_fall_through(&self, block: &Block) -> bool {
        block.0.last().is_none_or(|(_dst, instr)| {
            !matches!(
                instr,
                Instruction::Return(_, _)
                    | Instruction::ReturnFeed(_, _)
                    | Instruction::JmpIf(_, _, _, _)
                    | Instruction::Jmp(_)
                    | Instruction::Switch { .. }
            )
        })
    }

    fn collect_register_storage(
        &self,
        func: &Function,
    ) -> Result<HashMap<VReg, RegisterStorage>, String> {
        let mut storage = HashMap::new();
        for block in &func.body {
            for (dst, instr) in &block.0 {
                if let Value::Register(reg) = dst.as_ref() {
                    let register_storage = self.instruction_result_storage(func, instr)?;
                    storage.entry(*reg).or_insert(register_storage);
                }
            }
        }
        Ok(storage)
    }

    fn collect_stack_pointers(
        &self,
        func: &Function,
    ) -> Result<HashMap<VReg, StackPointer>, String> {
        let mut stack_pointers = func
            .body
            .iter()
            .flat_map(|block| block.0.iter())
            .filter_map(|(dst, instr)| match (dst.as_ref(), instr) {
                (Value::Register(reg), Instruction::Alloc(ty)) if ty.word_size() > 0 => Some((
                    *reg,
                    StackPointer {
                        root_reg: *reg,
                        word_offset: 0,
                    },
                )),
                _ => None,
            })
            .collect::<HashMap<_, _>>();

        let mut changed = true;
        while changed {
            changed = false;
            for block in &func.body {
                for (dst, instr) in &block.0 {
                    let (Value::Register(dst_reg), Instruction::GetElement { value, .. }) =
                        (dst.as_ref(), instr)
                    else {
                        continue;
                    };
                    let Value::Register(base_reg) = value.as_ref() else {
                        continue;
                    };
                    let Some(base_pointer) = stack_pointers.get(base_reg).copied() else {
                        continue;
                    };
                    let next_pointer = StackPointer {
                        root_reg: base_pointer.root_reg,
                        word_offset: base_pointer.word_offset
                            + self.getelement_word_offset(instr)?,
                    };
                    if stack_pointers.get(dst_reg) != Some(&next_pointer) {
                        stack_pointers.insert(*dst_reg, next_pointer);
                        changed = true;
                    }
                }
            }
        }

        let escaped_roots = self.collect_escaped_stack_pointer_roots(func, &stack_pointers);
        stack_pointers.retain(|_, pointer| !escaped_roots.contains(&pointer.root_reg));
        Ok(stack_pointers)
    }

    fn collect_escaped_stack_pointer_roots(
        &self,
        func: &Function,
        stack_pointers: &HashMap<VReg, StackPointer>,
    ) -> HashSet<VReg> {
        let mut escaped_roots = HashSet::new();
        let mut mark = |value: &VPtr, safe_pointer_use: bool| {
            if let Value::Register(reg) = value.as_ref() {
                if let Some(pointer) = stack_pointers.get(reg) {
                    if !safe_pointer_use {
                        escaped_roots.insert(pointer.root_reg);
                    }
                }
            }
        };

        for block in &func.body {
            for (_dst, instr) in &block.0 {
                match instr {
                    Instruction::Uinteger(_)
                    | Instruction::Integer(_)
                    | Instruction::Float(_)
                    | Instruction::String(_)
                    | Instruction::Alloc(_)
                    | Instruction::GetUpValue(_, _)
                    | Instruction::PushStateOffset(_)
                    | Instruction::PopStateOffset(_)
                    | Instruction::GetState(_)
                    | Instruction::Jmp(_)
                    | Instruction::Error => {}
                    Instruction::Load(ptr, _) => mark(ptr, true),
                    Instruction::Store(ptr, src, _) => {
                        mark(ptr, true);
                        mark(src, false);
                    }
                    Instruction::GetElement { value, .. } => mark(value, true),
                    Instruction::Call(callee, args, _)
                    | Instruction::CallCls(callee, args, _)
                    | Instruction::CallIndirect(callee, args, _) => {
                        mark(callee, false);
                        for (arg, _) in args {
                            mark(arg, false);
                        }
                    }
                    Instruction::GetGlobal(global, _) => mark(global, false),
                    Instruction::SetGlobal(global, src, _) => {
                        mark(global, false);
                        mark(src, false);
                    }
                    Instruction::Closure(value)
                    | Instruction::CloseUpValues(value, _)
                    | Instruction::CloseHeapClosure(value)
                    | Instruction::CloneHeap(value)
                    | Instruction::SetUpValue(_, value, _)
                    | Instruction::JmpIf(value, _, _, _)
                    | Instruction::TaggedUnionGetTag(value)
                    | Instruction::TaggedUnionGetValue(value, _)
                    | Instruction::CloneUserSum { value, .. }
                    | Instruction::ReleaseUserSum { value, .. }
                    | Instruction::Mem(value)
                    | Instruction::NegF(value)
                    | Instruction::AbsF(value)
                    | Instruction::SinF(value)
                    | Instruction::CosF(value)
                    | Instruction::LogF(value)
                    | Instruction::SqrtF(value)
                    | Instruction::NegI(value)
                    | Instruction::AbsI(value)
                    | Instruction::PowI(value)
                    | Instruction::Not(value)
                    | Instruction::CastFtoI(value)
                    | Instruction::CastItoF(value)
                    | Instruction::CastItoB(value) => mark(value, false),
                    Instruction::MakeClosure { fn_proto, .. } => {
                        mark(fn_proto, false);
                        if let Some(closure_function) = self.resolve_function_index(func, fn_proto)
                        {
                            if let Some(closure_func) = self
                                .mir
                                .functions
                                .iter()
                                .find(|candidate| candidate.index == closure_function)
                            {
                                for upvalue in &closure_func.upindexes {
                                    mark(upvalue, false);
                                }
                            }
                        }
                    }
                    Instruction::Return(value, ty) | Instruction::ReturnFeed(value, ty) => {
                        mark(value, self.uses_direct_return_pointer(*ty));
                    }
                    Instruction::Switch { scrutinee, .. } => mark(scrutinee, false),
                    Instruction::Phi(left, right)
                    | Instruction::Delay(_, left, right)
                    | Instruction::AddF(left, right)
                    | Instruction::SubF(left, right)
                    | Instruction::MulF(left, right)
                    | Instruction::DivF(left, right)
                    | Instruction::ModF(left, right)
                    | Instruction::PowF(left, right)
                    | Instruction::AddI(left, right)
                    | Instruction::SubI(left, right)
                    | Instruction::MulI(left, right)
                    | Instruction::DivI(left, right)
                    | Instruction::ModI(left, right)
                    | Instruction::LogI(left, right)
                    | Instruction::Eq(left, right)
                    | Instruction::Ne(left, right)
                    | Instruction::Gt(left, right)
                    | Instruction::Ge(left, right)
                    | Instruction::Lt(left, right)
                    | Instruction::Le(left, right)
                    | Instruction::And(left, right)
                    | Instruction::Or(left, right)
                    | Instruction::GetArrayElem(left, right, _) => {
                        mark(left, false);
                        mark(right, false);
                    }
                    Instruction::TaggedUnionWrap { value, .. } => mark(value, false),
                    Instruction::BoxAlloc { value, .. } => mark(value, false),
                    Instruction::BoxLoad { ptr, .. }
                    | Instruction::BoxClone { ptr }
                    | Instruction::BoxRelease { ptr, .. } => mark(ptr, false),
                    Instruction::BoxStore { ptr, value, .. } => {
                        mark(ptr, false);
                        mark(value, false);
                    }
                    Instruction::PhiSwitch(inputs) | Instruction::Array(inputs, _) => {
                        for input in inputs {
                            mark(input, false);
                        }
                    }
                    Instruction::SetArrayElem(array, index, value, _) => {
                        mark(array, false);
                        mark(index, false);
                        mark(value, false);
                    }
                }
            }
        }
        escaped_roots
    }

    fn instruction_result_storage(
        &self,
        func: &Function,
        instr: &Instruction,
    ) -> Result<RegisterStorage, String> {
        Ok(match instr {
            Instruction::Uinteger(_)
            | Instruction::Alloc(_)
            | Instruction::GetElement { .. }
            | Instruction::String(_)
            | Instruction::Array(_, _)
            | Instruction::MakeClosure { .. }
            | Instruction::CloneHeap(_)
            | Instruction::CloseHeapClosure(_)
            | Instruction::TaggedUnionWrap { .. }
            | Instruction::TaggedUnionGetTag(_) => RegisterStorage::Scalar(ScalarStorageKind::Word),
            Instruction::Integer(_)
            | Instruction::AddI(_, _)
            | Instruction::SubI(_, _)
            | Instruction::MulI(_, _)
            | Instruction::DivI(_, _)
            | Instruction::ModI(_, _)
            | Instruction::NegI(_)
            | Instruction::AbsI(_)
            | Instruction::CastFtoI(_) => RegisterStorage::Scalar(ScalarStorageKind::I64),
            Instruction::Float(_)
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
            | Instruction::Not(_)
            | Instruction::Eq(_, _)
            | Instruction::Ne(_, _)
            | Instruction::Gt(_, _)
            | Instruction::Ge(_, _)
            | Instruction::Lt(_, _)
            | Instruction::Le(_, _)
            | Instruction::And(_, _)
            | Instruction::Or(_, _)
            | Instruction::CastItoF(_)
            | Instruction::CastItoB(_) => RegisterStorage::Scalar(ScalarStorageKind::F64),
            Instruction::Load(_, ty)
            | Instruction::Call(_, _, ty)
            | Instruction::CallIndirect(_, _, ty)
            | Instruction::GetArrayElem(_, _, ty)
            | Instruction::GetGlobal(_, ty)
            | Instruction::GetState(ty)
            | Instruction::GetUpValue(_, ty) => self.register_storage_for_type(*ty),
            Instruction::TaggedUnionGetValue(_, _) => {
                RegisterStorage::Scalar(ScalarStorageKind::Word)
            }
            Instruction::Mem(src) => self.value_storage(func, src)?,
            Instruction::Delay(_, src, _) => self.value_storage(func, src)?,
            Instruction::SetUpValue(_, src, _) => self.value_storage(func, src)?,
            Instruction::Phi(left, _) => self.value_storage(func, left)?,
            Instruction::PhiSwitch(inputs) => {
                let Some(first) = inputs.first() else {
                    return Err("PhiSwitch without inputs is invalid".to_string());
                };
                self.value_storage(func, first)?
            }
            Instruction::CloneUserSum { ty, .. } => self.register_storage_for_type(*ty),
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
            | Instruction::CloseUpValues(_, _)
            | Instruction::BoxAlloc { .. }
            | Instruction::BoxLoad { .. }
            | Instruction::BoxClone { .. }
            | Instruction::BoxRelease { .. }
            | Instruction::BoxStore { .. }
            | Instruction::ReleaseUserSum { .. }
            | Instruction::PowI(_)
            | Instruction::LogI(_, _)
            | Instruction::Return(_, _)
            | Instruction::ReturnFeed(_, _)
            | Instruction::Error => RegisterStorage::Scalar(ScalarStorageKind::Word),
        })
    }

    fn register_storage_for_type(&self, ty: TypeNodeId) -> RegisterStorage {
        if ty.word_size() == 0 {
            RegisterStorage::Words(0)
        } else if self.is_deepcopy_aggregate_type(ty) {
            RegisterStorage::Scalar(ScalarStorageKind::Word)
        } else if let Some(kind) = self.scalar_storage_kind_for_type(ty) {
            RegisterStorage::Scalar(kind)
        } else {
            RegisterStorage::Words(self.runtime_value_size(ty))
        }
    }

    fn flat_value_storage_for_type(&self, ty: TypeNodeId) -> RegisterStorage {
        if ty.word_size() == 0 {
            RegisterStorage::Words(0)
        } else if let Some(kind) = self.scalar_storage_kind_for_type(ty) {
            RegisterStorage::Scalar(kind)
        } else {
            RegisterStorage::Words(ty.word_size() as usize)
        }
    }

    fn value_storage(&self, func: &Function, value: &VPtr) -> Result<RegisterStorage, String> {
        match value.as_ref() {
            Value::Argument(index) => func
                .args
                .get(*index)
                .map(|arg| self.flat_value_storage_for_type(arg.1))
                .ok_or_else(|| format!("invalid argument index {}", index)),
            Value::Register(reg) => self
                .resolve_register_storage(func, *reg)
                .ok_or_else(|| format!("unknown register {}", reg)),
            Value::Function(_) | Value::ExtFunction(_, _) | Value::Global(_) | Value::State(_) => {
                Ok(RegisterStorage::Scalar(ScalarStorageKind::Word))
            }
            Value::Constructor(_, _, ty) => Ok(self.flat_value_storage_for_type(*ty)),
            Value::None => Ok(RegisterStorage::Words(0)),
        }
    }

    fn resolve_register_storage(
        &self,
        func: &Function,
        target_reg: VReg,
    ) -> Option<RegisterStorage> {
        func.body.iter().find_map(|block| {
            block.0.iter().find_map(|(dst, instr)| match dst.as_ref() {
                Value::Register(reg) if *reg == target_reg => {
                    self.instruction_result_storage(func, instr).ok()
                }
                _ => None,
            })
        })
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
        stack_pointers: &HashMap<VReg, StackPointer>,
    ) -> Result<(), String> {
        match instr {
            Instruction::Uinteger(value) => {
                self.assign_scalar(writer, dst, format!("{value}u64"))?
            }
            Instruction::Integer(value) => {
                self.assign_scalar(writer, dst, format!("{value}i64"))?
            }
            Instruction::Float(value) => {
                self.assign_scalar(writer, dst, format!("{value:?} as mmmfloat"))?
            }
            Instruction::Alloc(ty) => {
                if self.stack_pointer_for_value(stack_pointers, dst).is_none() {
                    self.assign_scalar(
                        writer,
                        dst,
                        format!("memory.alloc({}usize)", ty.word_size()),
                    )?;
                }
            }
            Instruction::Load(ptr, ty) => {
                if let Some(stack_pointer) = self.stack_pointer_for_value(stack_pointers, ptr) {
                    let size = ty.word_size() as usize;
                    let slice_expr = self.stack_pointer_slice_expr(stack_pointer, size);
                    if self.is_deepcopy_aggregate_type(*ty) {
                        let dest = self.reg_name(dst)?;
                        writer.line(format!("{dest} = memory.alloc({size}usize);"))?;
                        writer
                            .line(format!("memory.store({dest}, {slice_expr}, {size}usize)?;"))?;
                    } else if size == 1 {
                        self.assign_runtime_scalar_from_word_expr(
                            writer,
                            func,
                            dst,
                            self.stack_pointer_word_expr(stack_pointer),
                        )?;
                    } else {
                        let dest = self.reg_name(dst)?;
                        writer.line(format!(
                            "{dest} = {};",
                            self.slice_to_array_expr(slice_expr, self.runtime_value_size(*ty))
                        ))?;
                    }
                } else if self.can_load_scalar_directly(func, ptr, *ty) {
                    match self.value_storage(func, dst)? {
                        RegisterStorage::Scalar(kind) => {
                            let scalar_expr = self.scalar_expr_as(func, ptr, kind)?;
                            self.assign_scalar(writer, dst, scalar_expr)?;
                        }
                        RegisterStorage::Words(_) => {
                            let word_expr = self.word0_expr(func, ptr)?;
                            self.assign_runtime_scalar_from_word_expr(
                                writer, func, dst, word_expr,
                            )?;
                        }
                    }
                } else if self.is_deepcopy_aggregate_type(*ty) {
                    let is_state_ptr = matches!(ptr.as_ref(),
                        Value::Register(r) if self.is_getstate_register(func, *r));
                    if !is_state_ptr {
                        let dest = self.reg_name(dst)?;
                        writer.line(format!("{dest} = memory.alloc({}usize);", ty.word_size()))?;
                        self.emit_deep_copy_value_into_ptr(writer, func, &dest, ptr, *ty)?;
                    }
                } else {
                    let dest = self.reg_name(dst)?;
                    let ptr_expr = self.word0_expr(func, ptr)?;
                    if ty.word_size() == 1 {
                        self.assign_runtime_scalar_from_word_expr(
                            writer,
                            func,
                            dst,
                            format!("memory.load_word({ptr_expr})?"),
                        )?;
                    } else {
                        writer.line(format!(
                            "{dest} = {};",
                            self.vec_to_array_expr(
                                format!("memory.load({ptr_expr}, {}usize)?", ty.word_size()),
                                self.runtime_value_size(*ty),
                            )
                        ))?;
                    }
                }
            }
            Instruction::Store(ptr, src, ty) => {
                if let Some(stack_pointer) = self.stack_pointer_for_value(stack_pointers, ptr) {
                    let size = ty.word_size() as usize;
                    if self.is_deepcopy_aggregate_type(*ty) {
                        let src_expr =
                            self.boundary_value_slice_expr(func, stack_pointers, src, *ty)?;
                        writer.line(format!(
                            "{}.copy_from_slice({src_expr});",
                            self.stack_pointer_slice_target_expr(stack_pointer, size)
                        ))?;
                    } else if size == 1 {
                        let is_pointer = matches!(src.as_ref(), Value::Register(r) if self.is_pointer_register(func, *r));
                        let word_expr = if is_pointer {
                            let src_expr = self.word0_expr(func, src)?;
                            format!("memory.load_word({src_expr}).unwrap()")
                        } else {
                            self.scalar_word_expr(func, src)?
                        };
                        writer.line(format!(
                            "{} = {word_expr};",
                            self.stack_pointer_word_expr(stack_pointer)
                        ))?;
                    } else {
                        let src_expr = self.context_value_slice_expr(func, src, *ty)?;
                        writer.line(format!(
                            "{}.copy_from_slice({src_expr});",
                            self.stack_pointer_slice_target_expr(stack_pointer, size)
                        ))?;
                    }
                } else {
                    let ptr_expr = self.word0_expr(func, ptr)?;
                    if self.is_deepcopy_aggregate_type(*ty) {
                        self.emit_deep_copy_value_into_ptr(writer, func, &ptr_expr, src, *ty)?;
                    } else if ty.word_size() == 1 {
                        let is_pointer = matches!(src.as_ref(), Value::Register(r) if self.is_pointer_register(func, *r));
                        let word_expr = if is_pointer {
                            let src_expr = self.word0_expr(func, src)?;
                            format!("memory.load_word({src_expr}).unwrap()")
                        } else {
                            self.scalar_word_expr(func, src)?
                        };
                        writer.line(format!("memory.store_word({ptr_expr}, {word_expr})?;"))?;
                    } else {
                        let src_expr = self.context_value_slice_expr(func, src, *ty)?;
                        writer.line(format!(
                            "memory.store({ptr_expr}, {src_expr}, {}usize)?;",
                            ty.word_size()
                        ))?;
                    }
                }
            }
            Instruction::GetElement {
                value,
                tuple_offset: _,
                ..
            } => {
                if self
                    .stack_pointer_for_value(stack_pointers, value)
                    .is_none()
                {
                    let base_expr = self.word0_expr(func, value)?;
                    let word_offset = self.getelement_word_offset(instr)?;
                    self.assign_scalar(
                        writer,
                        dst,
                        format!("memory.get_element({base_expr}, {}usize)?", word_offset),
                    )?;
                }
            }
            Instruction::Call(callee, args, ret_ty) => {
                let dest = self.reg_name(dst)?;
                let dst_reg = match dst.as_ref() {
                    Value::Register(r) => *r,
                    _ => return Err(format!("call destination is not a register: {dst:?}")),
                };
                let call_kind = self.call_kind(callee)?;
                let result_size = self.instruction_result_size(func, instr)?;
                let static_callee = self.resolve_function_index(func, callee);
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
                    }
                    let direct_name = self.function_direct_name(target_func);
                    let mut provided_cursor = 0usize;
                    let mut call_args = target_func
                        .args
                        .iter()
                        .take(args.len().min(target_func.args.len()))
                        .map(|target_arg| {
                            self.consume_direct_call_arg_expr(
                                func,
                                args,
                                &mut provided_cursor,
                                target_arg.1,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    if provided_cursor != args.len() && call_args.len() == target_func.args.len() {
                        return Err(format!(
                            "direct call argument packing mismatch for {}: consumed {} of {} MIR operands",
                            target_func.label,
                            provided_cursor,
                            args.len()
                        ));
                    }
                    let provided_args = call_args.len();
                    let expected_args = target_func.args.len();
                    if provided_args < expected_args {
                        let default_functions = self.default_argument_functions(target_index);
                        let missing_args = expected_args - provided_args;
                        for default_index in default_functions.into_iter().take(missing_args) {
                            let default_func = self
                                .mir
                                .functions
                                .iter()
                                .find(|candidate| candidate.index == default_index)
                                .ok_or_else(|| {
                                    format!("missing default function for index {default_index}")
                                })?;
                            call_args.push(self.default_direct_call_expr(
                                default_func,
                                target_func.args[call_args.len()].1,
                            )?);
                        }
                    }
                    let mut direct_call_args = Vec::new();
                    direct_call_args.extend(call_args);
                    if self.uses_direct_return_pointer(*ret_ty) {
                        let size = ret_ty.word_size() as usize;
                        let cr_var = format!("call_result_{dst_reg}");
                        writer.line(format!("let mut {cr_var} = [0u64; {size}];"))?;
                        direct_call_args.push(format!("&mut {cr_var}"));
                        writer.line(format!(
                            "self.{direct_name}({});",
                            direct_call_args.join(", ")
                        ))?;
                        writer.line(format!("{dest} = memory.alloc({size}usize);"))?;
                        writer.line(format!("memory.store({dest}, &{cr_var}, {size}usize)?;"))?;
                    } else {
                        writer.line(format!(
                            "let call_result = self.{direct_name}({});",
                            direct_call_args.join(", ")
                        ))?;
                        self.emit_assign_abi_value_to_runtime(
                            writer,
                            func,
                            dst,
                            "call_result",
                            *ret_ty,
                        )?;
                    }
                    return Ok(());
                }
                writer.line("let mut call_args = Vec::new();")?;
                for (arg, ty) in args {
                    let expr = self.boundary_value_slice_expr(func, stack_pointers, arg, *ty)?;
                    writer.line(format!("call_args.extend_from_slice({expr});"))?;
                }
                match call_kind {
                    CallKind::FunctionHandle(handle_expr) => {
                        if self.is_deepcopy_aggregate_type(*ret_ty) {
                            let cr_var = format!("call_result_{dst_reg}");
                            writer.line(format!(
                                "let {cr_var} = self.call_function_handle({handle_expr}, &call_args);"
                            ))?;
                            writer.line(format!(
                                "{dest} = memory.alloc({}usize);",
                                ret_ty.word_size()
                            ))?;
                            writer.line(format!(
                                "memory.store({dest}, &{cr_var}, {}usize)?;",
                                ret_ty.word_size()
                            ))?;
                        } else if ret_ty.word_size() == 1 {
                            writer.line(format!(
                                "let call_result = self.call_function_handle({handle_expr}, &call_args);"
                            ))?;
                            self.assign_runtime_scalar_from_word_expr(
                                writer,
                                func,
                                dst,
                                "call_result[0]".to_string(),
                            )?;
                        } else {
                            writer.line(format!(
                                "{dest} = {};",
                                self.vec_to_array_expr(
                                    format!("self.call_function_handle({handle_expr}, &call_args)"),
                                    result_size,
                                )
                            ))?;
                        }
                    }
                    CallKind::Ext(symbol) => {
                        let name = format!("{:?}", symbol.as_str());
                        let ret_words = ret_ty.word_size() as usize;
                        if self.is_deepcopy_aggregate_type(*ret_ty) {
                            let cr_var = format!("call_result_{dst_reg}");
                            writer.line(format!(
                                "let {cr_var} = self.call_ext({name}, &call_args, {ret_words})?;"
                            ))?;
                            writer.line(format!("{dest} = memory.alloc({ret_words}usize);"))?;
                            writer.line(format!(
                                "memory.store({dest}, &{cr_var}, {ret_words}usize)?;"
                            ))?;
                        } else if ret_ty.word_size() == 1 {
                            writer.line(format!(
                                "let call_result = self.call_ext({name}, &call_args, {ret_words})?;"
                            ))?;
                            self.assign_runtime_scalar_from_word_expr(
                                writer,
                                func,
                                dst,
                                "call_result[0]".to_string(),
                            )?;
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
                            "_mimium_getnow" => Some("self.host.current_time()".to_string()),
                            "_mimium_getsamplerate" => Some("self.host.sample_rate()".to_string()),
                            _ => None,
                        }
                    }
                    _ => None,
                };

                if let Some(expr) = runtime_expr {
                    self.assign_scalar(writer, dst, expr)?;
                } else {
                    let dest = self.reg_name(dst)?;
                    let dst_reg = match dst.as_ref() {
                        Value::Register(r) => *r,
                        _ => {
                            return Err(format!(
                                "call-indirect destination is not a register: {dst:?}"
                            ));
                        }
                    };
                    let result_size = self.instruction_result_size(func, instr)?;
                    let handle_expr = self.word0_expr(func, callee)?;
                    writer.line("let mut call_args = Vec::new();")?;
                    for (arg, ty) in args {
                        let expr =
                            self.boundary_value_slice_expr(func, stack_pointers, arg, *ty)?;
                        writer.line(format!("call_args.extend_from_slice({expr});"))?;
                    }
                    if self.is_deepcopy_aggregate_type(*ret_ty) {
                        let cr_var = format!("call_result_{dst_reg}");
                        writer.line(format!(
                            "let {cr_var} = self.call_function_handle({handle_expr}, &call_args);"
                        ))?;
                        writer.line(format!(
                            "{dest} = memory.alloc({}usize);",
                            ret_ty.word_size()
                        ))?;
                        writer.line(format!(
                            "memory.store({dest}, &{cr_var}, {}usize)?;",
                            ret_ty.word_size()
                        ))?;
                    } else if ret_ty.word_size() == 1 {
                        writer.line(format!(
                            "let call_result = self.call_function_handle({handle_expr}, &call_args);"
                        ))?;
                        self.assign_runtime_scalar_from_word_expr(
                            writer,
                            func,
                            dst,
                            "call_result[0]".to_string(),
                        )?;
                    } else {
                        writer.line(format!(
                            "{dest} = {};",
                            self.vec_to_array_expr(
                                format!("self.call_function_handle({handle_expr}, &call_args)"),
                                result_size,
                            )
                        ))?;
                    }
                }
            }
            Instruction::MakeClosure { fn_proto, .. } => {
                let dest = self.reg_name(dst)?;
                let handle_expr = self.word0_expr(func, fn_proto)?;
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
                                let upvalue_expr = self.word0_expr(func, upvalue)?;
                                if alloc_size == 1 {
                                    if self.should_capture_alloc_by_value(alloc_ty) {
                                        writer.line(format!(
                                            "closure_upvalues.push(memory.load_word({upvalue_expr})?);"
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
                                    let upvalue_expr = self.word0_expr(func, upvalue)?;
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
                            let upvalue_expr = self.word0_expr(func, upvalue)?;
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
                    "{dest} = self.closures.alloc({handle_expr}, closure_upvalues, closure_indirect, {}usize)?;"
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
                writer.line("let closure_handle = self.get_current_closure().ok_or_else(|| \"missing closure context for GetUpValue\".to_string())?;")?;
                if self.is_deepcopy_aggregate_type(*ty) {
                    let dest = self.reg_name(dst)?;
                    writer.line(format!("{dest} = memory.alloc({size}usize);"))?;
                    writer.line("{")?;
                    writer.indented(1, |writer| {
                        writer.line(format!(
                            "let copied_words = self.load_upvalue(closure_handle, {}usize, {size}usize)?;",
                            *index as usize
                        ))?;
                        writer.line(format!("memory.store({dest}, &copied_words, {size}usize)?;"))
                    })?;
                    writer.line("}")?;
                } else if ty.word_size() == 1 {
                    self.assign_runtime_scalar_from_word_expr(
                        writer,
                        func,
                        dst,
                        format!(
                            "self.load_upvalue_word(closure_handle, {}usize)?",
                            *index as usize
                        ),
                    )?;
                } else {
                    let dest = self.reg_name(dst)?;
                    writer.line(format!(
                        "{dest} = {};",
                        self.vec_to_array_expr(
                            format!(
                                "self.load_upvalue(closure_handle, {}usize, {size}usize)?",
                                *index as usize
                            ),
                            size,
                        )
                    ))?;
                }
            }
            Instruction::SetUpValue(index, src, ty) => {
                let dest = self.reg_name(dst)?;
                let src_expr = self.boundary_value_slice_expr(func, stack_pointers, src, *ty)?;
                let size = ty.word_size() as usize;
                writer.line("let closure_handle = self.get_current_closure().ok_or_else(|| \"missing closure context for SetUpValue\".to_string())?;")?;
                if size == 1 {
                    let src_word = self.scalar_word_expr(func, src)?;
                    writer.line(format!(
                        "self.store_upvalue_word(closure_handle, {}usize, {src_word})?;",
                        *index as usize
                    ))?;
                } else {
                    writer.line(format!(
                        "self.store_upvalue(closure_handle, {}usize, {src_expr}, {size}usize)?;",
                        *index as usize
                    ))?;
                }
                if ty.word_size() == 1 {
                    let src_scalar = self.scalar_expr_as(
                        func,
                        src,
                        self.scalar_storage_kind_for_type(*ty)
                            .unwrap_or(ScalarStorageKind::Word),
                    )?;
                    writer.line(format!("{dest} = {src_scalar};"))?;
                } else {
                    let expr = match self.value_storage(func, dst)? {
                        RegisterStorage::Scalar(kind) => self.scalar_expr_as(func, src, kind)?,
                        RegisterStorage::Words(_) => {
                            self.context_value_array_expr(func, src, *ty)?
                        }
                    };
                    writer.line(format!("{dest} = {expr};"))?;
                }
            }
            Instruction::GetGlobal(global, ty) => {
                let global_index = self.global_index(global_slots, global)?;
                if self.is_deepcopy_aggregate_type(*ty) {
                    let dest = self.reg_name(dst)?;
                    writer.line(format!("{dest} = memory.alloc({}usize);", ty.word_size()))?;
                    writer.line(format!(
                        "memory.store({dest}, &self.globals[{global_index}][..{}], {}usize)?;",
                        ty.word_size(),
                        ty.word_size()
                    ))?;
                } else if ty.word_size() == 1 {
                    self.assign_runtime_scalar_from_word_expr(
                        writer,
                        func,
                        dst,
                        format!("self.globals[{global_index}][0]"),
                    )?;
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
                    let src_expr =
                        self.boundary_value_slice_expr(func, stack_pointers, src, *ty)?;
                    writer.line(format!(
                        "self.globals[{global_index}][..{size}].copy_from_slice({src_expr});"
                    ))?;
                } else if ty.word_size() == 1 {
                    let is_getelement = matches!(src.as_ref(), Value::Register(r) if self.is_getelement_register(func, *r));
                    if is_getelement {
                        let src_expr = self.word0_expr(func, src)?;
                        writer.line(format!("self.globals[{global_index}][0] = memory.load_word({src_expr}).unwrap();"))?;
                    } else {
                        let src_expr = self.scalar_word_expr(func, src)?;
                        writer.line(format!("self.globals[{global_index}][0] = {src_expr};"))?;
                    }
                } else {
                    let src_expr = self.context_value_slice_expr(func, src, *ty)?;
                    writer.line(format!(
                        "self.globals[{global_index}][..{}].copy_from_slice({src_expr});",
                        ty.word_size()
                    ))?;
                }
            }
            Instruction::PushStateOffset(offset) => {
                writer.line(format!(
                    "self.get_current_statestorage().push_pos({offset}usize);"
                ))?;
            }
            Instruction::PopStateOffset(offset) => {
                writer.line(format!(
                    "self.get_current_statestorage().pop_pos({offset}usize);"
                ))?;
            }
            Instruction::GetState(ty) => {
                if self.is_deepcopy_aggregate_type(*ty) {
                    // Bypassed by find_state_load_source in all aggregate slice expressions;
                    // the downstream Load is also skipped, so no alloc+store is needed.
                } else if ty.word_size() == 1 {
                    self.assign_runtime_scalar_from_word_expr(
                        writer,
                        func,
                        dst,
                        "{ let state = self.get_current_statestorage(); state.get_state_word() }"
                            .to_string(),
                    )?;
                } else {
                    let dest = self.reg_name(dst)?;
                    writer.line(format!(
                        "{dest} = {};",
                        self.vec_to_array_expr(
                            format!(
                                "{{ let state = self.get_current_statestorage(); state.get_state({}usize) }}",
                                ty.word_size()
                            ),
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
                let dest_storage = self.value_storage(func, dst)?;
                let (left_expr, right_expr) = match dest_storage {
                    RegisterStorage::Scalar(kind) => (
                        self.scalar_expr_as(func, left, kind)?,
                        self.scalar_expr_as(func, right, kind)?,
                    ),
                    RegisterStorage::Words(_) => (
                        self.value_array_expr(func, left)?,
                        self.value_array_expr(func, right)?,
                    ),
                };
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
                let scrutinee_expr =
                    self.scalar_expr_as(func, scrutinee, ScalarStorageKind::I64)?;
                writer.line(format!("let scrutinee = {scrutinee_expr};"))?;
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
                let dest_storage = self.value_storage(func, dst)?;
                writer.line(format!("{dest} = match pred_bb {{"))?;
                writer.indented(1, |writer| {
                    for (pred, input) in preds.iter().zip(inputs.iter()) {
                        let expr = match dest_storage {
                            RegisterStorage::Scalar(kind) => self.scalar_expr_as(func, input, kind)?,
                            RegisterStorage::Words(_) => self.value_array_expr(func, input)?,
                        };
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
                    writer.line("return Ok(());")?;
                } else if self.uses_direct_return_pointer(*ty) {
                    let result_expr =
                        self.aggregate_source_slice_expr(func, stack_pointers, value, *ty)?;
                    writer.line(format!(
                        "{}.copy_from_slice({result_expr});",
                        self.direct_return_words_name()
                    ))?;
                    writer.line("return Ok(());")?;
                } else {
                    let expr = self.abi_value_expr(func, value, *ty)?;
                    writer.line(format!("return Ok({expr});"))?;
                }
            }
            Instruction::ReturnFeed(value, ty) => {
                let uses_return_pointer = self.uses_direct_return_pointer(*ty);
                let expr = (!uses_return_pointer)
                    .then(|| self.abi_value_expr(func, value, *ty))
                    .transpose()?;
                match self.config.self_eval_mode {
                    SelfEvalMode::SimpleState => {
                        if ty.word_size() == 1 && !self.is_deepcopy_aggregate_type(*ty) {
                            let next_state_word = self.scalar_word_expr(func, value)?;
                            writer.line("{")?;
                            writer.indented(1, |writer| {
                                writer.line("let state = self.get_current_statestorage();")?;
                                writer.line(format!("state.set_state_word({next_state_word});"))
                            })?;
                            writer.line("}")?;
                        } else {
                            let state_expr =
                                self.aggregate_source_slice_expr(func, stack_pointers, value, *ty)?;
                            writer.line("{")?;
                            writer.indented(1, |writer| {
                                writer.line("let state = self.get_current_statestorage();")?;
                                writer.line(format!(
                                    "state.set_state({state_expr}, {}usize);",
                                    ty.word_size()
                                ))
                            })?;
                            writer.line("}")?;
                            if uses_return_pointer {
                                writer.line(format!(
                                    "{}.copy_from_slice({state_expr});",
                                    self.direct_return_words_name()
                                ))?;
                            }
                        }
                        if uses_return_pointer {
                            writer.line("return Ok(());")?;
                        } else {
                            writer.line(format!(
                                "let result = {};",
                                expr.as_deref().unwrap_or("()")
                            ))?;
                            writer.line("return Ok(result);")?;
                        }
                    }
                    SelfEvalMode::ZeroAtInit => {
                        if self.is_deepcopy_aggregate_type(*ty) {
                            let size = ty.word_size() as usize;
                            writer.line(format!(
                                "let previous_words = {{ let state = self.get_current_statestorage(); state.get_state({size}usize) }};"
                            ))?;
                        } else if ty.word_size() == 1 {
                            writer.line(
                                "let previous_word = { let state = self.get_current_statestorage(); state.get_state_word() };",
                            )?;
                        } else {
                            writer.line(format!(
                                "let previous = {{ let state = self.get_current_statestorage(); state.get_state({}usize) }};",
                                ty.word_size()
                            ))?;
                        }
                        if !uses_return_pointer {
                            writer.line(format!(
                                "let result = {};",
                                expr.as_deref().unwrap_or("()")
                            ))?;
                        }
                        if ty.word_size() == 1 && !self.is_deepcopy_aggregate_type(*ty) {
                            let next_state_word = self.scalar_word_expr(func, value)?;
                            writer.line("{")?;
                            writer.indented(1, |writer| {
                                writer.line("let state = self.get_current_statestorage();")?;
                                writer.line(format!("state.set_state_word({next_state_word});"))
                            })?;
                            writer.line("}")?;
                        } else {
                            let state_expr =
                                self.aggregate_source_slice_expr(func, stack_pointers, value, *ty)?;
                            writer.line("{")?;
                            writer.indented(1, |writer| {
                                writer.line("let state = self.get_current_statestorage();")?;
                                writer.line(format!(
                                    "state.set_state({state_expr}, {}usize);",
                                    ty.word_size()
                                ))
                            })?;
                            writer.line("}")?;
                        }
                        if uses_return_pointer {
                            writer.line(format!(
                                "{}.copy_from_slice(&previous_words);",
                                self.direct_return_words_name()
                            ))?;
                            writer.line("return Ok(());")?;
                        } else if self.is_deepcopy_aggregate_type(*ty) {
                            writer.line(format!(
                                "return Ok({});",
                                self.abi_expr_from_word_source("previous_words", *ty)?
                            ))?;
                        } else if ty.word_size() == 1 {
                            writer.line("return Ok(previous_word);")?;
                        } else {
                            writer.line(format!(
                                "return Ok({});",
                                self.abi_expr_from_word_source("previous", *ty)?
                            ))?;
                        }
                    }
                }
            }
            Instruction::Delay(max_len, src, time) => {
                let src_expr = self.scalar_word_expr(func, src)?;
                let time_expr = self.word0_expr(func, time)?;
                writer.line("{")?;
                writer.indented(1, |writer| {
                    writer.line("let state = self.get_current_statestorage();")?;
                    self.assign_runtime_scalar_from_word_expr(
                        writer,
                        func,
                        dst,
                        format!("state.delay({src_expr}, {time_expr}, {}usize)", max_len),
                    )
                })?;
                writer.line("}")?;
            }
            Instruction::Mem(src) => {
                let src_expr = self.scalar_word_expr(func, src)?;
                writer.line("{")?;
                writer.indented(1, |writer| {
                    writer.line("let state = self.get_current_statestorage();")?;
                    self.assign_runtime_scalar_from_word_expr(
                        writer,
                        func,
                        dst,
                        format!("state.mem({src_expr})"),
                    )
                })?;
                writer.line("}")?;
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
                let expr = self.scalar_truthy_expr(func, value)?;
                self.assign_scalar(writer, dst, format!("if !{expr} {{ 1.0 }} else {{ 0.0 }}"))?;
            }
            Instruction::CastFtoI(value) => {
                let expr = self.scalar_expr_as(func, value, ScalarStorageKind::F64)?;
                self.assign_scalar(writer, dst, format!("({expr} as i64)"))?;
            }
            Instruction::CastItoF(value) => {
                let expr = self.scalar_expr_as(func, value, ScalarStorageKind::I64)?;
                self.assign_scalar(writer, dst, format!("({expr} as mmmfloat)"))?;
            }
            Instruction::CastItoB(value) => {
                let expr = self.scalar_expr_as(func, value, ScalarStorageKind::I64)?;
                self.assign_scalar(
                    writer,
                    dst,
                    format!("if {expr} != 0 {{ 1.0 }} else {{ 0.0 }}"),
                )?;
            }
            Instruction::Array(values, elem_ty) => {
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
                self.assign_scalar(writer, dst, "array_handle".to_string())?;
            }
            Instruction::GetArrayElem(arr, idx, elem_ty) => {
                let dest = self.reg_name(dst)?;
                let dest_storage = self.value_storage(func, dst)?;
                let arr_expr = self.word0_expr(func, arr)?;
                let idx_expr = self.scalar_expr_as(func, idx, ScalarStorageKind::F64)?;
                let elem_words = elem_ty.word_size() as usize;
                let empty_assign = match dest_storage {
                    RegisterStorage::Scalar(kind) => {
                        format!("{dest} = {};", self.scalar_storage_default_expr(kind))
                    }
                    RegisterStorage::Words(_) => format!("{dest}.fill(0);"),
                };
                writer.line("{")?;
                writer.indented(1, |writer| {
                    writer.line(format!("let array = self.arrays.get({arr_expr})?;"))?;
                    writer.line(
                        "let len = if array.elem_size_words == 0 { 0usize } else { array.data.len() / array.elem_size_words };",
                    )?;
                    writer.line(format!("let index_value = {idx_expr};"))?;
                    writer.line(
                        "let index = if len == 0 { 0usize } else if !index_value.is_finite() { 0usize } else { (index_value as i64).clamp(0, (len - 1) as i64) as usize };",
                    )?;
                    writer.line(format!("if len == 0 {{ {empty_assign} }} else {{"))?;
                    writer.indented(1, |writer| {
                        writer.line(format!("let start = index * {}usize;", elem_words))?;
                        writer.line(format!("let end = start + {}usize;", elem_words))?;
                        if self.is_deepcopy_aggregate_type(*elem_ty) {
                            writer.line(format!("{dest} = memory.alloc({elem_words}usize);"))?;
                            writer.line(format!(
                                "memory.store({dest}, &array.data[start..end], {elem_words}usize)?;"
                            ))
                        } else if elem_ty.word_size() == 1 {
                            self.assign_runtime_scalar_from_word_expr(
                                writer,
                                func,
                                dst,
                                "array.data[start]".to_string(),
                            )
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
                let arr_expr = self.word0_expr(func, arr)?;
                let idx_expr = self.scalar_expr_as(func, idx, ScalarStorageKind::F64)?;
                let elem_words = elem_ty.word_size() as usize;
                let value_expr = self.context_value_slice_expr(func, value, *elem_ty)?;
                writer.line("{")?;
                writer.indented(1, |writer| {
                    writer.line(format!("let array = self.arrays.get_mut({arr_expr})?;"))?;
                    writer.line(
                        "let len = if array.elem_size_words == 0 { 0usize } else { array.data.len() / array.elem_size_words };",
                    )?;
                    writer.line(format!("let index_value = {idx_expr};"))?;
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
                writer.line(format!("{dest} = memory.alloc({union_size}usize);"))?;
                writer.line(format!(
                    "memory.store({dest}, &[i64_to_word({}i64)], 1usize)?;",
                    *tag as i64
                ))?;
                if let Some(payload_ty) =
                    self.union_variant_payload_type(*union_type, *tag as usize)
                {
                    if payload_ty.word_size() > 0 {
                        writer.line(format!(
                            "let union_value_ptr = memory.get_element({dest}, 1usize)?;"
                        ))?;
                        if self.is_deepcopy_aggregate_type(payload_ty) {
                            self.emit_deep_copy_value_into_ptr(
                                writer,
                                func,
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
                let ptr_expr = self.word0_expr(func, value)?;
                self.assign_scalar(writer, dst, format!("memory.load_word({ptr_expr})?"))?;
            }
            Instruction::TaggedUnionGetValue(value, _ty) => {
                let dest = self.reg_name(dst)?;
                let ptr_expr = self.word0_expr(func, value)?;
                writer.line(format!(
                    "let union_value_ptr = memory.get_element({ptr_expr}, 1usize)?;"
                ))?;
                self.assign_scalar(writer, dst, "union_value_ptr".to_string())?;
            }
            Instruction::CloneUserSum { value, ty } => {
                let dest = self.reg_name(dst)?;
                if self.is_deepcopy_aggregate_type(*ty) {
                    let size = ty.word_size() as usize;
                    let src_ptr = self.scalar_word_expr(func, value)?;
                    writer.line(format!("{dest} = memory.alloc({size}usize);"))?;
                    writer.line(format!(
                        "let cloned_words = memory.load({src_ptr}, {size}usize)?;"
                    ))?;
                    writer.line(format!(
                        "memory.store({dest}, &cloned_words, {size}usize)?;"
                    ))?;
                } else {
                    let expr = match self.value_storage(func, dst)? {
                        RegisterStorage::Scalar(kind) => self.scalar_expr_as(func, value, kind)?,
                        RegisterStorage::Words(_) => {
                            self.context_value_array_expr(func, value, *ty)?
                        }
                    };
                    writer.line(format!("{dest} = {expr};"))?;
                }
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

    fn assign_float_binop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        left: &VPtr,
        right: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let left_expr = self.scalar_expr_as(func, left, ScalarStorageKind::F64)?;
        let right_expr = self.scalar_expr_as(func, right, ScalarStorageKind::F64)?;
        self.assign_scalar(writer, dst, format!("{left_expr} {op} {right_expr}"))
    }

    fn assign_float_method1<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        value: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let expr = self.scalar_expr_as(func, value, ScalarStorageKind::F64)?;
        self.assign_scalar(writer, dst, format!("{expr}.{method}()"))
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
        let left_expr = self.scalar_expr_as(func, left, ScalarStorageKind::F64)?;
        let right_expr = self.scalar_expr_as(func, right, ScalarStorageKind::F64)?;
        self.assign_scalar(writer, dst, format!("{left_expr}.{method}({right_expr})"))
    }

    fn assign_float_unop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        value: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let expr = self.scalar_expr_as(func, value, ScalarStorageKind::F64)?;
        self.assign_scalar(writer, dst, format!("{op}{expr}"))
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
        let left_expr = self.scalar_expr_as(func, left, ScalarStorageKind::I64)?;
        let right_expr = self.scalar_expr_as(func, right, ScalarStorageKind::I64)?;
        self.assign_scalar(writer, dst, format!("{left_expr} {op} {right_expr}"))
    }

    fn assign_int_unop<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        value: &VPtr,
        op: &str,
    ) -> Result<(), String> {
        let expr = self.scalar_expr_as(func, value, ScalarStorageKind::I64)?;
        self.assign_scalar(writer, dst, format!("{op}{expr}"))
    }

    fn assign_int_method1<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        value: &VPtr,
        method: &str,
    ) -> Result<(), String> {
        let expr = self.scalar_expr_as(func, value, ScalarStorageKind::I64)?;
        self.assign_scalar(writer, dst, format!("{expr}.{method}()"))
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
        let left_expr = self.scalar_expr_as(func, left, ScalarStorageKind::F64)?;
        let right_expr = self.scalar_expr_as(func, right, ScalarStorageKind::F64)?;
        self.assign_scalar(
            writer,
            dst,
            format!("if {left_expr} {op} {right_expr} {{ 1.0 }} else {{ 0.0 }}"),
        )
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
        let left_expr = self.scalar_truthy_expr(func, left)?;
        let right_expr = self.scalar_truthy_expr(func, right)?;
        self.assign_scalar(
            writer,
            dst,
            format!("if {left_expr} {op} {right_expr} {{ 1.0 }} else {{ 0.0 }}"),
        )
    }

    fn function_return_type(&self, func: &Function) -> Result<TypeNodeId, String> {
        func.return_type
            .get()
            .copied()
            .ok_or_else(|| format!("function {} is missing an inferred return type", func.label))
    }

    fn function_direct_name(&self, func: &Function) -> String {
        Self::escape_rust_identifier(&self.function_name_stem(func))
    }

    fn function_dispatch_name(&self, func: &Function) -> String {
        format!("dispatch_{}", self.function_name_stem(func))
    }

    fn function_name_stem(&self, func: &Function) -> String {
        let base = Self::sanitize_rust_identifier_base(func.label.as_str());
        let collisions = self
            .mir
            .functions
            .iter()
            .filter(|candidate| {
                Self::sanitize_rust_identifier_base(candidate.label.as_str()) == base
            })
            .count();
        if collisions > 1 || base == "_" {
            format!("{base}__{}", func.index)
        } else {
            base
        }
    }

    fn sanitize_rust_identifier_base(raw: &str) -> String {
        let mut ident = String::new();
        for ch in raw.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                ident.push(ch);
            } else {
                ident.push('_');
            }
        }
        if ident.is_empty() {
            ident.push_str("mimium");
        }
        if ident
            .chars()
            .next()
            .is_some_and(|first| first.is_ascii_digit())
        {
            ident.insert(0, '_');
        }
        ident
    }

    fn escape_rust_identifier(ident: &str) -> String {
        const KEYWORDS: &[&str] = &[
            "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
            "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
            "return", "self", "Self", "static", "struct", "super", "trait", "true", "type",
            "unsafe", "use", "where", "while", "async", "await", "dyn", "abstract", "become",
            "box", "do", "final", "macro", "override", "priv", "try", "typeof", "unsized",
            "virtual", "yield",
        ];
        if KEYWORDS.contains(&ident) {
            format!("r#{ident}")
        } else {
            ident.to_string()
        }
    }

    fn abi_type_expr(&self, ty: TypeNodeId) -> Result<String, String> {
        Ok(match ty.to_type() {
            Type::Primitive(crate::types::PType::Unit) => "()".to_string(),
            Type::Primitive(crate::types::PType::Numeric) => "mmmfloat".to_string(),
            Type::Primitive(crate::types::PType::Int) => "i64".to_string(),
            Type::Primitive(crate::types::PType::String)
            | Type::Array(_)
            | Type::Function { .. }
            | Type::Ref(_)
            | Type::Code(_)
            | Type::Boxed(_) => "Word".to_string(),
            Type::Tuple(elems) => self.abi_tuple_type_expr(
                elems
                    .iter()
                    .map(|elem| self.abi_type_expr(*elem))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Type::Record(fields) => self.abi_tuple_type_expr(
                fields
                    .iter()
                    .map(|field| self.abi_type_expr(field.ty))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Type::Union(_) | Type::UserSum { .. } => {
                let payload_words = ty.word_size().saturating_sub(1) as usize;
                format!("(Word, [Word; {payload_words}])")
            }
            _ => self.opaque_abi_type_expr(ty),
        })
    }

    fn direct_return_words_name(&self) -> &'static str {
        "ret_words"
    }

    fn uses_direct_return_pointer(&self, ty: TypeNodeId) -> bool {
        self.is_deepcopy_aggregate_type(ty)
    }

    fn direct_function_return_type_expr(&self, ty: TypeNodeId) -> Result<String, String> {
        if self.uses_direct_return_pointer(ty) {
            Ok("()".to_string())
        } else {
            self.abi_type_expr(ty)
        }
    }

    fn default_direct_call_expr(
        &self,
        default_func: &Function,
        target_ty: TypeNodeId,
    ) -> Result<String, String> {
        if self.uses_direct_return_pointer(target_ty) {
            let wrapper_name = self.function_dispatch_name(default_func);
            Ok(format!(
                "({{ let words = self.{wrapper_name}(&[]); {} }})",
                self.abi_expr_from_word_source("words", target_ty)?
            ))
        } else {
            Ok(format!(
                "self.{}()",
                self.function_direct_name(default_func)
            ))
        }
    }

    fn abi_tuple_type_expr(&self, items: Vec<String>) -> String {
        match items.len() {
            0 => "()".to_string(),
            1 => format!("({},)", items[0]),
            _ => format!("({})", items.join(", ")),
        }
    }

    fn abi_tuple_expr(&self, items: Vec<String>) -> String {
        match items.len() {
            0 => "()".to_string(),
            1 => format!("({},)", items[0]),
            _ => format!("({})", items.join(", ")),
        }
    }

    fn abi_word_exprs(&self, value_expr: &str, ty: TypeNodeId) -> Result<Vec<String>, String> {
        Ok(match ty.to_type() {
            Type::Primitive(crate::types::PType::Unit) => Vec::new(),
            Type::Primitive(crate::types::PType::Numeric) => {
                vec![format!("f64_to_word({value_expr})")]
            }
            Type::Primitive(crate::types::PType::Int) => {
                vec![format!("i64_to_word({value_expr})")]
            }
            Type::Primitive(crate::types::PType::String)
            | Type::Array(_)
            | Type::Function { .. }
            | Type::Ref(_)
            | Type::Code(_)
            | Type::Boxed(_) => vec![value_expr.to_string()],
            Type::Tuple(elems) => elems
                .iter()
                .enumerate()
                .map(|(index, elem)| self.abi_word_exprs(&format!("{value_expr}.{index}"), *elem))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect(),
            Type::Record(fields) => fields
                .iter()
                .enumerate()
                .map(|(index, field)| {
                    self.abi_word_exprs(&format!("{value_expr}.{index}"), field.ty)
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect(),
            Type::Union(_) | Type::UserSum { .. } => {
                let payload_words = ty.word_size().saturating_sub(1) as usize;
                let mut words = vec![format!("{value_expr}.0")];
                words.extend((0..payload_words).map(|index| format!("{value_expr}.1[{index}]")));
                words
            }
            _ => self.opaque_abi_word_exprs(value_expr, ty),
        })
    }

    fn abi_to_words_array_expr(&self, value_expr: &str, ty: TypeNodeId) -> Result<String, String> {
        let words = self.abi_word_exprs(value_expr, ty)?;
        if words.is_empty() {
            Ok("[0u64; 0]".to_string())
        } else {
            Ok(format!("[{}]", words.join(", ")))
        }
    }

    fn abi_expr_from_word_source(
        &self,
        source_expr: &str,
        ty: TypeNodeId,
    ) -> Result<String, String> {
        let mut offset = 0usize;
        self.abi_expr_from_word_source_at(source_expr, ty, &mut offset)
    }

    fn abi_expr_from_word_source_at(
        &self,
        source_expr: &str,
        ty: TypeNodeId,
        offset: &mut usize,
    ) -> Result<String, String> {
        Ok(match ty.to_type() {
            Type::Primitive(crate::types::PType::Unit) => "()".to_string(),
            Type::Primitive(crate::types::PType::Numeric) => {
                let expr = format!("word_to_f64({source_expr}[{}])", *offset);
                *offset += 1;
                expr
            }
            Type::Primitive(crate::types::PType::Int) => {
                let expr = format!("word_to_i64({source_expr}[{}])", *offset);
                *offset += 1;
                expr
            }
            Type::Primitive(crate::types::PType::String)
            | Type::Array(_)
            | Type::Function { .. }
            | Type::Ref(_)
            | Type::Code(_)
            | Type::Boxed(_) => {
                let expr = format!("{source_expr}[{}]", *offset);
                *offset += 1;
                expr
            }
            Type::Tuple(elems) => self.abi_tuple_expr(
                elems
                    .iter()
                    .map(|elem| self.abi_expr_from_word_source_at(source_expr, *elem, offset))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Type::Record(fields) => self.abi_tuple_expr(
                fields
                    .iter()
                    .map(|field| self.abi_expr_from_word_source_at(source_expr, field.ty, offset))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Type::Union(_) | Type::UserSum { .. } => {
                let tag_expr = format!("{source_expr}[{}]", *offset);
                *offset += 1;
                let payload_words = ty.word_size().saturating_sub(1) as usize;
                let payload_expr = if payload_words == 0 {
                    "[0u64; 0]".to_string()
                } else {
                    let payload = (*offset..(*offset + payload_words))
                        .map(|index| format!("{source_expr}[{index}]"))
                        .collect::<Vec<_>>();
                    format!("[{}]", payload.join(", "))
                };
                *offset += payload_words;
                format!("({tag_expr}, {payload_expr})")
            }
            _ => self.opaque_abi_expr_from_word_source(source_expr, ty, offset),
        })
    }

    fn opaque_abi_type_expr(&self, ty: TypeNodeId) -> String {
        match ty.word_size() as usize {
            0 => "()".to_string(),
            1 => "Word".to_string(),
            size => format!("[Word; {size}]"),
        }
    }

    fn opaque_abi_word_exprs(&self, value_expr: &str, ty: TypeNodeId) -> Vec<String> {
        match ty.word_size() as usize {
            0 => Vec::new(),
            1 => vec![value_expr.to_string()],
            size => (0..size)
                .map(|index| format!("{value_expr}[{index}]"))
                .collect(),
        }
    }

    fn opaque_abi_expr_from_word_source(
        &self,
        source_expr: &str,
        ty: TypeNodeId,
        offset: &mut usize,
    ) -> String {
        match ty.word_size() as usize {
            0 => "()".to_string(),
            1 => {
                let expr = format!("{source_expr}[{}]", *offset);
                *offset += 1;
                expr
            }
            size => {
                let items = (*offset..(*offset + size))
                    .map(|index| format!("{source_expr}[{index}]"))
                    .collect::<Vec<_>>();
                *offset += size;
                format!("[{}]", items.join(", "))
            }
        }
    }

    fn abi_value_expr(
        &self,
        func: &Function,
        value: &VPtr,
        ty: TypeNodeId,
    ) -> Result<String, String> {
        if ty.word_size() == 0 || matches!(value.as_ref(), Value::None) {
            return Ok("()".to_string());
        }
        if self.is_deepcopy_aggregate_type(ty) {
            return match value.as_ref() {
                Value::Argument(index) => {
                    self.abi_expr_from_word_source(&format!("arg_{index}"), ty)
                }
                _ => {
                    let ptr_expr = self.word0_expr(func, value)?;
                    Ok(format!(
                        "({{ let words = memory.load({ptr_expr}, {}usize)?; {} }})",
                        ty.word_size(),
                        self.abi_expr_from_word_source("words", ty)?
                    ))
                }
            };
        }

        if ty.word_size() == 1 {
            let abi_kind = self
                .scalar_storage_kind_for_type(ty)
                .unwrap_or(ScalarStorageKind::Word);
            if let Value::Register(r) = value.as_ref()
                && self.is_pointer_register(func, *r)
            {
                let loaded_expr = format!("memory.load_word(reg_{r}).unwrap()");
                return Ok(self.convert_scalar_expr(
                    loaded_expr,
                    ScalarStorageKind::Word,
                    abi_kind,
                ));
            }
            return self.scalar_expr_as(func, value, abi_kind);
        }

        match value.as_ref() {
            Value::Argument(index) => self.abi_expr_from_word_source(&format!("arg_{index}"), ty),
            Value::Register(reg) => self.abi_expr_from_word_source(&format!("reg_{reg}"), ty),
            _ => Err(format!(
                "value is not representable as a concrete ABI value in Rust codegen: {:?}",
                value
            )),
        }
    }

    fn consume_direct_call_arg_expr(
        &self,
        func: &Function,
        args: &[(VPtr, TypeNodeId)],
        cursor: &mut usize,
        target_ty: TypeNodeId,
    ) -> Result<String, String> {
        if target_ty.word_size() == 0 {
            return Ok("()".to_string());
        }

        let Some((value, value_ty)) = args.get(*cursor) else {
            return Err(format!(
                "missing MIR operand while packing direct call argument of type {:?}",
                target_ty.to_type()
            ));
        };

        if *value_ty == target_ty {
            *cursor += 1;
            return self.abi_value_expr(func, value, target_ty);
        }

        match target_ty.to_type() {
            Type::Tuple(elems) => Ok(self.abi_tuple_expr(
                elems
                    .iter()
                    .map(|elem| self.consume_direct_call_arg_expr(func, args, cursor, *elem))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Type::Record(fields) => Ok(self.abi_tuple_expr(
                fields
                    .iter()
                    .map(|field| self.consume_direct_call_arg_expr(func, args, cursor, field.ty))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            _ => {
                *cursor += 1;
                self.abi_value_expr(func, value, *value_ty)
            }
        }
    }

    fn emit_assign_abi_value_to_runtime<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        abi_expr: &str,
        ty: TypeNodeId,
    ) -> Result<(), String> {
        if ty.word_size() == 0 {
            return Ok(());
        }
        let dest = self.reg_name(dst)?;
        if self.is_deepcopy_aggregate_type(ty) {
            let size = ty.word_size() as usize;
            writer.line(format!(
                "let abi_words = {};",
                self.abi_to_words_array_expr(abi_expr, ty)?
            ))?;
            writer.line(format!("{dest} = memory.alloc({size}usize);"))?;
            writer.line(format!("memory.store({dest}, &abi_words, {size}usize)?;"))
        } else if ty.word_size() == 1 {
            self.assign_runtime_scalar_from_abi_value(writer, func, dst, abi_expr.to_string(), ty)
        } else {
            writer.line(format!(
                "{dest} = {};",
                self.abi_to_words_array_expr(abi_expr, ty)?
            ))
        }
    }

    fn emit_pack_abi_result<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        result_expr: &str,
        ty: TypeNodeId,
    ) -> Result<(), String> {
        if ty.word_size() == 0 {
            writer.line("Vec::new()")
        } else if self.is_deepcopy_aggregate_type(ty) {
            let size = ty.word_size() as usize;
            writer.line(format!(
                "let abi_words = {};",
                self.abi_to_words_array_expr(result_expr, ty)?
            ))?;
            writer.line(format!(
                "let result_handle = self.memory.alloc({size}usize);"
            ))?;
            writer.line(format!(
                "self.memory.store(result_handle, &abi_words, {size}usize).unwrap();"
            ))?;
            writer.line("vec![result_handle]")
        } else {
            writer.line(format!(
                "{}.to_vec()",
                self.abi_to_words_array_expr(result_expr, ty)?
            ))
        }
    }

    fn call_kind(&self, callee: &VPtr) -> Result<CallKind, String> {
        match callee.as_ref() {
            Value::Function(index) => Ok(CallKind::FunctionHandle(format!(
                "encode_function({index})"
            ))),
            Value::ExtFunction(symbol, _) => Ok(CallKind::Ext(*symbol)),
            Value::Register(reg) => Ok(CallKind::FunctionHandle(format!("reg_{reg}"))),
            Value::Argument(index) => {
                Ok(CallKind::FunctionHandle(self.argument_scalar_name(*index)))
            }
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
                    // TaggedUnionGetValue returns a pointer; its type is not
                    // the payload type, so we don't match it here.
                    _ => None,
                })
        })
    }

    fn is_pointer_register(&self, func: &Function, reg: VReg) -> bool {
        func.body.iter().any(|block| {
            block.0.iter().any(|(dst, instr)| {
                matches!(dst.as_ref(), Value::Register(r) if *r == reg)
                    && matches!(
                        instr,
                        Instruction::GetElement { .. } | Instruction::TaggedUnionGetValue(..)
                    )
            })
        })
    }

    fn is_getelement_register(&self, func: &Function, reg: VReg) -> bool {
        func.body.iter().any(|block| {
            block.0.iter().any(|(dst, instr)| {
                matches!(dst.as_ref(), Value::Register(r) if *r == reg)
                    && matches!(instr, Instruction::GetElement { .. })
            })
        })
    }

    fn resolve_value_type(&self, func: &Function, value: &VPtr) -> Option<TypeNodeId> {
        match value.as_ref() {
            Value::Argument(index) => func.args.get(*index).map(|arg| arg.1),
            Value::Register(reg) => self.resolve_register_value_type(func, *reg),
            Value::ExtFunction(_, ty) => Some(*ty),
            _ => None,
        }
    }

    fn is_immediate_scalar_type(&self, ty: TypeNodeId) -> bool {
        matches!(
            ty.to_type(),
            Type::Primitive(PType::Numeric | PType::Int | PType::Unit)
        )
    }

    fn can_load_scalar_directly(&self, func: &Function, ptr: &VPtr, loaded_ty: TypeNodeId) -> bool {
        loaded_ty.word_size() == 1
            && !self.is_deepcopy_aggregate_type(loaded_ty)
            && self
                .resolve_value_type(func, ptr)
                .is_some_and(|ptr_ty| ptr_ty == loaded_ty && self.is_immediate_scalar_type(ptr_ty))
    }

    fn should_capture_alloc_by_value(&self, ty: TypeNodeId) -> bool {
        matches!(
            ty.to_type(),
            Type::Function { .. }
                | Type::Array(_)
                | Type::Code(_)
                | Type::Boxed(_)
                | Type::Primitive(PType::String)
        )
    }

    fn union_variant_payload_type(&self, union_type: TypeNodeId, tag: usize) -> Option<TypeNodeId> {
        match union_type.to_type() {
            Type::UserSum { variants, .. } => variants.get(tag).and_then(|(_, payload)| *payload),
            Type::Union(variants) => variants.get(tag).copied(),
            _ => None,
        }
    }

    fn stack_pointer_for_value(
        &self,
        stack_pointers: &HashMap<VReg, StackPointer>,
        value: &VPtr,
    ) -> Option<StackPointer> {
        match value.as_ref() {
            Value::Register(reg) => stack_pointers.get(reg).copied(),
            _ => None,
        }
    }

    fn stack_alloc_name(&self, root_reg: VReg) -> String {
        format!("stack_alloc_{root_reg}")
    }

    fn stack_pointer_word_expr(&self, pointer: StackPointer) -> String {
        format!(
            "{}[{}usize]",
            self.stack_alloc_name(pointer.root_reg),
            pointer.word_offset
        )
    }

    fn stack_pointer_slice_target_expr(&self, pointer: StackPointer, size: usize) -> String {
        let start = pointer.word_offset;
        let end = start + size;
        format!(
            "{}[{start}usize..{end}usize]",
            self.stack_alloc_name(pointer.root_reg)
        )
    }

    fn stack_pointer_slice_expr(&self, pointer: StackPointer, size: usize) -> String {
        format!("&{}", self.stack_pointer_slice_target_expr(pointer, size))
    }

    fn aggregate_source_slice_expr(
        &self,
        func: &Function,
        stack_pointers: &HashMap<VReg, StackPointer>,
        value: &VPtr,
        ty: TypeNodeId,
    ) -> Result<String, String> {
        if !self.is_deepcopy_aggregate_type(ty) {
            return self.context_value_slice_expr(func, value, ty);
        }

        match value.as_ref() {
            Value::Argument(index) => Ok(format!("&arg_{index}")),
            Value::Register(reg) => {
                if let Some(pointer) = stack_pointers.get(reg) {
                    return Ok(self.stack_pointer_slice_expr(*pointer, ty.word_size() as usize));
                }
                if let Some(arg_idx) = self.find_argument_load_source(func, *reg) {
                    return Ok(format!("&arg_{arg_idx}"));
                }
                if let Some(size) = self.find_state_load_source(func, *reg) {
                    return Ok(format!(
                        "{{ let state = self.get_current_statestorage(); state.get_state_slice({size}usize) }}"
                    ));
                }
                if let Some(cr_var) = self.find_call_result_var(func, *reg) {
                    return Ok(format!("&{cr_var}"));
                }
                let ptr_expr = self.word0_expr(func, value)?;
                Ok(format!(
                    "&memory.load({ptr_expr}, {}usize)?",
                    ty.word_size()
                ))
            }
            _ => {
                let ptr_expr = self.word0_expr(func, value)?;
                Ok(format!(
                    "&memory.load({ptr_expr}, {}usize)?",
                    ty.word_size()
                ))
            }
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

    fn scalar_storage_kind_for_type(&self, ty: TypeNodeId) -> Option<ScalarStorageKind> {
        match ty.to_type() {
            Type::Primitive(PType::Numeric) => Some(ScalarStorageKind::F64),
            Type::Primitive(PType::Int) => Some(ScalarStorageKind::I64),
            Type::Primitive(PType::Unit) => None,
            Type::Primitive(PType::String)
            | Type::Array(_)
            | Type::Function { .. }
            | Type::Ref(_)
            | Type::Code(_)
            | Type::Boxed(_) => Some(ScalarStorageKind::Word),
            _ if ty.word_size() == 1 && !self.is_deepcopy_aggregate_type(ty) => {
                Some(ScalarStorageKind::Word)
            }
            _ => None,
        }
    }

    fn scalar_storage_rust_type(&self, kind: ScalarStorageKind) -> &'static str {
        match kind {
            ScalarStorageKind::Word => "Word",
            ScalarStorageKind::I64 => "i64",
            ScalarStorageKind::F64 => "mmmfloat",
        }
    }

    fn scalar_storage_default_expr(&self, kind: ScalarStorageKind) -> &'static str {
        match kind {
            ScalarStorageKind::Word => "0u64",
            ScalarStorageKind::I64 => "0i64",
            ScalarStorageKind::F64 => "0.0 as mmmfloat",
        }
    }

    fn argument_scalar_name(&self, index: usize) -> String {
        format!("arg_{index}_scalar")
    }

    fn scalar_from_word_expr(&self, word_expr: String, kind: ScalarStorageKind) -> String {
        match kind {
            ScalarStorageKind::Word => word_expr,
            ScalarStorageKind::I64 => format!("word_to_i64({word_expr})"),
            ScalarStorageKind::F64 => format!("word_to_f64({word_expr})"),
        }
    }

    fn scalar_to_word_expr(&self, scalar_expr: String, kind: ScalarStorageKind) -> String {
        match kind {
            ScalarStorageKind::Word => scalar_expr,
            ScalarStorageKind::I64 => format!("i64_to_word({scalar_expr})"),
            ScalarStorageKind::F64 => format!("f64_to_word({scalar_expr})"),
        }
    }

    fn convert_scalar_expr(
        &self,
        expr: String,
        from: ScalarStorageKind,
        to: ScalarStorageKind,
    ) -> String {
        if from == to {
            return expr;
        }
        match (from, to) {
            (ScalarStorageKind::Word, kind) => self.scalar_from_word_expr(expr, kind),
            (kind, ScalarStorageKind::Word) => self.scalar_to_word_expr(expr, kind),
            (ScalarStorageKind::I64, ScalarStorageKind::F64) => format!("({expr} as mmmfloat)"),
            (ScalarStorageKind::F64, ScalarStorageKind::I64) => format!("({expr} as i64)"),
            _ => expr,
        }
    }

    fn scalar_source_expr(
        &self,
        func: &Function,
        value: &VPtr,
    ) -> Result<(String, ScalarStorageKind), String> {
        match value.as_ref() {
            Value::Argument(index) => match self.value_storage(func, value)? {
                RegisterStorage::Scalar(kind) => Ok((self.argument_scalar_name(*index), kind)),
                RegisterStorage::Words(_) => Err(format!(
                    "argument {} is not representable as a scalar value",
                    index
                )),
            },
            Value::Register(reg) => match self
                .resolve_register_storage(func, *reg)
                .ok_or_else(|| format!("unknown register {}", reg))?
            {
                RegisterStorage::Scalar(kind) => Ok((format!("reg_{reg}"), kind)),
                RegisterStorage::Words(_) => Err(format!(
                    "register {} is not representable as a scalar value",
                    reg
                )),
            },
            Value::Function(index) => {
                Ok((format!("encode_function({index})"), ScalarStorageKind::Word))
            }
            Value::None => Ok(("0u64".to_string(), ScalarStorageKind::Word)),
            _ => Err(format!(
                "value is not representable as a scalar in Rust codegen: {:?}",
                value
            )),
        }
    }

    fn scalar_expr_as(
        &self,
        func: &Function,
        value: &VPtr,
        kind: ScalarStorageKind,
    ) -> Result<String, String> {
        let (expr, actual_kind) = self.scalar_source_expr(func, value)?;
        let needs_deref =
            matches!(value.as_ref(), Value::Register(r) if self.is_pointer_register(func, *r));
        let effective = if needs_deref && kind != ScalarStorageKind::Word {
            self.convert_scalar_expr(
                format!("memory.load_word({expr}).unwrap()"),
                ScalarStorageKind::Word,
                kind,
            )
        } else {
            self.convert_scalar_expr(expr, actual_kind, kind)
        };
        Ok(effective)
    }

    fn runtime_scalar_expr_from_word_expr(
        &self,
        word_expr: String,
        ty: TypeNodeId,
    ) -> Result<String, String> {
        let kind = self
            .scalar_storage_kind_for_type(ty)
            .unwrap_or(ScalarStorageKind::Word);
        Ok(self.scalar_from_word_expr(word_expr, kind))
    }

    fn scalar_truthy_expr(&self, func: &Function, value: &VPtr) -> Result<String, String> {
        let (expr, kind) = self.scalar_source_expr(func, value)?;
        Ok(match kind {
            ScalarStorageKind::F64 => format!("({expr} > 0.0)"),
            ScalarStorageKind::I64 => format!("({expr} != 0)"),
            ScalarStorageKind::Word => format!("truthy({expr})"),
        })
    }

    fn scalar_word_expr(&self, func: &Function, value: &VPtr) -> Result<String, String> {
        self.scalar_expr_as(func, value, ScalarStorageKind::Word)
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
            self.value_vec_expr(func, value)
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
            self.value_array_expr(func, value)
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
                    let ptr_expr = self.word0_expr(func, value)?;
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
            self.value_slice_expr(func, value)
        }
    }

    fn assign_scalar<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        dst: &VPtr,
        expr: String,
    ) -> Result<(), String> {
        let dest = self.reg_name(dst)?;
        writer.line(format!("{dest} = {expr};"))
    }

    fn assign_from_word_expr<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        word_expr: String,
    ) -> Result<(), String> {
        let dest_storage = self.value_storage(func, dst)?;
        match dest_storage {
            RegisterStorage::Scalar(kind) => {
                self.assign_scalar(writer, dst, self.scalar_from_word_expr(word_expr, kind))
            }
            RegisterStorage::Words(_) => self.assign_scalar(writer, dst, word_expr),
        }
    }

    fn assign_runtime_scalar_from_word_expr<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        word_expr: String,
    ) -> Result<(), String> {
        match self.value_storage(func, dst)? {
            RegisterStorage::Scalar(kind) => {
                self.assign_scalar(writer, dst, self.scalar_from_word_expr(word_expr, kind))
            }
            RegisterStorage::Words(_) => self.assign_scalar(writer, dst, word_expr),
        }
    }

    fn assign_runtime_scalar_from_abi_value<W: Write>(
        &self,
        writer: &mut CodeWriter<W>,
        func: &Function,
        dst: &VPtr,
        abi_expr: String,
        ty: TypeNodeId,
    ) -> Result<(), String> {
        match self.value_storage(func, dst)? {
            RegisterStorage::Scalar(target_kind) => {
                let abi_kind = self
                    .scalar_storage_kind_for_type(ty)
                    .unwrap_or(ScalarStorageKind::Word);
                let expr = self.convert_scalar_expr(abi_expr, abi_kind, target_kind);
                self.assign_scalar(writer, dst, expr)
            }
            RegisterStorage::Words(_) => self.assign_scalar(writer, dst, abi_expr),
        }
    }

    fn reg_name(&self, value: &VPtr) -> Result<String, String> {
        match value.as_ref() {
            Value::Register(reg) => Ok(format!("reg_{reg}")),
            _ => Err(format!("destination is not a register: {value:?}")),
        }
    }

    fn word0_expr(&self, func: &Function, value: &VPtr) -> Result<String, String> {
        match value.as_ref() {
            Value::Argument(index) => match self.value_storage(func, value)? {
                RegisterStorage::Scalar(_) => {
                    self.scalar_expr_as(func, value, ScalarStorageKind::Word)
                }
                RegisterStorage::Words(_) => Ok(format!("arg_{index}[0]")),
            },
            Value::Register(reg) => match self.resolve_register_storage(func, *reg) {
                Some(RegisterStorage::Scalar(_)) => {
                    self.scalar_expr_as(func, value, ScalarStorageKind::Word)
                }
                Some(RegisterStorage::Words(_)) => Ok(format!("reg_{reg}[0]")),
                None => Err(format!("unknown register {}", reg)),
            },
            Value::Function(index) => Ok(format!("encode_function({index})")),
            Value::None => Ok("0u64".to_string()),
            _ => Err(format!(
                "value is not representable as a single word in the initial Rust backend: {:?}",
                value
            )),
        }
    }

    fn value_vec_expr(&self, func: &Function, value: &VPtr) -> Result<String, String> {
        if matches!(self.value_storage(func, value)?, RegisterStorage::Scalar(_)) {
            return Ok(format!("vec![{}]", self.word0_expr(func, value)?));
        }
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

    fn value_array_expr(&self, func: &Function, value: &VPtr) -> Result<String, String> {
        if matches!(self.value_storage(func, value)?, RegisterStorage::Scalar(_)) {
            return Ok(format!("[{}]", self.word0_expr(func, value)?));
        }
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

    fn value_slice_expr(&self, func: &Function, value: &VPtr) -> Result<String, String> {
        if matches!(self.value_storage(func, value)?, RegisterStorage::Scalar(_)) {
            return Ok(format!("&[{}]", self.word0_expr(func, value)?));
        }
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

    /// Returns true if `reg` was defined by a `GetState` instruction.
    fn is_getstate_register(&self, func: &Function, reg: VReg) -> bool {
        func.body
            .iter()
            .flat_map(|block| block.0.iter())
            .any(|(dst, instr)| {
                matches!((dst.as_ref(), instr),
                (Value::Register(r), Instruction::GetState(_)) if *r == reg)
            })
    }

    /// If `reg` was defined by `Load(getstate_reg, deepcopy-aggregate-ty)`, return the state size.
    /// Used to bypass the heap roundtrip and read state directly via `get_state_slice`.
    fn find_state_load_source(&self, func: &Function, reg: VReg) -> Option<usize> {
        func.body
            .iter()
            .flat_map(|block| block.0.iter())
            .find_map(|(dst, instr)| match (dst.as_ref(), instr) {
                (Value::Register(r), Instruction::Load(ptr, ty))
                    if *r == reg && self.is_deepcopy_aggregate_type(*ty) =>
                {
                    match ptr.as_ref() {
                        Value::Register(ptr_reg) if self.is_getstate_register(func, *ptr_reg) => {
                            Some(ty.word_size() as usize)
                        }
                        _ => None,
                    }
                }
                _ => None,
            })
    }

    /// If `reg` was defined by `Load(arg(N), deepcopy-aggregate-ty)`, return Some(N).
    /// Used to avoid roundtripping through the heap when the source is a direct argument load.
    fn find_argument_load_source(&self, func: &Function, reg: VReg) -> Option<usize> {
        func.body
            .iter()
            .flat_map(|block| block.0.iter())
            .find_map(|(dst, instr)| match (dst.as_ref(), instr) {
                (Value::Register(r), Instruction::Load(ptr, ty))
                    if *r == reg && self.is_deepcopy_aggregate_type(*ty) =>
                {
                    match ptr.as_ref() {
                        Value::Argument(idx) => Some(*idx),
                        _ => None,
                    }
                }
                _ => None,
            })
    }

    /// If `reg` is the destination of a Call/CallIndirect returning a deepcopy aggregate type,
    /// return the Rust variable name holding the stack-local result buffer (`call_result_N`).
    /// Used to bypass the heap memory handle when accessing call results as slices.
    fn find_call_result_var(&self, func: &Function, reg: VReg) -> Option<String> {
        func.body
            .iter()
            .flat_map(|block| block.0.iter())
            .find_map(|(dst, instr)| {
                let ret_ty = match instr {
                    Instruction::Call(_, _, ret_ty) | Instruction::CallIndirect(_, _, ret_ty) => {
                        *ret_ty
                    }
                    _ => return None,
                };
                match dst.as_ref() {
                    Value::Register(r) if *r == reg && self.uses_direct_return_pointer(ret_ty) => {
                        Some(format!("call_result_{r}"))
                    }
                    _ => None,
                }
            })
    }

    fn boundary_value_slice_expr(
        &self,
        func: &Function,
        stack_pointers: &HashMap<VReg, StackPointer>,
        value: &VPtr,
        ty: TypeNodeId,
    ) -> Result<String, String> {
        if !self.is_deepcopy_aggregate_type(ty) {
            return self.context_value_slice_expr(func, value, ty);
        }
        match value.as_ref() {
            Value::Argument(index) => Ok(format!("&arg_{index}")),
            Value::Register(reg) => {
                if let Some(pointer) = stack_pointers.get(reg) {
                    return Ok(self.stack_pointer_slice_expr(*pointer, ty.word_size() as usize));
                }
                if let Some(arg_idx) = self.find_argument_load_source(func, *reg) {
                    return Ok(format!("&arg_{arg_idx}"));
                }
                if let Some(size) = self.find_state_load_source(func, *reg) {
                    return Ok(format!(
                        "{{ let state = self.get_current_statestorage(); state.get_state_slice({size}usize) }}"
                    ));
                }
                if let Some(cr_var) = self.find_call_result_var(func, *reg) {
                    return Ok(format!("&{cr_var}"));
                }
                let ptr_expr = self.word0_expr(func, value)?;
                Ok(format!(
                    "&memory.load({ptr_expr}, {}usize)?",
                    ty.word_size()
                ))
            }
            _ => {
                let ptr_expr = self.word0_expr(func, value)?;
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
        func: &Function,
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
                let src_handle = self.word0_expr(func, src)?;
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
