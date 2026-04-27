pub mod bytecodegen;
pub(crate) mod intrinsics;
pub mod mirgen;
pub mod parser;
pub mod rustgen;
pub mod translate_staging;
pub mod typing;

#[cfg(not(target_arch = "wasm32"))]
pub mod wasmgen;

use serde::{Deserialize, Serialize};

use crate::plugin::{ExtFunTypeInfo, MacroFunction};
use thiserror::Error;

/// Stage information for multi-stage programming.
/// Moved from plugin.rs to be shared across compiler modules.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum EvalStage {
    /// Persistent stage - accessible from all stages (like builtins)
    Persistent,
    /// Specific stage number
    Stage(u8),
}

impl std::fmt::Display for EvalStage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalStage::Persistent => write!(f, "persistent"),
            EvalStage::Stage(n) => write!(f, "{n}"),
        }
    }
}

impl EvalStage {
    pub fn is_available_in_macro(&self) -> bool {
        matches!(self, EvalStage::Persistent | EvalStage::Stage(0))
    }
    pub fn is_available_in_vm(&self) -> bool {
        matches!(self, EvalStage::Persistent | EvalStage::Stage(1))
    }

    /// Format the stage for error messages
    pub fn format_for_error(&self) -> String {
        match self {
            EvalStage::Persistent => "persistent".to_string(),
            EvalStage::Stage(n) => n.to_string(),
        }
    }

    /// Increment the current stage for bracket expressions
    pub fn increment(self) -> EvalStage {
        match self {
            EvalStage::Persistent => EvalStage::Persistent, // Persistent stays persistent
            EvalStage::Stage(n) => EvalStage::Stage(n + 1),
        }
    }

    /// Decrement the current stage for escape expressions
    pub fn decrement(self) -> EvalStage {
        match self {
            EvalStage::Persistent => EvalStage::Persistent, // Persistent stays persistent
            EvalStage::Stage(n) => EvalStage::Stage(n.saturating_sub(1)),
        }
    }
}

#[derive(Debug, Clone, Error)]
pub enum ErrorKind {
    #[error("Type Mismatch, expected {0}, but the actual was {1}.")]
    TypeMismatch(Type, Type),
    #[error("Circular loop of type definition")]
    CircularType,
    #[error("Tuple index out of range, number of elements are {0} but accessed with {1}.")]
    IndexOutOfRange(u16, u16),
    #[error("Index access for non tuple-type {0}.")]
    IndexForNonTuple(Type),
    #[error("Variable Not Found.")]
    VariableNotFound(String),
    #[error("Feed can take only non-funtion type.")]
    NonPrimitiveInFeed,
    #[error("Application to non-function type value.")]
    NotApplicable,
    #[error("Array index out of bounds.")]
    IndexOutOfBounds,
    #[error("Type error in expression.")]
    TypeError,
    #[error("Unknown error.")]
    Unknown,
}

#[derive(Debug, Clone, Error)]
#[error("{0}")]
pub struct CompileError(pub ErrorKind, pub Span);

impl ReportableError for CompileError {
    fn get_labels(&self) -> Vec<(crate::utils::metadata::Location, String)> {
        todo!()
    }
}

use std::path::PathBuf;

use mirgen::recursecheck;

use crate::{
    interner::{ExprNodeId, Symbol, TypeNodeId},
    mir::Mir,
    runtime::vm,
    types::Type,
    utils::{error::ReportableError, metadata::Span},
};
pub fn emit_ast(
    src: &str,
    path: Option<PathBuf>,
) -> Result<ExprNodeId, Vec<Box<dyn ReportableError>>> {
    let (ast, _module_info, errs) = parser::parse_to_expr(src, path.clone());
    if errs.is_empty() {
        let ast = parser::add_global_context(ast, path.clone().unwrap_or_default());
        let (ast, _errs) =
            mirgen::convert_pronoun::convert_pronoun(ast, path.clone().unwrap_or_default());
        Ok(recursecheck::convert_recurse(
            ast,
            path.clone().unwrap_or_default(),
        ))
    } else {
        Err(errs)
    }
}
#[derive(Clone, Copy, Debug, Default)]
pub struct Config {
    pub self_eval_mode: bytecodegen::SelfEvalMode,
}

pub struct Context {
    ext_fns: Vec<ExtFunTypeInfo>,
    macros: Vec<Box<dyn MacroFunction>>,
    file_path: Option<PathBuf>,
    config: Config,
}
// Compiler implements Send because MacroFunction may modify system plugins but it will never conflict with VM execution
unsafe impl Send for Context {}

#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct IoChannelInfo {
    pub input: u32,
    pub output: u32,
}

impl Context {
    pub fn new(
        ext_fns: impl IntoIterator<Item = ExtFunTypeInfo>,
        macros: impl IntoIterator<Item = Box<dyn MacroFunction>>,
        file_path: Option<PathBuf>,
        config: Config,
    ) -> Self {
        Self {
            ext_fns: ext_fns.into_iter().collect(),
            macros: macros.into_iter().collect(),
            file_path,
            config,
        }
    }
    pub fn get_ext_typeinfos(&self) -> Vec<(Symbol, TypeNodeId)> {
        self.ext_fns
            .clone()
            .into_iter()
            .map(|ExtFunTypeInfo { name, ty, .. }| (name, ty))
            .chain(self.macros.iter().map(|m| (m.get_name(), m.get_type())))
            .collect()
    }
    pub fn emit_mir(&self, src: &str) -> Result<Mir, Vec<Box<dyn ReportableError>>> {
        let path = self.file_path.clone();
        let (ast, module_info, mut parse_errs) = parser::parse_to_expr(src, path);
        // let ast = parser::add_global_context(ast, self.file_path.unwrap_or_default());
        let mir = mirgen::compile_with_module_info(
            ast,
            self.get_ext_typeinfos().as_slice(),
            &self.macros,
            self.file_path.clone(),
            module_info,
        );
        if parse_errs.is_empty() {
            mir
        } else {
            let _ = mir.map_err(|mut e| {
                parse_errs.append(&mut e);
            });
            Err(parse_errs)
        }
    }
    pub fn emit_bytecode(&self, src: &str) -> Result<vm::Program, Vec<Box<dyn ReportableError>>> {
        let mir = self.emit_mir(src)?;
        let config = bytecodegen::Config {
            self_eval_mode: self.config.self_eval_mode,
        };
        Ok(bytecodegen::gen_bytecode(mir, config))
    }

    /// Compile source code to Rust source via MIR.
    pub fn emit_rust(&self, src: &str) -> Result<RustOutput, Vec<Box<dyn ReportableError>>> {
        let mir = self.emit_mir(src)?;
        let io_channels = mir.get_dsp_iochannels();
        let dsp_state_skeleton = mir.get_dsp_state_skeleton().cloned();
        let source = rustgen::RustGenerator::new(std::sync::Arc::new(mir), self.config)
            .generate()
            .map_err(|e| {
                vec![Box::new(crate::utils::error::SimpleError {
                    message: e,
                    span: crate::utils::metadata::Location::default(),
                }) as Box<dyn ReportableError>]
            })?;
        Ok(RustOutput {
            source,
            dsp_state_skeleton,
            io_channels,
        })
    }

    /// Compile source code to a WASM module.
    ///
    /// Returns the WASM binary bytes together with the DSP function's
    /// [`StateTreeSkeleton`] (used for state-preserving hot-swap) and the
    /// I/O channel configuration.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn emit_wasm(&self, src: &str) -> Result<WasmOutput, Vec<Box<dyn ReportableError>>> {
        let mir = self.emit_mir(src)?;
        let io_channels = mir.get_dsp_iochannels();
        let dsp_state_skeleton = mir.get_dsp_state_skeleton().cloned();
        let ext_fns: Vec<crate::plugin::ExtFunTypeInfo> = self.ext_fns.clone();
        let mut generator = wasmgen::WasmGenerator::new(std::sync::Arc::new(mir), &ext_fns);
        let bytes = generator.generate().map_err(|e| {
            vec![Box::new(crate::utils::error::SimpleError {
                message: e,
                span: crate::utils::metadata::Location::default(),
            }) as Box<dyn ReportableError>]
        })?;
        Ok(WasmOutput {
            bytes,
            dsp_state_skeleton,
            io_channels,
            ext_fns,
        })
    }
}

/// Output of WASM compilation via [`Context::emit_wasm`].
#[cfg(not(target_arch = "wasm32"))]
pub struct WasmOutput {
    /// The compiled WASM module binary.
    pub bytes: Vec<u8>,
    /// State tree skeleton of the DSP function (for state migration).
    pub dsp_state_skeleton: Option<state_tree::tree::StateTreeSkeleton<crate::mir::StateType>>,
    /// I/O channel info extracted from the DSP function signature.
    pub io_channels: Option<IoChannelInfo>,
    /// External function type infos required to instantiate the generated module.
    pub ext_fns: Vec<crate::plugin::ExtFunTypeInfo>,
}

/// Output of Rust source generation via [`Context::emit_rust`].
pub struct RustOutput {
    /// The generated Rust source.
    pub source: String,
    /// State tree skeleton of the DSP function.
    pub dsp_state_skeleton: Option<state_tree::tree::StateTreeSkeleton<crate::mir::StateType>>,
    /// I/O channel info extracted from the DSP function signature.
    pub io_channels: Option<IoChannelInfo>,
}

// pub fn interpret_top(
//     content: String,
//     global_ctx: &mut ast_interpreter::Context,
// ) -> Result<ast_interpreter::Value, Vec<Box<dyn ReportableError>>> {
//     let ast = emit_ast(&content, None)?;
//     ast_interpreter::eval_ast(ast, global_ctx).map_err(|e| {
//         let eb: Box<dyn ReportableError> = Box::new(e);
//         vec![eb]
//     })
// }

#[cfg(test)]
mod test {
    use std::fs;
    use std::path::PathBuf;
    use std::process::Command;
    use std::time::{SystemTime, UNIX_EPOCH};

    use crate::{function, interner::ToSymbol, numeric};

    use super::*;
    fn get_source() -> &'static str {
        //type annotation input:float is not necessary ideally
        // but we have to for now for because of subtyping issue
        r#"
fn counter(){
    self + 1
}
fn dsp(input:float){
    let res = input + counter()
    (0,res)
}
"#
    }

    fn get_tuple_source() -> &'static str {
        r#"
fn dsp(input:(float,float)){
    input
}
"#
    }

    fn test_context() -> Context {
        let addfn = ExtFunTypeInfo::new(
            "add".to_symbol(),
            function!(vec![numeric!(), numeric!()], numeric!()),
            EvalStage::Persistent,
        );
        let extfns = [addfn];
        Context::new(extfns, [], None, Config::default())
    }

    fn comprehensive_rust_source() -> &'static str {
        r#"
let arr = [10.0, 20.0, 40.0]

fn counter(){
    self + 1
}

fn remember(x){
    mem(x)
}

fn branch(x){
    if (x > 2.0) {
        x + arr[2]
    } else {
        x + arr[1]
    }
}

fn dsp(input:float){
    let step = counter()
    let mixed = input + step + arr[0]
    let prev = remember(mixed)
    branch(prev)
}
"#
    }

    fn rust_test_tmp_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../../tmp")
            .join("rustgen-tests")
    }
    #[test]
    fn mir_channelcount() {
        let src = &get_source();
        let ctx = test_context();
        let mir = ctx.emit_mir(src).unwrap();
        log::trace!("Mir: {mir}");
        let iochannels = mir.get_dsp_iochannels().unwrap();
        assert_eq!(iochannels.input, 1);
        assert_eq!(iochannels.output, 2);
    }

    #[test]
    fn mir_tuple_channelcount() {
        let ctx = test_context();
        let mir = ctx.emit_mir(get_tuple_source()).unwrap();
        let iochannels = mir.get_dsp_iochannels().unwrap();
        assert_eq!(iochannels.input, 2);
        assert_eq!(iochannels.output, 2);
    }

    #[test]
    fn bytecode_channelcount() {
        let src = &get_source();
        let ctx = test_context();
        let prog = ctx.emit_bytecode(src).unwrap();
        let iochannels = prog.iochannels.unwrap();
        assert_eq!(iochannels.input, 1);
        assert_eq!(iochannels.output, 2);
    }

    #[test]
    fn bytecode_tuple_channelcount() {
        let ctx = test_context();
        let prog = ctx.emit_bytecode(get_tuple_source()).unwrap();
        let iochannels = prog.iochannels.unwrap();
        assert_eq!(iochannels.input, 2);
        assert_eq!(iochannels.output, 2);
    }

    #[test]
    fn emit_rust_generates_program_scaffold() {
        let src = r#"
fn counter(){
    self + 1
}
fn dsp(input:float){
    input + counter()
}
"#;

        let output = test_context().emit_rust(src).unwrap();
        assert!(output.source.contains("pub struct MimiumProgram"));
        assert!(output.source.contains("pub fn call_dsp"));
        assert!(output.source.contains("fn func_"));
    }

    #[test]
    fn emit_rust_compiles_and_runs_generated_program() {
        let output = test_context()
            .emit_rust(comprehensive_rust_source())
            .unwrap();
        let maybe_call_main = if output.source.contains("pub fn call_main") {
            "    program.call_main().unwrap();\n"
        } else {
            ""
        };

        let harness = [
            "\nfn main() {\n",
            "    let mut program = MimiumProgram::new();\n",
            maybe_call_main,
            "    for input in [1.0f64, 2.0, 3.0, 4.0] {\n",
            "        let output = program.call_dsp(&[f64_to_word(input)]).unwrap();\n",
            "        for word in output {\n",
            "            println!(\"{:.12}\", word_to_f64(word));\n",
            "        }\n",
            "    }\n",
            "}\n",
        ]
        .concat();

        let tmp_dir = rust_test_tmp_dir();
        fs::create_dir_all(&tmp_dir).unwrap();
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let source_path = tmp_dir.join(format!("emit_rust_{stamp}.rs"));
        let binary_path = tmp_dir.join(format!("emit_rust_{stamp}"));
        fs::write(&source_path, format!("{}{harness}", output.source)).unwrap();

        let rustc = std::env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());
        let compile = Command::new(&rustc)
            .arg("--edition=2024")
            .arg(&source_path)
            .arg("-o")
            .arg(&binary_path)
            .output()
            .unwrap();
        assert!(
            compile.status.success(),
            "generated Rust failed to compile\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&compile.stdout),
            String::from_utf8_lossy(&compile.stderr)
        );

        let run = Command::new(&binary_path).output().unwrap();
        assert!(
            run.status.success(),
            "generated Rust binary failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&run.stdout),
            String::from_utf8_lossy(&run.stderr)
        );

        let actual = String::from_utf8(run.stdout)
            .unwrap()
            .lines()
            .map(|line| line.parse::<f64>().unwrap())
            .collect::<Vec<_>>();
        let expected = vec![20.0, 52.0, 54.0, 56.0];
        assert_eq!(actual, expected);
    }
}
