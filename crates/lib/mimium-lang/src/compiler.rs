pub mod bytecodegen;
pub(crate) mod intrinsics;
pub mod mirgen;
pub mod parser;
pub mod typing;
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
    use crate::{
        function,
        interner::ToSymbol,
        numeric,
        types::{PType, Type},
    };

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
    fn test_context() -> Context {
        let addfn = ExtFunTypeInfo::new(
            "add".to_symbol(),
            function!(vec![numeric!(), numeric!()], numeric!()),
            EvalStage::Persistent,
        );
        let extfns = [addfn];
        Context::new(extfns, [], None, Config::default())
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
    fn bytecode_channelcount() {
        let src = &get_source();
        let ctx = test_context();
        let prog = ctx.emit_bytecode(src).unwrap();
        let iochannels = prog.iochannels.unwrap();
        assert_eq!(iochannels.input, 1);
        assert_eq!(iochannels.output, 2);
    }
}
