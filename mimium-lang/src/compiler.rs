pub mod parser;
pub mod typing;
// pub mod hirgen;
pub mod bytecodegen;
mod intrinsics;
pub mod mirgen;

#[derive(Debug, Clone)]
pub enum ErrorKind {
    TypeMismatch(Type, Type),
    CircularType,
    IndexOutOfRange(u16, u16),
    IndexForNonTuple(Type),
    VariableNotFound(String),
    NonPrimitiveInFeed,
    NotApplicable, //need?
    Unknown,
}
#[derive(Debug, Clone)]
pub struct Error(pub ErrorKind, pub Span);

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::VariableNotFound(_) => {
                write!(f, "Variable Not Found.")
            }
            ErrorKind::TypeMismatch(expect, actual) => {
                write!(
                    f,
                    "Type Mismatch, expected {}, but the actual was {}.",
                    expect.to_string(),
                    actual.to_string()
                )
            }
            ErrorKind::IndexForNonTuple(t) => {
                write!(f, "Index access for non tuple-type {}.", t.to_string())
            }
            ErrorKind::IndexOutOfRange(r, a) => {
                write!(
                    f,
                    "Tuple index out of range, number of elements are {} but accessed with {}.",
                    r, a
                )
            }
            ErrorKind::NotApplicable => {
                write!(f, "Application to non-function type value.")
            }
            ErrorKind::CircularType => write!(f, "Circular loop of type definition"),
            ErrorKind::NonPrimitiveInFeed => write!(f, "Feed can take only non-funtion type."),
            ErrorKind::Unknown => write!(f, "unknwon error."),
        }
    }
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for Error {}

impl ReportableError for Error {
    fn get_labels(&self) -> Vec<(crate::utils::metadata::Location, String)> {
        todo!()
    }
}

use std::path::PathBuf;

use mirgen::recursecheck;

use crate::{
    ast_interpreter,
    interner::{ExprNodeId, Symbol, TypeNodeId},
    mir::Mir,
    runtime::vm,
    types::Type,
    utils::{error::ReportableError, metadata::Span},
};
pub fn emit_ast(
    src: &str,
    filepath: Option<Symbol>,
) -> Result<ExprNodeId, Vec<Box<dyn ReportableError>>> {
    let path = filepath.map(|sym| PathBuf::from(sym.to_string()));
    let (ast, errs) = parser::parse(src, path);
    if errs.is_empty() {
        let ast = parser::add_global_context(ast, filepath.unwrap_or_default());
        Ok(recursecheck::convert_recurse(
            ast,
            filepath.unwrap_or_default(),
        ))
    } else {
        Err(errs)
    }
}
#[derive(Clone, Copy)]
pub struct ExtFunTypeInfo {
    pub name: Symbol,
    pub ty: TypeNodeId,
}

pub struct Context {
    ext_fns: Vec<ExtFunTypeInfo>,
    file_path: Option<Symbol>,
}
impl Context {
    pub fn new(
        ext_fns: impl IntoIterator<Item = ExtFunTypeInfo>,
        file_path: Option<Symbol>,
    ) -> Self {
        Self {
            ext_fns: ext_fns.into_iter().collect(),

            file_path,
        }
    }
    fn get_ext_typeinfos(&self) -> Vec<(Symbol, TypeNodeId)> {
        self.ext_fns
            .clone()
            .into_iter()
            .map(|ExtFunTypeInfo { name, ty }| (name, ty))
            .collect()
    }
    pub fn emit_mir(&self, src: &str) -> (Mir, Vec<Box<dyn ReportableError>>) {
        let path = self.file_path.map(|sym| PathBuf::from(sym.to_string()));
        let (ast,mut parse_errs) = parser::parse(src, path);
        let ast =parser::add_global_context(ast, self.file_path.unwrap_or_default());
        let (mir,mut errs) = mirgen::compile(ast, &self.get_ext_typeinfos(), self.file_path);
        errs.append(&mut parse_errs);
        (mir,errs)
    }
    pub fn emit_bytecode(&self, src: &str) -> Result<vm::Program, Vec<Box<dyn ReportableError>>> {
        let (mir, err) = self.emit_mir(src);
        if err.is_empty() {
            Ok(bytecodegen::gen_bytecode(mir))
        } else {
            Err(err)
        }
    }
}

pub fn interpret_top(
    content: String,
    global_ctx: &mut ast_interpreter::Context,
) -> Result<ast_interpreter::Value, Vec<Box<dyn ReportableError>>> {
    let ast = emit_ast(&content, None)?;
    ast_interpreter::eval_ast(ast, global_ctx).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        vec![eb]
    })
}
