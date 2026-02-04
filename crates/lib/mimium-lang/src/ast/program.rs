use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::resolve_include::resolve_include;
use super::statement::Statement;
use crate::ast::Expr;
use crate::ast::statement::into_then_expr;
use crate::interner::{ExprNodeId, Symbol, TypeNodeId};
use crate::pattern::TypedId;
use crate::types::{RecordTypeField, Type};
use crate::utils::error::ReportableError;
use crate::utils::metadata::{Location, Span};

use super::StageKind;

/// Visibility modifier for module members
#[derive(Clone, Debug, PartialEq)]
#[derive(Default)]
pub enum Visibility {
    #[default]
    Private,
    Public,
}


/// Qualified path for module references (e.g., modA::modB::func)
#[derive(Clone, Debug, PartialEq)]
pub struct QualifiedPath {
    pub segments: Vec<Symbol>,
}

impl QualifiedPath {
    pub fn new(segments: Vec<Symbol>) -> Self {
        Self { segments }
    }

    pub fn single(name: Symbol) -> Self {
        Self {
            segments: vec![name],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ProgramStatement {
    FnDefinition {
        visibility: Visibility,
        name: Symbol,
        args: (Vec<TypedId>, Location),
        return_type: Option<TypeNodeId>,
        body: ExprNodeId,
    },
    StageDeclaration {
        stage: StageKind,
    },
    GlobalStatement(Statement),
    Import(Symbol),
    /// Module definition: mod name { ... } (inline) or mod name; (external file)
    ModuleDefinition {
        visibility: Visibility,
        name: Symbol,
        /// Body of the module. None means external file module (mod foo;)
        body: Option<Vec<(ProgramStatement, Span)>>,
    },
    /// Use statement: use path::to::module
    UseStatement {
        path: QualifiedPath,
    },
    Comment(Symbol),
    DocComment(Symbol),
    Error,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Program {
    pub statements: Vec<(ProgramStatement, Span)>,
}

/// Convert a qualified path to a mangled symbol name.
/// For example, `foo::bar::baz` becomes `foo$bar$baz`.
fn mangle_qualified_name(prefix: &[Symbol], name: Symbol) -> Symbol {
    use crate::interner::ToSymbol;
    if prefix.is_empty() {
        name
    } else {
        let path_str = prefix
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join("$");
        format!("{}${}", path_str, name.as_str()).to_symbol()
    }
}

/// Convert a full qualified path (all segments) to a mangled symbol name.
/// For example, `[foo, bar, baz]` becomes `foo$bar$baz`.
fn mangle_qualified_path(segments: &[Symbol]) -> Symbol {
    use crate::interner::ToSymbol;
    segments
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<_>>()
        .join("$")
        .to_symbol()
}

/// Resolve an external file module (`mod foo;` syntax).
/// Looks for `{name}.mmm` in the same directory as the current file.
fn resolve_external_module(
    name: Symbol,
    file_path: &Path,
    span: Span,
    errs: &mut Vec<Box<dyn ReportableError>>,
    module_prefix: &[Symbol],
    module_info: &mut ModuleInfo,
) -> Vec<(Statement, Location)> {
    let module_filename = format!("{}.mmm", name.as_str());
    let (imported_program, mut new_errs) =
        resolve_include(file_path.to_str().unwrap(), &module_filename, span);
    errs.append(&mut new_errs);

    // Get the actual file path for the imported module
    let module_file_path = file_path
        .parent()
        .map(|p| p.join(&module_filename))
        .unwrap_or_else(|| PathBuf::from(&module_filename));

    // Process imported program with the module prefix
    stmts_from_program_with_prefix(
        imported_program.statements,
        module_file_path,
        errs,
        module_prefix,
        module_info,
    )
}

/// Map from mangled symbol name to whether it's public.
/// Only contains entries for module members (not top-level definitions).
pub type VisibilityMap = HashMap<Symbol, bool>;

/// Map from alias name to the mangled name it refers to.
/// Created from `use` statements, e.g., `use foo::bar` creates `bar -> foo$bar`.
pub type UseAliasMap = HashMap<Symbol, Symbol>;

/// Module-related information collected during parsing.
/// Contains visibility information for module members and use aliases.
#[derive(Clone, Debug, Default)]
pub struct ModuleInfo {
    /// Map from mangled symbol name to whether it's public (only for module members)
    pub visibility_map: VisibilityMap,
    /// Map from alias name to mangled name (from use statements)
    pub use_alias_map: UseAliasMap,
}

impl ModuleInfo {
    pub fn new() -> Self {
        Self::default()
    }
}

fn stmts_from_program(
    program: Program,
    file_path: PathBuf,
    errs: &mut Vec<Box<dyn ReportableError>>,
    module_info: &mut ModuleInfo,
) -> Vec<(Statement, Location)> {
    stmts_from_program_with_prefix(program.statements, file_path, errs, &[], module_info)
}

fn stmts_from_program_with_prefix(
    statements: Vec<(ProgramStatement, Span)>,
    file_path: PathBuf,
    errs: &mut Vec<Box<dyn ReportableError>>,
    module_prefix: &[Symbol],
    module_info: &mut ModuleInfo,
) -> Vec<(Statement, Location)> {
    statements
        .into_iter()
        .filter_map(|(stmt, span)| match stmt {
            ProgramStatement::FnDefinition {
                visibility,
                name,
                args,
                return_type,
                body,
            } => {
                let loc = Location::new(span, file_path.clone());
                let argloc = args.1.clone();
                let argsty = args
                    .clone()
                    .0
                    .into_iter()
                    .map(RecordTypeField::from)
                    .collect::<Vec<_>>();
                let fnty = Type::Function {
                    arg: Type::Record(argsty).into_id_with_location(argloc),
                    ret: return_type.unwrap_or(Type::Unknown.into_id_with_location(loc.clone())),
                }
                .into_id_with_location(loc.clone());
                // Use mangled name if inside a module
                let mangled_name = mangle_qualified_name(module_prefix, name);
                // Track visibility for module members
                if !module_prefix.is_empty() {
                    module_info.visibility_map.insert(mangled_name, visibility == Visibility::Public);
                }
                Some(vec![(
                    Statement::LetRec(
                        TypedId::new(mangled_name, fnty),
                        Expr::Lambda(args.0, return_type, body).into_id(loc.clone()),
                    ),
                    loc,
                )])
            }
            ProgramStatement::GlobalStatement(statement) => {
                Some(vec![(statement, Location::new(span, file_path.clone()))])
            }
            ProgramStatement::Comment(_) | ProgramStatement::DocComment(_) => None,
            ProgramStatement::Import(filename) => {
                let (imported_program, mut new_errs) =
                    resolve_include(file_path.to_str().unwrap(), filename.as_str(), span.clone());
                errs.append(&mut new_errs);
                let res = stmts_from_program(imported_program, file_path.clone(), errs, module_info);
                Some(res)
            }
            ProgramStatement::StageDeclaration { stage } => Some(vec![(
                Statement::DeclareStage(stage),
                Location::new(span, file_path.clone()),
            )]),
            ProgramStatement::ModuleDefinition {
                visibility: _,
                name,
                body,
            } => {
                // Flatten module contents with qualified names
                let mut new_prefix = module_prefix.to_vec();
                new_prefix.push(name);
                
                match body {
                    Some(inline_body) => {
                        // Inline module: mod foo { ... }
                        let inner_stmts =
                            stmts_from_program_with_prefix(inline_body, file_path.clone(), errs, &new_prefix, module_info);
                        Some(inner_stmts)
                    }
                    None => {
                        // External file module: mod foo;
                        let inner_stmts = resolve_external_module(
                            name,
                            &file_path,
                            span,
                            errs,
                            &new_prefix,
                            module_info,
                        );
                        Some(inner_stmts)
                    }
                }
            }
            ProgramStatement::UseStatement { path } => {
                // use foo::bar creates an alias: bar -> foo$bar
                // The last segment becomes the alias name
                if let Some(alias_name) = path.segments.last().copied() {
                    let mangled = mangle_qualified_path(&path.segments);
                    module_info.use_alias_map.insert(alias_name, mangled);
                }
                None
            }
            ProgramStatement::Error => Some(vec![(
                Statement::Error,
                Location::new(span, file_path.clone()),
            )]),
        })
        .flatten()
        .collect()
}

pub(crate) fn expr_from_program(
    program: Program,
    file_path: PathBuf,
) -> (ExprNodeId, ModuleInfo, Vec<Box<dyn ReportableError>>) {
    let mut errs = vec![];
    let mut module_info = ModuleInfo::new();
    let stmts = stmts_from_program(program, file_path.clone(), &mut errs, &mut module_info);

    let res = into_then_expr(stmts.as_slice()).unwrap_or(Expr::Error.into_id_without_span());

    (res, module_info, errs)
}
