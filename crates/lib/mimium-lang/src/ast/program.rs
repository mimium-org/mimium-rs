use std::collections::HashMap;
use std::path::PathBuf;

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
pub enum Visibility {
    Private,
    Public,
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Private
    }
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
    /// Module definition: mod name { ... }
    ModuleDefinition {
        visibility: Visibility,
        name: Symbol,
        body: Vec<(ProgramStatement, Span)>,
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

/// Map from mangled symbol name to whether it's public.
/// Only contains entries for module members (not top-level definitions).
pub type VisibilityMap = HashMap<Symbol, bool>;

fn stmts_from_program(
    program: Program,
    file_path: PathBuf,
    errs: &mut Vec<Box<dyn ReportableError>>,
    visibility_map: &mut VisibilityMap,
) -> Vec<(Statement, Location)> {
    stmts_from_program_with_prefix(program.statements, file_path, errs, &[], visibility_map)
}

fn stmts_from_program_with_prefix(
    statements: Vec<(ProgramStatement, Span)>,
    file_path: PathBuf,
    errs: &mut Vec<Box<dyn ReportableError>>,
    module_prefix: &[Symbol],
    visibility_map: &mut VisibilityMap,
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
                    visibility_map.insert(mangled_name, visibility == Visibility::Public);
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
                let res = stmts_from_program(imported_program, file_path.clone(), errs, visibility_map);
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
                let inner_stmts =
                    stmts_from_program_with_prefix(body, file_path.clone(), errs, &new_prefix, visibility_map);
                Some(inner_stmts)
            }
            ProgramStatement::UseStatement { .. } => {
                // Use statements are handled during qualified name resolution
                // They don't generate any statements directly
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
) -> (ExprNodeId, VisibilityMap, Vec<Box<dyn ReportableError>>) {
    let mut errs = vec![];
    let mut visibility_map = VisibilityMap::new();
    let stmts = stmts_from_program(program, file_path.clone(), &mut errs, &mut visibility_map);

    let res = into_then_expr(stmts.as_slice()).unwrap_or(Expr::Error.into_id_without_span());

    (res, visibility_map, errs)
}
