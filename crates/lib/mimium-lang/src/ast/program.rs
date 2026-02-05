use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::resolve_include::resolve_include;
use super::statement::Statement;
use crate::ast::Expr;
use crate::ast::statement::into_then_expr;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::TypedId;
use crate::types::{RecordTypeField, Type};
use crate::utils::error::ReportableError;
use crate::utils::metadata::{Location, Span};

use super::StageKind;

/// Visibility modifier for module members
#[derive(Clone, Debug, PartialEq, Default)]
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

/// Target of a use statement: single symbol, multiple symbols, or wildcard
#[derive(Clone, Debug, PartialEq)]
pub enum UseTarget {
    /// Single import: `use foo::bar`
    Single,
    /// Multiple imports: `use foo::{bar, baz}`
    Multiple(Vec<Symbol>),
    /// Wildcard import: `use foo::*`
    Wildcard,
}

/// A variant definition for user-defined sum types
#[derive(Clone, Debug, PartialEq)]
pub struct VariantDef {
    /// The name of the variant constructor
    pub name: Symbol,
    /// Optional payload type for the variant
    pub payload: Option<TypeNodeId>,
}

impl VariantDef {
    pub fn new(name: Symbol, payload: Option<TypeNodeId>) -> Self {
        Self { name, payload }
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
    /// Use statement: `use path::to::item`, `use path::{a, b}`, or `use path::*`
    /// Can be prefixed with `pub` for re-exporting
    UseStatement {
        /// Visibility of the re-export (pub use for re-exporting)
        visibility: Visibility,
        /// The base path (for `use foo::bar`, this is `[foo, bar]`; for `use foo::{a, b}`, this is `[foo]`)
        path: QualifiedPath,
        /// The import target type
        target: UseTarget,
    },
    /// Type declaration: type Name = Variant1 | Variant2 | ...
    TypeDeclaration {
        visibility: Visibility,
        name: Symbol,
        variants: Vec<VariantDef>,
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

/// Map from mangled function name to its module context (prefix).
/// Used for relative path resolution within modules.
pub type ModuleContextMap = HashMap<Symbol, Vec<Symbol>>;

/// Map from type name to its variant definitions.
pub type TypeDeclarationMap = HashMap<Symbol, Vec<VariantDef>>;

/// Module-related information collected during parsing.
/// Contains visibility information for module members and use aliases.
#[derive(Clone, Debug, Default)]
pub struct ModuleInfo {
    /// Map from mangled symbol name to whether it's public (only for module members)
    pub visibility_map: VisibilityMap,
    /// Map from alias name to mangled name (from use statements)
    pub use_alias_map: UseAliasMap,
    /// Map from mangled function name to its module context (for relative path resolution)
    pub module_context_map: ModuleContextMap,
    /// List of wildcard import base paths (e.g., `use foo::*` stores "foo")
    pub wildcard_imports: Vec<Symbol>,
    /// Map from type name to its variant definitions
    pub type_declarations: TypeDeclarationMap,
}

impl ModuleInfo {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Resolve a qualified path, trying relative resolution from current module context.
/// Returns `(mangled_name, resolved_path_segments)`.
///
/// For example, if current context is `[outer]` and path is `[inner, secret]`:
/// 1. First tries `inner$secret` (absolute path)
/// 2. If not found, tries `outer$inner$secret` (relative path from current module)
///
/// The `exists` closure should return `true` if the given mangled name exists in the environment.
pub fn resolve_qualified_path<F>(
    path_segments: &[Symbol],
    absolute_mangled: Symbol,
    current_module_context: &[Symbol],
    exists: F,
) -> (Symbol, Vec<Symbol>)
where
    F: Fn(&Symbol) -> bool,
{
    // First, try the absolute path
    if exists(&absolute_mangled) {
        return (absolute_mangled, path_segments.to_vec());
    }

    // If not found and we have a module context, try relative path
    if !current_module_context.is_empty() {
        // Build the relative path: context + path_segments
        let mut relative_path = current_module_context.to_vec();
        relative_path.extend(path_segments.iter().copied());

        let relative_mangled = relative_path
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join("$")
            .to_symbol();

        if exists(&relative_mangled) {
            return (relative_mangled, relative_path);
        }
    }

    // Return absolute path if relative resolution failed
    (absolute_mangled, path_segments.to_vec())
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
                    module_info
                        .visibility_map
                        .insert(mangled_name, visibility == Visibility::Public);
                    // Track module context for relative path resolution
                    module_info
                        .module_context_map
                        .insert(mangled_name, module_prefix.to_vec());
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
                let res =
                    stmts_from_program(imported_program, file_path.clone(), errs, module_info);
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

                let inner_stmts = match body {
                    Some(inline_body) => {
                        // Inline module: mod foo { ... }
                        stmts_from_program_with_prefix(
                            inline_body,
                            file_path.clone(),
                            errs,
                            &new_prefix,
                            module_info,
                        )
                    }
                    None => {
                        // External file module: mod foo;
                        resolve_external_module(
                            name,
                            &file_path,
                            span.clone(),
                            errs,
                            &new_prefix,
                            module_info,
                        )
                    }
                };

                // Wrap module contents with stage boundary:
                // - Start with implicit #stage(main)
                // - End with #stage(main) to restore default stage
                // This ensures stage declarations inside modules don't leak out
                let module_loc = Location::new(span, file_path.clone());
                let maindecl = (Statement::DeclareStage(StageKind::Main), module_loc.clone());
                let result = [vec![maindecl.clone()], inner_stmts, vec![maindecl]].concat();

                Some(result)
            }
            ProgramStatement::UseStatement {
                visibility,
                path,
                target,
            } => {
                process_use_statement(&visibility, &path, &target, module_prefix, module_info);
                None
            }
            ProgramStatement::TypeDeclaration {
                visibility: _,
                name,
                variants,
            } => {
                // Store type declaration for later use in type environment
                // For now, register as a type alias in module info
                let mangled_name = mangle_qualified_name(module_prefix, name);
                module_info.type_declarations.insert(mangled_name, variants);
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

/// Process a use statement, registering aliases in module_info.
/// - Single: `use foo::bar` → alias `bar` → `foo$bar`
/// - Multiple: `use foo::{a, b}` → alias `a` → `foo$a`, `b` → `foo$b`
/// - Wildcard: `use foo::*` → import all public members from foo
/// If visibility is Public, the imported names are re-exported as public.
fn process_use_statement(
    visibility: &Visibility,
    path: &QualifiedPath,
    target: &UseTarget,
    module_prefix: &[Symbol],
    module_info: &mut ModuleInfo,
) {
    // Helper to register an alias and optionally mark as public
    let mut register_alias = |alias_name: Symbol, mangled: Symbol| {
        module_info.use_alias_map.insert(alias_name, mangled);

        // If pub use, register the re-exported name as public in the current module
        if *visibility == Visibility::Public {
            let exported_name = mangle_qualified_name(module_prefix, alias_name);
            module_info.visibility_map.insert(exported_name, true);
            // Also add the mangled target as alias for the exported name
            module_info.use_alias_map.insert(exported_name, mangled);
        }
    };

    match target {
        UseTarget::Single => {
            // use foo::bar creates an alias: bar -> foo$bar
            if let Some(alias_name) = path.segments.last().copied() {
                let mangled = mangle_qualified_path(&path.segments);
                register_alias(alias_name, mangled);
            }
        }
        UseTarget::Multiple(names) => {
            // use foo::{bar, baz} creates:
            //   bar -> foo$bar
            //   baz -> foo$baz
            for name in names {
                let mut full_path = path.segments.clone();
                full_path.push(*name);
                let mangled = mangle_qualified_path(&full_path);
                register_alias(*name, mangled);
            }
        }
        UseTarget::Wildcard => {
            // use foo::* imports all public members from foo
            // We need to defer this until we know all the public members
            // For now, store the base path for later resolution
            let base_mangled = if path.segments.is_empty() {
                // use ::* at module level means import from parent (current prefix)
                module_prefix
                    .iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join("$")
            } else {
                mangle_qualified_path(&path.segments).as_str().to_string()
            };

            // Store wildcard import for later resolution
            // The key is the base path (e.g., "foo"), value is the prefix for resolving members
            module_info.wildcard_imports.push(base_mangled.to_symbol());

            // Note: For pub use foo::*, we can't know all exported names at this point.
            // Wildcard re-exports would require a second pass or runtime resolution.
            // For now, wildcard imports with pub are stored but individual re-exports
            // need to be resolved later when the symbols are actually accessed.
        }
    }
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
