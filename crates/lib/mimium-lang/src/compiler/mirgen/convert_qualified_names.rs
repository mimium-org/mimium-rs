//! AST transformation pass for resolving qualified names.
//!
//! This pass converts `Expr::QualifiedVar` to `Expr::Var` with mangled names,
//! and resolves `use` aliases and wildcard imports. It runs before type checking,
//! allowing subsequent compiler phases to work with simple variable references.
//!
//! The resolution uses a 2-pass approach:
//! 1. First pass: Collect all defined names from the AST (Let/LetRec bindings)
//! 2. Second pass: Transform the AST, resolving qualified names, aliases, and wildcards
//!
//! Error handling:
//! - Private member access: Reports an error but allows type checking to proceed
//! - Unresolved names: Kept as-is for the type checker to report

use std::collections::HashSet;
use std::path::PathBuf;

use thiserror::Error;

use crate::ast::Expr;
use crate::ast::program::{ModuleInfo, resolve_qualified_path};
use crate::interner::{ExprNodeId, Symbol, ToSymbol};
use crate::pattern::Pattern;
use crate::utils::error::ReportableError;
use crate::utils::metadata::Location;

/// Error types specific to qualified name resolution.
#[derive(Debug, Clone, Error)]
#[error("Private member access")]
pub enum Error {
    /// Attempted to access a private module member
    PrivateMemberAccess {
        module_path: Vec<Symbol>,
        member: Symbol,
        location: Location,
    },
}

impl ReportableError for Error {
    fn get_message(&self) -> String {
        match self {
            Error::PrivateMemberAccess {
                module_path,
                member,
                ..
            } => {
                let path_str = module_path
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                format!("Member \"{member}\" in module \"{path_str}\" is private")
            }
        }
    }

    fn get_labels(&self) -> Vec<(Location, String)> {
        match self {
            Error::PrivateMemberAccess { location, .. } => {
                vec![(location.clone(), "private member accessed here".to_string())]
            }
        }
    }
}

// ============================================================================
// Pass 1: Collect all defined names from the AST
// ============================================================================

/// Collect all names defined in the AST (from Let and LetRec bindings).
fn collect_defined_names(expr: ExprNodeId, names: &mut HashSet<Symbol>) {
    match expr.to_expr() {
        Expr::Let(typed_pat, body, then) => {
            // Collect names from the pattern
            collect_names_from_pattern(&typed_pat.pat, names);
            collect_defined_names(body, names);
            if let Some(t) = then {
                collect_defined_names(t, names);
            }
        }
        Expr::LetRec(typed_id, body, then) => {
            names.insert(typed_id.id);
            collect_defined_names(body, names);
            if let Some(t) = then {
                collect_defined_names(t, names);
            }
        }
        Expr::Lambda(params, _, body) => {
            for param in params {
                names.insert(param.id);
            }
            collect_defined_names(body, names);
        }
        Expr::Tuple(v) => {
            for e in v {
                collect_defined_names(e, names);
            }
        }
        Expr::Proj(e, _) => collect_defined_names(e, names),
        Expr::Apply(fun, args) => {
            collect_defined_names(fun, names);
            for arg in args {
                collect_defined_names(arg, names);
            }
        }
        Expr::BinOp(lhs, _, rhs) => {
            collect_defined_names(lhs, names);
            collect_defined_names(rhs, names);
        }
        Expr::UniOp(_, e) => collect_defined_names(e, names),
        Expr::MacroExpand(fun, args) => {
            collect_defined_names(fun, names);
            for arg in args {
                collect_defined_names(arg, names);
            }
        }
        Expr::If(cond, then, opt_else) => {
            collect_defined_names(cond, names);
            collect_defined_names(then, names);
            if let Some(e) = opt_else {
                collect_defined_names(e, names);
            }
        }
        Expr::Block(body) => {
            if let Some(b) = body {
                collect_defined_names(b, names);
            }
        }
        Expr::Escape(e) | Expr::Bracket(e) | Expr::Paren(e) | Expr::Feed(_, e) => {
            collect_defined_names(e, names);
        }
        Expr::FieldAccess(record, _) => collect_defined_names(record, names),
        Expr::ArrayAccess(array, index) => {
            collect_defined_names(array, names);
            collect_defined_names(index, names);
        }
        Expr::ArrayLiteral(elems) => {
            for e in elems {
                collect_defined_names(e, names);
            }
        }
        Expr::RecordLiteral(fields) | Expr::ImcompleteRecord(fields) => {
            for f in fields {
                collect_defined_names(f.expr, names);
            }
        }
        Expr::RecordUpdate(record, fields) => {
            collect_defined_names(record, names);
            for f in fields {
                collect_defined_names(f.expr, names);
            }
        }
        Expr::Assign(target, value) => {
            collect_defined_names(target, names);
            collect_defined_names(value, names);
        }
        Expr::Then(e, then) => {
            collect_defined_names(e, names);
            if let Some(t) = then {
                collect_defined_names(t, names);
            }
        }
        // Leaf nodes - no names to collect
        Expr::Var(_) | Expr::QualifiedVar(_) | Expr::Literal(_) | Expr::Error => {}
    }
}

/// Extract all names bound by a pattern.
fn collect_names_from_pattern(pat: &Pattern, names: &mut HashSet<Symbol>) {
    match pat {
        Pattern::Single(name) => {
            names.insert(*name);
        }
        Pattern::Tuple(pats) => {
            for p in pats {
                collect_names_from_pattern(p, names);
            }
        }
        Pattern::Record(fields) => {
            for (_, p) in fields {
                collect_names_from_pattern(p, names);
            }
        }
        Pattern::Placeholder | Pattern::Error => {}
    }
}

// ============================================================================
// Pass 2: Transform the AST
// ============================================================================

/// Context for qualified name resolution.
struct ResolveContext<'a> {
    /// Module information (visibility, aliases, wildcards)
    module_info: &'a ModuleInfo,
    /// Set of all known names (from AST definitions + builtins)
    known_names: &'a HashSet<Symbol>,
    /// Current module context for relative path resolution
    current_module_context: Vec<Symbol>,
    /// File path for error reporting
    file_path: PathBuf,
    /// Accumulated errors
    errors: Vec<Error>,
}

impl<'a> ResolveContext<'a> {
    fn new(
        module_info: &'a ModuleInfo,
        known_names: &'a HashSet<Symbol>,
        file_path: PathBuf,
    ) -> Self {
        Self {
            module_info,
            known_names,
            current_module_context: Vec::new(),
            file_path,
            errors: Vec::new(),
        }
    }

    /// Check if a name exists in the known names set.
    fn name_exists(&self, name: &Symbol) -> bool {
        self.known_names.contains(name)
    }

    /// Try to resolve a simple variable name through wildcard imports.
    /// Returns the first matching mangled name if found.
    fn resolve_through_wildcards(&self, name: Symbol) -> Option<Symbol> {
        for base in &self.module_info.wildcard_imports {
            let mangled = if base.as_str().is_empty() {
                name
            } else {
                format!("{}${}", base.as_str(), name.as_str()).to_symbol()
            };

            if self.name_exists(&mangled) {
                // Check visibility - only allow public members through wildcards
                if let Some(&is_public) = self.module_info.visibility_map.get(&mangled) {
                    if is_public {
                        return Some(mangled);
                    }
                } else {
                    // If not in visibility map, treat as accessible
                    return Some(mangled);
                }
            }
        }
        None
    }

    /// Check if the resolved path is within the same module hierarchy as current context.
    fn is_within_module_hierarchy(&self, resolved_path: &[Symbol]) -> bool {
        if self.current_module_context.is_empty() || resolved_path.len() < 2 {
            return false;
        }
        let target_module = &resolved_path[..resolved_path.len() - 1];
        self.current_module_context.starts_with(target_module)
    }

    fn make_location(&self, e_id: ExprNodeId) -> Location {
        Location {
            span: e_id.to_span().clone(),
            path: self.file_path.clone(),
        }
    }
}

/// Convert qualified names in an AST, returning the transformed AST and any errors.
///
/// This function performs the following transformations:
/// 1. Converts `QualifiedVar` to `Var` with mangled names (e.g., `foo::bar` → `foo$bar`)
/// 2. Resolves explicit `use` aliases for simple `Var` references
/// 3. Resolves wildcard imports (`use foo::*`) by checking the collected environment
/// 4. Resolves relative paths within modules
/// 5. Reports errors for private member access (but allows type checking to proceed)
///
/// The `builtin_names` parameter should contain names from builtin functions and external functions.
pub fn convert_qualified_names(
    expr: ExprNodeId,
    module_info: &ModuleInfo,
    builtin_names: &[Symbol],
    file_path: PathBuf,
) -> (ExprNodeId, Vec<Error>) {
    // Pass 1: Collect all defined names from the AST
    let mut known_names: HashSet<Symbol> = builtin_names.iter().copied().collect();
    collect_defined_names(expr, &mut known_names);

    // Pass 2: Transform the AST
    let mut ctx = ResolveContext::new(module_info, &known_names, file_path);
    let result = convert_expr(&mut ctx, expr);
    (result, ctx.errors)
}

fn convert_expr(ctx: &mut ResolveContext, e_id: ExprNodeId) -> ExprNodeId {
    let loc = ctx.make_location(e_id);

    match e_id.to_expr().clone() {
        Expr::Var(name) => convert_var(ctx, name, loc),
        Expr::QualifiedVar(path) => convert_qualified_var(ctx, &path.segments, loc),

        // Update module context when entering a LetRec (function definition)
        Expr::LetRec(id, body, then) => {
            let name = id.id;
            // Save current context
            let prev_context = std::mem::take(&mut ctx.current_module_context);

            // Update context if this function has a module context
            if let Some(new_context) = ctx.module_info.module_context_map.get(&name) {
                ctx.current_module_context = new_context.clone();
            }

            let new_body = convert_expr(ctx, body);

            // Restore context
            ctx.current_module_context = prev_context;

            let new_then = then.map(|t| convert_expr(ctx, t));
            Expr::LetRec(id, new_body, new_then).into_id(loc)
        }

        // Recursively process other expression types
        Expr::Tuple(v) => {
            let new_v: Vec<_> = v.into_iter().map(|e| convert_expr(ctx, e)).collect();
            Expr::Tuple(new_v).into_id(loc)
        }
        Expr::Proj(e, idx) => {
            let new_e = convert_expr(ctx, e);
            Expr::Proj(new_e, idx).into_id(loc)
        }
        Expr::Let(pat, body, then) => {
            let new_body = convert_expr(ctx, body);
            let new_then = then.map(|t| convert_expr(ctx, t));
            Expr::Let(pat, new_body, new_then).into_id(loc)
        }
        Expr::Lambda(params, r_type, body) => {
            let new_body = convert_expr(ctx, body);
            Expr::Lambda(params, r_type, new_body).into_id(loc)
        }
        Expr::Apply(fun, args) => {
            let new_fun = convert_expr(ctx, fun);
            let new_args: Vec<_> = args.into_iter().map(|e| convert_expr(ctx, e)).collect();
            Expr::Apply(new_fun, new_args).into_id(loc)
        }
        Expr::BinOp(lhs, op, rhs) => {
            let new_lhs = convert_expr(ctx, lhs);
            let new_rhs = convert_expr(ctx, rhs);
            Expr::BinOp(new_lhs, op, new_rhs).into_id(loc)
        }
        Expr::UniOp(op, expr) => {
            let new_expr = convert_expr(ctx, expr);
            Expr::UniOp(op, new_expr).into_id(loc)
        }
        Expr::MacroExpand(fun, args) => {
            let new_fun = convert_expr(ctx, fun);
            let new_args: Vec<_> = args.into_iter().map(|e| convert_expr(ctx, e)).collect();
            Expr::MacroExpand(new_fun, new_args).into_id(loc)
        }
        Expr::If(cond, then, opt_else) => {
            let new_cond = convert_expr(ctx, cond);
            let new_then = convert_expr(ctx, then);
            let new_else = opt_else.map(|e| convert_expr(ctx, e));
            Expr::If(new_cond, new_then, new_else).into_id(loc)
        }
        Expr::Block(body) => {
            let new_body = body.map(|e| convert_expr(ctx, e));
            Expr::Block(new_body).into_id(loc)
        }
        Expr::Escape(e) => {
            let new_e = convert_expr(ctx, e);
            Expr::Escape(new_e).into_id(loc)
        }
        Expr::Bracket(e) => {
            let new_e = convert_expr(ctx, e);
            Expr::Bracket(new_e).into_id(loc)
        }
        Expr::FieldAccess(record, field) => {
            let new_record = convert_expr(ctx, record);
            Expr::FieldAccess(new_record, field).into_id(loc)
        }
        Expr::ArrayAccess(array, index) => {
            let new_array = convert_expr(ctx, array);
            let new_index = convert_expr(ctx, index);
            Expr::ArrayAccess(new_array, new_index).into_id(loc)
        }
        Expr::ArrayLiteral(elems) => {
            let new_elems: Vec<_> = elems.into_iter().map(|e| convert_expr(ctx, e)).collect();
            Expr::ArrayLiteral(new_elems).into_id(loc)
        }
        Expr::RecordLiteral(fields) => {
            let new_fields = fields
                .into_iter()
                .map(|f| crate::ast::RecordField {
                    name: f.name,
                    expr: convert_expr(ctx, f.expr),
                })
                .collect();
            Expr::RecordLiteral(new_fields).into_id(loc)
        }
        Expr::ImcompleteRecord(fields) => {
            let new_fields = fields
                .into_iter()
                .map(|f| crate::ast::RecordField {
                    name: f.name,
                    expr: convert_expr(ctx, f.expr),
                })
                .collect();
            Expr::ImcompleteRecord(new_fields).into_id(loc)
        }
        Expr::RecordUpdate(record, fields) => {
            let new_record = convert_expr(ctx, record);
            let new_fields = fields
                .into_iter()
                .map(|f| crate::ast::RecordField {
                    name: f.name,
                    expr: convert_expr(ctx, f.expr),
                })
                .collect();
            Expr::RecordUpdate(new_record, new_fields).into_id(loc)
        }
        Expr::Assign(target, value) => {
            let new_target = convert_expr(ctx, target);
            let new_value = convert_expr(ctx, value);
            Expr::Assign(new_target, new_value).into_id(loc)
        }
        Expr::Then(e, then) => {
            let new_e = convert_expr(ctx, e);
            let new_then = then.map(|t| convert_expr(ctx, t));
            Expr::Then(new_e, new_then).into_id(loc)
        }
        Expr::Feed(sym, e) => {
            let new_e = convert_expr(ctx, e);
            Expr::Feed(sym, new_e).into_id(loc)
        }
        Expr::Paren(e) => {
            let new_e = convert_expr(ctx, e);
            Expr::Paren(new_e).into_id(loc)
        }

        // Leaf nodes that don't need transformation
        Expr::Literal(_) | Expr::Error => e_id,
    }
}

/// Convert a simple variable reference, resolving explicit `use` aliases and wildcards.
fn convert_var(ctx: &mut ResolveContext, name: Symbol, loc: Location) -> ExprNodeId {
    // Check if this is a use alias (explicit `use foo::bar` or `use foo::bar as alias`)
    if let Some(&mangled_name) = ctx.module_info.use_alias_map.get(&name) {
        // Check visibility
        if let Some(&is_public) = ctx.module_info.visibility_map.get(&mangled_name)
            && !is_public
            && !ctx.is_within_module_hierarchy(&extract_path_from_mangled(mangled_name))
        {
            let parts: Vec<&str> = mangled_name.as_str().split('$').collect();
            let module_path: Vec<Symbol> = parts[..parts.len() - 1]
                .iter()
                .map(|s| s.to_symbol())
                .collect();
            let member = parts.last().unwrap().to_symbol();
            ctx.errors.push(Error::PrivateMemberAccess {
                module_path,
                member,
                location: loc.clone(),
            });
            // Continue with the resolved name despite the error
        }
        return Expr::Var(mangled_name).into_id(loc);
    }

    // Try wildcard resolution
    if let Some(mangled) = ctx.resolve_through_wildcards(name) {
        return Expr::Var(mangled).into_id(loc);
    }

    // Keep as-is - will be resolved by type checker (local variable or error)
    Expr::Var(name).into_id(loc)
}

/// Convert a qualified variable reference (e.g., `module::func`).
fn convert_qualified_var(
    ctx: &mut ResolveContext,
    segments: &[Symbol],
    loc: Location,
) -> ExprNodeId {
    // Build mangled name for absolute path
    let mangled_name = if segments.len() == 1 {
        segments[0]
    } else {
        segments
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join("$")
            .to_symbol()
    };

    // Try to resolve the path with relative fallback
    let (resolved_name, resolved_path) = resolve_qualified_path(
        segments,
        mangled_name,
        &ctx.current_module_context,
        |name| ctx.name_exists(name),
    );

    // Check if it's a re-exported alias
    let lookup_name = ctx
        .module_info
        .use_alias_map
        .get(&resolved_name)
        .copied()
        .unwrap_or(resolved_name);

    // Check visibility for module members
    if resolved_path.len() > 1
        && let Some(&is_public) = ctx.module_info.visibility_map.get(&resolved_name)
    {
        let is_same_module = ctx.is_within_module_hierarchy(&resolved_path);
        if !is_public && !is_same_module {
            ctx.errors.push(Error::PrivateMemberAccess {
                module_path: resolved_path[..resolved_path.len() - 1].to_vec(),
                member: *resolved_path.last().unwrap(),
                location: loc.clone(),
            });
            // Continue with the resolved name despite the error
        }
    }

    Expr::Var(lookup_name).into_id(loc)
}

/// Extract path segments from a mangled name (e.g., "foo$bar$baz" -> ["foo", "bar", "baz"])
fn extract_path_from_mangled(mangled: Symbol) -> Vec<Symbol> {
    mangled.as_str().split('$').map(|s| s.to_symbol()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::program::QualifiedPath;
    use crate::pattern::TypedPattern;
    use crate::types::Type;

    fn make_loc() -> Location {
        Location {
            span: 0..1,
            path: PathBuf::from("/test"),
        }
    }

    #[test]
    fn test_qualified_var_to_mangled() {
        let loc = make_loc();
        let mut module_info = ModuleInfo::default();
        module_info
            .visibility_map
            .insert("foo$bar".to_symbol(), true);

        let expr = Expr::QualifiedVar(QualifiedPath {
            segments: vec!["foo".to_symbol(), "bar".to_symbol()],
        })
        .into_id(loc.clone());

        // Add foo$bar to builtin_names so it's recognized
        let builtin_names = vec!["foo$bar".to_symbol()];
        let (result, errors) =
            convert_qualified_names(expr, &module_info, &builtin_names, PathBuf::from("/test"));

        assert!(errors.is_empty());
        assert!(matches!(result.to_expr(), Expr::Var(name) if name.as_str() == "foo$bar"));
    }

    #[test]
    fn test_private_member_access_reports_error_but_continues() {
        let loc = make_loc();
        let mut module_info = ModuleInfo::default();
        module_info
            .visibility_map
            .insert("foo$secret".to_symbol(), false);

        let expr = Expr::QualifiedVar(QualifiedPath {
            segments: vec!["foo".to_symbol(), "secret".to_symbol()],
        })
        .into_id(loc.clone());

        let builtin_names = vec!["foo$secret".to_symbol()];
        let (result, errors) =
            convert_qualified_names(expr, &module_info, &builtin_names, PathBuf::from("/test"));

        // Error should be reported
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], Error::PrivateMemberAccess { .. }));
        // But the expression should still be converted
        assert!(matches!(result.to_expr(), Expr::Var(name) if name.as_str() == "foo$secret"));
    }

    #[test]
    fn test_use_alias_resolution() {
        let loc = make_loc();
        let mut module_info = ModuleInfo::default();
        module_info
            .use_alias_map
            .insert("func".to_symbol(), "module$func".to_symbol());
        module_info
            .visibility_map
            .insert("module$func".to_symbol(), true);

        let expr = Expr::Var("func".to_symbol()).into_id(loc.clone());

        let builtin_names = vec!["module$func".to_symbol()];
        let (result, errors) =
            convert_qualified_names(expr, &module_info, &builtin_names, PathBuf::from("/test"));

        assert!(errors.is_empty());
        assert!(matches!(result.to_expr(), Expr::Var(name) if name.as_str() == "module$func"));
    }

    #[test]
    fn test_wildcard_resolution() {
        let loc = make_loc();
        let mut module_info = ModuleInfo::default();
        module_info.wildcard_imports.push("mymod".to_symbol());
        module_info
            .visibility_map
            .insert("mymod$helper".to_symbol(), true);

        let expr = Expr::Var("helper".to_symbol()).into_id(loc.clone());

        // mymod$helper is in the "environment" (builtin_names here for test)
        let builtin_names = vec!["mymod$helper".to_symbol()];
        let (result, errors) =
            convert_qualified_names(expr, &module_info, &builtin_names, PathBuf::from("/test"));

        assert!(errors.is_empty());
        // Now the variable SHOULD be resolved through wildcard
        assert!(matches!(result.to_expr(), Expr::Var(name) if name.as_str() == "mymod$helper"));
    }

    #[test]
    fn test_wildcard_not_resolved_when_name_not_exists() {
        let loc = make_loc();
        let mut module_info = ModuleInfo::default();
        module_info.wildcard_imports.push("mymod".to_symbol());
        // mymod$helper is NOT in visibility_map and NOT in known_names

        let expr = Expr::Var("helper".to_symbol()).into_id(loc.clone());

        let builtin_names = vec![]; // Empty - no names known
        let (result, errors) =
            convert_qualified_names(expr, &module_info, &builtin_names, PathBuf::from("/test"));

        assert!(errors.is_empty());
        // The variable should NOT be resolved - remains as "helper"
        assert!(matches!(result.to_expr(), Expr::Var(name) if name.as_str() == "helper"));
    }

    #[test]
    fn test_simple_var_unchanged() {
        let loc = make_loc();
        let module_info = ModuleInfo::default();

        let expr = Expr::Var("local_var".to_symbol()).into_id(loc.clone());

        let builtin_names = vec![];
        let (result, errors) =
            convert_qualified_names(expr, &module_info, &builtin_names, PathBuf::from("/test"));

        assert!(errors.is_empty());
        assert!(matches!(result.to_expr(), Expr::Var(name) if name.as_str() == "local_var"));
    }

    #[test]
    fn test_names_collected_from_let() {
        // Test that names defined in Let are collected and can be used
        let loc = make_loc();
        let module_info = ModuleInfo::default();

        let unknownty = Type::Unknown.into_id_with_location(loc.clone());
        // let x = 1; x
        let expr = Expr::Let(
            TypedPattern {
                pat: Pattern::Single("x".to_symbol()),
                ty: unknownty,
                default_value: None,
            },
            Expr::Literal(crate::ast::Literal::Float("1.0".to_symbol())).into_id(loc.clone()),
            Some(Expr::Var("x".to_symbol()).into_id(loc.clone())),
        )
        .into_id(loc.clone());

        let builtin_names = vec![];
        let (result, errors) =
            convert_qualified_names(expr, &module_info, &builtin_names, PathBuf::from("/test"));

        assert!(errors.is_empty());
        // The structure should be preserved
        assert!(matches!(result.to_expr(), Expr::Let(..)));
    }
}
