//! Module environment for namespace resolution.
//!
//! This module provides the infrastructure for building and querying module
//! hierarchies. Modules provide namespace separation - they are not first-class
//! values, only containers for organizing code.

use std::collections::HashMap;

use crate::ast::program::{Program, ProgramStatement, QualifiedPath, Visibility};
use crate::interner::Symbol;
use crate::utils::metadata::Span;

/// Result of looking up a member in a module.
#[derive(Clone, Debug)]
pub enum ModuleLookupResult {
    /// Found a submodule at the given path.
    Module(Vec<Symbol>),
    /// Found a member (function/constant) with the given definition info.
    Member {
        /// Full path to the member.
        full_path: Vec<Symbol>,
        /// Whether the member is public.
        is_public: bool,
    },
}

/// Entry in a module namespace.
#[derive(Clone, Debug)]
pub enum ModuleItem {
    /// A submodule.
    SubModule {
        visibility: Visibility,
        inner: ModuleNamespace,
    },
    /// A member (function, constant, etc.).
    Member {
        visibility: Visibility,
        /// Original statement for later processing.
        statement: ProgramStatement,
        span: Span,
    },
}

/// A module namespace containing items.
#[derive(Clone, Debug, Default)]
pub struct ModuleNamespace {
    /// Items in this module, keyed by name.
    pub items: HashMap<Symbol, ModuleItem>,
}

impl ModuleNamespace {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    /// Look up a member by qualified path segments.
    /// Returns the item if found, along with the remaining path.
    pub fn lookup(&self, path: &[Symbol]) -> Option<&ModuleItem> {
        if path.is_empty() {
            return None;
        }

        let first = &path[0];
        let item = self.items.get(first)?;

        if path.len() == 1 {
            Some(item)
        } else {
            // Need to traverse into a submodule
            match item {
                ModuleItem::SubModule { inner, .. } => inner.lookup(&path[1..]),
                ModuleItem::Member { .. } => None, // Can't traverse into a member
            }
        }
    }

    /// Get all public members for completion suggestions.
    pub fn public_members(&self) -> Vec<Symbol> {
        self.items
            .iter()
            .filter_map(|(name, item)| {
                let is_public = match item {
                    ModuleItem::SubModule { visibility, .. } => *visibility == Visibility::Public,
                    ModuleItem::Member { visibility, .. } => *visibility == Visibility::Public,
                };
                if is_public {
                    Some(*name)
                } else {
                    None
                }
            })
            .collect()
    }
}

/// Module environment built from a parsed program.
#[derive(Clone, Debug, Default)]
pub struct ModuleEnv {
    /// Root namespace containing all top-level modules.
    pub root: ModuleNamespace,
    /// Use statements that bring items into scope.
    pub use_imports: Vec<QualifiedPath>,
}

impl ModuleEnv {
    pub fn new() -> Self {
        Self {
            root: ModuleNamespace::new(),
            use_imports: Vec::new(),
        }
    }

    /// Build module environment from a parsed program.
    pub fn from_program(program: &Program) -> Self {
        let mut env = Self::new();
        env.collect_from_statements(&program.statements);
        env
    }

    fn collect_from_statements(&mut self, statements: &[(ProgramStatement, Span)]) {
        for (stmt, span) in statements {
            match stmt {
                ProgramStatement::ModuleDefinition {
                    visibility,
                    name,
                    body,
                } => {
                    let inner = body
                        .as_ref()
                        .map(|b| Self::build_namespace(b))
                        .unwrap_or_else(ModuleNamespace::new);
                    self.root.items.insert(
                        *name,
                        ModuleItem::SubModule {
                            visibility: visibility.clone(),
                            inner,
                        },
                    );
                }
                ProgramStatement::UseStatement { path, .. } => {
                    self.use_imports.push(path.clone());
                }
                ProgramStatement::FnDefinition { visibility, name, .. } => {
                    self.root.items.insert(
                        *name,
                        ModuleItem::Member {
                            visibility: visibility.clone(),
                            statement: stmt.clone(),
                            span: span.clone(),
                        },
                    );
                }
                _ => {}
            }
        }
    }

    fn build_namespace(statements: &[(ProgramStatement, Span)]) -> ModuleNamespace {
        let mut namespace = ModuleNamespace::new();

        for (stmt, span) in statements {
            match stmt {
                ProgramStatement::ModuleDefinition {
                    visibility,
                    name,
                    body,
                } => {
                    let inner = body
                        .as_ref()
                        .map(|b| Self::build_namespace(b))
                        .unwrap_or_else(ModuleNamespace::new);
                    namespace.items.insert(
                        *name,
                        ModuleItem::SubModule {
                            visibility: visibility.clone(),
                            inner,
                        },
                    );
                }
                ProgramStatement::FnDefinition { visibility, name, .. } => {
                    namespace.items.insert(
                        *name,
                        ModuleItem::Member {
                            visibility: visibility.clone(),
                            statement: stmt.clone(),
                            span: span.clone(),
                        },
                    );
                }
                ProgramStatement::GlobalStatement(_) => {
                    // Global statements in modules will be processed later
                }
                ProgramStatement::UseStatement { .. } => {
                    // Use statements in modules are handled separately
                }
                _ => {}
            }
        }

        namespace
    }

    /// Look up a qualified path in the module environment.
    pub fn lookup_qualified(&self, path: &QualifiedPath) -> Option<&ModuleItem> {
        self.root.lookup(&path.segments)
    }

    /// Resolve a qualified path considering use imports.
    /// This checks if the path refers to something imported via `use`.
    pub fn resolve_with_imports(&self, path: &QualifiedPath) -> Option<&ModuleItem> {
        // First try direct lookup
        if let Some(item) = self.lookup_qualified(path) {
            return Some(item);
        }

        // Try to resolve through use imports
        for imported in &self.use_imports {
            // Check if the last segment of the import matches the first segment of our path
            if let Some(last_import) = imported.segments.last() {
                if path.segments.first() == Some(last_import) {
                    // Build a new path: imported_prefix + rest_of_path
                    let mut full_path = imported.segments[..imported.segments.len() - 1].to_vec();
                    full_path.extend_from_slice(&path.segments);
                    let full_qualified = QualifiedPath::new(full_path);
                    if let Some(item) = self.root.lookup(&full_qualified.segments) {
                        return Some(item);
                    }
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interner::ToSymbol;

    #[test]
    fn test_empty_module_env() {
        let env = ModuleEnv::new();
        let path = QualifiedPath::new(vec!["foo".to_symbol()]);
        assert!(env.lookup_qualified(&path).is_none());
    }
}
