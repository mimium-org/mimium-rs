use std::collections::LinkedList;

use crate::interner::Symbol;

type EnvInner<T> = LinkedList<Vec<(Symbol, T)>>;

/// Environment stack used during compilation.
///
/// Each scope is represented by a vector of bindings and the whole stack is
/// maintained as a `LinkedList`.  The top of the stack is the innermost scope.
#[derive(Clone, Debug, PartialEq)]
pub struct Environment<T>(pub EnvInner<T>);

/// Result of name lookup on [`Environment`].
#[derive(Clone, Debug, PartialEq)]
pub enum LookupRes<T: Clone> {
    /// Binding exists in the current scope.
    Local(T),
    /// Binding is found in an outer scope. `usize` indicates how many scopes
    /// above the current one the binding resides in.
    UpValue(usize, T),
    /// Binding is located in the global scope.
    Global(T),
    /// Binding was not found.
    None,
}
impl<T: Clone> Default for Environment<T> {
    fn default() -> Self {
        Self(EnvInner::new())
    }
}
impl<T: Clone> Environment<T> {
    /// Create a new empty environment stack.
    pub fn new() -> Self {
        Self(EnvInner::new())
    }

    /// Returns `true` if the current scope is the outermost global scope.
    pub fn is_global(&self) -> bool {
        self.0.len() <= 1
    }

    /// Push a new empty scope on top of the stack.
    pub fn extend(&mut self) {
        self.0.push_front(Vec::new());
    }

    /// Pop the current scope.
    pub fn to_outer(&mut self) {
        let _ = self.0.pop_front();
    }

    /// Add new bindings to the current scope.
    pub fn add_bind(&mut self, binds: &[(Symbol, T)]) {
        assert!(!self.0.is_empty());
        self.0.front_mut().unwrap().extend_from_slice(binds);
    }

    /// Perform a lookup with information about where the value was found.
    pub fn lookup_cls(&self, name: &Symbol) -> LookupRes<&T> {
        match self
            .0
            .iter()
            .enumerate()
            .find(|(_level, vec)| vec.iter().any(|(n, _)| n == name))
            .and_then(|(level, vec)| {
                vec.iter()
                    .rfind(|(n, _)| n == name)
                    .map(|(_, v)| (level, v))
            }) {
            None => LookupRes::None,
            Some((level, e)) if level >= self.0.len() - 1 => LookupRes::Global(e),
            Some((0, e)) if self.0.len() <= 1 => LookupRes::Global(e),
            Some((0, e)) => LookupRes::Local(e),
            Some((level, e)) => LookupRes::UpValue(level, e),
        }
    }
    /// Look up `name` and return the associated value if it exists.
    ///
    /// This helper ignores where the binding was found in the environment and
    /// simply returns its value.
    pub fn lookup(&self, name: &Symbol) -> Option<&T> {
        match self.lookup_cls(name) {
            LookupRes::None => None,
            LookupRes::Global(e) | LookupRes::Local(e) | LookupRes::UpValue(_, e) => Some(e),
        }
    }
}
