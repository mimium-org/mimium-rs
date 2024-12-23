use std::collections::LinkedList;

use crate::interner::Symbol;

type EnvInner<T> = LinkedList<Vec<(Symbol, T)>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment<T>(pub EnvInner<T>);

#[derive(Clone, Debug, PartialEq)]
pub enum LookupRes<T: Clone> {
    Local(T),
    UpValue(usize, T),
    Global(T),
    None,
}
impl<T: Clone> Default for Environment<T> {
    fn default() -> Self {
        Self(EnvInner::new())
    }
}
impl<T: Clone> Environment<T> {
    pub fn new() -> Self {
        Self(EnvInner::new())
    }
    pub fn is_global(&self) -> bool {
        self.0.len() <= 1
    }
    pub fn extend(&mut self) {
        self.0.push_front(Vec::new());
    }
    pub fn to_outer(&mut self) {
        let _ = self.0.pop_front();
    }
    pub fn add_bind(&mut self, binds: &[(Symbol, T)]) {
        assert!(!self.0.is_empty());
        self.0.front_mut().unwrap().extend_from_slice(binds);
    }

    pub fn lookup_cls(&self, name: &Symbol) -> LookupRes<&T> {
        match self
            .0
            .iter()
            .enumerate()
            .find(|(_level, vec)| vec.iter().any(|(n, _)| n == name))
            .and_then(|(level, vec)| vec.iter().rfind(|(n, _)| n == name).map(|(_, v)| (level, v)))
        {
            None => LookupRes::None,
            Some((level, e)) if level >= self.0.len() - 1 => LookupRes::Global(e),
            Some((0, e)) if self.0.len() <= 1 => LookupRes::Global(e),
            Some((0, e)) => LookupRes::Local(e),
            Some((level, e)) => LookupRes::UpValue(level, e),
        }
    }
    pub fn lookup(&self, name: &Symbol) -> Option<&T> {
        match self.lookup_cls(name) {
            LookupRes::None => None,
            LookupRes::Global(e) | LookupRes::Local(e) | LookupRes::UpValue(_, e) => Some(e),
        }
    }
}
