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
    pub fn lookup(&self, name: &Symbol) -> Option<&T> {
        match self.lookup_cls(name) {
            LookupRes::None => None,
            LookupRes::Global(e) | LookupRes::Local(e) | LookupRes::UpValue(_, e) => Some(e),
        }
    }
}

pub mod newenv {
    use itertools::*;
    use std::{collections::HashMap, hash::Hash, rc::Rc};
    #[derive(Clone, Debug, PartialEq)]
    pub enum LookupRes<T: Clone> {
        Local(T),
        UpValue(usize, T),
        Global(T),
        External(T),
    }

    #[derive(Debug)]
    pub struct Error;
    #[derive(Default)]
    pub struct Env<K, T>
    where
        K: Clone + PartialEq + Eq + Hash,
        T: Clone,
    {
        parent: Option<Rc<Self>>,
        siblings: Vec<Rc<Self>>,
        local: HashMap<K, T>,
    }

    impl<K, T> Env<K, T>
    where
        K: Clone + PartialEq + Eq + Hash,
        T: Clone,
    {
        pub fn from_iter(parent: Rc<Self>, mut iter: impl Iterator<Item = (K, T)>) -> Self {
            debug_assert!(iter.by_ref().duplicates_by(|(k, _t)| k.clone()).count() == 0);
            let local = HashMap::from_iter(iter);
            Self {
                parent: Some(parent),
                local,
                siblings: vec![],
            }
        }
        pub fn is_global(&self) -> bool {
            self.parent.is_none()
        }
        fn try_lookup(&self, name: K) -> Result<LookupRes<T>, Error> {
            let is_global = self.is_global();
            match (self.local.get(&name), is_global) {
                (None, _) => match &self.parent {
                    None => Err(Error),
                    Some(p) => p.try_lookup(name),
                },
                (Some(e), false) => Ok(LookupRes::Local(e.clone())),
                (Some(e), true) => Ok(LookupRes::Global(e.clone())),
            }
        }
        fn lookup_candidates(&self, name: K) -> Vec<T> {
            todo!()
        }
        pub fn lookup(&self, name: K) -> Result<LookupRes<T>, Error> {
            match self.try_lookup(name.clone()) {
                Ok(_) => todo!(),
                Err(_) => {
                    let candidates = self.lookup_candidates(name);
                    todo!()
                }
            }
        }
    }
}
