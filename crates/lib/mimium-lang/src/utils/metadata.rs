use std::path::PathBuf;

use crate::interner::{Symbol, ToSymbol};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq)]
pub struct Location {
    pub span: Span,
    pub path: PathBuf,
}
impl Location {
    pub fn new(span: Span, path: PathBuf) -> Self {
        Self { span, path }
    }

    pub fn internal() -> Self {
        Self {
            span: Span { start: 0, end: 0 },
            path: PathBuf::from("internal"),
        }
    }
}
impl Default for Location {
    fn default() -> Self {
        Self {
            span: 0..0,
            path: PathBuf::new(),
        }
    }
}

impl ariadne::Span for Location {
    type SourceId = PathBuf;

    fn source(&self) -> &Self::SourceId {
        &self.path
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

// #[derive(Clone, Debug, PartialEq)]
// pub struct WithMeta<T>{
//     pub location: Span,
//     pub value : T
// }
pub(crate) const GLOBAL_LABEL: &str = "_mimium_global";

// #[derive(Clone, Debug, PartialEq)]
// pub struct WithMeta<T: Clone + PartialEq>(pub T, pub Span);

// impl<T: Clone + PartialEq> WithMeta<T> {
//     fn map<F, O>(self, f: F) -> WithMeta<O>
//     where
//         F: FnOnce(T) -> O,
//         O: Clone + PartialEq,
//     {
//         WithMeta(f(self.0), self.1)
//     }
// }
