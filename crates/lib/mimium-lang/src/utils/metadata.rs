use crate::interner::{Symbol, ToSymbol};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq)]
pub struct Location {
    pub span: Span,
    pub path: Symbol,
}
impl Location {
    pub fn new(span: Span, path: Symbol) -> Self {
        Self { span, path }
    }
}
impl Default for Location {
    fn default() -> Self {
        Self {
            span: 0..0,
            path: "".to_symbol(),
        }
    }
}

impl ariadne::Span for Location {
    type SourceId = Symbol;

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
