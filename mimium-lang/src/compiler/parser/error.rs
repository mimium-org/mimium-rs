use crate::interner::Symbol;
use crate::utils::error::ReportableError;
use crate::utils::metadata::Location;
use chumsky;
use chumsky::error::{Rich, RichReason};
use chumsky::span::Span;
use std::fmt;
use std::hash::Hash;
#[derive(Debug)]
pub struct ParseError<'a, T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    pub content: Rich<'a, T>,
    pub file: Symbol,
}

impl<'src, 'b, T> ParseError<'b, T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display + Clone,
{
    pub fn new(content: Rich<'src, T>, file: Symbol) -> Self {
        Self {
            content: content.into_owned(),
            file,
        }
    }
}

impl<'src, T> fmt::Display for ParseError<'src, T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.content)
    }
}

impl<'a, T> std::error::Error for ParseError<'a, T> where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display
{
}

impl<'a, T> ReportableError for ParseError<'a, T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn get_message(&self) -> String {
        match self.content.reason() {
            RichReason::ExpectedFound { expected, found } => {
                let label = found
                    .as_ref()
                    .map_or_else(|| "unexpected token".to_string(), |found| found.to_string());
                let expected_labels = expected
                    .iter()
                    .map(|expected| expected.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{label}, expected {expected_labels}")
            }
            RichReason::Custom(msg) => msg.clone(),
        }
    }

    fn get_labels(&self) -> Vec<(Location, String)> {
        vec![(
            Location {
                span: self.content.span().start()..self.content.span().end(),
                path: self.file,
            },
            self.get_message(),
        )]
    }
}
