use std::ops::Range;

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

use crate::interner::{Symbol, ToSymbol};

use super::metadata::Location;

/// A dynamic error type that can hold specific error messages and the location where the error happened.
pub trait ReportableError: std::error::Error {
    /// message is used for reporting verbose message for `ariadne``.
    fn get_message(&self) -> String {
        self.to_string()
    }
    /// Label is used for indicating error with the specific position for `ariadne``.
    /// One error may have multiple labels, because the reason of the error may be caused by the mismatch of the properties in 2 or more different locations in the source (such as the type mismatch).
    fn get_labels(&self) -> Vec<(Location, String)>;
}

/// ReportableError implements `PartialEq`` mostly for testing purpose.
impl PartialEq for dyn ReportableError + '_ {
    fn eq(&self, other: &Self) -> bool {
        self.get_labels() == other.get_labels()
    }
}

#[derive(Debug, Clone)]
pub struct SimpleError {
    pub message: String,
    pub span: Location,
}
impl std::fmt::Display for SimpleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl std::error::Error for SimpleError {}
impl ReportableError for SimpleError {
    fn get_labels(&self) -> Vec<(Location, String)> {
        vec![(self.span.clone(), self.message.clone())]
    }
}

struct FileCache {
    src: ariadne::Source<Symbol>,
}

impl ariadne::Cache<usize> for FileCache {
    type Storage = Symbol;

    fn fetch(&mut self, _id: &usize) -> Result<&Source<Self::Storage>, impl std::fmt::Debug> {
        Ok::<&ariadne::Source<Symbol>, String>(&self.src)
    }

    fn display<'a>(&self, id: &'a usize) -> Option<impl std::fmt::Display + 'a> {
        Some(Box::new(id.to_string()))
    }
}

pub fn report(src: &str, path: Symbol, errs: &[Box<dyn ReportableError + '_>]) {
    let mut colors = ColorGenerator::new();
    for e in errs {
        // let a_span = (src.source(), span);color
        let rawlabels = e.get_labels();
        let labels = rawlabels.iter().map(|(loc, message)| {
            let span = (path.0, loc.span.clone());
            Label::new(span)
                .with_message(message)
                .with_color(colors.next())
        });
        let span: (usize, Range<usize>) = (path.0, rawlabels[0].0.span.clone());
        let builder = Report::build(ReportKind::Error, span)
            .with_message(e.get_message())
            .with_labels(labels)
            .finish();
        let cache = FileCache {
            src: ariadne::Source::from(src.to_symbol()),
        };
        builder.eprint(cache).unwrap();
    }
}

pub fn dump_to_string(errs: &[Box<dyn ReportableError>]) -> String {
    let mut res = String::new();
    for e in errs {
        res += e.get_message().as_str();
    }
    res
}
