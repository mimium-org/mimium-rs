use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{LazyLock, Mutex},
};

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use thiserror::Error;

use super::fileloader;
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

#[derive(Debug, Clone, Error)]
#[error("{message}")]
pub struct SimpleError {
    pub message: String,
    pub span: Location,
}

impl ReportableError for SimpleError {
    fn get_labels(&self) -> Vec<(Location, String)> {
        vec![(self.span.clone(), self.message.clone())]
    }
}

#[derive(Debug, Clone, Error)]
#[error("{message}")]
pub struct RichError {
    pub message: String,
    pub labels: Vec<(Location, String)>,
}

impl ReportableError for RichError {
    fn get_message(&self) -> String {
        self.message.clone()
    }
    fn get_labels(&self) -> Vec<(Location, String)> {
        self.labels.clone()
    }
}
impl From<Box<dyn ReportableError + '_>> for RichError {
    fn from(e: Box<dyn ReportableError + '_>) -> Self {
        Self {
            message: e.get_message(),
            labels: e.get_labels(),
        }
    }
}

struct FileCache {
    pub storage: HashMap<PathBuf, ariadne::Source<String>>,
}

impl ariadne::Cache<PathBuf> for FileCache {
    type Storage = String;

    fn fetch(&mut self, id: &PathBuf) -> Result<&Source<Self::Storage>, impl std::fmt::Debug> {
        if !self.storage.contains_key(id)
            && let Ok(content) = fileloader::load(id.to_string_lossy().as_ref())
        {
            self.storage.insert(id.clone(), Source::from(content));
        }

        self.storage
            .get(id)
            .ok_or_else(|| format!("File not found: {}", id.display()))
    }

    fn display<'a>(&self, id: &'a PathBuf) -> Option<impl std::fmt::Display + 'a> {
        Some(id.display())
    }
}

static FILE_BUCKET: LazyLock<Mutex<FileCache>> = LazyLock::new(|| {
    Mutex::new(FileCache {
        storage: HashMap::new(),
    })
});

pub fn report(src: &str, path: PathBuf, errs: &[Box<dyn ReportableError + '_>]) {
    let mut colors = ColorGenerator::new();
    for e in errs {
        // let a_span = (src.source(), span);color
        let rawlabels = e.get_labels();
        if rawlabels.is_empty() {
            continue;
        }
        let labels = rawlabels.iter().map(|(loc, message)| {
            let span = (loc.path.clone(), loc.span.clone());
            Label::new(span)
                .with_message(message)
                .with_color(colors.next())
        });
        let span = (rawlabels[0].0.path.clone(), rawlabels[0].0.span.clone());
        let builder = Report::build(ReportKind::Error, span)
            .with_message(e.get_message())
            .with_labels(labels)
            .finish();
        if let Ok(mut cache) = FILE_BUCKET.lock() {
            let mut cache: &mut FileCache = &mut cache;
            cache
                .storage
                .insert(path.clone(), Source::from(src.to_string()));
            builder.eprint(&mut cache).ok();
        }
    }
}

pub fn dump_to_string(errs: &[Box<dyn ReportableError>]) -> String {
    let mut res = String::new();
    for e in errs {
        res += e.get_message().as_str();
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::{parse_program, parser_errors_to_reportable};
    use std::fs;

    #[test]
    fn report_handles_labels_in_other_files() {
        let base_dir = std::env::current_dir().unwrap().join("tmp");
        fs::create_dir_all(&base_dir).unwrap();

        let root_path = base_dir.join("report_root.mmm");
        let included_path = base_dir.join("report_included.mmm");

        let root_src = "include(\"./report_included.mmm\")\n";
        let included_src = "fn bad( {\n";
        let expected_start = included_src.find('{').unwrap();

        fs::write(&root_path, root_src).unwrap();
        fs::write(&included_path, included_src).unwrap();

        let canonical_included_path = fs::canonicalize(&included_path).unwrap();
        let (_program, parse_errs) = parse_program(included_src, canonical_included_path.clone());
        assert!(!parse_errs.is_empty());

        let errs = parser_errors_to_reportable(included_src, canonical_included_path.clone(), parse_errs);

        assert!(errs[0].get_message().starts_with("Parse error:"));
        let labels = errs[0].get_labels();
        assert_eq!(labels.len(), 1);
        assert!(labels[0].1.starts_with("Parse error:"));
        assert_eq!(labels[0].0.span, expected_start..expected_start + 1);
        assert_eq!(labels[0].0.path, canonical_included_path);

        report(root_src, root_path, &errs);
    }
}
