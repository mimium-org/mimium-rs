use crate::utils::{error::ReportableError, metadata::Location};

pub mod vm;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time(pub u64);

#[derive(Debug)]
pub enum ErrorKind {
    Unknown,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Unknown => write!(f, "Unknown Error"),
        }
    }
}
#[derive(Debug)]
pub struct Error(pub ErrorKind, pub Location);
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = write!(f, "Runtime Error: ");
        self.0.fmt(f)
    }
}

impl std::error::Error for Error {}

impl ReportableError for Error {
    fn get_labels(&self) -> Vec<(crate::utils::metadata::Location, String)> {
        vec![(self.1.clone(), self.0.to_string())]
    }
}
