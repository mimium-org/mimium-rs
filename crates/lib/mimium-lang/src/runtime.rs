use crate::utils::{error::ReportableError, metadata::Location};
use thiserror::Error;

pub mod primitives;
pub mod vm;
pub mod wasm;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time(pub u64);

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error("Unknown Error")]
    Unknown,
}

#[derive(Debug, Error)]
#[error("Runtime Error: {0}")]
pub struct RuntimeError(pub ErrorKind, pub Location);

impl ReportableError for RuntimeError {
    fn get_labels(&self) -> Vec<(crate::utils::metadata::Location, String)> {
        vec![(self.1.clone(), self.0.to_string())]
    }
}
