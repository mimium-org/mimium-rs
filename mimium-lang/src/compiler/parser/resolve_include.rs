use super::{parse, Expr, Location, Span};
use crate::interner::{ExprNodeId, ToSymbol};
use crate::utils::error::{ReportableError, SimpleError};
use crate::utils::fileloader;

fn make_vec_error<E: std::error::Error>(e: E, loc: Location) -> Vec<Box<dyn ReportableError>> {
    vec![Box::new(SimpleError {
        message: e.to_string(),
        span: loc,
    }) as Box<dyn ReportableError>]
}

pub(super) fn resolve_include(
    mmm_filepath: &str,
    target_path: &str,
    span: Span,
) -> (ExprNodeId, Vec<Box<dyn ReportableError>>) {
    let loc = Location {
        span: span.clone(),
        path: mmm_filepath.to_symbol(),
    };
    let abspath = fileloader::get_canonical_path(mmm_filepath, target_path)
        .map_err(|e| make_vec_error(e, loc.clone()));
    match abspath {
        Ok(abspath) => {
            match fileloader::load(abspath.to_str().unwrap()).map_err(|e| make_vec_error(e, loc)) {
                Ok(content) => parse(&content, Some(abspath)),
                Err(errs) => (Expr::Error.into_id_without_span(), errs),
            }
        }
        Err(errs) => (Expr::Error.into_id_without_span(), errs),
    }
}
