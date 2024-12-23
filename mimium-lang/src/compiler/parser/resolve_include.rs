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
    let res = fileloader::load_mmmlibfile(mmm_filepath, target_path)
        .map_err(|e| make_vec_error(e, loc.clone()));
    match res {
        Ok((content, path)) => parse(&content, Some(path)),
        Err(err) => (Expr::Error.into_id(loc), err),
    }
}

#[cfg(all(test, target_arch = "wasm32"))]
mod test {
    use super::*;
    use crate::utils::fileloader;
    use wasm_bindgen_test::*;
    #[wasm_bindgen_test]
    fn test_resolve_include() {
        let file = format!(
            "{}/../mimium-test/tests/mmm/{}",
            fileloader::get_env("TEST_ROOT").expect("TEST_ROOT is not set"),
            "error_include_itself.mmm"
        );
        let (id, errs) = resolve_include(&file, &file, 0..0);
        assert_eq!(errs.len(), 1);
        assert!(errs[0]
            .get_message()
            .contains("File tried to include itself recusively"));
    }
}
