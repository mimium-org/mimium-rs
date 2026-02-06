use std::path::PathBuf;

use crate::ast::program::Program;
use crate::compiler::parser::parse_program;
use crate::utils::error::{ReportableError, SimpleError};
use crate::utils::fileloader;
use crate::utils::metadata::{Location, Span};

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
) -> (Program, Vec<Box<dyn ReportableError>>) {
    let loc = Location {
        span: span.clone(),
        path: PathBuf::from(mmm_filepath),
    };
    let res = fileloader::load_mmmlibfile(mmm_filepath, target_path)
        .map_err(|e| make_vec_error(e, loc.clone()));
    match res {
        Ok((content, path)) => {
            let (prog, parse_errs) = parse_program(&content, path);
            let errs = parse_errs
                .into_iter()
                .map(|e| -> Box<dyn ReportableError> {
                    Box::new(SimpleError {
                        message: format!("Parse error: {e:?}"),
                        span: loc.clone(),
                    })
                })
                .collect();
            (prog, errs)
        }
        Err(err) => (Program::default(), err),
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
        assert!(
            errs[0]
                .get_message()
                .contains("File tried to include itself recusively")
        );
    }
}
