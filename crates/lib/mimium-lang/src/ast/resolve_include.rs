use std::path::PathBuf;

use crate::ast::program::Program;
use crate::compiler::parser::{parse_program, parser_errors_to_reportable};
use crate::utils::error::{ReportableError, SimpleError};
use crate::utils::fileloader;
use crate::utils::metadata::{Location, Span};

pub(super) struct ResolveIncludeResult {
    pub program: Program,
    pub resolved_path: PathBuf,
}

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
) -> (ResolveIncludeResult, Vec<Box<dyn ReportableError>>) {
    let loc = Location {
        span: span.clone(),
        path: PathBuf::from(mmm_filepath),
    };
    let res = fileloader::load_mmmlibfile(mmm_filepath, target_path)
        .map_err(|e| make_vec_error(e, loc.clone()));
    match res {
        Ok((content, path)) => {
            let (prog, parse_errs) = parse_program(&content, path.clone());
            let errs = parser_errors_to_reportable(&content, path.clone(), parse_errs);
            (
                ResolveIncludeResult {
                    program: prog,
                    resolved_path: path,
                },
                errs,
            )
        }
        Err(err) => (
            ResolveIncludeResult {
                program: Program::default(),
                resolved_path: PathBuf::from(mmm_filepath),
            },
            err,
        ),
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
        let (res, errs) = resolve_include(&file, &file, 0..0);
        let id = res.program;
        assert_eq!(errs.len(), 1);
        assert!(
            errs[0]
                .get_message()
                .contains("File tried to include itself recusively")
        );
        let _ = id;
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod native_test {
    use super::*;
    use std::fs;

    #[test]
    fn parse_error_in_included_file_tracks_included_path() {
        let base_dir = std::env::current_dir().unwrap().join("tmp");
        fs::create_dir_all(&base_dir).unwrap();

        let child_rel = "resolve_include_child_error.mmm";
        let root_path = base_dir.join("resolve_include_root_error.mmm");
        let child_path = base_dir.join(child_rel);

        fs::write(
            &root_path,
            format!("include(\"./{child_rel}\")\nfn dsp(){{\n    0.0\n}}\n"),
        )
        .unwrap();
        let child_source = "fn bad( {\n";
        fs::write(&child_path, child_source).unwrap();

        let (res, errs) = resolve_include(root_path.to_str().unwrap(), child_rel, 0..0);
        let expected_child_path = fs::canonicalize(&child_path).unwrap();

        let _ = res;
        assert!(!errs.is_empty());

        let message = errs[0].get_message();
        assert!(message.starts_with("Parse error:"));

        let labels = errs[0].get_labels();
        assert!(!labels.is_empty());
        assert!(labels[0].1.starts_with("Parse error:"));
        assert_eq!(labels[0].0.path, expected_child_path);
        let expected_start = child_source.find('{').unwrap();
        assert_eq!(labels[0].0.span, expected_start..expected_start + 1);
    }
}
