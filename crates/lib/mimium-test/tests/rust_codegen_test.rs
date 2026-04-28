use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use mimium_lang::utils::error::report;
use mimium_lang::{Config as RuntimeConfig, ExecContext};
use mimium_test::{AnnotatedFileTestSpec, load_src, read_annotated_file_test_spec};

const RUST_TEST_MAIN_TEMPLATE: &str =
    include_str!("../../mimium-lang/src/compiler/mimium_test_main.rs.template");

const TEST_HOST_DECLS: &str = r#"
struct TestHost {
    now: f64,
    sample_rate: f64,
}

impl TestHost {
    fn advance_time(&mut self) {
        self.now += 1.0 / self.sample_rate;
    }
}

impl MimiumHost for TestHost {
    fn call_ext(
        &mut self,
        name: &str,
        _args: &[Word],
        _ret_words: usize,
    ) -> Result<Vec<Word>, String> {
        Err(format!("unexpected external call: {}", name))
    }

    fn current_time(&mut self) -> f64 {
        self.now
    }

    fn sample_rate(&mut self) -> f64 {
        self.sample_rate
    }
}
"#;

enum FixtureMode {
    Run,
    Skip(&'static str),
}

fn assert_with_spec(res: &[f64], spec: &AnnotatedFileTestSpec) {
    let tol = spec.tol.unwrap_or(1e-12);
    assert_eq!(res.len(), spec.expected.len());
    for (actual, expected) in res.iter().zip(spec.expected.iter()) {
        assert!((actual - expected).abs() <= tol);
    }
}

fn render_rust_test_main(
    decls: &str,
    program_init: &str,
    call_main: Option<&str>,
    run_body: &str,
) -> String {
    RUST_TEST_MAIN_TEMPLATE
        .replace("/*__DECLS__*/", decls)
        .replace("/*__PROGRAM_INIT__*/", program_init)
        .replace("/*__CALL_MAIN__*/", call_main.unwrap_or_default())
        .replace("/*__RUN_BODY__*/", run_body)
}

fn rust_test_tmp_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../tmp")
        .join("rustgen-fixtures")
}

fn fixture_mode(name: &str, spec: &AnnotatedFileTestSpec) -> FixtureMode {
    if name == "parser_combinators.mmm" {
        return FixtureMode::Skip(
            "parser combinator fixture is excluded from the shared all-fixture runner too",
        );
    }
    if spec.plugins {
        return FixtureMode::Skip(
            "plugin-backed externals are not wired into the generated Rust host yet",
        );
    }
    FixtureMode::Run
}

fn collect_fixture_names() -> Vec<String> {
    let fixture_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/mmm");
    let mut fixture_names = fs::read_dir(&fixture_dir)
        .unwrap()
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("mmm"))
        .filter_map(|path| {
            let name = path.file_name()?.to_str()?.to_string();
            let src = fs::read_to_string(&path).ok()?;
            let has_meta = src
                .lines()
                .take(8)
                .map(str::trim)
                .any(|line| line.starts_with("// @test "));
            has_meta.then_some(name)
        })
        .collect::<Vec<_>>();

    fixture_names.sort();
    fixture_names
}

fn compile_and_run_rust_fixture(
    path: &str,
    spec: &AnnotatedFileTestSpec,
) -> Result<Vec<f64>, String> {
    let (file, src) = load_src(path);
    let mut ctx = ExecContext::new([].into_iter(), Some(file.clone()), RuntimeConfig::default());
    ctx.prepare_compiler();
    let compiler = ctx
        .get_compiler()
        .ok_or_else(|| format!("compiler context was not prepared for {path}"))?;
    let output = compiler.emit_rust(&src).map_err(|errs| {
        report(&src, file, &errs);
        format!("emit_rust failed for {path}")
    })?;

    let maybe_call_main = output
        .source
        .contains("pub fn call_main")
        .then_some("    program.call_main().unwrap();\n");
    let run_body = format!(
        "    for _ in 0..{}u64 {{\n        let output = program.call_dsp(&[]).unwrap();\n        for word in output {{\n            println!(\"{{:.12}}\", word_to_f64(word));\n        }}\n        program.host.advance_time();\n    }}\n",
        spec.times
    );
    let harness = render_rust_test_main(
        TEST_HOST_DECLS,
        "let host = TestHost { now: 0.0, sample_rate: 48_000.0 };\n    let mut program = MimiumProgram::with_host(host);",
        maybe_call_main,
        &run_body,
    );

    let tmp_dir = rust_test_tmp_dir();
    fs::create_dir_all(&tmp_dir).map_err(|err| format!("failed to create tmp dir: {err}"))?;
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|err| format!("failed to get timestamp: {err}"))?
        .as_nanos();
    let stem = path.strip_suffix(".mmm").unwrap_or(path);
    let sanitized = stem.replace('/', "_");
    let source_path = tmp_dir.join(format!("{sanitized}_{stamp}.rs"));
    let binary_path = tmp_dir.join(format!("{sanitized}_{stamp}"));
    fs::write(&source_path, format!("{}{harness}", output.source))
        .map_err(|err| format!("failed to write generated Rust source: {err}"))?;

    let rustc = std::env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());
    let compile = Command::new(&rustc)
        .arg("--edition=2024")
        .arg(&source_path)
        .arg("-o")
        .arg(&binary_path)
        .output()
        .map_err(|err| format!("failed to launch rustc for {path}: {err}"))?;
    if !compile.status.success() {
        return Err(format!(
            "generated Rust failed to compile for {path}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&compile.stdout),
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&binary_path)
        .output()
        .map_err(|err| format!("failed to run generated binary for {path}: {err}"))?;
    if !run.status.success() {
        return Err(format!(
            "generated Rust binary failed for {path}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&run.stdout),
            String::from_utf8_lossy(&run.stderr)
        ));
    }

    String::from_utf8(run.stdout)
        .map_err(|err| format!("generated binary output was not UTF-8 for {path}: {err}"))?
        .lines()
        .map(|line| {
            line.parse::<f64>().map_err(|err| {
                format!("failed to parse generated output `{line}` for {path}: {err}")
            })
        })
        .collect()
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn run_all_annotated_fixtures_via_rust_codegen() {
    let fixture_names = collect_fixture_names();
    assert!(!fixture_names.is_empty());

    let mut skipped = Vec::new();
    let mut failures = Vec::new();

    fixture_names.iter().for_each(|name| {
        let spec = read_annotated_file_test_spec(name)
            .unwrap_or_else(|e| panic!("{name} metadata failed to load: {e}"));
        match fixture_mode(name, &spec) {
            FixtureMode::Skip(reason) => skipped.push(format!("{name}: {reason}")),
            FixtureMode::Run => match compile_and_run_rust_fixture(name, &spec) {
                Ok(actual) => {
                    let check = std::panic::catch_unwind(|| assert_with_spec(&actual, &spec));
                    if let Err(payload) = check {
                        let detail = if let Some(message) = payload.downcast_ref::<String>() {
                            message.clone()
                        } else if let Some(message) = payload.downcast_ref::<&str>() {
                            message.to_string()
                        } else {
                            "assertion failed without message".to_string()
                        };
                        failures.push(format!("{name}: {detail}"));
                    }
                }
                Err(err) => failures.push(format!("{name}: {err}")),
            },
        }
    });

    if !skipped.is_empty() {
        eprintln!("Rust codegen fixture skips ({}):", skipped.len());
        skipped.iter().for_each(|entry| eprintln!("  {entry}"));
    }

    assert!(
        failures.is_empty(),
        "Rust codegen fixture failures ({}):\n{}",
        failures.len(),
        failures.join("\n")
    );
}
