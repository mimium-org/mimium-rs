use std::{collections::HashMap, path::PathBuf};

use mimium_audiodriver::{
    backends::local_buffer::LocalBufferDriver,
    driver::{Driver, RuntimeData},
};
use mimium_lang::{
    Config, ExecContext,
    plugin::Plugin,
    runtime::{self, vm},
    utils::{
        error::{ReportableError, RichError, report},
        fileloader,
        metadata::Location,
    },
};
use serde::Deserialize;

#[cfg(not(target_arch = "wasm32"))]
use mimium_lang::runtime::wasm::WasmRuntime;

/// Check if WASM backend should be used based on MIMIUM_BACKEND environment variable.
///
/// Tests can use this to skip or adjust behavior for VM-specific checks
/// (e.g. GC internals) that are not applicable to the WASM backend.
#[cfg(not(target_arch = "wasm32"))]
pub fn should_use_wasm_backend() -> bool {
    std::env::var("MIMIUM_BACKEND")
        .map(|v| v.to_lowercase() == "wasm")
        .unwrap_or(false)
}

#[cfg(target_arch = "wasm32")]
pub fn should_use_wasm_backend() -> bool {
    false
}

pub fn run_bytecode_test(
    machine: &mut vm::Machine,
    n: usize,
) -> Result<&[f64], Vec<Box<dyn ReportableError>>> {
    let retcode = machine.execute_entry("dsp");
    if retcode >= 0 {
        Ok(vm::Machine::get_as_array::<f64>(machine.get_top_n(n)))
    } else {
        Err(vec![Box::new(runtime::RuntimeError(
            runtime::ErrorKind::Unknown,
            Location::default(),
        ))])
    }
}

pub fn run_bytecode_test_multiple(
    bytecodes: vm::Program,
    times: u64,
    stereo: bool,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let mut ctx = ExecContext::new([].into_iter(), None, Config::default());
    ctx.prepare_machine_with_bytecode(bytecodes);
    let machine = ctx.get_vm_mut().unwrap();
    let _retcode = machine.execute_main();
    let n = if stereo { 2 } else { 1 };
    let mut ret = Vec::with_capacity(times as usize * n);
    for i in 0..times {
        let res = run_bytecode_test(machine, n)?;
        ret.extend_from_slice(res);
        println!("time:{i}, res: {res:?}")
    }
    Ok(ret)
}

pub fn run_source_with_plugins(
    src: &str,
    path: Option<&str>,
    times: u64,
    plugins: impl Iterator<Item = Box<dyn Plugin>>,
    with_scheduler: bool,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    // Check if WASM backend should be used
    #[cfg(not(target_arch = "wasm32"))]
    if should_use_wasm_backend() {
        return run_source_with_scheduler_wasm(src, path, times, with_scheduler);
    }

    let mut driver = LocalBufferDriver::new(times as _);
    let audiodriverplug: Box<dyn Plugin> = Box::new(driver.get_as_plugin());
    let mut ctx = ExecContext::new(
        plugins.chain([audiodriverplug]),
        path.map(PathBuf::from),
        Config::default(),
    );
    if with_scheduler {
        ctx.add_system_plugin(mimium_scheduler::get_default_scheduler_plugin());
    }
    ctx.prepare_machine(src).unwrap();
    let _ = ctx.run_main();
    let runtimedata = {
        let ctxmut: &mut ExecContext = &mut ctx;
        RuntimeData::try_from(ctxmut).unwrap()
    };
    driver.init(runtimedata, None);
    driver.play();
    Ok(driver.get_generated_samples().to_vec())
}

pub fn run_source_with_scheduler(
    src: &str,
    times: u64,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    run_source_with_plugins(src, None, times, [].into_iter(), true)
}

// if stereo, this returns values in flattened form [L1, R1, L2, R2, ...]
pub fn run_source_test(
    src: &str,
    times: u64,
    stereo: bool,
    path: Option<PathBuf>,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    // Check if WASM backend should be used
    #[cfg(not(target_arch = "wasm32"))]
    if should_use_wasm_backend() {
        return run_wasm_test(src, times, stereo, path);
    }

    let mut ctx = ExecContext::new([].into_iter(), path, Config::default());

    ctx.prepare_machine(src)?;
    let bytecode = ctx.take_vm().unwrap().prog;
    run_bytecode_test_multiple(bytecode, times, stereo)
}

pub fn run_file_with_plugins(
    path: &'static str,
    times: u64,
    plugins: impl Iterator<Item = Box<dyn Plugin>>,
    with_scheduler: bool,
) -> Option<Vec<f64>> {
    let (file, src) = load_src(path);
    let res = run_source_with_plugins(
        &src,
        Some(&file.to_string_lossy()),
        times,
        plugins,
        with_scheduler,
    );
    match res {
        Ok(res) => Some(res),
        Err(errs) => {
            report(&src, file, &errs);
            None
        }
    }
}
pub fn run_file_with_scheduler(path: &'static str, times: u64) -> Option<Vec<f64>> {
    run_file_with_plugins(path, times, [].into_iter(), true)
}
pub fn run_file_test(path: &'static str, times: u64, stereo: bool) -> Option<Vec<f64>> {
    // Check if WASM backend should be used
    #[cfg(not(target_arch = "wasm32"))]
    if should_use_wasm_backend() {
        return run_file_test_wasm(path, times, stereo);
    }

    let (file, src) = load_src(path);
    let res = run_source_test(&src, times, stereo, Some(file));
    match res {
        Ok(res) => Some(res),
        Err(errs) => {
            report(&src, path.into(), &errs);
            None
        }
    }
}

pub fn run_error_test(path: &'static str, stereo: bool) -> Vec<Box<dyn ReportableError>> {
    let (file, src) = load_src(path);
    let res = run_source_test(&src, 1, stereo, Some(file));
    match res {
        Ok(_res) => {
            panic!("this test should emit errors")
        }
        Err(errs) => errs,
    }
}

pub fn run_error_test_rich(path: &'static str, stereo: bool) -> Vec<RichError> {
    run_error_test(path, stereo)
        .into_iter()
        .map(RichError::from)
        .collect()
}

pub fn load_src(path: &str) -> (PathBuf, String) {
    #[cfg(not(target_arch = "wasm32"))]
    let file = {
        let crate_root =
            std::env::var("TEST_ROOT").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_string());
        PathBuf::from(crate_root)
            .join("tests/mmm")
            .join(path)
            .canonicalize()
            .expect("canonicalize failed")
            .to_str()
            .expect("canonicalize failed")
            .to_string()
    };
    #[cfg(target_arch = "wasm32")]
    let file = {
        let crate_root = fileloader::get_env("TEST_ROOT")
            .unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_string());
        format!("{}/tests/mmm/{}", crate_root, path)
    };

    println!("{file}");
    let src = fileloader::load(&file).expect("failed to load file");
    (PathBuf::from(file), src)
}

pub fn run_file_test_mono(path: &'static str, times: u64) -> Option<Vec<f64>> {
    run_file_test(path, times, false)
}

pub fn run_file_test_stereo(path: &'static str, times: u64) -> Option<Vec<f64>> {
    run_file_test(path, times, true)
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedFileTestSpec {
    pub times: u64,
    pub stereo: bool,
    pub expected: Vec<f64>,
    pub web: bool,
    pub plugins: bool,
    pub tol: Option<f64>,
}

#[derive(Debug, Clone, Deserialize)]
struct AnnotatedFileTestSpecRaw {
    times: u64,
    #[serde(default)]
    stereo: bool,
    expected: Vec<f64>,
    #[serde(default)]
    web: bool,
    #[serde(default)]
    plugins: bool,
    #[serde(default)]
    tol: Option<f64>,
}

const TEST_META_PREFIX: &str = "// @test ";

fn parse_annotated_file_test_spec(src: &str) -> Result<AnnotatedFileTestSpec, String> {
    let metadata_line = src
        .lines()
        .take(8)
        .map(str::trim)
        .find(|line| line.starts_with(TEST_META_PREFIX))
        .ok_or_else(|| "Missing `// @test {...}` metadata in first 8 lines".to_string())?;

    let raw_json = metadata_line.trim_start_matches(TEST_META_PREFIX);
    let raw: AnnotatedFileTestSpecRaw =
        serde_json::from_str(raw_json).map_err(|e| format!("Invalid @test metadata JSON: {e}"))?;

    Ok(AnnotatedFileTestSpec {
        times: raw.times,
        stereo: raw.stereo,
        expected: raw.expected,
        web: raw.web,
        plugins: raw.plugins,
        tol: raw.tol,
    })
}

pub fn read_annotated_file_test_spec(path: &str) -> Result<AnnotatedFileTestSpec, String> {
    let (_file, src) = load_src(path);
    parse_annotated_file_test_spec(&src)
}

pub fn run_annotated_file_test(path: &str) -> Result<(Vec<f64>, AnnotatedFileTestSpec), String> {
    let (file, src) = load_src(path);
    let spec = parse_annotated_file_test_spec(&src)?;

    let result = run_source_test(&src, spec.times, spec.stereo, Some(file.clone()));
    match result {
        Ok(samples) => Ok((samples, spec)),
        Err(errs) => {
            report(&src, file, &errs);
            Err("Compilation/runtime failed for annotated file test".to_string())
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn run_annotated_file_test_wasm(
    path: &str,
) -> Result<(Vec<f64>, AnnotatedFileTestSpec), String> {
    let (file, src) = load_src(path);
    let spec = parse_annotated_file_test_spec(&src)?;

    let result = run_wasm_test(&src, spec.times, spec.stereo, Some(file.clone()));
    match result {
        Ok(samples) => Ok((samples, spec)),
        Err(errs) => {
            report(&src, file, &errs);
            Err("Compilation/runtime failed for annotated WASM file test".to_string())
        }
    }
}

pub fn run_annotated_file_test_with_plugins(
    path: &str,
    with_scheduler: bool,
) -> Result<(Vec<f64>, AnnotatedFileTestSpec), String> {
    let (file, src) = load_src(path);
    let spec = parse_annotated_file_test_spec(&src)?;

    let result = run_source_with_plugins(
        &src,
        Some(&file.to_string_lossy()),
        spec.times,
        [].into_iter(),
        with_scheduler,
    );
    match result {
        Ok(samples) => Ok((samples, spec)),
        Err(errs) => {
            report(&src, file, &errs);
            Err("Compilation/runtime failed for annotated file test with plugins".to_string())
        }
    }
}

pub fn test_state_sizes<T: IntoIterator<Item = (&'static str, u64)>>(path: &'static str, ans: T) {
    let state_sizes: HashMap<&str, u64> = HashMap::from_iter(ans);
    let (file, src) = load_src(path);
    let mut ctx = ExecContext::new([].into_iter(), Some(file), Config::default());
    ctx.prepare_machine(&src).unwrap();
    let bytecode = ctx.take_vm().expect("failed to emit bytecode").prog;
    // let bytecode = match ctx.compiler.emit_bytecode(&src) {
    //     Ok(res) => res,
    //     Err(errs) => {
    //         report(&src, file, &errs);
    //         panic!("failed to emit bytecode");
    //     }
    // };

    for (sym, proto) in bytecode.global_fn_table {
        let fn_name = sym.as_str();

        if fn_name == "_mimium_global" {
            continue;
        }

        let actual = proto.state_skeleton.total_size();
        match state_sizes.get(fn_name) {
            Some(&expected) => {
                assert_eq!(
                    actual, expected,
                    "state size of function `{fn_name}` is wrong"
                );
            }
            None => panic!("no such function: {fn_name}"),
        };
    }
}

/// Run a WASM backend test with the given source code
#[cfg(not(target_arch = "wasm32"))]
pub fn run_wasm_test(
    src: &str,
    times: u64,
    stereo: bool,
    path: Option<PathBuf>,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    use mimium_lang::compiler::wasmgen::WasmGenerator;
    use std::sync::Arc;

    // Compile to MIR
    let mut ctx = ExecContext::new([].into_iter(), path, Config::default());
    ctx.prepare_compiler();
    let ext_fns = ctx.get_extfun_types();
    let mir = ctx.get_compiler().unwrap().emit_mir(src)?;

    // Generate WASM
    let mut wasmgen = WasmGenerator::new(Arc::new(mir), &ext_fns);
    let wasm_bytes = wasmgen.generate().map_err(|e| {
        eprintln!("[WASM] Code generation error: {e}");
        vec![Box::new(runtime::RuntimeError(
            runtime::ErrorKind::Unknown,
            Location::default(),
        )) as Box<dyn ReportableError>]
    })?;

    // Load and execute with WasmRuntime
    let mut wasm_runtime = WasmRuntime::new(&ext_fns, None).map_err(|e| {
        eprintln!("[WASM] Runtime creation error: {e}");
        vec![Box::new(runtime::RuntimeError(
            runtime::ErrorKind::Unknown,
            Location::default(),
        )) as Box<dyn ReportableError>]
    })?;

    let mut wasm_module = wasm_runtime.load_module(&wasm_bytes).map_err(|e| {
        eprintln!("[WASM] Module load error: {e}");
        vec![Box::new(runtime::RuntimeError(
            runtime::ErrorKind::Unknown,
            Location::default(),
        )) as Box<dyn ReportableError>]
    })?;

    // Execute main to initialize
    let _init_result = wasm_module.call_function("main", &[]);

    // Execute dsp multiple times
    let n = if stereo { 2 } else { 1 };
    let mut results = Vec::with_capacity(times as usize * n);

    for _ in 0..times {
        let result = wasm_module.call_function("dsp", &[]).map_err(|e| {
            eprintln!("[WASM] dsp() call error: {e}");
            vec![Box::new(runtime::RuntimeError(
                runtime::ErrorKind::Unknown,
                Location::default(),
            )) as Box<dyn ReportableError>]
        })?;

        // Extract f64 values from result
        if let Some(val) = result.first() {
            if stereo {
                // dsp() returns a pointer to a (f64, f64) tuple in linear memory
                let ptr = *val as usize;
                let left = wasm_module.read_memory_f64(ptr).unwrap_or(0.0);
                let right = wasm_module.read_memory_f64(ptr + 8).unwrap_or(0.0);
                results.push(left);
                results.push(right);
            } else {
                results.push(f64::from_bits(*val));
            }
        }
    }

    Ok(results)
}

/// Run a file test using WASM backend
#[cfg(not(target_arch = "wasm32"))]
pub fn run_file_test_wasm(path: &'static str, times: u64, stereo: bool) -> Option<Vec<f64>> {
    let (file, src) = load_src(path);
    let res = run_wasm_test(&src, times, stereo, Some(file));
    match res {
        Ok(res) => Some(res),
        Err(errs) => {
            report(&src, path.into(), &errs);
            None
        }
    }
}

/// Run a source test with scheduler plugin via the WASM backend.
///
/// This mirrors [`run_source_with_plugins`] but compiles to WASM and registers
/// scheduler plugin functions as host trampolines via `freeze_for_wasm()`.
/// Uses `WasmDspRuntime` to ensure the test exercises the same code path
/// as the CLI.
#[cfg(not(target_arch = "wasm32"))]
pub fn run_source_with_scheduler_wasm(
    src: &str,
    path: Option<&str>,
    times: u64,
    with_scheduler: bool,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    use mimium_lang::compiler::wasmgen::WasmGenerator;
    use mimium_lang::runtime::DspRuntime;
    use mimium_lang::runtime::wasm::engine::{WasmDspRuntime, WasmEngine};
    use std::sync::Arc;

    let path_buf = path.map(PathBuf::from);

    // Build ExecContext with scheduler so that ext_fns includes
    // `_mimium_schedule_at` type information for WASM import generation.
    let mut ctx = ExecContext::new([].into_iter(), path_buf, Config::default());
    if with_scheduler {
        ctx.add_system_plugin(mimium_scheduler::get_default_scheduler_plugin());
    }

    // Compile to MIR
    ctx.prepare_compiler();
    let ext_fns = ctx.get_extfun_types();
    let mir = ctx.get_compiler().unwrap().emit_mir(src)?;

    // Generate WASM bytecode
    let mut wasmgen = WasmGenerator::new(Arc::new(mir), &ext_fns);
    let wasm_bytes = wasmgen.generate().map_err(|e| {
        eprintln!("[WASM] Code generation error: {e}");
        vec![Box::new(runtime::RuntimeError(
            runtime::ErrorKind::Unknown,
            Location::default(),
        )) as Box<dyn ReportableError>]
    })?;

    // Collect WASM plugin function handlers from system plugins (scheduler, etc.)
    // freeze_for_wasm() must be called before generate_wasm_audioworkers() so
    // that both share the same underlying WasmSchedulerHandle.
    let plugin_fns = ctx.freeze_wasm_plugin_fns();

    // Collect per-sample audio workers from all plugins.
    let wasm_workers = ctx.generate_wasm_audioworkers();

    // Create WASM engine and load module
    let mut wasm_engine = WasmEngine::new(&ext_fns, plugin_fns).map_err(|e| {
        eprintln!("[WASM] Engine creation error: {e}");
        vec![Box::new(runtime::RuntimeError(
            runtime::ErrorKind::Unknown,
            Location::default(),
        )) as Box<dyn ReportableError>]
    })?;

    wasm_engine.load_module(&wasm_bytes).map_err(|e| {
        eprintln!("[WASM] Module load error: {e}");
        vec![Box::new(runtime::RuntimeError(
            runtime::ErrorKind::Unknown,
            Location::default(),
        )) as Box<dyn ReportableError>]
    })?;

    // Create WasmDspRuntime, the same code path as CLI
    let mut wasm_runtime = WasmDspRuntime::new(wasm_engine, None, None);
    wasm_runtime.set_wasm_audioworkers(wasm_workers);
    let _ = wasm_runtime.run_main();

    // Execute dsp for each sample via DspRuntime trait
    let mut results = Vec::with_capacity(times as usize);
    for t in 0..times {
        wasm_runtime.run_dsp(runtime::Time(t));
        let output = wasm_runtime.get_output(1);
        if let Some(&val) = output.first() {
            results.push(val);
        }
    }

    Ok(results)
}

/// Run a file test with scheduler plugin via the WASM backend.
#[cfg(not(target_arch = "wasm32"))]
pub fn run_file_with_scheduler_wasm(path: &'static str, times: u64) -> Option<Vec<f64>> {
    let (file, src) = load_src(path);
    let res = run_source_with_scheduler_wasm(&src, Some(&file.to_string_lossy()), times, true);
    match res {
        Ok(res) => Some(res),
        Err(errs) => {
            report(&src, path.into(), &errs);
            None
        }
    }
}
