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
        error::{ReportableError, report},
        fileloader,
        metadata::Location,
    },
};

#[cfg(not(target_arch = "wasm32"))]
use mimium_lang::runtime::wasm::{WasmModule, WasmRuntime};

/// Check if WASM backend should be used based on MIMIUM_BACKEND environment variable
#[cfg(not(target_arch = "wasm32"))]
fn should_use_wasm_backend() -> bool {
    std::env::var("MIMIUM_BACKEND")
        .map(|v| v.to_lowercase() == "wasm")
        .unwrap_or(false)
}

#[cfg(target_arch = "wasm32")]
fn should_use_wasm_backend() -> bool {
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

pub fn load_src(path: &'static str) -> (PathBuf, String) {
    #[cfg(not(target_arch = "wasm32"))]
    let file = {
        let crate_root = std::env::var("TEST_ROOT").expect(
            r#"You must set TEST_ROOT environment variable to run test.
            You should put the line like below to your build.rs.
            fn main() {
                println!("cargo:rustc-env=TEST_ROOT={}", env!("CARGO_MANIFEST_DIR"));
                }
                "#,
        );
        [crate_root.as_str(), "tests/mmm", path]
            .iter()
            .collect::<PathBuf>()
            .canonicalize()
            .expect("canonicalize failed")
            .to_str()
            .expect("canonicalize failed")
            .to_string()
    };
    #[cfg(target_arch = "wasm32")]
    let file = format!(
        "{}/tests/mmm/{}",
        fileloader::get_env("TEST_ROOT").expect("TEST_ROOT is not set"),
        path
    );

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
