//! State migration on WASM hot-swap: delay buffers (host storage) and `self` values
//! (linear memory) must both follow the state-tree diff, also when the swapped-in module
//! only comes as bytes (the CLI compiles it in a subprocess).
#![cfg(not(target_arch = "wasm32"))]

use mimium_lang::{
    compiler::{self, wasmgen::read_dsp_state_skeleton},
    mir::StateType,
    plugin,
    runtime::{
        DspRuntime, ProgramPayload, Time,
        wasm::engine::{WasmDspRuntime, WasmEngine},
    },
};
use state_tree::{StateStoragePatchPlan, build_state_storage_patch_plan, tree::StateTreeSkeleton};

fn compile(src: &str) -> compiler::WasmOutput {
    let plugins = [plugin::get_builtin_fns_as_plugins()];
    let ext_fns = plugin::get_extfun_types(&plugins).collect::<Vec<_>>();
    let macros = plugin::get_macro_functions(&plugins).collect::<Vec<_>>();
    let ctx = compiler::Context::new(ext_fns, macros, None, compiler::Config::default());
    ctx.emit_wasm(src).unwrap_or_else(|_| panic!("compile failed:\n{src}"))
}

fn start(src: &str) -> WasmDspRuntime {
    let o = compile(src);
    let mut e = WasmEngine::new(&o.ext_fns, None).unwrap();
    e.load_module(&o.bytes).unwrap();
    let mut r = WasmDspRuntime::new(e, o.io_channels, o.dsp_state_skeleton);
    r.run_main().unwrap();
    r
}

/// Prepare a payload the way mimium-cli does for a module compiled in a subprocess:
/// only the bytes are known, the skeleton is read back from the module.
fn payload(old: &StateTreeSkeleton<StateType>, src: &str) -> ProgramPayload {
    let o = compile(src);
    let skel = read_dsp_state_skeleton(&o.bytes).expect("skeleton section");
    let mut e = WasmEngine::new(&o.ext_fns, None).unwrap();
    e.load_module(&o.bytes).unwrap();
    let mut r = WasmDspRuntime::new(e, None, None);
    r.run_main().unwrap();
    let mut pre = r.engine_mut().get_global_state_data().unwrap().to_vec();
    let plan = build_state_storage_patch_plan(old.clone(), skel.clone())
        .unwrap_or_else(|| StateStoragePatchPlan::identity(&skel));
    if pre.len() < plan.total_size {
        pre.resize(plan.total_size, 0);
    }
    ProgramPayload::WasmModule {
        bytes: o.bytes,
        prepared_engine: Box::new(r.into_engine()),
        dsp_state_skeleton: Some(skel),
        state_patch_plan: plan,
        prewarmed_global_state: pre,
    }
}

fn run(r: &mut WasmDspRuntime, t0: u64, n: u64) -> Vec<f64> {
    (t0..t0 + n)
        .map(|t| {
            r.run_dsp(Time(t));
            r.get_output(1)[0]
        })
        .collect()
}

const ECHO: &str = "fn cnt(){ self + 1.0 }\nfn echo(x){ x + delay(48000.0, self, 4800.0) * 0.5 }\n";

fn swap_and_run(a: &str, b: &str) -> Vec<f64> {
    let old = compile(a).dsp_state_skeleton.unwrap();
    let mut r = start(a);
    run(&mut r, 0, 1000);
    assert!(r.try_hot_swap(payload(&old, b)));
    run(&mut r, 1000, 9600)
}

#[test]
fn skeleton_section_roundtrips() {
    let o = compile(&format!("{ECHO}fn dsp(){{ echo(cnt()) }}\n"));
    assert_eq!(read_dsp_state_skeleton(&o.bytes), o.dsp_state_skeleton);
    assert_eq!(read_dsp_state_skeleton(b"\0asm\x01\0\0\0"), None);
    assert_eq!(read_dsp_state_skeleton(b"junk"), None);
}

#[test]
fn self_counter_survives_swap() {
    let a = "fn cnt(){ self + 1.0 }\nfn dsp(){ cnt() }\n";
    let b = "fn cnt(){ self + 1.0 }\nfn dsp(){ cnt() * 1.0 }\n";
    let out = swap_and_run(a, b);
    assert_eq!(out[0], 1001.0, "self state must carry over, got {}", out[0]);
}

#[test]
fn echo_tail_survives_node_inserted_before_it() {
    // An impulse at sample 1 echoes every 4800 samples. After the swap a new stateful
    // node sits before the echo, which shifts every state offset.
    let a = format!(
        "{ECHO}fn dsp(){{\n let imp = if (cnt() < 2.0) 1.0 else 0.0\n echo(imp)\n}}\n"
    );
    let b = format!(
        "{ECHO}fn dsp(){{\n let imp = if (cnt() < 2.0) 1.0 else 0.0\n let d = delay(48000.0, imp, 10.0)\n echo(imp) + d * 0.0\n}}\n"
    );
    let out = swap_and_run(&a, &b);
    // The counter keeps counting (no new impulse), and the old echo is still there.
    assert!(out[..100].iter().all(|v| v.abs() < 1e-9), "impulse fired again: {:?}", &out[..5]);
    let echo2 = out[8500..8700].iter().fold(0.0f64, |m, v| m.max(v.abs()));
    assert!((echo2 - 0.25).abs() < 1e-9, "second echo should be 0.25, got {echo2}");
}

#[test]
fn echo_tail_survives_source_removed() {
    let a = format!(
        "{ECHO}fn dsp(){{\n let imp = if (cnt() < 2.0) 1.0 else 0.0\n echo(imp)\n}}\n"
    );
    let b = format!("{ECHO}fn dsp(){{\n echo(0.0)\n}}\n");
    let out = swap_and_run(&a, &b);
    let echo2 = out[8500..8700].iter().fold(0.0f64, |m, v| m.max(v.abs()));
    assert!((echo2 - 0.25).abs() < 1e-9, "second echo should be 0.25, got {echo2}");
}

#[test]
fn delay_after_last_used_state_does_not_panic() {
    // Host storage grows lazily up to the last delay used, so it can be shorter than
    // the skeleton when Feed slots follow it; migration must not index past it.
    let a = "fn f(x){ self * 0.5 + x }\nfn dsp(){\n let d = delay(100.0, 1.0, 10.0)\n f(d)\n}\n";
    let b = "fn f(x){ self * 0.5 + x }\nfn dsp(){\n let e = delay(50.0, 1.0, 5.0)\n let d = delay(100.0, 1.0, 10.0)\n f(d) + e * 0.0\n}\n";
    swap_and_run(a, b);
}
