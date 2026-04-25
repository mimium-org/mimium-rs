// If you want to run benchmark, you need to run with nightly channel.
// Run with `cargo +nightly bench -p mimium-audiodriver --bench bench`.
#![feature(test)]
extern crate test;

fn main() {}

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};

    use mimium_audiodriver::{
        backends::local_buffer::LocalBufferDriver,
        driver::{Driver, RuntimeData, VmDspRuntime},
    };
    use mimium_lang::{
        Config, ExecContext,
        plugin::DynSystemPlugin,
        runtime::{DspRuntime, ProgramPayload, vm::Program},
    };
    use test::{Bencher, black_box};

    fn bench_local_buffer_render(b: &mut Bencher, source: &str, frames: usize, block_size: usize) {
        let mut driver = LocalBufferDriver::with_block_size(frames, block_size);
        let mut ctx = ExecContext::new([].into_iter(), None, Config::default());
        ctx.add_plugin(driver.get_as_plugin());
        ctx.prepare_machine(source)
            .expect("program compilation should succeed");
        let _ = ctx.run_main();
        let runtime = RuntimeData::try_from(&mut ctx).expect("runtime data should exist");
        driver.init(runtime, None);

        b.iter(|| {
            driver.count.store(0, std::sync::atomic::Ordering::Relaxed);
            black_box(driver.play());
            black_box(driver.get_generated_samples().len());
        });
    }

    fn compile_program(source: &str) -> Program {
        let mut ctx = ExecContext::new([].into_iter(), None, Config::default());
        let localdriver = LocalBufferDriver::new(0);
        ctx.add_plugin(localdriver.get_as_plugin());
        ctx.prepare_compiler();
        ctx.get_compiler()
            .expect("compiler should be prepared")
            .emit_bytecode(source)
            .expect("bytecode compilation should succeed")
    }

    fn make_vm_runtime(source: &str) -> VmDspRuntime {
        let mut ctx = ExecContext::new([].into_iter(), None, Config::default());
        let localdriver = LocalBufferDriver::new(0);
        ctx.add_plugin(localdriver.get_as_plugin());
        ctx.prepare_compiler();
        ctx.prepare_machine(source)
            .expect("initial program compilation should succeed");
        let _ = ctx.run_main();

        let vm = ctx.take_vm().expect("VM should be initialized");
        let mut system_plugins: Vec<DynSystemPlugin> = vec![];
        VmDspRuntime::new(vm, &mut system_plugins)
    }

    fn make_swap_source(gain: f64) -> String {
        format!(
            "fn phasor(freq){{\n  (self + freq / 48000.0) % 1.0\n}}\n\nfn dsp(){{\n  let sig = sin(phasor(440.0) * 6.28318530718)\n  ({gain} * sig, {gain} * sig)\n}}"
        )
    }

    fn make_large_swap_source(gain: f64, voices: usize) -> String {
        format!(
            "let pi = 3.14159265359
let sr = 48000.0
fn phasor(freq,phase){{
  (self + freq/sr + phase)%1.0
}}
fn osc(freq,phase){{
  sin(phasor(freq,phase)*pi*2.0)
}}
fn amosc(freq,rate,phase){{
  osc(freq + osc(rate,phase)*100.0,phase)
}}

fn replicate(n:float,gen:()->(float,float,float)->float){{
    if (n>0.0){{
        let c = replicate(n - 1.0,gen)
        let g = gen()
        |x,rate,phase| {{ g(x,rate,phase) + c(x + 11.0,rate + 0.23,phase + 0.013) }}
    }}else{{
        |x,rate,phase| {{ 0.0 }}
    }}
}}

let stacked = replicate({voices}.0,| |amosc);
fn dsp(){{
    let sig = stacked(220.0,0.7,0.0) * {gain}
    (sig,sig)
}}"
        )
    }

    fn load_pattern_test_source() -> String {
        let path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../../examples/pattern_test.mmm");
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
        source.replace("    |> Probe!(\"out\")\n", "")
    }

    fn make_pattern_test_variant(source: &str, variant_id: usize) -> String {
        format!("{source}\n\nlet __bench_pattern_variant_{variant_id} = {variant_id}.0\n")
    }

    fn make_render_source(voices: usize) -> String {
        make_large_swap_source(0.1, voices)
    }

    fn make_dispatch_source() -> String {
        "fn dsp(){ (0.0, 0.0) }".to_string()
    }

    fn make_linear_block_source() -> String {
        "fn dsp(){
    let base = sin(0.25 * 6.28318530718)
    let folded = ((base * 0.7 + 0.3) * (base * 0.5 + 0.1)) + sqrt(abs(base) + 1.0)
    (folded, folded * 0.5)
}".to_string()
    }

    #[bench]
    fn bench_try_hot_swap(b: &mut Bencher) {
        let mut runtime = make_vm_runtime(&make_swap_source(1.0));
        let program_a = compile_program(&make_swap_source(0.7));
        let program_b = compile_program(&make_swap_source(1.3));
        let mut toggle = false;

        b.iter(|| {
            let next_program = if toggle {
                program_a.clone()
            } else {
                program_b.clone()
            };
            toggle = !toggle;
            let did_swap = runtime.try_hot_swap(ProgramPayload::VmProgram(next_program));
            black_box(did_swap);
        });
    }

    #[bench]
    fn bench_try_hot_swap_large_voices20(b: &mut Bencher) {
        let mut runtime = make_vm_runtime(&make_large_swap_source(1.0, 20));
        let program_a = compile_program(&make_large_swap_source(0.8, 20));
        let program_b = compile_program(&make_large_swap_source(1.2, 20));
        let mut toggle = false;

        b.iter(|| {
            let next_program = if toggle {
                program_a.clone()
            } else {
                program_b.clone()
            };
            toggle = !toggle;
            let did_swap = runtime.try_hot_swap(ProgramPayload::VmProgram(next_program));
            black_box(did_swap);
        });
    }

    #[bench]
    fn bench_recompile_and_hot_swap_large_voices20(b: &mut Bencher) {
        let mut runtime = make_vm_runtime(&make_large_swap_source(1.0, 20));
        let src_a = make_large_swap_source(0.8, 20);
        let src_b = make_large_swap_source(1.2, 20);
        let mut toggle = false;

        b.iter(|| {
            let src = if toggle { &src_a } else { &src_b };
            toggle = !toggle;
            let program = compile_program(src);
            let did_swap = runtime.try_hot_swap(ProgramPayload::VmProgram(program));
            black_box(did_swap);
        });
    }

    #[bench]
    fn bench_try_hot_swap_large_voices100(b: &mut Bencher) {
        let mut runtime = make_vm_runtime(&make_large_swap_source(1.0, 100));
        let program_a = compile_program(&make_large_swap_source(0.8, 100));
        let program_b = compile_program(&make_large_swap_source(1.2, 100));
        let mut toggle = false;

        b.iter(|| {
            let next_program = if toggle {
                program_a.clone()
            } else {
                program_b.clone()
            };
            toggle = !toggle;
            let did_swap = runtime.try_hot_swap(ProgramPayload::VmProgram(next_program));
            black_box(did_swap);
        });
    }

    #[bench]
    fn bench_recompile_and_hot_swap_large_voices100(b: &mut Bencher) {
        let mut runtime = make_vm_runtime(&make_large_swap_source(1.0, 100));
        let src_a = make_large_swap_source(0.8, 100);
        let src_b = make_large_swap_source(1.2, 100);
        let mut toggle = false;

        b.iter(|| {
            let src = if toggle { &src_a } else { &src_b };
            toggle = !toggle;
            let program = compile_program(src);
            let did_swap = runtime.try_hot_swap(ProgramPayload::VmProgram(program));
            black_box(did_swap);
        });
    }

    #[bench]
    fn bench_try_hot_swap_pattern_test(b: &mut Bencher) {
        let base = load_pattern_test_source();
        let src_a = make_pattern_test_variant(&base, 1);
        let src_b = make_pattern_test_variant(&base, 2);

        let mut runtime = make_vm_runtime(&src_a);
        let program_a = compile_program(&src_a);
        let program_b = compile_program(&src_b);
        let mut toggle = false;

        b.iter(|| {
            let next_program = if toggle {
                program_a.clone()
            } else {
                program_b.clone()
            };
            toggle = !toggle;
            let did_swap = runtime.try_hot_swap(ProgramPayload::VmProgram(next_program));
            black_box(did_swap);
        });
    }

    #[bench]
    fn bench_recompile_and_hot_swap_pattern_test(b: &mut Bencher) {
        let base = load_pattern_test_source();
        let src_a = make_pattern_test_variant(&base, 1);
        let src_b = make_pattern_test_variant(&base, 2);

        let mut runtime = make_vm_runtime(&src_a);
        let mut toggle = false;

        b.iter(|| {
            let src = if toggle { &src_a } else { &src_b };
            toggle = !toggle;
            let program = compile_program(src);
            let did_swap = runtime.try_hot_swap(ProgramPayload::VmProgram(program));
            black_box(did_swap);
        });
    }

    #[bench]
    fn bench_render_sample_by_sample_voices32(b: &mut Bencher) {
        bench_local_buffer_render(b, &make_render_source(32), 256, 1);
    }

    #[bench]
    fn bench_render_block_64_voices32(b: &mut Bencher) {
        bench_local_buffer_render(b, &make_render_source(32), 256, 64);
    }

    #[bench]
    fn bench_render_block_256_voices32(b: &mut Bencher) {
        bench_local_buffer_render(b, &make_render_source(32), 256, 256);
    }

    #[bench]
    fn bench_render_sample_by_sample_dispatch(b: &mut Bencher) {
        bench_local_buffer_render(b, &make_dispatch_source(), 4096, 1);
    }

    #[bench]
    fn bench_render_block_64_dispatch(b: &mut Bencher) {
        bench_local_buffer_render(b, &make_dispatch_source(), 4096, 64);
    }

    #[bench]
    fn bench_render_block_256_dispatch(b: &mut Bencher) {
        bench_local_buffer_render(b, &make_dispatch_source(), 4096, 256);
    }

    #[bench]
    fn bench_render_sample_by_sample_linear_block_kernel(b: &mut Bencher) {
        bench_local_buffer_render(b, &make_linear_block_source(), 4096, 1);
    }

    #[bench]
    fn bench_render_block_256_linear_block_kernel(b: &mut Bencher) {
        bench_local_buffer_render(b, &make_linear_block_source(), 4096, 256);
    }
}
