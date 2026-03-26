use mimium_test::*;
use wasm_bindgen_test::*;

fn prep_box_gc_test_machine(src: &str) -> mimium_lang::ExecContext {
    let mut ctx = mimium_lang::ExecContext::new([].into_iter(), None, Default::default());
    ctx.prepare_machine(src).expect("Failed to prepare machine");
    let _ = ctx.run_main();
    if let Some(vm) = ctx.get_vm_mut() {
        let _ = vm.execute_entry("dsp");
    }
    ctx
}

#[wasm_bindgen_test(unsupported = test)]
fn box_gc_stress_test() {
    let (_, src) = load_src("box_gc_test.mmm");

    // Keep this as a separate integration binary so each run gets a fresh
    // process and does not inherit runtime-global state from the basic GC test.
    let iteration_count = 5;
    println!("Running stress test with {iteration_count} iterations...");

    let mut heap_sizes = Vec::new();
    for i in 0..iteration_count {
        let mut ctx = prep_box_gc_test_machine(&src);
        let heap_size = ctx.take_vm().unwrap().heap.len();
        heap_sizes.push(heap_size);
        println!("Iteration {}: heap size = {}", i + 1, heap_size);

        if i > 0 {
            assert_eq!(
                heap_size, heap_sizes[0],
                "Heap size changed on iteration {}: expected {}, got {}",
                i, heap_sizes[0], heap_size
            );
        }
    }

    assert!(
        heap_sizes.iter().all(|&size| size == heap_sizes[0]),
        "Heap size should stay constant, got: {heap_sizes:?}"
    );

    println!(
        "✓ Box GC stress test passed: no memory leaks over {} iterations",
        heap_sizes.len()
    );
}