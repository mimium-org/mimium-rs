use mimium_test::*;
use wasm_bindgen_test::*;

// Test that boxed heap objects are properly garbage collected
// by running the same program multiple times and checking that
// the heap size doesn't grow unboundedly

fn prep_box_gc_test_machine(src: &str) -> mimium_lang::ExecContext {
    let mut ctx = mimium_lang::ExecContext::new([].into_iter(), None, Default::default());
    ctx.prepare_machine(src).expect("Failed to prepare machine");
    let _ = ctx.run_main();
    // Actually execute dsp function to test heap allocation/deallocation
    if let Some(vm) = ctx.get_vm_mut() {
        let _ = vm.execute_entry("dsp");
    }
    ctx
}

#[wasm_bindgen_test(unsupported = test)]
fn box_gc_test() {
    let (_, src) = load_src("box_gc_test.mmm");

    // Run the program multiple times
    // Each run should clean up the heap from the previous run
    let mut ctx1 = prep_box_gc_test_machine(&src);
    let heap_size_1 = ctx1.take_vm().unwrap().heap.len();
    println!("Run 1: heap size = {heap_size_1}");

    let mut ctx2 = prep_box_gc_test_machine(&src);
    let heap_size_2 = ctx2.take_vm().unwrap().heap.len();
    println!("Run 2: heap size = {heap_size_2}");

    let mut ctx3 = prep_box_gc_test_machine(&src);
    let heap_size_3 = ctx3.take_vm().unwrap().heap.len();
    println!("Run 3: heap size = {heap_size_3}");

    // All runs should have the same heap size (no leaks)
    // The heap should be empty after each run completes
    assert_eq!(
        heap_size_1, heap_size_2,
        "Heap size changed between first and second run: {heap_size_1} vs {heap_size_2}"
    );
    assert_eq!(
        heap_size_2, heap_size_3,
        "Heap size changed between second and third run: {heap_size_2} vs {heap_size_3}"
    );

    // Additionally, the heap should be empty after all operations complete
    assert_eq!(
        heap_size_3, 0,
        "Heap should be empty after program completion, but has {heap_size_3} objects"
    );

    println!("✓ Box GC test passed: heap size = {heap_size_3} (improved from initial implementation)");
}

#[wasm_bindgen_test(unsupported = test)]
fn box_gc_stress_test() {
    let (_, src) = load_src("box_gc_test.mmm");

    println!("Running stress test with 10 iterations...");
    // Run many times to ensure no gradual memory leak
    let mut heap_sizes = Vec::new();
    for i in 0..10 {
        let mut ctx = prep_box_gc_test_machine(&src);
        let heap_size = ctx.take_vm().unwrap().heap.len();
        heap_sizes.push(heap_size);
        println!("Iteration {}: heap size = {}", i + 1, heap_size);

        // Every heap size should be the same (no accumulation)
        if i > 0 {
            assert_eq!(
                heap_size, heap_sizes[0],
                "Heap size changed on iteration {}: expected {}, got {}",
                i, heap_sizes[0], heap_size
            );
        }
    }

    // All heap sizes should be zero (no leaks)
    assert!(
        heap_sizes.iter().all(|&size| size == 0),
        "Some runs had objects remaining in heap: {heap_sizes:?}"
    );

    println!(
        "✓ Box GC stress test passed: no memory leaks over {} iterations",
        heap_sizes.len()
    );
}
