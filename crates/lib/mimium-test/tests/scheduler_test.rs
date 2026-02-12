use mimium_audiodriver::{
    backends::local_buffer::LocalBufferDriver,
    driver::{Driver, RuntimeData},
};
use mimium_lang::{ExecContext, plugin::Plugin};
use mimium_test::*;
use wasm_bindgen_test::*;

#[wasm_bindgen_test(unsupported = test)]
fn scheduler_global_recursion() {
    let res = run_file_with_scheduler("scheduler_global_recursion.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn scheduler_multiple_at_sametime() {
    let res = run_file_with_scheduler("scheduler_multiple_at_sametime.mmm", 5).unwrap();
    let ans = vec![0.0, 2.0, 4.0, 6.0, 8.0];
    assert_eq!(res, ans);
}

/// On the native VM, recursion with @now causes infinite recursion and panics.
/// On the WASM backend, the scheduler trampoline is not yet implemented, so no
/// panic occurs.  We skip this test when running under WASM.
#[wasm_bindgen_test(unsupported = test)]
#[should_panic]
fn scheduler_invalid() {
    let _ = run_file_with_scheduler("scheduler_invalid.mmm", 10);
}

#[wasm_bindgen_test(unsupported = test)]
fn scheduler_counter() {
    let res = run_file_with_scheduler("scheduler_counter.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn scheduler_counter_indirect() {
    let res = run_file_with_scheduler("scheduler_counter_indirect.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn scheduler_reactive() {
    let res = run_file_with_scheduler("scheduler_reactive.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 3.0, 3.0];
    assert_eq!(res, ans);
}

fn prep_gc_test_machine(times: usize, src: &str) -> LocalBufferDriver {
    let mut driver = LocalBufferDriver::new(times);
    let driverplug: Box<dyn Plugin> = Box::new(driver.get_as_plugin());
    let mut ctx = ExecContext::new([driverplug].into_iter(), None, Default::default());
    ctx.add_system_plugin(mimium_scheduler::get_default_scheduler_plugin());
    let _ = ctx.prepare_machine(src);
    let _ = ctx.run_main();
    let runtimedata = {
        let ctxmut: &mut ExecContext = &mut ctx;
        RuntimeData::try_from(ctxmut).unwrap()
    };
    driver.init(runtimedata, None);
    driver
}
// Check that the number of closures does not grow over time.
// This test inspects VM internals (closure storage) and is only
// meaningful on the native VM backend.
#[wasm_bindgen_test(unsupported = test)]
fn scheduler_gc_test() {
    if should_use_wasm_backend() {
        // GC internals are not accessible in the WASM runtime.
        return;
    }
    let (_, src) = load_src("scheduler_counter_indirect.mmm");
    let mut driver1 = prep_gc_test_machine(2, &src);
    driver1.play();
    let first = driver1
        .vmdata
        .unwrap()
        .downcast_runtime_ref::<mimium_audiodriver::driver::VmDspRuntime>()
        .unwrap()
        .vm
        .closures
        .len();

    let mut driver2 = prep_gc_test_machine(3, &src);
    driver2.play();
    let second = driver2
        .vmdata
        .unwrap()
        .downcast_runtime_ref::<mimium_audiodriver::driver::VmDspRuntime>()
        .unwrap()
        .vm
        .closures
        .len();

    let mut driver3 = prep_gc_test_machine(4, &src);
    driver3.play();
    let third = driver3
        .vmdata
        .unwrap()
        .downcast_runtime_ref::<mimium_audiodriver::driver::VmDspRuntime>()
        .unwrap()
        .vm
        .closures
        .len();
    assert!(first == second && second == third)
}
