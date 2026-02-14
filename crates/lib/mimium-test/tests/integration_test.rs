use mimium_audiodriver::driver::{Driver, RuntimeData};
use mimium_lang::utils::error::report;
use mimium_test::*;
use wasm_bindgen_test::*;

fn run_simple_test(expr: &str, expect: f64, times: u64) {
    let src = format!(
        "fn test(hoge){{
    {expr}
}}
fn dsp(){{
    test(2.0)
}}"
    );
    let res = run_source_test(&src, times, false, None);
    match res {
        Ok(res) => {
            let ans = [expect].repeat(times as usize);
            assert_eq!(res, ans, "expr: {expr}");
        }
        Err(errs) => {
            report(&src, "(from template)".into(), &errs);
            panic!("invalid syntax");
        }
    }
}

#[wasm_bindgen_test(unsupported = test)]
fn simple_arithmetic() {
    // unary
    run_simple_test("1.0", 1.0, 3);
    run_simple_test("-1.0", -1.0, 3);
    run_simple_test("- -1.0", 1.0, 3);
    run_simple_test("-hoge", -2.0, 3);
    run_simple_test("-(-hoge)", 2.0, 3);
    run_simple_test("-cos(0.0)", -1.0, 3);

    // binary
    run_simple_test("hoge+1.0", 3.0, 3);
    run_simple_test("hoge-1.0", 1.0, 3);
    run_simple_test("hoge*3.0", 6.0, 3);
    run_simple_test("hoge/2.0", 1.0, 3);
    run_simple_test("hoge^3.0", 8.0, 3);

    // complex expression to test the evaluation order
    run_simple_test("hoge*10.0+hoge/10.0+1.0", 21.2, 3);
    run_simple_test("1.0+hoge^2.0*1.5", 7.0, 3);
}

#[wasm_bindgen_test(unsupported = test)]
fn parser_firstbreak() {
    let res = run_file_test_mono("parser_firstbreak.mmm", 1).unwrap();
    let ans = vec![0.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn array_test() {
    let res = run_file_test_mono("array_test.mmm", 1).unwrap();
    let ans = vec![70.0]; // 10.0 + 15.0 + 40.0
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn array_tuple() {
    let res = run_file_test_mono("array_tuple.mmm", 1).unwrap();
    let ans = vec![21.0]; // 1.0+ 2.0 + 3.0 + 4.0 + 5.0 + 6.0
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn array_length() {
    let res = run_file_test_mono("array_length.mmm", 1).unwrap();
    let ans = vec![5.0]; // 1.0 + 2.0 + 3.0 + 4.0 + 5.0
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn split_tail() {
    let res = run_file_test_mono("split_tail.mmm", 1).unwrap();
    let ans = vec![24.0]; // (1+2+3)*4 = 6*4 = 24
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn split_tail_macro() {
    let res = run_file_test_mono("split_tail_macro.mmm", 1).unwrap();
    let ans = vec![4.0]; // tail element of [1,2,3,4]
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn split_head() {
    let res = run_file_test_mono("split_head.mmm", 1).unwrap();
    let ans = vec![9.0]; // 1*(2+3+4) = 1*9 = 9
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn split_head_macro() {
    let res = run_file_test_mono("split_head_macro.mmm", 1).unwrap();
    let ans = vec![1.0]; // head element of [1,2,3,4]
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn lift_arrayf_extended() {
    let res = run_file_test_mono("lift_arrayf_extended.mmm", 1).unwrap();
    let ans = vec![65.0]; // 5.0 + 10.0 + 20.0 + 30.0
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn recursion() {
    let res = run_file_test_mono("recursion.mmm", 1).unwrap();
    let ans = vec![5.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn placeholder() {
    let res = run_file_test_mono("placeholder.mmm", 1).unwrap();
    let ans = vec![123.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn pipe() {
    let res = run_file_test_stereo("pipe.mmm", 1).unwrap();
    let ans = vec![123.0, 123.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn counter() {
    let res = run_file_test_mono("counter.mmm", 10).unwrap();
    let ans = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn statefn() {
    let res = run_file_test_mono("statefn.mmm", 10).unwrap();
    let ans = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn statefn2_same() {
    let res = run_file_test_mono("statefn2_same.mmm", 3).unwrap();
    let ans = vec![6.0, 12.0, 18.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn statefn2() {
    let res = run_file_test_mono("statefn2.mmm", 3).unwrap();
    let ans = vec![8.0, 16.0, 24.];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn loopcounter() {
    let res = run_file_test_mono("loopcounter.mmm", 10).unwrap();
    let ans = vec![1.0, 2.0, 3.0, 4.0, 0.0, 1.0, 2.0, 3.0, 4.0, 0.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn primitive_log() {
    let res = run_file_test_mono("primitive_log.mmm", 1).unwrap();
    let ans = [2.0f64.log10()];
    let r = (res[0] - ans[0]).abs() < f64::EPSILON;
    assert!(r, "res:{} expected: {}", res[0], ans[0]);
}

#[wasm_bindgen_test(unsupported = test)]
fn primitive_sin() {
    let res = run_file_test_mono("primitive_sin.mmm", 1).unwrap();
    let ans = [0.0];
    let r = (res[0] - ans[0]).abs() < f64::EPSILON;
    assert!(r);
}
#[wasm_bindgen_test(unsupported = test)]
fn primitive_sqrt() {
    let res = run_file_test_mono("primitive_sqrt.mmm", 1).unwrap();
    let ans = [2.0f64.sqrt()];
    let r = (res[0] - ans[0]).abs() < f64::EPSILON;
    assert!(r);
}

#[wasm_bindgen_test(unsupported = test)]
fn primitive_min() {
    let res = run_file_test_mono("primitive_min.mmm", 1).unwrap();
    let ans = [2.1];
    let r = (res[0] - ans[0]).abs() < f64::EPSILON;
    assert!(r);
}

#[wasm_bindgen_test(unsupported = test)]
fn primitive_max() {
    let res = run_file_test_mono("primitive_max.mmm", 1).unwrap();
    let ans = [3.5];
    let r = (res[0] - ans[0]).abs() < f64::EPSILON;
    assert!(r);
}

#[wasm_bindgen_test(unsupported = test)]
fn primitive_minmax_combo() {
    let res = run_file_test_mono("primitive_minmax_combo.mmm", 1).unwrap();
    let ans = [7.0]; // min(5.0, 3.0) + max(3.0, 4.0) = 3.0 + 4.0
    let r = (res[0] - ans[0]).abs() < f64::EPSILON;
    assert!(r);
}

#[wasm_bindgen_test(unsupported = test)]
fn adsr_simple() {
    // Nested conditionals in stateful function (JmpIf within merge blocks).
    // This is a regression test for WASM backend's emit_merge_block handling.
    let res = run_file_test_mono("adsr_simple.mmm", 7).unwrap();
    let ans = vec![1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 5.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn ifblock() {
    let res = run_file_test_mono("if.mmm", 1).unwrap();
    let ans = vec![4120.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn nested_ifblock() {
    let res = run_file_test_mono("nested_if.mmm", 1).unwrap();
    let ans = vec![119.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn match_int() {
    let res = run_file_test_mono("match_int.mmm", 1).unwrap();
    let ans = vec![900.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn letmulti() {
    let res = run_file_test_mono("let_multi.mmm", 1).unwrap();
    let ans = vec![3.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn let_tuple() {
    let res = run_file_test_mono("let_tuple.mmm", 1).unwrap();
    let ans = vec![18.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn let_tuple_nested() {
    let res = run_file_test_mono("let_tuple_nested.mmm", 1).unwrap();
    let ans = vec![34.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn closure_tuple_escape() {
    let res = run_file_test_mono("closure_tuple_escape.mmm", 2).unwrap();
    let ans = vec![44.0, 44.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn state_tuple() {
    let res = run_file_test_stereo("state_tuple.mmm", 3).unwrap();
    let ans = vec![1.0, 2.0, 2.0, 4.0, 3.0, 6.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn closure_open() {
    let res = run_file_test_mono("closure_open.mmm", 1).unwrap();
    let ans = vec![4.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn closure_open_3nested() {
    let res = run_file_test_mono("closure_open_3nested.mmm", 2).unwrap();
    let ans = vec![2.0, 2.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn closure_open_inline() {
    let res = run_file_test_mono("closure_open_inline.mmm", 2).unwrap();
    let ans = vec![2.0, 2.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn closure_closed() {
    let res = run_file_test_mono("closure_closed.mmm", 2).unwrap();
    let ans = vec![-6.0, -6.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn closure_argument() {
    let res = run_file_test_mono("closure_argument.mmm", 1).unwrap();
    let ans = vec![24.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn stateful_closure() {
    let res = run_file_test_mono("stateful_closure.mmm", 10).unwrap();
    let ans = vec![
        20.3,
        20.599999999999998,
        20.900000000000002,
        21.2,
        21.5,
        21.8,
        22.099999999999998,
        22.400000000000002,
        22.7,
        23.0,
    ];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn closure_counter() {
    let res = run_file_test_mono("closure_counter.mmm", 5).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn closure_counter2() {
    let res = run_file_test_mono("closure_counter2.mmm", 5).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn closure_escape_3nested() {
    let res = run_file_test_mono("closure_escape_3nested.mmm", 5).unwrap();
    let ans = vec![1.0, 2.0, 3.0, 4.0, 5.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn closure_counter_tuple() {
    let res = run_file_test_stereo("closure_counter_tuple.mmm", 5).unwrap();
    #[rustfmt::skip]
    let ans = vec![
        0.0,  0.0,
        1.0, -1.0,
        2.0, -2.0,
        3.0, -3.0,
        4.0, -4.0
    ];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn closure_counter_multistage() {
    let res = run_file_test_stereo("closure_counter_multistage.mmm", 1).unwrap();
    let ans = vec![1.0, 2.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn test_tuple_global() {
    let res = run_file_test_mono("tuple_global.mmm", 5).unwrap();
    let ans = vec![100.0, 100.0, 100.0, 100.0, 100.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn test_tuple_global2() {
    let res = run_file_test_mono("tuple_global2.mmm", 5).unwrap();
    let ans = vec![100.0, 100.0, 100.0, 100.0, 100.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn hof_state() {
    let res = run_file_test_mono("hof_state.mmm", 10).unwrap();
    let ans = vec![
        0.6000000000000001,
        1.2000000000000002,
        1.8000000000000003,
        2.4000000000000004,
        3.0,
        3.5999999999999996,
        4.199999999999999,
        4.8,
        5.3999999999999995,
        5.999999999999999,
    ];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn hof_infer() {
    let res = run_file_test_mono("hof_infer.mmm", 10).unwrap();
    let ans = vec![
        0.6000000000000001,
        1.2000000000000002,
        1.8000000000000003,
        2.4000000000000004,
        3.0,
        3.5999999999999996,
        4.199999999999999,
        4.8,
        5.3999999999999995,
        5.999999999999999,
    ];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn simple_stereo() {
    let res = run_file_test_stereo("simple_stereo.mmm", 3).unwrap();
    let ans = vec![1.0, 2.0, 1.0, 2.0, 1.0, 2.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn tuple_args() {
    let res = run_file_test_stereo("tuple_args.mmm", 3).unwrap();
    let ans = vec![30.0, 50.0, 30.0, 50.0, 30.0, 50.0];
    assert_eq!(res, ans);
}

// implement one-sample delay on mimium with `self`
#[wasm_bindgen_test(unsupported = test)]
fn fb_mem() {
    let res = run_file_test_stereo("fb_mem.mmm", 10).unwrap();
    let ans = vec![
        1.0, 0.0, 2.0, 1.0, 3.0, 2.0, 4.0, 3.0, 5.0, 4.0, 6.0, 5.0, 7.0, 6.0, 8.0, 7.0, 9.0, 8.0,
        10.0, 9.0,
    ];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn fb_mem2() {
    let res = run_file_test_stereo("fb_mem2.mmm", 10).unwrap();
    let ans = vec![
        1.0, 0.0, 2.0, 0.0, 3.0, 1.0, 4.0, 2.0, 5.0, 3.0, 6.0, 4.0, 7.0, 5.0, 8.0, 6.0, 9.0, 7.0,
        10.0, 8.0,
    ];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn fb_mem3_state_size() {
    test_state_sizes(
        "fb_mem3.mmm",
        [("counter", 1), ("mem_by_hand", 4), ("dsp", 5)],
    );
}

#[wasm_bindgen_test(unsupported = test)]
fn fb_and_stateful_call() {
    let res = run_file_test_mono("fb_and_stateful_call.mmm", 10).unwrap();
    let ans = vec![1.0, 3.0, 6.0, 10.0, 15.0, 21.0, 28.0, 36.0, 45.0, 55.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn stateful_conditional() {
    let res = run_file_test_mono("stateful_conditional.mmm", 10).unwrap();
    let ans = vec![2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn delay() {
    let res = run_file_test_mono("delay.mmm", 10).unwrap();
    let ans = vec![0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn delay2() {
    let res = run_file_test_mono("delay2.mmm", 10).unwrap();
    let ans = vec![0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn include_file() {
    let res = run_file_test_mono("test_include.mmm", 10).unwrap();
    let ans = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn if_state() {
    let res = run_file_test_stereo("if_state.mmm", 10).unwrap();
    let ans = vec![
        1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0, 5.0, 0.0, 6.0, 0.0, 7.0, 0.0, 8.0, 0.0, 9.0, 0.0,
        10.0, 0.0,
    ];
    assert_eq!(res, ans);
}
#[test]
fn shadowing() {
    let res = run_file_test_mono("shadowing.mmm", 1).unwrap();
    let ans = vec![2.0];
    assert_eq!(res, ans);
}
#[test]
fn shadowing_assign() {
    let res = run_file_test_mono("shadowing_assign.mmm", 1).unwrap();
    let ans = vec![7.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn many_errors() {
    let res = run_error_test("many_errors.mmm", false);
    //todo! check error types
    assert_eq!(res.len(), 9);
}
#[wasm_bindgen_test(unsupported = test)]
fn hof_typefail() {
    //check false positive
    let res = run_error_test("hof_typefail.mmm", false);
    //todo! check error types
    assert_eq!(res.len(), 1);
}

#[wasm_bindgen_test(unsupported = test)]
fn error_include_itself() {
    let res = run_error_test("error_include_itself.mmm", false);
    assert_eq!(res.len(), 1);
    assert!(
        res[0]
            .get_message()
            .contains("File tried to include itself recusively:"),
        "{:?}",
        res[0]
    )
}

#[wasm_bindgen_test(unsupported = test)]
fn typing_tuple_fail() {
    let res = run_error_test("typing_tuple_fail.mmm", false);
    assert_eq!(res.len(), 1);
    assert!(res[0].get_message().contains("Type mismatch"))
}
#[wasm_bindgen_test(unsupported = test)]
fn block_local_scope() {
    let res = run_file_test_mono("block_local_scope.mmm", 1).unwrap();
    let ans = vec![3.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn block_local_scope_fail() {
    let res = run_error_test("block_local_scope_fail.mmm", false);
    assert_eq!(res.len(), 1);
    assert!(
        res[0]
            .get_message()
            .contains("Variable \"local1\" not found in this scope")
    )
}

#[wasm_bindgen_test(unsupported = test)]
fn fail_invalid_stage() {
    let res = run_error_test("fail_invalid_stage.mmm", false);
    assert_eq!(res.len(), 1);
    assert!(
        res[0]
            .get_message()
            .contains("Variable x is defined in stage 0 but accessed from stage 1")
    )
}

#[wasm_bindgen_test(unsupported = test)]
fn test_phase_reset() {
    let res = run_file_test_stereo("test_phase_reset.mmm", 10).unwrap();
    let ans = vec![
        0.0, 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0, 5.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0,
        1.0, 0.0,
    ];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn many_comments() {
    let res = run_file_test_mono("many_comments.mmm", 1).unwrap();
    let ans = vec![0.0];
    assert_eq!(res, ans);
}

#[test]
fn record_creation() {
    let res = run_file_test_mono("record_creation.mmm", 1).unwrap();
    let ans = vec![79.0]; // myrec.testb.1
    assert_eq!(res, ans);
}
#[test]
fn record_infer() {
    let res = run_file_test_mono("record_infer.mmm", 1).unwrap();
    let ans = vec![81.5];
    assert_eq!(res, ans);
}
#[test]
fn record_shuffle_field() {
    let res = run_file_test_mono("record_shuffle_field.mmm", 1).unwrap();
    let ans = vec![100.0];
    assert_eq!(res, ans);
}

#[test]
fn record_pattern_capture() {
    let res = run_file_test_mono("record_pattern_capture.mmm", 1).unwrap();
    let ans = vec![300.0]; // v1 + v2
    assert_eq!(res, ans);
}

#[test]
fn record_assign_field() {
    let res = run_file_test_mono("record_assign_field.mmm", 1).unwrap();
    let ans = vec![20.0];
    assert_eq!(res, ans);
}

#[test]
fn record_update() {
    let res = run_file_test_mono("record_update.mmm", 1).unwrap();
    let ans = vec![6000.0]; // 4000.0 + 2000.0
    assert_eq!(res, ans);
}

#[test]
fn record_update_immutable() {
    let res = run_file_test_mono("record_update_immutable.mmm", 1).unwrap();
    let ans = vec![10.0]; // 0.0 + 10.0 - original record unchanged
    assert_eq!(res, ans);
}

#[test]
fn parameter_pack_tuple() {
    let res = run_file_test_mono("parameter_pack_tuple.mmm", 1).unwrap();
    let ans = vec![13.0]; // 1 + 5 + 7
    assert_eq!(res, ans);
}

#[test]
fn parameter_pack_tuple_fail() {
    let res = run_error_test("parameter_pack_tuple_fail.mmm", false);
    assert_eq!(res.len(), 1);
}
#[test]
fn parameter_pack_record() {
    let res = run_file_test_mono("parameter_pack_record.mmm", 1).unwrap();
    let ans = vec![13.0]; // 1 + 5 + 7
    assert_eq!(res, ans);
}
#[test]
fn parameter_pack_record_order() {
    let res = run_file_test_mono("parameter_pack_record_order.mmm", 1).unwrap();
    let ans = vec![-4.0]; // 2*(5-7)
    assert_eq!(res, ans);
}

#[test]
fn parameter_pack_record_fail() {
    let res = run_error_test("parameter_pack_record_fail.mmm", false);
    assert_eq!(res.len(), 1);
}
#[test]
fn parameter_pack_record_fail2() {
    let res = run_error_test("parameter_pack_record_fail2.mmm", false);
    assert_eq!(res.len(), 1);
}

#[test]
fn record_imcomplete() {
    let res = run_file_test_mono("record_imcomplete.mmm", 1).unwrap();
    let ans = vec![701.0]; // 2*(5-7)
    assert_eq!(res, ans);
}

#[test]
fn multistage() {
    let res = run_file_test_mono("multistage.mmm", 1).unwrap();
    let ans = vec![32.0];
    assert_eq!(res, ans);
}
#[test]
fn multistage_macro() {
    let res = run_file_test_mono("multistage_macro.mmm", 1).unwrap();
    let ans = vec![8.0];
    assert_eq!(res, ans);
}
#[test]
fn multistage_globalsyntax() {
    let res = run_file_test_mono("multistage_globalsyntax.mmm", 1).unwrap();
    let ans = vec![8.0];
    assert_eq!(res, ans);
}

#[test]
fn multistage_lift() {
    let res = run_file_test_mono("multistage_lift.mmm", 1).unwrap();
    let ans = vec![8.0];
    assert_eq!(res, ans);
}
#[test]
fn multistage_explicit_type() {
    let res = run_file_test_mono("multistage_explicit_type.mmm", 1).unwrap();
    let ans = vec![32.0];
    assert_eq!(res, ans);
}

#[test]
fn probe_macro() {
    let (_, src) = load_src("probe_macro.mmm");

    // Create a custom execution with the guitools plugin
    let mut driver = mimium_audiodriver::backends::local_buffer::LocalBufferDriver::new(1);
    let audiodriverplug: Box<dyn mimium_lang::plugin::Plugin> = Box::new(driver.get_as_plugin());
    let mut ctx = mimium_lang::ExecContext::new(
        [audiodriverplug].into_iter(),
        None,
        mimium_lang::Config::default(),
    );

    // Add the guitools system plugin
    ctx.add_system_plugin(mimium_guitools::GuiToolPlugin::default());

    ctx.prepare_machine(&src).unwrap();
    let _ = ctx.run_main();
    let runtimedata = {
        let ctxmut: &mut mimium_lang::ExecContext = &mut ctx;
        RuntimeData::try_from(ctxmut).unwrap()
    };
    driver.init(runtimedata, None);
    driver.play();
    let res = driver.get_generated_samples().to_vec();

    let ans = vec![42.0]; // Probe should pass through the value
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn twodelay() {
    let res = run_file_test_stereo("twodelay.mmm", 5).unwrap();
    let ans = vec![0.0, 0.0, 4.0, 0.0, 4.0, 6.0, 4.0, 6.0, 4.0, 6.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn module_basic() {
    // mymath::add(2.0, 3.0) + mymath::mul(4.0, 5.0) = 5.0 + 20.0 = 25.0
    let res = run_file_test_mono("module_basic.mmm", 3).unwrap();
    let ans = vec![25.0, 25.0, 25.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn module_nested() {
    // outer::exposed() calls inner::secret() which returns 42.0
    let res = run_file_test_mono("module_nested.mmm", 3).unwrap();
    let ans = vec![42.0, 42.0, 42.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn module_visibility_fail() {
    // Test that private module members cannot be accessed from outside
    let res = run_error_test("module_visibility_fail.mmm", false);
    assert_eq!(res.len(), 1);
    assert!(
        res[0].get_message().contains("is private"),
        "Expected 'is private' error message, got: {:?}",
        res[0].get_message()
    );
}

#[wasm_bindgen_test(unsupported = test)]
fn module_use() {
    // Test that `use mymath::add` allows using `add` without qualification
    let res = run_file_test_mono("module_use.mmm", 3).unwrap();
    let ans = vec![15.0, 15.0, 15.0]; // add(10.0, 5.0) = 15.0
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn module_use_private_fail() {
    // Test that `use` statement respects visibility - using private function should fail
    let res = run_error_test("module_use_private_fail.mmm", false);
    assert_eq!(res.len(), 1);
    assert!(
        res[0].get_message().contains("is private"),
        "Expected 'is private' error message, got: {:?}",
        res[0].get_message()
    );
}

#[wasm_bindgen_test(unsupported = test)]
fn module_external() {
    // Test external file module: mod foo; syntax (Rust-like)
    // Loads module_external_math.mmm from same directory
    let res = run_file_test_mono("module_external.mmm", 3).unwrap();
    let ans = vec![15.0, 15.0, 15.0]; // add(1,2) + mul(3,4) = 3 + 12 = 15
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn module_external_use() {
    // Test external file module with use statement
    let res = run_file_test_mono("module_external_use.mmm", 3).unwrap();
    let ans = vec![12.0, 12.0, 12.0]; // add(5, 7) = 12
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn module_relative_path() {
    // Test relative path resolution: inner::secret() from within outer module
    // resolves to outer::inner::secret()
    let res = run_file_test_mono("module_relative_path.mmm", 3).unwrap();
    let ans = vec![42.0, 42.0, 42.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn module_use_multiple() {
    // Test multiple imports: use math::{double, triple}
    let res = run_file_test_mono("module_use_multiple.mmm", 3).unwrap();
    let ans = vec![50.0, 50.0, 50.0]; // double(10) + triple(10) = 20 + 30 = 50
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn module_use_wildcard() {
    // Test wildcard import: use math::*
    let res = run_file_test_mono("module_use_wildcard.mmm", 3).unwrap();
    let ans = vec![50.0, 50.0, 50.0]; // double(10) + triple(10) = 20 + 30 = 50
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn module_pub_use() {
    // Test pub use for re-exporting
    let res = run_file_test_mono("module_pub_use.mmm", 3).unwrap();
    let ans = vec![42.0, 42.0, 42.0]; // api::helper() returns 42.0
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn module_macro() {
    // Test pub use for re-exporting
    let res = run_file_test_mono("module_macro.mmm", 3).unwrap();
    let ans = vec![2.0, 2.0, 2.0];
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn module_macro_sugar() {
    // Test pub use for re-exporting
    let res = run_file_test_mono("module_macro_sugar.mmm", 3).unwrap();
    let ans = vec![2.0, 2.0, 2.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn sum_type_basic() {
    // Test union type float | int with constructor pattern matching
    let res = run_file_test_mono("sum_type_basic.mmm", 1).unwrap();
    let ans = vec![3.0]; // test_union(5)=1 + test_union(99.0)=2
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn enum_basic() {
    // Test user-defined enum type: type MyEnum = One | Two | Three
    let res = run_file_test_mono("enum_basic.mmm", 1).unwrap();
    let ans = vec![6.0]; // test(One)=1 + test(Two)=2 + test(Three)=3 = 6
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn enum_complex() {
    let res = run_file_test_mono("enum_complex.mmm", 1).unwrap();
    let ans = vec![26.0]; // 3*1 + 4*2 + 5*3 = 3 + 8 + 15 = 26
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn enum_multi_scrutinee() {
    let res = run_file_test_mono("enum_multi_scrutinee.mmm", 1).unwrap();
    let ans = vec![160.0]; // test_simple(1,1)=10 + test_simple(1,2)=20 + test_simple(2,1)=30 + test_simple(3,3)=100 = 160
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn enum_macro() {
    let res = run_file_test_mono("enum_macro.mmm", 1).unwrap();
    let ans = vec![3.0]; // mymacro(One(100.0))=1.0 + mymacro(Two(200.0))=2.0 = 3.0
    assert_eq!(res, ans);
}
#[wasm_bindgen_test(unsupported = test)]
fn enum_multi_scrutinee2() {
    let res = run_file_test_mono("enum_multi_scrutinee2.mmm", 1).unwrap();
    let ans = vec![213.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn enum_simple_tuple() {
    // Simplified tuple pattern matching test: match (One(1), One(2))
    let res = run_file_test_mono("enum_simple_tuple.mmm", 1).unwrap();
    let ans = vec![3.0]; // One(1) + One(2) = 3
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn type_alias_simple() {
    // Test simple type alias: type alias Freq = float
    let res = run_file_test_mono("type_alias_simple.mmm", 1).unwrap();
    let ans = vec![880.0]; // let x: Freq = 440.0; x * 2.0 = 880.0
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn type_alias_comprehensive() {
    // Test multiple type aliases used in functions
    // oscillator(440.0, 0.5) * 2.0 = (440.0 * 0.5) * 2.0 = 220.0 * 2.0 = 440.0
    let res = run_file_test_mono("type_alias_comprehensive.mmm", 1).unwrap();
    let ans = vec![440.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn stateful_match() {
    // Test basic enum matching without stateful functions first
    // move_player(Up) = 1.0, move_player(Down) = 2.0
    // Total: 1.0 + 2.0 = 3.0
    let res = run_file_test_mono("stateful_match.mmm", 1).unwrap();
    let ans = vec![3.0];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn stateful_in_match() {
    // Test stateful function called within match expression
    // dir = Up, so counter(1.0) is called
    // First call: 1.0 + 0 = 1.0 (initial state 0)
    let res = run_file_test_mono("stateful_in_match.mmm", 1).unwrap();
    let ans = vec![1.0];
    assert_eq!(res, ans);
}

// #[wasm_bindgen_test(unsupported = test)]
// fn map_record() {
//     let res = run_file_test_stereo("map_record.mmm", 1).unwrap();
//     // src = [{freq = 1000.0, time = 0.0}, {freq = 2000.0, time = 2.0}]
//     // mapped = map(src, |v| {freq = v.freq*2.0, time = v.time+10.0} )
//     // ansf = mapped[0].freq + mapped[1].freq = 2000 + 4000 = 6000
//     // anst = mapped[0].time + mapped[1].time = 10.0 + 12.0 = 22.0
//     let ans = vec![6000.0, 22.0];
//     assert_eq!(res, ans);
// }
#[wasm_bindgen_test(unsupported = test)]
fn union_type_multi_arg() {
    // Test union type with multi-arg syntax (treated as tuple)
    // calculateArea(Rectangle((5.0, 2.5))) = 5.0 * 2.5 = 12.5
    let res = run_file_test_mono("union_type_multi_arg.mmm", 1).unwrap();
    let ans = vec![12.5];
    assert_eq!(res, ans);
}

#[wasm_bindgen_test(unsupported = test)]
fn mixed_type_syntax() {
    // Test mixing type aliases and union types
    // generateWave(Square, 440.0, 0.5) = 440.0 * 0.5 * 1.5 = 330.0
    let res = run_file_test_mono("mixed_type_syntax.mmm", 1).unwrap();
    let ans = vec![330.0];
    assert_eq!(res, ans);
}

// ============ Match Exhaustiveness Tests (should fail) ============

#[wasm_bindgen_test(unsupported = test)]
fn match_exhaustiveness_fail_union() {
    // Test non-exhaustive match on union type (missing 'string' branch)
    let errs = run_error_test("match_exhaustiveness_fail_union.mmm", false);
    assert!(!errs.is_empty(), "Expected exhaustiveness error");

    // Check that the error message mentions non-exhaustive match
    let err_message = errs[0].get_message();
    assert!(
        err_message.contains("not exhaustive"),
        "Expected 'not exhaustive' in error message, got: {err_message}"
    );

    // Check that 'string' is mentioned as missing
    assert!(
        err_message.contains("string")
            || errs[0]
                .get_labels()
                .iter()
                .any(|(_, label)| label.contains("string")),
        "Expected 'string' to be mentioned as missing pattern, got: {err_message}"
    );
}

#[wasm_bindgen_test(unsupported = test)]
fn match_exhaustiveness_fail_enum() {
    // Test non-exhaustive match on enum type (missing 'Third' constructor)
    let errs = run_error_test("match_exhaustiveness_fail_enum.mmm", false);
    assert!(!errs.is_empty(), "Expected exhaustiveness error");

    // Check that the error message mentions non-exhaustive match
    let err_message = errs[0].get_message();
    assert!(
        err_message.contains("not exhaustive"),
        "Expected 'not exhaustive' in error message, got: {err_message}",
    );

    // Check that 'Third' is mentioned as missing
    assert!(
        err_message.contains("Third")
            || errs[0]
                .get_labels()
                .iter()
                .any(|(_, label)| label.contains("Third")),
        "Expected 'Third' to be mentioned as missing pattern, got: {err_message}",
    );
}

// ============ Recursive Type Declaration Tests ============

#[wasm_bindgen_test(unsupported = test)]
fn type_recursive_invalid_direct() {
    // Test invalid direct recursive type alias: type A = A
    let errs = run_error_test("type_recursive_invalid_direct.mmm", false);
    assert!(!errs.is_empty(), "Expected circular type error");

    let err_message = errs[0].get_message();
    assert!(
        err_message.contains("ircular") || err_message.contains("ecursive"),
        "Expected circular/recursive type error, got: {err_message}"
    );
}

#[wasm_bindgen_test(unsupported = test)]
fn type_recursive_invalid_mutual() {
    // Test invalid mutual recursive type aliases: type A = B, type B = A
    let errs = run_error_test("type_recursive_invalid_mutual.mmm", false);
    assert!(!errs.is_empty(), "Expected circular type error");

    let err_message = errs[0].get_message();
    assert!(
        err_message.contains("ircular") || err_message.contains("ecursive"),
        "Expected circular/recursive type error, got: {err_message}"
    );
}

#[wasm_bindgen_test(unsupported = test)]
fn type_recursive_invalid_list() {
    // Test recursive type in constructor: type List = Nil | Cons(float, List)
    // This should still fail because it doesn't use 'rec' keyword
    let errs = run_error_test("type_recursive_invalid_list.mmm", false);
    assert!(!errs.is_empty(), "Expected recursive type error");

    let err_message = errs[0].get_message();
    assert!(
        err_message.contains("circular") || err_message.contains("ecursive"),
        "Expected circular/recursive type error, got: {err_message}"
    );
}

#[wasm_bindgen_test(unsupported = test)]
fn type_recursive_list() {
    let (file, src) = load_src("type_recursive_list.mmm");
    let res = run_source_test(&src, 1, false, Some(file));
    match res {
        Ok(res) => assert_eq!(res, vec![6.0]),
        Err(errs) => {
            for err in &errs {
                eprintln!("Error: {}", err.get_message());
            }
            panic!("type_recursive_list failed");
        }
    }
}

#[wasm_bindgen_test(unsupported = test)]
fn type_recursive_tree() {
    let (file, src) = load_src("type_recursive_tree.mmm");
    let res = run_source_test(&src, 1, false, Some(file));
    match res {
        Ok(res) => assert_eq!(res, vec![28.0]),
        Err(errs) => {
            for err in &errs {
                eprintln!("Error: {}", err.get_message());
            }
            panic!("type_recursive_tree failed");
        }
    }
}
#[wasm_bindgen_test(unsupported = test)]
fn type_recursive_tree_macro() {
    let (file, src) = load_src("type_recursive_tree_macro.mmm");
    let res = run_source_test(&src, 1, false, Some(file));
    match res {
        Ok(res) => assert_eq!(res, vec![28.0]),
        Err(errs) => {
            for err in &errs {
                eprintln!("Error: {}", err.get_message());
            }
            panic!("type_recursive_tree failed");
        }
    }
}

#[wasm_bindgen_test(unsupported = test)]
fn type_recursive_option() {
    let (file, src) = load_src("type_recursive_option.mmm");
    let res = run_source_test(&src, 1, false, Some(file));
    match res {
        Ok(res) => assert_eq!(res, vec![52.0]),
        Err(errs) => {
            for err in &errs {
                eprintln!("Error: {}", err.get_message());
            }
            panic!("type_recursive_option failed");
        }
    }
}

#[wasm_bindgen_test(unsupported = test)]
fn box_clone_sharing() {
    let (file, src) = load_src("box_clone_sharing.mmm");
    let res = run_source_test(&src, 1, false, Some(file));
    match res {
        Ok(res) => assert_eq!(res, vec![9.0]),
        Err(errs) => {
            for err in &errs {
                eprintln!("Error: {}", err.get_message());
            }
            panic!("box_clone_sharing failed");
        }
    }
}

#[wasm_bindgen_test(unsupported = test)]
fn box_nested_scopes() {
    let (file, src) = load_src("box_nested_scopes.mmm");
    let res = run_source_test(&src, 1, false, Some(file));
    match res {
        Ok(res) => assert_eq!(res, vec![11.0]),
        Err(errs) => {
            for err in &errs {
                eprintln!("Error: {}", err.get_message());
            }
            panic!("box_nested_scopes failed");
        }
    }
}

#[wasm_bindgen_test(unsupported = test)]
fn box_multiple_refs() {
    let (file, src) = load_src("box_multiple_refs.mmm");
    let res = run_source_test(&src, 1, false, Some(file));
    match res {
        Ok(res) => assert_eq!(res, vec![15.0]),
        Err(errs) => {
            for err in &errs {
                eprintln!("Error: {}", err.get_message());
            }
            panic!("box_multiple_refs failed");
        }
    }
}

#[wasm_bindgen_test(unsupported = test)]
fn box_gc_test() {
    let (file, src) = load_src("box_gc_test.mmm");
    let res = run_source_test(&src, 1, false, Some(file));
    match res {
        Ok(res) => assert_eq!(res, vec![31.0]),
        Err(errs) => {
            for err in &errs {
                eprintln!("Error: {}", err.get_message());
            }
            panic!("box_gc_test failed");
        }
    }
}

// ============ Type Visibility Tests ============

#[wasm_bindgen_test(unsupported = test)]
fn type_visibility_private_alias_fail2() {
    // Test that private type aliases cannot be accessed from outside
    let res = run_error_test("type_visibility_private_alias_fail2.mmm", false);
    assert!(!res.is_empty(), "Expected PrivateTypeAccess error");
    // Check if any error message contains "private"
    let has_private_error = res.iter().any(|err| err.get_message().contains("private"));
    assert!(
        has_private_error,
        "Expected 'private' type access error, got: {}",
        res.iter()
            .map(|e| e.get_message())
            .collect::<Vec<_>>()
            .join(", ")
    );
}
#[wasm_bindgen_test(unsupported = test)]
fn type_visibility_private_alias_fail() {
    // Test that private type aliases cannot be accessed from outside
    let res = run_error_test("type_visibility_private_alias_fail.mmm", false);
    assert!(!res.is_empty(), "Expected PrivateTypeAccess error");
    let err_message = res[0].get_message();
    assert!(
        err_message.contains("private"),
        "Expected 'private' type access error, got: {err_message}"
    );
}

#[wasm_bindgen_test(unsupported = test)]
fn type_visibility_private_declaration_fail() {
    // Test that private type declarations cannot be accessed from outside
    let res = run_error_test("type_visibility_private_declaration_fail.mmm", false);
    assert!(!res.is_empty(), "Expected PrivateTypeAccess error");
    let err_message = res[0].get_message();
    assert!(
        err_message.contains("private"),
        "Expected 'private' type access error, got: {err_message}"
    );
}
#[test]
fn type_visibility_leak() {
    // Test that public functions cannot leak private types in their signatures
    let res = run_error_test("type_visibility_leak_fail.mmm", false);
    assert!(!res.is_empty(), "Expected PrivateTypeLeak error");
    let err_message = res[0].get_message();
    assert!(
        err_message.contains("private type") && err_message.contains("signature"),
        "Expected private type leak error, got: {err_message}"
    );
}

// ============================================================================
// WASM Backend Tests
// ============================================================================
// These tests specifically verify the WASM backend implementation.
// They are crucial for catching backend-specific issues like state management
// and global variable initialization that may not be caught by VM tests alone.

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn wasm_counter() {
    // Test basic stateful function (self) in WASM backend
    // This catches issues with state persistence across dsp() calls
    let res = run_file_test_wasm("counter.mmm", 10, false).unwrap();
    let ans = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
    assert_eq!(
        res, ans,
        "WASM backend: stateful function should accumulate"
    );
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn wasm_tuple_pass() {
    // Test tuple argument flattening at call sites in WASM backend.
    // A function receiving a tuple should have its params flattened,
    // and the call site must expand the tuple pointer to individual values.
    let res = run_file_test_wasm("tuple_pass.mmm", 3, false).unwrap();
    // add_tuple(make_tuple(1.0)) = 1.0 + 2.0 + 3.0 = 6.0
    let ans = vec![6.0, 6.0, 6.0];
    assert_eq!(res, ans, "WASM backend: tuple arg flattening at call site");
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn wasm_global_state() {
    // Test global variable initialization and stateful function with globals
    // This catches issues where global initializer (_mimium_global) is not called
    let res = run_file_test_wasm("global_state.mmm", 5, false).unwrap();
    // Expected values: PI * (n+1) where n is iteration count
    let ans = [
        3.14159265359,
        6.28318530718,
        9.42477796077,
        12.56637061436,
        15.70796326795,
    ];
    // Use approximate comparison for floating-point values
    assert_eq!(res.len(), ans.len(), "WASM backend: output length mismatch");
    for (i, (actual, expected)) in res.iter().zip(ans.iter()).enumerate() {
        assert!(
            (actual - expected).abs() < 1e-9,
            "WASM backend: value mismatch at index {i}: {actual} vs {expected}"
        );
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn wasm_stereo_output() {
    // Test stereo (tuple) output from dsp() in WASM backend.
    // dsp() returns (0.5, -0.5); the runtime must dereference the tuple
    // pointer from linear memory to extract L and R channels.
    let res = run_file_test_wasm("stereo_output.mmm", 3, true).unwrap();
    // Stereo results are flattened: [L1, R1, L2, R2, L3, R3]
    let ans = [0.5, -0.5, 0.5, -0.5, 0.5, -0.5];
    assert_eq!(
        res.len(),
        ans.len(),
        "WASM backend: stereo output length mismatch"
    );
    for (i, (actual, expected)) in res.iter().zip(ans.iter()).enumerate() {
        assert!(
            (actual - expected).abs() < 1e-10,
            "WASM backend: stereo sample {i} mismatch: {actual} vs {expected}"
        );
    }
}
