use mimium_audiodriver::driver::{Driver, RuntimeData};
use mimium_lang::{interner::ToSymbol, utils::error::report};
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
