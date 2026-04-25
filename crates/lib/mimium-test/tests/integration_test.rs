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

fn assert_with_spec(res: &[f64], spec: &AnnotatedFileTestSpec) {
    let tol = spec.tol.unwrap_or(1e-12);
    assert_eq!(res.len(), spec.expected.len());
    for (actual, expected) in res.iter().zip(spec.expected.iter()) {
        assert!((actual - expected).abs() <= tol);
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn run_all_annotated_fixtures() {
    use std::fs;
    use std::path::Path;

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
            if !has_meta || name == "parser_combinators.mmm" {
                return None;
            }
            Some(name)
        })
        .collect::<Vec<_>>();

    fixture_names.sort();
    assert!(!fixture_names.is_empty());

    fixture_names.iter().for_each(|name| {
        let spec = read_annotated_file_test_spec(name)
            .unwrap_or_else(|e| panic!("{name} metadata failed to load: {e}"));
        let (res, spec) = if spec.plugins {
            run_annotated_file_test_with_plugins(name, false)
                .unwrap_or_else(|e| panic!("{name} failed to run with plugins: {e}"))
        } else {
            run_annotated_file_test(name)
                .unwrap_or_else(|e| panic!("{name} failed to run: {e}"))
        };
        assert_with_spec(&res, &spec);
    });
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn run_all_web_annotated_fixtures_wasm() {
    use std::fs;
    use std::path::Path;

    let fixture_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/mmm");
    let mut fixture_names = fs::read_dir(&fixture_dir)
        .unwrap()
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("mmm"))
        .filter_map(|path| {
            let name = path.file_name()?.to_str()?.to_string();
            let spec = read_annotated_file_test_spec(&name).ok()?;
            spec.web.then_some(name)
        })
        .collect::<Vec<_>>();

    fixture_names.sort();
    assert!(!fixture_names.is_empty());

    fixture_names.iter().for_each(|name| {
        let (res, spec) = run_annotated_file_test_wasm(name)
            .unwrap_or_else(|e| panic!("{name} failed to run on WASM: {e}"));
        assert_with_spec(&res, &spec);
    });
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
#[cfg(not(target_arch = "wasm32"))]
fn parser_combinators() {
    let result = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(|| {
            let (res, spec) = run_annotated_file_test("parser_combinators.mmm").unwrap();
            assert_with_spec(&res, &spec);
        })
        .unwrap()
        .join();
    if let Err(e) = result {
        std::panic::resume_unwind(e);
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn pipe_and_macro_pipe_mix_left_associatively() {
    let (res, spec) = run_annotated_file_test("pipe_mixed_precedence.mmm").unwrap();
    assert_with_spec(&res, &spec);
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn block_linear_kernel_fixture() {
    let (res, spec) = run_annotated_file_test("block_linear_kernel.mmm").unwrap();
    assert_with_spec(&res, &spec);
}

#[wasm_bindgen_test(unsupported = test)]
#[cfg(not(target_arch = "wasm32"))]
fn mininotation() {
    // Mini-notation parser uses parser combinators + pattern library, needs larger stack
    // Pattern library depends on osc::phasor -> samplerate, so we need audio driver
    let result = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024) // 16 MB
        .spawn(|| {
            let res = run_file_with_plugins("mininotation.mmm", 1, [].into_iter(), false).unwrap();
            let ans = vec![25.0]; // 25 boolean checks, each contributing 1.0
            assert_eq!(res, ans);
        })
        .unwrap()
        .join();
    if let Err(e) = result {
        std::panic::resume_unwind(e);
    }
}

#[wasm_bindgen_test(unsupported = test)]
#[cfg(not(target_arch = "wasm32"))]
fn mininotation_alternate_grouping() {
    // Mini-notation alternation/grouping edge cases based on TidalCycles reference.
    let result = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(|| {
            let res = run_file_with_plugins(
                "mininotation_alternate_grouping.mmm",
                1,
                [].into_iter(),
                false,
            )
            .unwrap();
            let ans = vec![19.0];
            assert_eq!(res, ans);
        })
        .unwrap()
        .join();
    if let Err(e) = result {
        std::panic::resume_unwind(e);
    }
}

// implement one-sample delay on mimium with `self`

#[wasm_bindgen_test(unsupported = test)]
fn fb_mem3_state_size() {
    test_state_sizes(
        "fb_mem3.mmm",
        [("counter", 1), ("mem_by_hand", 4), ("dsp", 5)],
    );
}

#[wasm_bindgen_test(unsupported = test)]
fn many_errors() {
    let errs = run_error_test_rich("many_errors.mmm", false);
    assert_eq!(errs.len(), 10);
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
fn type_param_reserved_name_fail() {
    let errs = run_error_test("type_param_reserved_name_fail.mmm", false);
    assert!(!errs.is_empty());
    assert!(errs.iter().any(|e| {
        e.get_message()
            .contains("reserved for explicit type parameters")
    }));
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

#[test]
fn parameter_pack_tuple_fail() {
    let res = run_error_test("parameter_pack_tuple_fail.mmm", false);
    assert_eq!(res.len(), 1);
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
fn tuple_binop_len_mismatch_fail() {
    let res = run_error_test("tuple_binop_len_mismatch_fail.mmm", false);
    assert_eq!(res.len(), 1);
}

#[test]
fn tuple_binop_nonnumeric_fail() {
    let res = run_error_test("tuple_binop_nonnumeric_fail.mmm", false);
    assert_eq!(res.len(), 2);
}

#[test]
fn tuple_binop_arity_over16_fail() {
    let res = run_error_test("tuple_binop_arity_over16_fail.mmm", false);
    assert_eq!(res.len(), 1);
}

#[test]
fn tuple_binop_nested_shape_mismatch_fail() {
    let res = run_error_test("tuple_binop_nested_shape_mismatch_fail.mmm", false);
    assert_eq!(res.len(), 1);
}

#[wasm_bindgen_test(unsupported = test)]
fn monomorph_builtin_array_ops_same_arity_nested_array() {
    let (res, spec) =
        run_annotated_file_test("monomorph_builtin_array_ops_same_arity_nested_array.mmm").unwrap();
    assert_with_spec(&res, &spec);
}

#[wasm_bindgen_test(unsupported = test)]
// On web/wasm this currently overflows the host stack during compiler-side
// pronoun conversion (`Maximum call stack size exceeded` in Node), so keep
// native coverage and skip the wasm target until that recursion is flattened.
#[cfg(not(target_arch = "wasm32"))]
fn imported_core_generic_nested_array() {
    let (res, spec) = run_annotated_file_test("imported_core_generic_nested_array.mmm").unwrap();
    assert_with_spec(&res, &spec);
}

#[wasm_bindgen_test(unsupported = test)]
// Same limitation as above: macro-heavy expansion currently exceeds the host
// call stack on web/wasm (`Maximum call stack size exceeded` in Node).
#[cfg(not(target_arch = "wasm32"))]
fn macro_quote_imported_global_function() {
    let (res, spec) = run_annotated_file_test("macro_quote_imported_global_function.mmm").unwrap();
    assert_with_spec(&res, &spec);
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

#[test]
fn probe_value_macro() {
    let (_, src) = load_src("probe_value_macro.mmm");

    let mut driver = mimium_audiodriver::backends::local_buffer::LocalBufferDriver::new(1);
    let audiodriverplug: Box<dyn mimium_lang::plugin::Plugin> = Box::new(driver.get_as_plugin());
    let mut ctx = mimium_lang::ExecContext::new(
        [audiodriverplug].into_iter(),
        None,
        mimium_lang::Config::default(),
    );

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

    let ans = vec![1.5, 1.2]; // ProbeValue should pass through tuple runtime value
    assert_eq!(res, ans);
}

#[test]
fn slider_value_record_macro() {
    let (_, src) = load_src("slider_value_record.mmm");

    let mut driver = mimium_audiodriver::backends::local_buffer::LocalBufferDriver::new(1);
    let audiodriverplug: Box<dyn mimium_lang::plugin::Plugin> = Box::new(driver.get_as_plugin());
    let mut ctx = mimium_lang::ExecContext::new(
        [audiodriverplug].into_iter(),
        None,
        mimium_lang::Config::default(),
    );

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

    let ans = vec![0.75];
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
fn wasm_phi_multiword_arg_if() {
    let res = run_file_test_wasm("phi_multiword_arg_if.mmm", 1, false).unwrap();
    let ans = vec![10.0];
    assert_eq!(res, ans, "WASM backend: phi with multiword argument values");
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

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn wasm_record_default_adsr() {
    let res = run_file_test_wasm("wasm_record_default_adsr.mmm", 8, false).unwrap();
    assert_eq!(res.len(), 8, "WASM backend: output length mismatch");
    assert!(
        res.iter().any(|x| x.abs() > 1e-6),
        "WASM backend: record default ADSR unexpectedly produced silence"
    );
    for i in 1..res.len() {
        assert!(
            res[i] < res[i - 1],
            "WASM backend: record default ADSR should monotonically decrease at index {i}: {} !< {}",
            res[i],
            res[i - 1]
        );
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn wasm_stateful_array_self_zero_init() {
    let res = run_file_test_wasm("stateful_array_self_zero_init.mmm", 3, false).unwrap();
    let ans = vec![1.0, 2.0, 3.0];
    assert_eq!(
        res, ans,
        "WASM backend: array-valued self should start from zero"
    );
}
