//! Compile-time and basic runtime tests for the `mimium_plugin_fn` macro.

use mimium_lang::runtime::vm::{Machine, ReturnCode};
use mimium_plugin_macros::mimium_plugin_fn;

// ---------------------------------------------------------------------------
// Test plugin struct
// ---------------------------------------------------------------------------

struct TestPlugin {
    value: f64,
    samples: Vec<Vec<f64>>,
}

impl TestPlugin {
    fn new(value: f64) -> Self {
        Self {
            value,
            samples: vec![vec![1.0, 2.0, 3.0], vec![10.0, 20.0, 30.0]],
        }
    }
}

// ---------------------------------------------------------------------------
// Methods annotated with #[mimium_plugin_fn]
// ---------------------------------------------------------------------------

impl TestPlugin {
    /// Single f64 argument, single f64 return.
    #[mimium_plugin_fn]
    pub fn add_value(&mut self, x: f64) -> f64 {
        self.value + x
    }

    /// Two f64 arguments, single f64 return.
    #[mimium_plugin_fn]
    pub fn get_sample(&mut self, pos: f64, sample_idx: f64) -> f64 {
        let idx = sample_idx as usize;
        self.samples
            .get(idx)
            .and_then(|v| v.get(pos as usize).copied())
            .unwrap_or(0.0)
    }

    /// Single f64 argument, tuple return.
    #[mimium_plugin_fn]
    pub fn get_pair(&mut self, x: f64) -> (f64, f64) {
        (x, x * self.value)
    }

    /// Three-element tuple return.
    #[mimium_plugin_fn]
    pub fn get_triple(&mut self, x: f64) -> (f64, f64, f64) {
        (x, x * 2.0, x * 3.0)
    }

    /// Unit return — side-effect only.
    #[mimium_plugin_fn]
    pub fn set_value(&mut self, x: f64) {
        self.value = x;
    }

    /// No arguments besides self, f64 return.
    #[mimium_plugin_fn]
    pub fn get_value(&mut self) -> f64 {
        self.value
    }

    /// Early return — the helper method preserves the correct return type.
    #[mimium_plugin_fn]
    pub fn safe_divide(&mut self, a: f64, b: f64) -> f64 {
        if b == 0.0 {
            return 0.0;
        }
        a / b
    }

    /// Immutable self receiver.
    #[mimium_plugin_fn]
    pub fn read_value(&self) -> f64 {
        self.value
    }
}

// ---------------------------------------------------------------------------
// Compile-time: verify generated wrapper signatures match SystemPluginFnType
// ---------------------------------------------------------------------------

#[test]
fn generated_wrappers_have_correct_fn_ptr_type() {
    // SystemPluginFnType<T> = fn(&mut T, &mut Machine) -> ReturnCode
    let _: fn(&mut TestPlugin, &mut Machine) -> ReturnCode = TestPlugin::add_value;
    let _: fn(&mut TestPlugin, &mut Machine) -> ReturnCode = TestPlugin::get_sample;
    let _: fn(&mut TestPlugin, &mut Machine) -> ReturnCode = TestPlugin::get_pair;
    let _: fn(&mut TestPlugin, &mut Machine) -> ReturnCode = TestPlugin::get_triple;
    let _: fn(&mut TestPlugin, &mut Machine) -> ReturnCode = TestPlugin::set_value;
    let _: fn(&mut TestPlugin, &mut Machine) -> ReturnCode = TestPlugin::get_value;
    let _: fn(&mut TestPlugin, &mut Machine) -> ReturnCode = TestPlugin::safe_divide;
    let _: fn(&mut TestPlugin, &mut Machine) -> ReturnCode = TestPlugin::read_value;
}

// ---------------------------------------------------------------------------
// Runtime: verify the private _impl helpers keep the original logic
// ---------------------------------------------------------------------------

#[test]
fn impl_helpers_preserve_original_logic() {
    let mut p = TestPlugin::new(10.0);

    assert_eq!(p.__add_value_impl(5.0), 15.0);
    assert_eq!(p.__get_sample_impl(1.0, 0.0), 2.0);
    assert_eq!(p.__get_pair_impl(3.0), (3.0, 30.0));
    assert_eq!(p.__get_triple_impl(2.0), (2.0, 4.0, 6.0));

    p.__set_value_impl(42.0);
    assert_eq!(p.value, 42.0);

    assert_eq!(p.__get_value_impl(), 42.0);
    assert_eq!(p.__safe_divide_impl(10.0, 3.0), 10.0 / 3.0);
    assert_eq!(p.__safe_divide_impl(10.0, 0.0), 0.0); // early return
    assert_eq!(p.__read_value_impl(), 42.0);
}
