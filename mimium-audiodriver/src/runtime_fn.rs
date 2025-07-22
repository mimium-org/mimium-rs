//! Runtime functions supplied by the audio driver.
//!
//! These helpers expose the current sample count and sample rate to mimium
//! programs as external closures.

use std::{
    cell::RefCell,
    rc::Rc,
    sync::{
        Arc,
        atomic::{AtomicU32, AtomicU64, Ordering},
    },
};

use mimium_lang::{
    function,
    interner::ToSymbol,
    numeric,
    plugin::ExtClsInfo,
    runtime::vm::Machine,
    types::{PType, Type},
};

/// Generate an external closure returning the current sample position.
pub fn gen_getnowfn(count: Arc<AtomicU64>) -> ExtClsInfo {
    let func = Rc::new(RefCell::new(move |machine: &mut Machine| {
        let count = count.load(Ordering::Relaxed) as f64;
        machine.set_stack(0, Machine::to_value(count));
        1
    }));
    ExtClsInfo::new(
        "_mimium_getnow".to_symbol(),
        function!(vec![], numeric!()),
        func,
    )
}
/// Generate an external closure returning the audio sample rate.
pub fn gen_getsampleratefn(samplerate: Arc<AtomicU32>) -> ExtClsInfo {
    let func = Rc::new(RefCell::new(move |machine: &mut Machine| {
        let count = samplerate.load(Ordering::Relaxed) as f64;
        machine.set_stack(0, Machine::to_value(count));
        1
    }));
    ExtClsInfo::new(
        "_mimium_getsamplerate".to_symbol(),
        function!(vec![], numeric!()),
        func,
    )
}
