//! Defines the plugin interface used by mimium's runtime.
//!
//! A system plugin can hook into the VM execution by providing callback
//! functions.  Each plugin exposes its callbacks through [`SysPluginSignature`]
//! values that are registered as external closures.

use super::ExtClsInfo;
use crate::{
    compiler::EvalStage,
    interner::{ToSymbol, TypeNodeId},
    interpreter::Value,
    plugin::MacroInfo,
    runtime::{
        Time,
        vm::{Machine, ReturnCode},
    },
};
use std::{
    any::Any,
    cell::{RefCell, UnsafeCell},
    rc::Rc,
    sync::Arc,
};
pub type SystemPluginFnType<T> = fn(&mut T, &mut Machine) -> ReturnCode;
pub type SystemPluginMacroType<T> = fn(&mut T, &[(Value, TypeNodeId)]) -> Value;

/// Metadata for a callback provided by a [`SystemPlugin`].
///
/// Each signature stores the callback name, erased function pointer and the
/// type of the closure expected by the VM.
pub struct SysPluginSignature {
    name: &'static str,
    /// The function internally implements `Fn(&mut T:SystemPlugin,&mut Machine)->ReturnCode`
    /// but the type is erased for dynamic dispatching. later the function is downcasted into their own type.
    fun: Rc<dyn Any>,
    ty: TypeNodeId,
    /// The stage at which the function is available.
    /// This is used to determine whether the function can be called in a macro or
    /// in the VM. Note that any persistent functions are not allowed to be used in `SystemPlugin`.
    stage: EvalStage,
}
impl SysPluginSignature {
    pub fn new<F, T>(name: &'static str, fun: F, ty: TypeNodeId) -> Self
    where
        F: Fn(&mut T, &mut Machine) -> ReturnCode + 'static,
        T: SystemPlugin,
    {
        Self {
            name,
            fun: Rc::new(fun),
            ty,
            stage: EvalStage::Stage(1),
        }
    }
    pub fn new_macro<F, T>(name: &'static str, fun: F, ty: TypeNodeId) -> Self
    where
        F: Fn(&mut T, &[(Value, TypeNodeId)]) -> Value + 'static,
        T: SystemPlugin,
    {
        Self {
            name,
            fun: Rc::new(fun),
            ty,
            stage: EvalStage::Stage(0),
        }
    }
}

/// Trait implemented by runtime plugins.
///
/// The default implementations of the callback methods do nothing. Plugins can
/// override these to perform setup in [`on_init`], teardown in [`after_main`],
/// or per-sample processing in [`on_sample`].
pub trait SystemPlugin {
    fn on_init(&mut self, _machine: &mut Machine) -> ReturnCode {
        0
    }
    fn after_main(&mut self, _machine: &mut Machine) -> ReturnCode {
        0
    }
    fn on_sample(&mut self, _time: Time, _machine: &mut Machine) -> ReturnCode {
        0
    }
    fn gen_interfaces(&self) -> Vec<SysPluginSignature>;
    fn try_get_main_loop(&mut self) -> Option<Box<dyn FnOnce()>> {
        None
    }
}

#[derive(Clone)]
/// A dynamically dispatched plugin wrapped in reference-counted storage.
pub struct DynSystemPlugin {
    pub inner: Arc<UnsafeCell<dyn SystemPlugin>>,
    pub clsinfos: Vec<ExtClsInfo>,
    pub macroinfos: Vec<MacroInfo>,
}
/// Convert a plugin into the VM-facing representation.
///
/// The returned [`DynSystemPlugin`] is stored by the runtime, while the
/// accompanying `Vec<ExtClsInfo>` contains closures that expose the plugin's
/// callback methods to mimium code.
impl<T> From<T> for DynSystemPlugin
where
    T: SystemPlugin + Sized + 'static,
{
    fn from(plugin: T) -> Self {
        let ifs = plugin.gen_interfaces();
        let inner = Arc::new(UnsafeCell::new(plugin));
        let macroinfos = ifs
            .iter()
            .filter(|&SysPluginSignature { stage, .. }| matches!(stage, EvalStage::Stage(0)))
            .map(|SysPluginSignature { name, fun, ty, .. }| {
                let inner = inner.clone();
                let fun = fun
                    .clone()
                    .downcast::<SystemPluginMacroType<T>>()
                    .expect("invalid conversion applied in the system plugin resolution.");
                MacroInfo::new(
                    name.to_symbol(),
                    *ty,
                    Rc::new(RefCell::new(move |args: &[(Value, TypeNodeId)]| -> Value {
                        // breaking double borrow rule at here!!!
                        // Also here I do dirty downcasting because here the type of plugin is ensured as T.
                        unsafe {
                            let p = inner.get().as_mut().unwrap();
                            fun(p, args)
                        }
                    })),
                )
            })
            .collect();
        let clsinfos = ifs
            .into_iter()
            .filter(|SysPluginSignature { stage, .. }| matches!(stage, EvalStage::Stage(1)))
            .map(|SysPluginSignature { name, fun, ty, .. }| {
                let inner = inner.clone();
                let fun = fun
                    .clone()
                    .downcast::<SystemPluginFnType<T>>()
                    .expect("invalid conversion applied in the system plugin resolution.");
                let fun = Rc::new(RefCell::new(move |machine: &mut Machine| -> ReturnCode {
                    // breaking double borrow rule at here!!!
                    // Also here I do dirty downcasting because here the type of plugin is ensured as T.
                    unsafe {
                        let p = inner.get().as_mut().unwrap();
                        fun(p, machine)
                    }
                }));
                ExtClsInfo::new(name.to_symbol(), ty, fun)
            })
            .collect();

        DynSystemPlugin {
            inner,
            clsinfos,
            macroinfos,
        }
    }
}
