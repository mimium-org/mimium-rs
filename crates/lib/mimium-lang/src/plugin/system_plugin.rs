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
use std::{any::Any, cell::RefCell, rc::Rc};
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

    /// Get the public name of this signature.
    pub fn get_name(&self) -> &'static str {
        self.name
    }

    /// Get the type of this signature.
    pub fn get_type(&self) -> TypeNodeId {
        self.ty
    }

    /// Get the stage at which this function/macro is available.
    pub fn get_stage(&self) -> EvalStage {
        self.stage
    }
}

/// Trait implemented by runtime plugins.
///
/// The default implementations of the callback methods do nothing. Plugins can
/// override these to perform setup in [`on_init`], teardown in [`after_main`],
/// or per-sample processing in [`on_sample`].
pub trait SystemPlugin {
    /// Downcast helper for safe access through `RefCell<dyn SystemPlugin>`.
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn generate_audioworker(&mut self) -> Option<Box<dyn SystemPluginAudioWorker>> {
        None
    }
    fn on_init(&mut self, _machine: &mut Machine) -> ReturnCode {
        0
    }
    fn after_main(&mut self, _machine: &mut Machine) -> ReturnCode {
        0
    }

    /// WASM-side lifecycle hook called before `main()`.
    ///
    /// This is the WASM analogue of [`on_init`].  Receives a mutable
    /// reference to the [`WasmEngine`] so the plugin can interact with
    /// the WASM module before execution.
    #[cfg(not(target_arch = "wasm32"))]
    fn on_init_wasm(
        &mut self,
        _engine: &mut crate::runtime::wasm::engine::WasmEngine,
    ) -> ReturnCode {
        0
    }

    /// WASM-side lifecycle hook called after `main()` completes.
    ///
    /// This is the WASM analogue of [`after_main`].  Receives a mutable
    /// reference to the [`WasmEngine`] so the plugin can inspect the
    /// module state or connect to external devices.
    #[cfg(not(target_arch = "wasm32"))]
    fn after_main_wasm(
        &mut self,
        _engine: &mut crate::runtime::wasm::engine::WasmEngine,
    ) -> ReturnCode {
        0
    }

    fn gen_interfaces(&self) -> Vec<SysPluginSignature>;
    fn try_get_main_loop(&mut self) -> Option<Box<dyn FnOnce()>> {
        None
    }
    /// Produce a lock-free audio handle after setup completes.
    ///
    /// Called after macro expansion is finished and before the runtime is
    /// moved to the audio thread.  The returned `Box<dyn Any + Send>` is
    /// passed to the audio backend and can be downcast to the concrete
    /// handle type inside trampoline closures.
    fn freeze_audio_handle(&mut self) -> Option<Box<dyn Any + Send>> {
        None
    }

    /// Produce a per-function closure map for the WASM backend.
    ///
    /// Called after macro expansion and before WASM module instantiation.
    /// Each entry maps a plugin function name (e.g. `"__get_slider"`) to a
    /// closure that implements the function. The closures must be `Send +
    /// Sync` since they are captured by wasmtime host trampolines.
    ///
    /// Plugins that do not need custom WASM handlers can leave this as
    /// `None` — the runtime will provide a default pass-through / zero
    /// trampoline.
    #[cfg(not(target_arch = "wasm32"))]
    fn freeze_for_wasm(&mut self) -> Option<crate::runtime::wasm::WasmPluginFnMap> {
        None
    }

    /// Produce a per-sample audio worker for the WASM backend.
    ///
    /// This is the WASM analogue of [`generate_audioworker`].  The returned
    /// worker is stored by [`WasmDspRuntime`](crate::runtime::wasm::engine::WasmDspRuntime)
    /// and its [`on_sample`](crate::runtime::wasm::WasmSystemPluginAudioWorker::on_sample)
    /// method is called once per sample, before `dsp()`.
    #[cfg(not(target_arch = "wasm32"))]
    fn generate_wasm_audioworker(
        &mut self,
    ) -> Option<Box<dyn crate::runtime::wasm::WasmSystemPluginAudioWorker>> {
        None
    }
}

pub trait SystemPluginAudioWorker {
    fn on_sample(&mut self, _time: Time, _machine: &mut Machine) -> ReturnCode {
        0
    }
    fn gen_interfaces(&self) -> Vec<SysPluginSignature>;
}

/// A dynamically dispatched plugin wrapped in reference-counted storage.
pub struct DynSystemPlugin {
    inner: Rc<RefCell<dyn SystemPlugin>>,
    audioworker: Option<Box<dyn SystemPluginAudioWorker>>,
    pub clsinfos: Vec<ExtClsInfo>,
    pub macroinfos: Vec<MacroInfo>,
}

impl DynSystemPlugin {
    pub fn take_audioworker(&mut self) -> Option<Box<dyn SystemPluginAudioWorker>> {
        self.audioworker.take()
    }
    /// Delegate to the inner plugin's `freeze_audio_handle()`.
    ///
    /// Must only be called from the main thread before the audio thread starts.
    pub fn freeze_audio_handle(&mut self) -> Option<Box<dyn Any + Send>> {
        self.inner.borrow_mut().freeze_audio_handle()
    }

    /// Delegate to the inner plugin's `freeze_for_wasm()`.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn freeze_for_wasm(&mut self) -> Option<crate::runtime::wasm::WasmPluginFnMap> {
        self.inner.borrow_mut().freeze_for_wasm()
    }

    /// Delegate to the inner plugin's `generate_wasm_audioworker()`.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn generate_wasm_audioworker(
        &mut self,
    ) -> Option<Box<dyn crate::runtime::wasm::WasmSystemPluginAudioWorker>> {
        self.inner.borrow_mut().generate_wasm_audioworker()
    }

    /// Delegate to the inner plugin's `on_init_wasm()`.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn on_init_wasm(
        &self,
        engine: &mut crate::runtime::wasm::engine::WasmEngine,
    ) -> crate::runtime::vm::ReturnCode {
        self.inner.borrow_mut().on_init_wasm(engine)
    }

    /// Delegate to the inner plugin's `after_main_wasm()`.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn after_main_wasm(
        &self,
        engine: &mut crate::runtime::wasm::engine::WasmEngine,
    ) -> crate::runtime::vm::ReturnCode {
        self.inner.borrow_mut().after_main_wasm(engine)
    }

    /// Get a mutable reference to the inner plugin.
    ///
    /// Panics at runtime if the plugin is already borrowed (e.g. by a
    /// closure captured from `gen_interfaces()`).
    pub fn borrow_inner_mut(&self) -> std::cell::RefMut<'_, dyn SystemPlugin> {
        self.inner.borrow_mut()
    }
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
    fn from(mut plugin: T) -> Self {
        let mut audioworker = plugin.generate_audioworker();

        let ifs = plugin.gen_interfaces();
        let inner: Rc<RefCell<dyn SystemPlugin>> = Rc::new(RefCell::new(plugin));
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
                        // SAFETY: downcast is valid because T was the concrete type
                        // used when constructing this DynSystemPlugin.
                        let mut plugin_ref = inner.borrow_mut();
                        let p: &mut T = plugin_ref
                            .as_any_mut()
                            .downcast_mut::<T>()
                            .expect("plugin type mismatch");
                        fun(p, args)
                    })),
                )
            })
            .collect();
        let clsinfos = ifs
            .into_iter()
            .chain(
                audioworker
                    .as_mut()
                    .map(|worker| worker.gen_interfaces())
                    .into_iter()
                    .flatten(),
            )
            .filter(|SysPluginSignature { stage, .. }| matches!(stage, EvalStage::Stage(1)))
            .map(|SysPluginSignature { name, fun, ty, .. }| {
                let inner = inner.clone();
                let fun = fun
                    .clone()
                    .downcast::<SystemPluginFnType<T>>()
                    .expect("invalid conversion applied in the system plugin resolution.");
                let fun = Rc::new(RefCell::new(move |machine: &mut Machine| -> ReturnCode {
                    let mut plugin_ref = inner.borrow_mut();
                    let p: &mut T = plugin_ref
                        .as_any_mut()
                        .downcast_mut::<T>()
                        .expect("plugin type mismatch");
                    fun(p, machine)
                }));
                ExtClsInfo::new(name.to_symbol(), ty, fun)
            })
            .collect();
        DynSystemPlugin {
            inner,
            audioworker,
            clsinfos,
            macroinfos,
        }
    }
}
