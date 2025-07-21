//! # Plugin System for mimium
//! In order to extend mimium's capability to communicate between host system, mimium has its own FFI system.
//! The FFI is done through some traits in this plugin module in order to decouple dependencies(modules may depends on external crates).
//! There are 3 types of interfaces you need to define depending on what you need.
//!
//! 1. **IO Plugins** Sets of instance-free external functions such as `print` and `println`. They are mostly for glue functions for host system's IO. `mimium-core` is an example of this type of module.
//! 2. **External Unit Generator(UGen) Plugin.** If you need to define native Unit Generator, use `UGenPlugin` interface. In mimium code, you need to call higher-order function that returns instance of the UGen. You need to write small wrapper for this which simply calls object's constructor (In the future, this wrapper will be automatically implemented through proc-macro). Multiple instances may exist at the same time. `mimium-symphonia` is a example of this type of module.
//! 3. **System Plugin**. If your plugin needs to mutate states of system-wide instance (1 plugin instance per 1 vm), you need to implement `SystemPlugin` traits. System plugin can have callbacks invoked at the important timings of the system like `on_init`, `before_on_sample` & so on. Internal synchronous event scheduler is implemented through this plugins system. `mimium-rand` is also an example of this type of module.

mod builtin_functins;
mod system_plugin;
use std::{cell::RefCell, rc::Rc};

pub use system_plugin::{
    DynSystemPlugin, SysPluginSignature, SystemPlugin, SystemPluginFnType, to_ext_cls_info,
};

use crate::{
    compiler::ExtFunTypeInfo,
    interner::{Symbol, TypeNodeId},
    interpreter::Value,
    vm::{Machine, ReturnCode},
};
enum EvalStage {
    Persistent,
    Stage(u8),
}
trait EvalStageT {
    fn get_stage() -> EvalStage;
}
trait MacroStageT {}
trait MachineStageT {}
trait PersistentStageT: MacroStageT + MachineStageT {}
trait ExternalFunction {
    /// Declare the type signature of the external function.
    fn get_type_info(&self) -> TypeNodeId;
    fn get_name(&self) -> Symbol;
    type Stage: EvalStageT;
}
struct MacroStage {}
impl EvalStageT for MacroStage {
    fn get_stage() -> EvalStage {
        EvalStage::Stage(0)
    }
}
struct MachineStage;
impl EvalStageT for MachineStage {
    fn get_stage() -> EvalStage {
        EvalStage::Stage(1)
    }
}
struct PersistentStage;
impl EvalStageT for PersistentStage {
    fn get_stage() -> EvalStage {
        EvalStage::Persistent
    }
}
trait MacroFunction<T>: ExternalFunction<Stage = T> {
    type Context;
    /// Main macro function. If you need to receive 2 or more arguments, you need to pass struct or tuple as the argument instead.
    fn eval(
        &self,
        ctx: &mut Self::Context,
        args: impl ExactSizeIterator<Item = (Value, TypeNodeId)>,
    ) -> Value;
}
pub type ExtFunType = fn(&mut Machine) -> ReturnCode;
pub type ExtClsType = Rc<RefCell<dyn FnMut(&mut Machine) -> ReturnCode>>;
trait MachineFunction<T>: ExternalFunction<Stage = T> {
    /// Main function that will be called by the machine.
    fn call(&self, machine: &mut Machine) -> ReturnCode;
}

#[derive(Clone, Debug)]
pub struct ExtFunInfo {
    pub name: Symbol,
    pub ty: TypeNodeId,
    pub fun: ExtFunType,
}
impl ExtFunInfo {
    pub fn new(name: Symbol, ty: TypeNodeId, fun: ExtFunType) -> Self {
        Self { name, ty, fun }
    }
}
impl ExternalFunction for ExtFunInfo {
    type Stage = MachineStage;
    fn get_type_info(&self) -> TypeNodeId {
        self.ty
    }
    fn get_name(&self) -> Symbol {
        self.name
    }
}
impl MachineFunction<MachineStage> for ExtFunInfo {
    fn call(&self, machine: &mut Machine) -> ReturnCode {
        (self.fun)(machine)
    }
}
#[derive(Clone)]
pub struct ExtClsInfo {
    pub name: Symbol,
    pub ty: TypeNodeId,
    pub fun: ExtClsType,
}
impl ExtClsInfo {
    pub fn new(name: Symbol, ty: TypeNodeId, fun: ExtClsType) -> Self {
        Self { name, ty, fun }
    }
}
impl ExternalFunction for ExtClsInfo {
    type Stage = MachineStage;
    fn get_type_info(&self) -> TypeNodeId {
        self.ty
    }
    fn get_name(&self) -> Symbol {
        self.name
    }
}
impl MachineFunction<MachineStage> for ExtClsInfo {
    fn call(&self, machine: &mut Machine) -> ReturnCode {
        (self.fun.borrow_mut())(machine)
    }
}

pub struct CommonFunction {
    name: Symbol,
    ty: TypeNodeId,
    macro_fun: fn(&[(Value, TypeNodeId)]) -> Value,
    fun: ExtFunType,
}
impl ExternalFunction for CommonFunction {
    type Stage = PersistentStage;
    fn get_type_info(&self) -> TypeNodeId {
        self.ty
    }
    fn get_name(&self) -> Symbol {
        self.name
    }
}
impl MachineFunction<PersistentStage> for CommonFunction {
    fn call(&self, machine: &mut Machine) -> ReturnCode {
        (self.fun)(machine)
    }
}
impl MacroFunction<PersistentStage> for CommonFunction {
    type Context = ();
    fn eval(
        &self,
        _ctx: &mut Self::Context,
        args: impl ExactSizeIterator<Item = (Value, TypeNodeId)>,
    ) -> Value {
        (self.macro_fun)(&args.collect::<Vec<_>>())
    }
}

pub trait Plugin {
    fn get_ext_functions(&self) -> Vec<ExtFunInfo>;
    fn get_ext_closures(&self) -> Vec<ExtClsInfo>;
}

pub struct InstantPlugin {
    pub extfns: Vec<ExtFunInfo>,
    pub extcls: Vec<ExtClsInfo>,
}
impl Plugin for InstantPlugin {
    fn get_ext_functions(&self) -> Vec<ExtFunInfo> {
        self.extfns.clone()
    }

    fn get_ext_closures(&self) -> Vec<ExtClsInfo> {
        self.extcls.clone()
    }
}

pub trait IOPlugin {
    fn get_ext_functions(&self) -> Vec<ExtFunInfo>;
}

impl<T> Plugin for T
where
    T: IOPlugin,
{
    fn get_ext_functions(&self) -> Vec<ExtFunInfo> {
        <T as IOPlugin>::get_ext_functions(self)
    }
    fn get_ext_closures(&self) -> Vec<ExtClsInfo> {
        vec![]
    }
}

/// Todo: Make wrapper macro for auto impl `Plugin`
pub trait UGenPlugin {
    type InitParam;
    type Args;
    type Ret;
    fn new(param: Self::InitParam) -> Self;
    fn on_sample(&mut self, arg: Self::Args) -> Self::Ret;
}
// type DynUgenPlugin{}
// pub type UGenPluginCollection(Vec<DynUGenPlugin>);
// impl Plugin for UGenPluginCollection{}

pub fn get_extfun_types(plugins: &[Box<dyn Plugin>]) -> impl Iterator<Item = ExtFunTypeInfo> + '_ {
    plugins.iter().flat_map(|plugin| {
        let extfns = plugin
            .get_ext_functions()
            .into_iter()
            .map(|ExtFunInfo { name, ty, fun: _ }| ExtFunTypeInfo { name, ty });
        let extcls = plugin
            .get_ext_closures()
            .into_iter()
            .map(|ExtClsInfo { name, ty, fun: _ }| ExtFunTypeInfo { name, ty });
        extfns.chain(extcls)
    })
}

pub fn get_extfuninfos(plugins: &[Box<dyn Plugin>]) -> impl Iterator<Item = ExtFunInfo> + '_ {
    plugins
        .iter()
        .flat_map(|plugin| plugin.get_ext_functions().into_iter())
}
pub fn get_extclsinfos(plugins: &[Box<dyn Plugin>]) -> impl Iterator<Item = ExtClsInfo> + '_ {
    plugins
        .iter()
        .flat_map(|plugin| plugin.get_ext_closures().into_iter())
}
