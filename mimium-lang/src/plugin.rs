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
pub use builtin_functins::get_builtin_fns_as_plugins;
use std::{cell::RefCell, rc::Rc};

pub use system_plugin::{
    DynSystemPlugin, SysPluginSignature, SystemPlugin, SystemPluginFnType, to_ext_cls_info,
};

use crate::{
    interner::{Symbol, TypeNodeId},
    interpreter::Value,
    vm::{Machine, ReturnCode},
};
#[derive(Clone, Copy, Debug)]
pub enum EvalStage {
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
impl MacroStageT for MacroStage {}
struct MachineStage;
impl EvalStageT for MachineStage {
    fn get_stage() -> EvalStage {
        EvalStage::Stage(1)
    }
}
impl MachineStageT for MachineStage {}
struct PersistentStage;
impl EvalStageT for PersistentStage {
    fn get_stage() -> EvalStage {
        EvalStage::Persistent
    }
}
impl MacroStageT for PersistentStage {}
impl MachineStageT for PersistentStage {}
impl PersistentStageT for PersistentStage {}
pub trait MacroFunction {
    /// Main macro function. If you need to receive 2 or more arguments, you need to pass struct or tuple as the argument instead.
    fn eval(&self, args: &[(Value, TypeNodeId)]) -> Value;
}
pub type ExtFunType = fn(&mut Machine) -> ReturnCode;
pub type ExtClsType = Rc<RefCell<dyn FnMut(&mut Machine) -> ReturnCode>>;
pub trait MachineFunction {
    //name is still needed for linking program
    fn get_name(&self) -> Symbol;
    /// Main function that will be called by the machine.
    fn get_fn(&self) -> ExtClsType;
}
#[derive(Clone)]
pub struct MacroInfo {
    pub name: Symbol,
    pub ty: TypeNodeId,
    pub fun: Rc<RefCell<dyn Fn(&[(Value, TypeNodeId)]) -> Value>>,
}
impl ExternalFunction for MacroInfo {
    type Stage = MacroStage;
    fn get_type_info(&self) -> TypeNodeId {
        self.ty
    }
    fn get_name(&self) -> Symbol {
        self.name
    }
}
impl MacroFunction for MacroInfo {
    fn eval(&self, args: &[(Value, TypeNodeId)]) -> Value {
        (self.fun.borrow())(args)
    }
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
impl MachineFunction for ExtFunInfo {
    fn get_name(&self) -> Symbol {
        self.name
    }
    fn get_fn(&self) -> ExtClsType {
        Rc::new(RefCell::new(self.fun))
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
impl From<ExtClsInfo> for ExtFunTypeInfo {
    fn from(info: ExtClsInfo) -> Self {
        ExtFunTypeInfo {
            name: info.name,
            ty: info.ty,
            stage: MachineStage::get_stage(),
        }
    }
}
impl From<ExtFunInfo> for ExtFunTypeInfo {
    fn from(info: ExtFunInfo) -> Self {
        ExtFunTypeInfo {
            name: info.name,
            ty: info.ty,
            stage: MachineStage::get_stage(),
        }
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
impl MachineFunction for ExtClsInfo {
    fn get_name(&self) -> Symbol {
        self.name
    }
    fn get_fn(&self) -> ExtClsType {
        self.fun.clone()
    }
}

#[derive(Clone)]
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
impl MachineFunction for CommonFunction {
    fn get_name(&self) -> Symbol {
        self.name
    }
    fn get_fn(&self) -> ExtClsType {
        Rc::new(RefCell::new(self.fun))
    }
}
impl MacroFunction for CommonFunction {
    fn eval(&self, args: &[(Value, TypeNodeId)]) -> Value {
        (self.macro_fun)(args)
    }
}
#[derive(Clone, Copy)]
pub struct ExtFunTypeInfo {
    pub name: Symbol,
    pub ty: TypeNodeId,
    pub stage: EvalStage,
}
impl ExtFunTypeInfo {
    pub fn new(name: Symbol, ty: TypeNodeId, stage: EvalStage) -> Self {
        Self { name, ty, stage }
    }
}
pub trait Plugin {
    // type MacroT: MacroFunction<MacroStage>;
    // type MachineT: MachineFunction<MachineStage>;
    fn get_macro_functions(&self) -> Vec<Box<dyn MacroFunction>>;
    
    fn get_ext_closures(&self) -> Vec<Box<dyn MachineFunction>>;

    //limitation: if the functin contains persistent functions, you have to override this method.
    fn get_type_infos(&self) -> Vec<ExtFunTypeInfo>;
}

#[derive(Clone)]
pub struct InstantPlugin {
    pub macros: Vec<MacroInfo>,
    pub extcls: Vec<ExtClsInfo>,
    pub commonfns: Vec<CommonFunction>,
}
impl Plugin for InstantPlugin {
    // type MacroT = MacroInfo;
    // type MachineT = ExtClsInfo;

    fn get_macro_functions(&self) -> Vec<Box<dyn MacroFunction>> {
        let macros = self
            .macros
            .clone()
            .into_iter()
            .map(|m| Box::new(m) as Box<dyn MacroFunction>);
        let commons = self
            .commonfns
            .clone()
            .into_iter()
            .map(|c| Box::new(c) as Box<dyn MacroFunction>);
        macros.chain(commons).collect()
    }

    fn get_ext_closures(&self) -> Vec<Box<dyn MachineFunction>> {
        let extfns = self
            .extcls
            .clone()
            .into_iter()
            .map(|e| Box::new(e) as Box<dyn MachineFunction>);
        let commons = self
            .commonfns
            .clone()
            .into_iter()
            .map(|c| Box::new(c) as Box<dyn MachineFunction>);
        extfns.chain(commons).collect()
    }

    fn get_type_infos(&self) -> Vec<ExtFunTypeInfo> {
        let macros = self
            .macros
            .iter()
            .map(|m| ExtFunTypeInfo::new(m.name, m.ty, MacroStage::get_stage()));
        let extcls = self
            .extcls
            .iter()
            .map(|e| ExtFunTypeInfo::new(e.name, e.ty, MachineStage::get_stage()));
        let commons = self
            .commonfns
            .iter()
            .map(|c| ExtFunTypeInfo::new(c.name, c.ty, PersistentStage::get_stage()));
        macros.chain(extcls).chain(commons).collect()
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
    plugins
        .iter()
        .flat_map(|plugin| plugin.get_type_infos().into_iter())
}

pub fn get_macro_functions(
    plugins: &[Box<dyn Plugin>],
) -> impl Iterator<Item = Box<dyn MacroFunction>> + '_ {
    plugins
        .iter()
        .flat_map(|plugin| plugin.get_macro_functions().into_iter())
}
pub fn get_ext_closures(
    plugins: &[Box<dyn Plugin>],
) -> impl Iterator<Item = Box<dyn MachineFunction>> + '_ {
    plugins
        .iter()
        .flat_map(|plugin| plugin.get_ext_closures().into_iter())
}
