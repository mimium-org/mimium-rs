use crate::{
    interner::{ToSymbol, TypeNodeId},
    runtime::{
        vm::{ExtClsInfo, Machine, ReturnCode},
        Time,
    },
};
use std::{
    any::Any,
    cell::{RefCell, UnsafeCell},
    rc::Rc,
};
pub type SystemPluginFnType<T> = fn(&mut T, &mut Machine) -> ReturnCode;
pub struct SysPluginSignature {
    name: &'static str,
    /// The function internally implements Fn(&mut T:SystemPlugin,&mut Machine)->ReturnCode
    /// but the type is erased for dynamic dispatching. later the function is downcasted into their own type.
    fun: Rc<dyn Any>,
    ty: TypeNodeId,
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
        }
    }
}

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
pub struct DynSystemPlugin(pub Rc<UnsafeCell<dyn SystemPlugin>>);


pub fn to_ext_cls_info<T: SystemPlugin + 'static>(
    sysplugin: T,
) -> (DynSystemPlugin, Vec<ExtClsInfo>) {
    let ifs = sysplugin.gen_interfaces();
    let dyn_plugin = DynSystemPlugin(Rc::new(UnsafeCell::new(sysplugin)));
    let ifs_res = ifs
        .into_iter()
        .map(|SysPluginSignature { name, fun, ty }| -> ExtClsInfo {
            let plug = dyn_plugin.clone();
            let fun = fun
                .clone()
                .downcast::<fn(&mut T, &mut Machine) -> ReturnCode>()
                .expect("invalid conversion applied in the system plugin resolution.");
            let fun = Rc::new(RefCell::new(move |machine: &mut Machine| -> ReturnCode {
                // breaking double borrow rule at here!!!
                // Also here I do dirty downcasting because here the type of plugin is ensured as T.
                unsafe {
                    let p = (plug.0.get() as *mut T).as_mut().unwrap();
                    fun(p, machine)
                }
            }));
            let res: ExtClsInfo = (name.to_symbol(), fun, ty);
            res
        })
        .collect::<Vec<_>>();
    (dyn_plugin, ifs_res)
}

// impl<T: Sized> SysPluginSignature<T> {}
