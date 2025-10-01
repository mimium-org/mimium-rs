//! Main module of compiler and runtime for **mimium**, an infrastructural programming language for sound and music.

pub mod ast;
pub mod interner;
pub mod interpreter;
pub mod mir;
pub mod pattern;
pub mod types;
pub mod utils;

pub mod compiler;
pub mod runtime;

pub mod plugin;

use std::path::PathBuf;

use compiler::IoChannelInfo;
pub use log;
use plugin::{DynSystemPlugin, ExtFunTypeInfo, Plugin, SystemPlugin};
use runtime::vm::{self, Program, ReturnCode};
use utils::error::ReportableError;

#[cfg(not(target_arch = "wasm32"))]
use mimalloc::MiMalloc;

use crate::plugin::{MachineFunction, MacroFunction};
#[cfg(not(target_arch = "wasm32"))]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// Configuration for the compiler and runtime.
#[derive(Debug, Clone, Copy, Default)]
pub struct Config {
    pub compiler: compiler::Config,
    // pub runtime: runtime::Config,
}

/// Container for compiler context, virtual machine and plugins.
///
/// [`ExecContext`] is used as a high level API when running mimium from other
/// Rust programs.  After adding desired plugins, call
/// [`prepare_machine`](ExecContext::prepare_machine) to compile the source and
/// create a [`vm::Machine`].
pub struct ExecContext {
    compiler: Option<compiler::Context>,
    vm: Option<runtime::vm::Machine>,
    plugins: Vec<Box<dyn Plugin>>,
    sys_plugins: Vec<DynSystemPlugin>,
    path: Option<PathBuf>,
    config: Config,
}

impl ExecContext {
    //The Argument will be changed to the plugins, when the plugin system is introduced
    /// Create a new execution context with the given plugins and configuration.
    pub fn new(
        plugins: impl Iterator<Item = Box<dyn Plugin>>,
        path: Option<PathBuf>,
        config: Config,
    ) -> Self {
        let plugins = plugins
            .chain([plugin::get_builtin_fns_as_plugins()])
            .collect::<Vec<_>>();

        let sys_plugins = vec![];
        Self {
            compiler: None,
            vm: None,
            plugins,
            sys_plugins,
            path,
            config,
        }
    }
    pub fn add_plugin<T: Plugin + 'static>(&mut self, plug: T) {
        self.plugins.push(Box::new(plug))
    }
    pub fn get_plugins(&self) -> impl ExactSizeIterator<Item = &Box<dyn Plugin>> {
        self.plugins.iter()
    }
    pub fn get_system_plugins(&self) -> impl ExactSizeIterator<Item = &DynSystemPlugin> {
        self.sys_plugins.iter()
    }
    //todo: make it to builder pattern
    pub fn add_system_plugin<T: SystemPlugin + 'static>(&mut self, plug: T) {
        let plugin_dyn = DynSystemPlugin::from(plug);

        self.sys_plugins.push(plugin_dyn)
    }
    pub fn get_compiler(&self) -> Option<&compiler::Context> {
        self.compiler.as_ref()
    }
    pub fn take_compiler(&mut self) -> Option<compiler::Context> {
        self.compiler.take()
    }
    pub fn take_vm(&mut self) -> Option<runtime::vm::Machine> {
        self.vm.take()
    }
    pub fn get_vm(&self) -> Option<&runtime::vm::Machine> {
        self.vm.as_ref()
    }
    pub fn get_compiler_mut(&mut self) -> Option<&mut compiler::Context> {
        self.compiler.as_mut()
    }
    pub fn get_vm_mut(&mut self) -> Option<&mut runtime::vm::Machine> {
        self.vm.as_mut()
    }
    fn get_extfun_types(&self) -> Vec<ExtFunTypeInfo> {
        plugin::get_extfun_types(&self.plugins)
            .chain(
                self.sys_plugins
                    .iter()
                    .flat_map(|p| p.clsinfos.clone().into_iter().map(ExtFunTypeInfo::from)),
            )
            .collect()
    }
    fn get_macro_types(&self) -> Vec<Box<dyn MacroFunction>> {
        plugin::get_macro_functions(&self.plugins)
            .chain(self.sys_plugins.iter().flat_map(|p| {
                p.macroinfos
                    .iter()
                    .map(|i| Box::new(i.clone()) as Box<dyn MacroFunction>)
            }))
            .collect()
    }
    pub fn prepare_compiler(&mut self) {
        let macroinfos = plugin::get_macro_functions(&self.plugins).chain(
            self.sys_plugins.iter().flat_map(|p| {
                p.macroinfos
                    .iter()
                    .map(|i| Box::new(i.clone()) as Box<dyn MacroFunction>)
            }),
        );
        self.compiler = Some(compiler::Context::new(
            self.get_extfun_types(),
            macroinfos,
            self.path.clone(),
            self.config.compiler,
        ));
    }
    /// Compile `src` and prepare an executable VM.
    pub fn prepare_machine(&mut self, src: &str) -> Result<(), Vec<Box<dyn ReportableError>>> {
        if self.compiler.is_none() {
            self.prepare_compiler();
        }

        let prog = self.compiler.as_ref().unwrap().emit_bytecode(src)?;
        self.prepare_machine_with_bytecode(prog);
        Ok(())
    }
    pub fn prepare_machine_resume(
        &mut self,
        src: &str,
    ) -> Result<vm::Program, Vec<Box<dyn ReportableError>>> {
        if self.compiler.is_none() {
            self.prepare_compiler();
        }

        let prog = self.compiler.as_ref().unwrap().emit_bytecode(src)?;

        Ok(prog)
    }
    /// Build a VM from the given bytecode [`Program`].
    pub fn prepare_machine_with_bytecode(&mut self, prog: Program) {
        let cls =
            plugin::get_ext_closures(&self.plugins).chain(self.sys_plugins.iter().flat_map(|p| {
                p.clsinfos
                    .clone()
                    .into_iter()
                    .map(|c| Box::new(c) as Box<dyn MachineFunction>)
            }));
        let vm = vm::Machine::new(prog, [].into_iter(), cls);
        self.vm = Some(vm);
    }

    pub fn get_iochannel_count(&self) -> Option<IoChannelInfo> {
        self.vm.as_ref().and_then(|vm| vm.prog.iochannels)
    }
    /// Execute the `main` function in the loaded program.
    pub fn run_main(&mut self) -> ReturnCode {
        if let Some(vm) = self.vm.as_mut() {
            self.sys_plugins.iter().for_each(|plug: &DynSystemPlugin| {
                //todo: encapsulate unsafety within SystemPlugin functionality
                let p = unsafe { plug.inner.get().as_mut().unwrap_unchecked() };
                let _ = p.on_init(vm);
            });
            let res = vm.execute_main();
            self.sys_plugins.iter().for_each(|plug: &DynSystemPlugin| {
                //todo: encapsulate unsafety within SystemPlugin functionality
                let p = unsafe { plug.inner.get().as_mut().unwrap_unchecked() };
                let _ = p.after_main(vm);
            });
            res
        } else {
            0
        }
    }
    pub fn try_get_main_loop(&mut self) -> Option<Box<dyn FnOnce()>> {
        let mut mainloops = self.sys_plugins.iter_mut().filter_map(|p| {
            let p = unsafe { p.inner.get().as_mut().unwrap_unchecked() };
            p.try_get_main_loop()
        });
        let res = mainloops.next();
        if mainloops.next().is_some() {
            log::warn!("more than 2 main loops in system plugins found")
        }
        res
    }
}
//todo: remove
// pub mod ast_interpreter;
// pub mod repl;
