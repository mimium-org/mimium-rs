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

use crate::plugin::{MachineFunction, MacroFunction};
use compiler::IoChannelInfo;
pub use log;
use plugin::{DynSystemPlugin, ExtFunTypeInfo, Plugin, SystemPlugin};
use runtime::vm::{self, Program, ReturnCode};
use utils::error::ReportableError;

// #[cfg(not(target_arch = "wasm32"))]
// use mimalloc::MiMalloc;

// #[cfg(not(target_arch = "wasm32"))]
// #[global_allocator]
// static GLOBAL: MiMalloc = MiMalloc;

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
    #[cfg(not(target_arch = "wasm32"))]
    plugin_loader: Option<plugin::loader::PluginLoader>,
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
            #[cfg(not(target_arch = "wasm32"))]
            plugin_loader: None,
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
    pub fn get_system_plugins_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut DynSystemPlugin> {
        self.sys_plugins.iter_mut()
    }
    //todo: make it to builder pattern
    pub fn add_system_plugin<T: SystemPlugin + 'static>(&mut self, plug: T) {
        let plugin_dyn = DynSystemPlugin::from(plug);

        self.sys_plugins.push(plugin_dyn)
    }

    /// Initialize the dynamic plugin loader.
    ///
    /// This must be called before loading any dynamic plugins.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn init_plugin_loader(&mut self) {
        if self.plugin_loader.is_none() {
            self.plugin_loader = Some(plugin::loader::PluginLoader::new());
        }
    }

    /// Load a dynamic plugin from the specified path.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the plugin library (without extension).
    ///
    /// # Errors
    ///
    /// Returns an error if the plugin cannot be loaded or is invalid.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn load_dynamic_plugin<P: AsRef<std::path::Path>>(
        &mut self,
        path: P,
    ) -> Result<(), plugin::loader::PluginLoaderError> {
        self.init_plugin_loader();
        if let Some(loader) = &mut self.plugin_loader {
            loader.load_plugin(path)?;
        }
        Ok(())
    }

    /// Load all plugins from the standard plugin directory.
    ///
    /// This is a convenience method that loads all compatible plugins
    /// from the default plugin directory.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn load_builtin_dynamic_plugins(
        &mut self,
    ) -> Result<(), plugin::loader::PluginLoaderError> {
        self.init_plugin_loader();
        if let Some(loader) = &mut self.plugin_loader {
            loader.load_builtin_plugins()?;
        }
        Ok(())
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
    /// Collect type information for all external functions from all plugin
    /// sources: regular plugins, system plugins, and dynamically loaded plugins.
    pub fn get_extfun_types(&self) -> Vec<ExtFunTypeInfo> {
        let static_types = plugin::get_extfun_types(&self.plugins).chain(
            self.sys_plugins
                .iter()
                .flat_map(|p| p.clsinfos.clone().into_iter().map(ExtFunTypeInfo::from)),
        );

        #[cfg(not(target_arch = "wasm32"))]
        {
            let dynamic_types = self
                .plugin_loader
                .as_ref()
                .map(|loader| loader.get_type_infos().into_iter())
                .into_iter()
                .flatten();

            static_types.chain(dynamic_types).collect()
        }

        #[cfg(target_arch = "wasm32")]
        {
            static_types.collect()
        }
    }
    fn get_macro_types(&self) -> Vec<Box<dyn MacroFunction>> {
        let static_macros = plugin::get_macro_functions(&self.plugins).chain(
            self.sys_plugins.iter().flat_map(|p| {
                p.macroinfos
                    .iter()
                    .map(|i| Box::new(i.clone()) as Box<dyn MacroFunction>)
            }),
        );

        #[cfg(not(target_arch = "wasm32"))]
        {
            let dynamic_macros = self
                .plugin_loader
                .as_ref()
                .map(|loader| {
                    loader
                        .get_macro_functions()
                        .into_iter()
                        .map(|(_, _, macro_fn)| macro_fn)
                })
                .into_iter()
                .flatten();

            static_macros.chain(dynamic_macros).collect()
        }

        #[cfg(target_arch = "wasm32")]
        {
            static_macros.collect()
        }
    }
    pub fn prepare_compiler(&mut self) {
        let macroinfos = self.get_macro_types();

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
