//! Scheduler plugin for mimium.
//!
//! This plugin provides a simple synchronous event scheduler which is used by
//! the runtime to execute scheduled tasks at sample boundaries.
use mimium_lang::plugin::{SysPluginSignature, SystemPlugin, SystemPluginAudioWorker};

use mimium_lang::runtime::vm::{Machine, ReturnCode};
use mimium_lang::{
    function, numeric,
    types::{PType, Type},
    unit,
};
mod scheduler;
pub use scheduler::SimpleScheduler;

impl SystemPlugin for SimpleScheduler {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn on_init(&mut self, _machine: &mut Machine) -> ReturnCode {
        0
    }
    fn generate_audioworker(&mut self) -> Option<Box<dyn SystemPluginAudioWorker>> {
        Some(Box::new(self.take_audio_worker().unwrap()))
    }

    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        let fun: fn(&mut Self, &mut Machine) -> ReturnCode = Self::schedule_at;
        let schedule_fn = SysPluginSignature::new(
            "_mimium_schedule_at",
            fun,
            function!(vec![numeric!(), function!(vec![], unit!())], unit!()),
        );
        vec![schedule_fn]
    }

    fn after_main(&mut self, _machine: &mut Machine) -> ReturnCode {
        0
    }

    fn try_get_main_loop(&mut self) -> Option<Box<dyn FnOnce()>> {
        None
    }
}

/// Return a [`SystemPlugin`] with the default synchronous scheduler.
pub fn get_default_scheduler_plugin() -> impl SystemPlugin {
    SimpleScheduler::default()
}
