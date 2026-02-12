//! Scheduler plugin for mimium.
//!
//! This plugin provides a simple synchronous event scheduler which is used by
//! the runtime to execute scheduled tasks at sample boundaries.
//!
//! The scheduler communicates with the audio worker through a lock-free channel.
//! `schedule_at` enqueues tasks from the main thread, and the audio worker
//! dequeues and executes them at the correct sample time.
//!
//! All VM interaction goes through [`RuntimeHandle`](mimium_lang::runtime::ffi::RuntimeHandle)
//! rather than accessing `Machine` directly, making the scheduler compatible
//! with both the native VM and the WASM backend.

use mimium_lang::plugin::{SysPluginSignature, SystemPlugin, SystemPluginAudioWorker};
use mimium_lang::{
    function, numeric,
    plugin::SystemPluginFnType,
    types::Type,
    unit,
};

mod scheduler;
pub use scheduler::SimpleScheduler;

impl SystemPlugin for SimpleScheduler {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn generate_audioworker(&mut self) -> Option<Box<dyn SystemPluginAudioWorker>> {
        Some(Box::new(self.take_audio_worker().unwrap()))
    }

    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        let fun: SystemPluginFnType<Self> = Self::schedule_at;
        let schedule_fn = SysPluginSignature::new(
            "_mimium_schedule_at",
            fun,
            function!(vec![numeric!(), function!(vec![], unit!())], unit!()),
        );
        vec![schedule_fn]
    }
}

/// Return a [`SystemPlugin`] with the default synchronous scheduler.
pub fn get_default_scheduler_plugin() -> impl SystemPlugin {
    SimpleScheduler::default()
}
