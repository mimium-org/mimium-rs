//! Scheduler plugin for mimium.
//!
//! This plugin provides a simple synchronous event scheduler which is used by
//! the runtime to execute scheduled tasks at sample boundaries.
use mimium_lang::plugin::{SysPluginSignature, SystemPlugin};
use mimium_lang::runtime::vm::{self, ClosureIdx, Machine, ReturnCode};
use mimium_lang::runtime::Time;
use mimium_lang::{
    function, numeric,
    types::{PType, Type},
    unit,
};
mod scheduler;
pub use scheduler::{DummyScheduler, SchedulerInterface, SyncScheduler};

/// Wrapper type implementing [`SystemPlugin`] using a [`SchedulerInterface`].
pub struct Scheduler<T: SchedulerInterface>(T);

impl<T: SchedulerInterface> Scheduler<T> {
    fn schedule_at(&mut self, machine: &mut Machine) -> ReturnCode {
        let time = Time(vm::Machine::get_as::<f64>(machine.get_stack(0)) as u64);
        let clsid = vm::Machine::get_as::<ClosureIdx>(machine.get_stack(1));
        self.0.schedule_at(time, clsid);
        0
    }
}

impl<T: SchedulerInterface + 'static> SystemPlugin for Scheduler<T> {
    fn on_init(&mut self, _machine: &mut Machine) -> ReturnCode {
        0
    }

    fn on_sample(&mut self, time: Time, machine: &mut Machine) -> ReturnCode {
        self.0.set_cur_time(time);
        while let Some(task_cls) = self.0.pop_task(time) {
            let closure = machine.get_closure(task_cls);
            machine.execute(closure.fn_proto_pos, Some(task_cls));
            vm::drop_closure(&mut machine.closures, task_cls);
        }

        0
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
}

/// Return a [`SystemPlugin`] with the default synchronous scheduler.
pub fn get_default_scheduler_plugin() -> impl SystemPlugin {
    Scheduler::<_>(SyncScheduler::new())
}
