use std::{cmp::Reverse, collections::BinaryHeap, sync::mpsc};

use mimium_lang::{
    plugin::SystemPluginAudioWorker,
    runtime::{
        Time,
        vm::{Machine, ReturnCode},
        vm_ffi,
    },
};

/// Opaque, backend-agnostic closure handle.
///
/// On the native VM this is a transmuted `ClosureIdx`; on the WASM backend
/// it will be a table index or similar.  The scheduler never inspects its
/// contents — it simply passes it back through `RuntimeHandle::execute_closure`.
type ClosureHandle = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Scheduled task to be executed at a specific time.
pub struct Task {
    when: Time,
    closure: ClosureHandle,
}
impl PartialOrd for Task {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Task {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.when.cmp(&other.when)
    }
}

/// Scheduler that checks for ready tasks on every sample and executes them.
pub struct SimpleScheduler {
    sender: mpsc::Sender<Task>,
    audio_worker: Option<SchedulerAudioWorker>,
}
impl SimpleScheduler {
    pub fn take_audio_worker(&mut self) -> Option<SchedulerAudioWorker> {
        self.audio_worker.take()
    }
}
pub struct SchedulerAudioWorker {
    cur_time: Time,
    tasks: BinaryHeap<Reverse<Task>>,
    receiver: mpsc::Receiver<Task>,
}

impl SchedulerAudioWorker {
    fn pop_task(&mut self, now: Time) -> Option<ClosureHandle> {
        match self.tasks.peek() {
            Some(Reverse(Task { when, closure })) if *when <= now => {
                let res = Some(*closure);
                let _ = self.tasks.pop();
                res
            }
            _ => None,
        }
    }
    fn set_cur_time(&mut self, time: Time) {
        self.cur_time = time;
    }
}

impl SystemPluginAudioWorker for SchedulerAudioWorker {
    fn on_sample(&mut self, time: Time, machine: &mut Machine) -> ReturnCode {
        while let Ok(task) = self.receiver.try_recv() {
            if task.when <= self.cur_time {
                panic!(
                    "Scheduled time {:?} must be in the future (current time: {:?})",
                    task.when, self.cur_time
                );
            }
            self.tasks.push(Reverse(task));
        }
        self.set_cur_time(time);

        // Execute ready tasks through the RuntimeHandle abstraction.
        let mut handle = unsafe { vm_ffi::runtime_handle_from_machine(machine) };
        while let Some(closure) = self.pop_task(time) {
            handle.execute_closure(closure);
        }
        0
    }

    fn gen_interfaces(&self) -> Vec<mimium_lang::plugin::SysPluginSignature> {
        vec![]
    }
}
impl Default for SimpleScheduler {
    fn default() -> Self {
        let (sender, receiver) = mpsc::channel();
        Self {
            sender,
            audio_worker: Some(SchedulerAudioWorker {
                cur_time: Time(0),
                tasks: BinaryHeap::new(),
                receiver,
            }),
        }
    }
}
impl SimpleScheduler {
    fn schedule_at_inner(&mut self, when: Time, closure: ClosureHandle) {
        self.sender.send(Task { when, closure }).unwrap();
    }

    /// Schedule a closure for future execution.
    ///
    /// Reads the time and closure arguments from the VM stack via
    /// [`RuntimeHandle`], resolves the closure handle, and enqueues a
    /// task for the audio worker.
    pub fn schedule_at(&mut self, machine: &mut Machine) -> ReturnCode {
        let handle = unsafe { vm_ffi::runtime_handle_from_machine(machine) };
        let when = handle.get_arg_f64(0) as u64;
        let heap_raw = handle.get_arg_raw(1);
        let closure = handle.resolve_closure(heap_raw);
        self.schedule_at_inner(Time(when), closure);
        0
    }
}
