use std::{cmp::Reverse, collections::BinaryHeap, sync::mpsc};

use mimium_lang::{
    plugin::SystemPluginAudioWorker,
    runtime::{
        Time,
        vm::{self, ClosureIdx, Machine, ReturnCode, heap},
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Scheduled task to be executed at a specific time.
pub struct Task {
    when: Time,
    cls: ClosureIdx,
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
    fn pop_task(&mut self, now: Time) -> Option<ClosureIdx> {
        match self.tasks.peek() {
            Some(Reverse(Task { when, cls })) if *when <= now => {
                let res = Some(*cls);
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
        while let Some(task_cls) = self.pop_task(time) {
            let closure = machine.get_closure(task_cls);
            machine.execute(closure.fn_proto_pos, Some(task_cls));
            machine.drop_closure(task_cls);
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
    fn schedule_at_inner(&mut self, when: Time, cls: ClosureIdx) {
        self.sender.send(Task { when, cls }).unwrap();
    }
    pub fn schedule_at(&mut self, machine: &mut Machine) -> ReturnCode {
        let when = Machine::get_as::<f64>(machine.get_stack(0)) as u64;
        // The stack now holds a HeapIdx (heap-allocated closure), not a raw ClosureIdx
        let heap_idx = Machine::get_as::<heap::HeapIdx>(machine.get_stack(1));
        let closure_idx = machine.get_closure_idx_from_heap(heap_idx);
        self.schedule_at_inner(Time(when), closure_idx);
        0
    }
}
