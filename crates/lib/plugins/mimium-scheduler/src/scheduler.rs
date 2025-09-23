use std::{cmp::Reverse, collections::BinaryHeap};

use mimium_lang::runtime::{Time, vm::ClosureIdx};

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
/// Trait representing a scheduling strategy.
pub trait SchedulerInterface {
    fn new() -> Self
    where
        Self: Sized;
    fn schedule_at(&mut self, time: Time, task: ClosureIdx);
    fn pop_task(&mut self, now: Time) -> Option<ClosureIdx>;
    fn set_cur_time(&mut self, time: Time);
}

/// Scheduler that checks for ready tasks on every sample and executes them.
#[derive(Clone)]
pub struct SyncScheduler {
    tasks: BinaryHeap<Reverse<Task>>,
    cur_time: Time,
}

impl SchedulerInterface for SyncScheduler {
    fn new() -> Self {
        Self {
            tasks: Default::default(),
            cur_time: Time(0),
        }
    }
    fn schedule_at(&mut self, when: Time, cls: ClosureIdx) {
        if when <= self.cur_time {
            // TODO: ideally, this should not stop runtime.
            panic!(
                "A task must be scheduled in future (current time: {}, schedule: {})",
                self.cur_time.0, when.0
            )
        }
        self.tasks.push(Reverse(Task { when, cls }));
    }

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
        self.cur_time = time
    }
}

#[derive(Clone)]
pub struct DummyScheduler;
impl SchedulerInterface for DummyScheduler {
    fn new() -> Self
    where
        Self: Sized,
    {
        Self
    }

    fn schedule_at(&mut self, _time: Time, _task: ClosureIdx) {
        // do nothing
    }

    fn pop_task(&mut self, _now: Time) -> Option<ClosureIdx> {
        // do nothing
        None
    }

    fn set_cur_time(&mut self, _time: Time) {
        // do nothing
    }
}
