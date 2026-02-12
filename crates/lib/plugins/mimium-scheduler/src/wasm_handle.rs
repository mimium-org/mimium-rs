//! WASM-side scheduler handle.
//!
//! [`WasmSchedulerHandle`] is the WASM counterpart of [`SchedulerAudioWorker`].
//! It holds a shared, sorted task queue that the `_mimium_schedule_at`
//! trampoline pushes into and the per-sample driver drains from.
//!
//! The handle is created by [`SimpleScheduler::freeze_for_wasm()`] and shared
//! (via `Arc<Mutex<…>>`) between:
//!
//! * The `WasmPluginFnMap` closure registered as the host import for
//!   `_mimium_schedule_at`, and
//! * The caller that drives per-sample execution (e.g. the test runner or
//!   `WasmDspRuntime`), which calls [`drain_due_tasks`] each tick.

use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::sync::{Arc, Mutex};

use mimium_lang::runtime::Time;

use crate::scheduler::Task;

/// Mutable state shared between the WASM trampoline and the per-sample driver.
#[derive(Debug, Default)]
struct SharedState {
    /// Min-heap of scheduled tasks, ordered by `when`.
    tasks: BinaryHeap<Reverse<Task>>,
    /// Current time (samples). Updated by the driver before draining.
    current_time: u64,
}

/// Handle for driving the scheduler from the WASM host side.
///
/// Cheaply cloneable (inner state is `Arc<Mutex<…>>`).
#[derive(Clone, Debug, Default)]
pub struct WasmSchedulerHandle {
    state: Arc<Mutex<SharedState>>,
}

impl WasmSchedulerHandle {
    /// Update the current time. Call this once per sample *before*
    /// [`drain_due_tasks`].
    pub fn set_current_time(&self, time: u64) {
        self.state.lock().unwrap().current_time = time;
    }

    /// Drain all tasks whose `when <= now` and return their closure
    /// addresses (linear-memory pointers that should be passed to
    /// `_mimium_exec_closure_void`).
    pub fn drain_due_tasks(&self) -> Vec<i64> {
        let mut state = self.state.lock().unwrap();
        let now = Time(state.current_time);
        let mut ready = Vec::new();
        while let Some(Reverse(task)) = state.tasks.peek() {
            if task.when <= now {
                ready.push(state.tasks.pop().unwrap().0.closure as i64);
            } else {
                break;
            }
        }
        ready
    }

    /// Build a [`WasmPluginFnMap`] containing the `_mimium_schedule_at`
    /// trampoline closure.
    ///
    /// The closure captures a clone of the inner `Arc`, so calling this
    /// multiple times (or cloning the handle first) always operates on the
    /// same shared task queue.
    pub fn into_wasm_plugin_fn_map(
        &self,
    ) -> mimium_lang::runtime::wasm::WasmPluginFnMap {
        let state = Arc::clone(&self.state);
        let schedule_fn: mimium_lang::runtime::wasm::WasmPluginFn =
            Arc::new(move |args: &[f64]| -> Option<f64> {
                // args[0] = when (f64, absolute sample time)
                // args[1] = closure_addr (i64 bit-cast to f64)
                if args.len() < 2 {
                    log::error!(
                        "_mimium_schedule_at: expected 2 args, got {}",
                        args.len()
                    );
                    return Some(0.0);
                }
                let when = args[0] as u64;
                let closure_addr = args[1] as i64;

                let mut s = state.lock().unwrap();
                if when <= s.current_time {
                    panic!(
                        "Scheduled time {} must be in the future (current time: {})",
                        when, s.current_time
                    );
                }
                s.tasks.push(Reverse(Task::new(Time(when), closure_addr as u64)));
                // Return value is unused (unit), but the trampoline expects
                // `Option<f64>`.
                Some(0.0)
            });

        let mut map = mimium_lang::runtime::wasm::WasmPluginFnMap::new();
        map.insert("_mimium_schedule_at".to_string(), schedule_fn);
        map
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_schedule_and_drain() {
        let handle = WasmSchedulerHandle::default();
        let map = handle.into_wasm_plugin_fn_map();
        let schedule = map.get("_mimium_schedule_at").unwrap();

        // Schedule tasks at times 3 and 5
        schedule(&[3.0, 100.0]); // closure addr 100 at time 3
        schedule(&[5.0, 200.0]); // closure addr 200 at time 5

        // At time 2, nothing is due
        handle.set_current_time(2);
        assert!(handle.drain_due_tasks().is_empty());

        // At time 3, first task is due
        handle.set_current_time(3);
        let due = handle.drain_due_tasks();
        assert_eq!(due, vec![100]);

        // At time 10, second task is due
        handle.set_current_time(10);
        let due = handle.drain_due_tasks();
        assert_eq!(due, vec![200]);

        // Queue is empty now
        assert!(handle.drain_due_tasks().is_empty());
    }
}
