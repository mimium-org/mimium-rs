//! Audio driver abstraction used by mimium.
//!
//! Drivers provide an interface between the runtime and concrete audio backends
//! such as cpal or the CSV writer used for testing.

use std::{
    path::PathBuf,
    sync::{
        Arc,
        atomic::{AtomicU32, Ordering},
        mpsc,
    },
};

use mimium_lang::{
    ExecContext,
    compiler::IoChannelInfo,
    plugin::{DynSystemPlugin, ExtClsInfo, InstantPlugin, SystemPluginAudioWorker},
    runtime::{
        DspRuntime, Time,
        vm::{self, FuncProto, Program, ReturnCode},
    },
    utils::{error::SimpleError, metadata::Location},
};
use num_traits::Float;

// #[derive(Clone)]
// pub struct PlaybackInfo {
//     pub sample_rate: u32,
//     pub current_time: usize,
//     pub frame_per_buffer: u64,
//     pub channels: u64,
// }

// impl PlaybackInfo {
//     pub fn get_current_realtime(&self) -> f32 {
//         self.current_time as f32 / self.sample_rate as f32
//     }
//     pub fn rewind(&mut self) {
//         self.current_time = 0;
//     }
// }

// pub trait Component {
//     type Sample: Float;
//     fn get_input_channels(&self) -> u64;
//     fn get_output_channels(&self) -> u64;
//     fn prepare_play(&mut self, info: &PlaybackInfo);
//     fn render(&mut self, input: &[Self::Sample], output: &mut [Self::Sample], info: &PlaybackInfo);
// }

#[derive(Clone)]
/// Shared sample rate value used by audio drivers.
pub struct SampleRate(pub Arc<AtomicU32>);
impl From<u32> for SampleRate {
    fn from(value: u32) -> Self {
        Self(Arc::new(value.into()))
    }
}
impl SampleRate {
    pub fn get(&self) -> u32 {
        self.0.load(Ordering::Relaxed)
    }
}

/// Native bytecode VM implementation of [`DspRuntime`].
///
/// Wraps the VM `Machine`, the system plugin audio workers and the DSP
/// function index so that the audio driver backends can execute one sample
/// tick without knowing VM internals.
pub struct VmDspRuntime {
    pub vm: vm::Machine,
    sys_plugin_workers: Vec<Box<dyn SystemPluginAudioWorker>>,
    dsp_i: usize,
    /// Cached output buffer – filled after each `run_dsp` call.
    output_cache: Vec<f64>,
}

impl VmDspRuntime {
    pub fn new(mut vm: vm::Machine, sys_plugins: &mut [DynSystemPlugin]) -> Self {
        let dsp_i = vm.prog.get_fun_index("dsp").unwrap_or(0);
        if let Some(IoChannelInfo { input, .. }) = vm.prog.iochannels {
            vm.set_stack_range(0, &vec![0u64; input as usize]);
        }
        let sys_plugin_workers = sys_plugins
            .iter_mut()
            .filter_map(|p| p.take_audioworker())
            .collect();
        let ochannels = vm.prog.iochannels.map_or(0, |io| io.output as usize);
        Self {
            vm,
            sys_plugin_workers,
            dsp_i,
            output_cache: vec![0.0; ochannels],
        }
    }

    /// Direct access to the underlying [`FuncProto`] for the DSP function.
    pub fn get_dsp_fn(&self) -> &FuncProto {
        &self.vm.prog.global_fn_table[self.dsp_i].1
    }
}

impl DspRuntime for VmDspRuntime {
    fn run_dsp(&mut self, time: Time) -> ReturnCode {
        self.sys_plugin_workers.iter_mut().for_each(
            |plug: &mut Box<dyn SystemPluginAudioWorker>| {
                let _ = plug.on_sample(time, &mut self.vm);
            },
        );
        let rc = self.vm.execute_idx(self.dsp_i);

        // Cache the output so get_output() can return a slice.
        let ochannels = self.vm.prog.iochannels.map_or(0, |io| io.output as usize);
        if ochannels > 0 {
            let raw = vm::Machine::get_as_array::<f64>(self.vm.get_top_n(ochannels));
            self.output_cache.clear();
            self.output_cache.extend_from_slice(raw);
        }
        rc
    }

    fn get_output(&self, n_channels: usize) -> &[f64] {
        &self.output_cache[..n_channels.min(self.output_cache.len())]
    }

    fn set_input(&mut self, input: &[f64]) {
        let raw = unsafe { std::mem::transmute::<&[f64], &[u64]>(input) };
        self.vm.set_stack_range(0, raw);
    }

    fn io_channels(&self) -> Option<IoChannelInfo> {
        self.vm.prog.iochannels
    }

    fn try_hot_swap(&mut self, new_program: Box<dyn std::any::Any + Send>) -> bool {
        if let Ok(prog) = new_program.downcast::<vm::Program>() {
            self.dsp_i = prog.get_fun_index("dsp").unwrap_or(0);
            if let Some(IoChannelInfo { input, .. }) = prog.iochannels {
                self.vm.set_stack_range(0, &vec![0u64; input as usize]);
            }
            self.vm = self.vm.new_resume(*prog);

            let ochannels = self.vm.prog.iochannels.map_or(0, |io| io.output as usize);
            self.output_cache.resize(ochannels, 0.0);
            true
        } else {
            false
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

/// Abstraction over audio backends used by the runtime.
///
/// Note: the trait does not define `new()`, allowing each backend to expose a
/// constructor with custom parameters.  The [`init`] method is called after the
/// VM is prepared to pass it ownership along with the sample rate.
pub trait Driver {
    type Sample: Float;
    fn get_runtimefn_infos(&self) -> Vec<ExtClsInfo>;
    /// Call ctx.run_main() before moving ctx to Driver with this function.
    fn init(
        &mut self,
        runtimedata: RuntimeData,
        sample_rate: Option<SampleRate>,
    ) -> Option<IoChannelInfo>;
    fn play(&mut self) -> bool;
    fn pause(&mut self) -> bool;
    fn renew_program(&mut self, _new_program: Box<dyn std::any::Any + Send>) {}
    fn get_program_channel(&self) -> Option<mpsc::Sender<Box<dyn std::any::Any + Send>>> {
        None
    }
    fn get_samplerate(&self) -> u32;
    fn get_current_sample(&self) -> Time;
    fn is_playing(&self) -> bool;
    fn get_as_plugin(&self) -> InstantPlugin {
        InstantPlugin {
            extcls: self.get_runtimefn_infos(),
            macros: vec![],
            commonfns: vec![],
        }
    }
}

/// Objects required to execute a compiled program.
///
/// Holds a [`DspRuntime`] implementation that drives per-sample execution.
/// Backends interact exclusively through the `DspRuntime` trait methods so that
/// both the native VM and WASM runtimes can be used interchangeably.
pub struct RuntimeData {
    pub runtime: Box<dyn DspRuntime>,
}

impl RuntimeData {
    /// Create `RuntimeData` from a pre-built [`DspRuntime`].
    pub fn new_from_runtime(runtime: Box<dyn DspRuntime>) -> Self {
        Self { runtime }
    }

    /// Create `RuntimeData` by wrapping a native VM `Machine` in a [`VmDspRuntime`].
    pub fn new(vm: vm::Machine, sys_plugins: &mut [DynSystemPlugin]) -> Self {
        let runtime = Box::new(VmDspRuntime::new(vm, sys_plugins));
        Self { runtime }
    }

    /// Execute the compiled `dsp` function for one sample.
    pub fn run_dsp(&mut self, time: Time) -> ReturnCode {
        self.runtime.run_dsp(time)
    }

    /// I/O channel configuration (None if the dsp function was not found).
    pub fn io_channels(&self) -> Option<IoChannelInfo> {
        self.runtime.io_channels()
    }

    /// Read the output produced by the last `run_dsp` call.
    pub fn get_output(&self, n_channels: usize) -> &[f64] {
        self.runtime.get_output(n_channels)
    }

    /// Write input samples that will be available during the next `run_dsp` call.
    pub fn set_input(&mut self, input: &[f64]) {
        self.runtime.set_input(input);
    }

    /// Attempt to hot-swap the running program.
    pub fn resume_with_program(&mut self, new_program: Box<dyn std::any::Any + Send>) -> bool {
        self.runtime.try_hot_swap(new_program)
    }

    /// Downcast the inner runtime to a concrete type.
    ///
    /// Useful for tests that need to inspect backend-specific state (e.g. VM
    /// closures). Returns `None` if the runtime is not of the requested type.
    pub fn downcast_runtime_ref<T: DspRuntime + 'static>(&self) -> Option<&T> {
        self.runtime.as_any().downcast_ref::<T>()
    }

    /// Mutable downcast of the inner runtime.
    pub fn downcast_runtime_mut<T: DspRuntime + 'static>(&mut self) -> Option<&mut T> {
        self.runtime.as_any_mut().downcast_mut::<T>()
    }
}

impl TryFrom<&mut ExecContext> for RuntimeData {
    fn try_from(ctx: &mut ExecContext) -> Result<Self, Self::Error> {
        let vm = ctx.take_vm().ok_or(SimpleError {
            message: "Failed to take VM".into(),
            span: Location {
                span: 0..0,
                path: PathBuf::new(),
            },
        })?;
        let sys_plugin_workers: Vec<Box<dyn SystemPluginAudioWorker>> = ctx
            .get_system_plugins_mut()
            .flat_map(|p| p.take_audioworker())
            .collect();
        // We already took workers above, so create VmDspRuntime manually
        let dsp_i = vm.prog.get_fun_index("dsp").unwrap_or(0);
        let ochannels = vm.prog.iochannels.map_or(0, |io| io.output as usize);
        let mut vm = vm;
        if let Some(IoChannelInfo { input, .. }) = vm.prog.iochannels {
            vm.set_stack_range(0, &vec![0u64; input as usize]);
        }
        let runtime = Box::new(VmDspRuntime {
            vm,
            sys_plugin_workers,
            dsp_i,
            output_cache: vec![0.0; ochannels],
        });
        Ok(Self { runtime })
    }
    type Error = SimpleError;
}
