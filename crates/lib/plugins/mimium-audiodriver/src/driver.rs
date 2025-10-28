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
        Time,
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
    fn renew_vm(&mut self, _new_vm: vm::Program) {}
    fn get_vm_channel(&self) -> Option<mpsc::Sender<vm::Program>> {
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
pub struct RuntimeData {
    pub vm: vm::Machine,
    pub sys_plugin_workers: Vec<Box<dyn SystemPluginAudioWorker>>,
    pub dsp_i: usize,
}
impl RuntimeData {
    pub fn new(mut vm: vm::Machine, sys_plugins: &mut [DynSystemPlugin]) -> Self {
        //todo:error handling
        let dsp_i = vm.prog.get_fun_index("dsp").unwrap_or(0);
        if let Some(IoChannelInfo { input, .. }) = vm.prog.iochannels {
            vm.set_stack_range(0, &vec![0u64; input as usize]);
        }
        let sys_plugin_workers = sys_plugins
            .iter_mut()
            .filter_map(|p| p.take_audioworker())
            .collect();
        Self {
            vm,
            sys_plugin_workers,
            dsp_i,
        }
    }
    pub fn resume_with_program(&mut self, new_prog: Program) {
        if let Some(IoChannelInfo { input, .. }) = self.vm.prog.iochannels {
            self.vm.set_stack_range(0, &vec![0u64; input as usize]);
        }
        self.vm = self.vm.new_resume(new_prog);
        self.dsp_i = self.vm.prog.get_fun_index("dsp").unwrap_or(0);
    }
    // /// warn: Currently duplicated with ExecContext::run_main.
    // /// only LocalBufferDriver uses this function.
    // pub fn run_main(&mut self) -> ReturnCode {
    //     self.sys_plugins.iter().for_each(|plug: &DynSystemPlugin| {
    //         //todo: encapsulate unsafety within SystemPlugin functionality
    //         let p = unsafe { plug.inner.get().as_mut().unwrap_unchecked() };
    //         let _ = p.on_init(&mut self.vm);
    //     });
    //     let res = self.vm.execute_main();
    //     self.sys_plugins.iter().for_each(|plug: &DynSystemPlugin| {
    //         //todo: encapsulate unsafety within SystemPlugin functionality
    //         let p = unsafe { plug.inner.get().as_mut().unwrap_unchecked() };
    //         let _ = p.after_main(&mut self.vm);
    //     });
    //     res
    // }
    pub fn get_dsp_fn(&self) -> &FuncProto {
        &self.vm.prog.global_fn_table[self.dsp_i].1
    }
    /// Execute the compiled `dsp` function for one sample.
    pub fn run_dsp(&mut self, time: Time) -> ReturnCode {
        self.sys_plugin_workers.iter_mut().for_each(
            |plug: &mut Box<dyn SystemPluginAudioWorker>| {
                let _ = plug.on_sample(time, &mut self.vm);
            },
        );
        self.vm.execute_idx(self.dsp_i)
    }
}

impl TryFrom<&mut ExecContext> for RuntimeData {
    fn try_from(ctx: &mut ExecContext) -> Result<Self, Self::Error> {
        let mut vm = ctx.take_vm().ok_or(SimpleError {
            message: "Failed to take VM".into(),
            span: Location {
                span: 0..0,
                path: PathBuf::new(),
            },
        })?;
        let dsp_i = vm.prog.get_fun_index("dsp").unwrap_or(0);
        if let Some(IoChannelInfo { input, .. }) = vm.prog.iochannels {
            vm.set_stack_range(0, &vec![0u64; input as usize]);
        }
        let sys_plugin_workers = ctx
            .get_system_plugins_mut()
            .flat_map(|p| p.take_audioworker())
            .collect();
        Ok(Self {
            vm,
            sys_plugin_workers,
            dsp_i,
        })
    }
    type Error = SimpleError;
}
