//! Audio driver abstraction used by mimium.
//!
//! Drivers provide an interface between the runtime and concrete audio backends
//! such as cpal or the CSV writer used for testing.

use std::sync::{
    atomic::{AtomicU32, Ordering},
    Arc,
};

use mimium_lang::{
    compiler::IoChannelInfo, interner::ToSymbol, plugin::{DynSystemPlugin, ExtClsInfo, InstantPlugin}, runtime::{
        vm::{self, FuncProto, ReturnCode},
        Time,
    }, ExecContext
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
    fn init(&mut self, ctx: ExecContext, sample_rate: Option<SampleRate>) -> Option<IoChannelInfo>;
    fn play(&mut self) -> bool;
    fn pause(&mut self) -> bool;
    fn get_samplerate(&self) -> u32;
    fn get_current_sample(&self) -> Time;
    fn is_playing(&self) -> bool;
    fn get_as_plugin(&self) -> InstantPlugin {
        InstantPlugin {
            extfns: vec![],
            extcls: self.get_runtimefn_infos(),
        }
    }
}

/// Objects required to execute a compiled program.
pub struct RuntimeData {
    pub vm: vm::Machine,
    pub sys_plugins: Vec<DynSystemPlugin>,
    pub dsp_i: usize,
}
impl RuntimeData {
    pub fn new(vm: vm::Machine, sys_plugins: Vec<DynSystemPlugin>) -> Self {
        //todo:error handling
        let dsp_i = vm.prog.get_fun_index(&"dsp".to_symbol()).unwrap_or(0);
        Self {
            vm,
            sys_plugins,
            dsp_i,
        }
    }

    /// warn: Currently duplicated with ExecContext::run_main.
    /// only LocalBufferDriver uses this function.
    pub fn run_main(&mut self) -> ReturnCode {
        self.sys_plugins.iter().for_each(|plug: &DynSystemPlugin| {
            //todo: encapsulate unsafety within SystemPlugin functionality
            let p = unsafe { plug.0.get().as_mut().unwrap_unchecked() };
            let _ = p.on_init(&mut self.vm);
        });
        let res = self.vm.execute_main();
        self.sys_plugins.iter().for_each(|plug: &DynSystemPlugin| {
            //todo: encapsulate unsafety within SystemPlugin functionality
            let p = unsafe { plug.0.get().as_mut().unwrap_unchecked() };
            let _ = p.after_main(&mut self.vm);
        });
        res
    }
    pub fn get_dsp_fn(&self) -> &FuncProto {
        &self.vm.prog.global_fn_table[self.dsp_i].1
    }
    /// Execute the compiled `dsp` function for one sample.
    pub fn run_dsp(&mut self, time: Time) -> ReturnCode {
        self.sys_plugins.iter().for_each(|plug: &DynSystemPlugin| {
            //todo: encapsulate unsafety within SystemPlugin functionality
            let p = unsafe { plug.0.get().as_mut().unwrap_unchecked() };
            let _ = p.on_sample(time, &mut self.vm);
        });
        self.vm.execute_idx(self.dsp_i)
    }
}