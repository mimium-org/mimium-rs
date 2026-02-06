use std::sync::{
    Arc,
    atomic::{AtomicU64, Ordering},
};

use mimium_lang::{
    compiler::IoChannelInfo,
    plugin::ExtClsInfo,
    runtime::{Time, vm},
};

use crate::driver::{Driver, RuntimeData, SampleRate};

/// Execute the program n times and write the result values to `localbuffer`.
pub struct LocalBufferDriver {
    pub vmdata: Option<RuntimeData>,
    pub count: Arc<AtomicU64>,
    samplerate: SampleRate,
    localbuffer: Vec<f64>,
    times: usize,
}

impl Default for LocalBufferDriver {
    fn default() -> Self {
        let count = Arc::new(AtomicU64::new(0));

        Self {
            vmdata: None,
            count,
            samplerate: SampleRate::from(48000),
            localbuffer: vec![],
            times: 0,
        }
    }
}

impl LocalBufferDriver {
    pub fn new(times: usize) -> Self {
        let count = Arc::new(AtomicU64::new(0));

        Self {
            vmdata: None,
            count,
            samplerate: SampleRate::from(48000),
            localbuffer: vec![],
            times,
        }
    }

    pub fn get_generated_samples(&self) -> &[<LocalBufferDriver as Driver>::Sample] {
        &self.localbuffer
    }
}

impl Driver for LocalBufferDriver {
    type Sample = f64;

    fn get_runtimefn_infos(&self) -> Vec<ExtClsInfo> {
        let getnow = crate::runtime_fn::gen_getnowfn(self.count.clone());
        let getsamplerate = crate::runtime_fn::gen_getsampleratefn(self.samplerate.0.clone());

        vec![getnow, getsamplerate]
    }

    fn init(
        &mut self,
        runtime_data: RuntimeData,
        sample_rate: Option<crate::driver::SampleRate>,
    ) -> Option<IoChannelInfo> {
        if let Some(iochannels) = runtime_data.vm.prog.iochannels {
            self.localbuffer = Vec::with_capacity(iochannels.output as usize * self.times);
            self.samplerate = sample_rate.unwrap_or(SampleRate::from(48000));
            self.vmdata = Some(runtime_data);
            Some(iochannels)
        } else {
            None
        }
    }

    fn play(&mut self) -> bool {
        let vmdata = self.vmdata.as_mut().expect("Not initialized yet?");
        let iochannels = vmdata.vm.prog.iochannels;
        let (_ichannels, ochannels) = iochannels.map_or((0, 0), |io| (io.input, io.output));
        // let _ = vmdata.run_main();
        self.localbuffer.clear();
        for _ in 0..self.times {
            let now = self.count.load(Ordering::Relaxed);
            let _ = vmdata.run_dsp(Time(now));
            let res = vm::Machine::get_as_array::<<LocalBufferDriver as Driver>::Sample>(
                vmdata.vm.get_top_n(ochannels as _),
            );
            self.localbuffer.extend_from_slice(res);
            //update current time.
            self.count.store(now + 1, Ordering::Relaxed);
        }
        false
    }

    fn pause(&mut self) -> bool {
        false
    }

    fn get_samplerate(&self) -> u32 {
        self.samplerate.get()
    }

    fn get_current_sample(&self) -> Time {
        Time(self.count.load(Ordering::Relaxed))
    }

    fn is_playing(&self) -> bool {
        false
    }
}

pub fn local_buffer_driver(times: usize) -> Box<dyn Driver<Sample = f64>> {
    Box::new(LocalBufferDriver::new(times))
}
