use std::sync::{
    Arc,
    atomic::{AtomicU64, Ordering},
};

use mimium_lang::{compiler::IoChannelInfo, plugin::ExtClsInfo, runtime::Time};

use crate::driver::{Driver, RuntimeData, SampleRate};

/// Execute the program n times and write the result values to `localbuffer`.
pub struct LocalBufferDriver {
    pub vmdata: Option<RuntimeData>,
    pub count: Arc<AtomicU64>,
    samplerate: SampleRate,
    localbuffer: Vec<f64>,
    times: usize,
    block_size: usize,
    block_size_explicit: bool,
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
            block_size: 1,
            block_size_explicit: false,
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
            block_size: times.max(1),
            block_size_explicit: false,
        }
    }

    pub fn with_block_size(times: usize, block_size: usize) -> Self {
        let count = Arc::new(AtomicU64::new(0));

        Self {
            vmdata: None,
            count,
            samplerate: SampleRate::from(48000),
            localbuffer: vec![],
            times,
            block_size: block_size.max(1),
            block_size_explicit: true,
        }
    }

    pub fn set_block_size(&mut self, block_size: usize) {
        self.block_size = block_size.max(1);
        self.block_size_explicit = true;
    }

    pub fn block_size(&self) -> usize {
        self.block_size
    }

    /// Update sample rate while preserving the shared atomic captured by runtime closures.
    pub fn set_sample_rate(&mut self, sample_rate: SampleRate) {
        self.samplerate
            .0
            .store(sample_rate.get(), Ordering::Relaxed);
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
        let mut runtime_data = runtime_data;
        if !self.block_size_explicit
            && let Some(preferred_block_size) = runtime_data.preferred_block_size()
        {
            self.block_size = preferred_block_size.max(1);
        }
        if let Some(iochannels) = runtime_data.io_channels() {
            self.localbuffer = Vec::with_capacity(iochannels.output as usize * self.times);
            let effective_sr = sample_rate.unwrap_or(SampleRate::from(48000));
            self.set_sample_rate(effective_sr);
            runtime_data
                .runtime
                .set_sample_rate(self.samplerate.get() as f64);
            self.vmdata = Some(runtime_data);
            Some(iochannels)
        } else {
            None
        }
    }

    fn play(&mut self) -> bool {
        let vmdata = self.vmdata.as_mut().expect("Not initialized yet?");
        let iochannels = vmdata.io_channels();
        let (_ichannels, ochannels) = iochannels.map_or((0, 0), |io| (io.input, io.output));
        self.localbuffer.clear();
        let mut rendered = 0;
        while rendered < self.times {
            let now = self.count.load(Ordering::Relaxed);
            let frames = (self.times - rendered).min(self.block_size);
            let count = self.count.clone();
            let mut update_time = move |time: Time| {
                count.store(time.0, Ordering::Relaxed);
            };
            let rc = vmdata.run_dsp_block(Time(now), frames, &mut self.localbuffer, &mut update_time);
            if rc < 0 {
                break;
            }
            rendered += frames;
            self.count.store(now + frames as u64, Ordering::Relaxed);
        }
        debug_assert_eq!(
            self.localbuffer.len(),
            self.times * ochannels as usize,
            "local buffer length should match rendered sample count"
        );
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

#[cfg(test)]
mod tests {
    use crate::driver::{Driver, RuntimeData};
    use mimium_lang::{Config, ExecContext, plugin::Plugin};

    use super::LocalBufferDriver;

    fn render_samples(times: usize, block_size: usize, src: &str) -> Vec<f64> {
        let mut driver = LocalBufferDriver::with_block_size(times, block_size);
        let audiodriverplug: Box<dyn Plugin> = Box::new(driver.get_as_plugin());
        let mut ctx = ExecContext::new([audiodriverplug].into_iter(), None, Config::default());

        ctx.prepare_machine(src).unwrap();
        let _ = ctx.run_main();
        let runtimedata = {
            let ctxmut: &mut ExecContext = &mut ctx;
            RuntimeData::try_from(ctxmut).unwrap()
        };
        driver.init(runtimedata, None);
        driver.play();
        driver.get_generated_samples().to_vec()
    }

    #[test]
    fn block_render_matches_sample_render() {
        let src = r#"
fn counter(){
    self + 1.0
}

fn dsp(){
    let value = sin(counter() * 0.01)
    (value, value * 0.5)
}
"#;

        let sample = render_samples(128, 1, src);
        let block = render_samples(128, 64, src);

        assert_eq!(sample, block);
    }

    #[test]
    fn block_fast_path_matches_sample_render() {
        let src = r#"
fn dsp(){
    let base = sin(0.25 * 6.28318530718)
    let folded = ((base * 0.7 + 0.3) * (base * 0.5 + 0.1)) + sqrt(abs(base) + 1.0)
    (folded, folded * 0.5)
}
"#;

        let sample = render_samples(512, 1, src);
        let block = render_samples(512, 128, src);

        assert_eq!(sample, block);
    }

    #[test]
    fn init_adopts_runtime_preferred_block_size() {
        let mut driver = LocalBufferDriver::new(128);
        let audiodriverplug: Box<dyn Plugin> = Box::new(driver.get_as_plugin());
        let mut ctx = ExecContext::new(
            [audiodriverplug].into_iter(),
            None,
            Config {
                compiler: mimium_lang::compiler::Config {
                    dsp_block_size: Some(32),
                    ..Default::default()
                },
            },
        );

        ctx.prepare_machine("fn dsp(){ (0.0, 0.0) }").unwrap();
        let _ = ctx.run_main();
        let runtime = {
            let ctxmut: &mut ExecContext = &mut ctx;
            RuntimeData::try_from(ctxmut).unwrap()
        };

        driver.init(runtime, None);
        assert_eq!(driver.block_size(), 32);
    }
}
