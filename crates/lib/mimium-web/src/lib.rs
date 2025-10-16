use std::path::PathBuf;
use std::sync::mpsc;

use mimium_audiodriver::backends::local_buffer::LocalBufferDriver;
use mimium_audiodriver::driver::{Driver, RuntimeData};
use mimium_lang::ExecContext;
use mimium_lang::log;
use mimium_lang::runtime::vm;
use mimium_lang::utils::error::report;
use wasm_bindgen::prelude::*;
#[wasm_bindgen]
#[derive(Default)]
pub struct Config {
    pub sample_rate: f64,
    pub input_channels: u32,
    pub output_channels: u32,
    pub buffer_size: u32,
}
#[wasm_bindgen]
impl Config {
    #[wasm_bindgen]
    pub fn new() -> Self {
        Self::default()
    }
}

type Output<'a> = &'a mut [f32];
type Input<'a> = &'a [f32];

type Processer = Box<dyn FnMut(Input, Output) -> u64>;

#[wasm_bindgen]
#[derive(Default)]
pub struct Context {
    processor: Option<Processer>,
    swap_channel: Option<mpsc::Sender<vm::Program>>,
    config: Config,
}

fn get_default_context() -> ExecContext {
    let mut ctx = ExecContext::new([].into_iter(), None, Default::default());
    ctx.add_system_plugin(mimium_scheduler::get_default_scheduler_plugin());
    if let Some(midi_plug) = mimium_midi::MidiPlugin::try_new() {
        ctx.add_system_plugin(midi_plug);
    } else {
        log::warn!("Midi is not supported on this platform.")
    }
    ctx
}

#[wasm_bindgen]
impl Context {
    #[wasm_bindgen(constructor)]
    pub fn new(config: Config) -> Self {
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        Context {
            config,
            ..Default::default()
        }
    }
    #[wasm_bindgen]
    pub fn compile(&mut self, src: String) {
        let (sender, receiver) = mpsc::channel();
        self.swap_channel = Some(sender);
        let mut ctx = get_default_context();
        let mut driver = LocalBufferDriver::new(self.config.buffer_size as usize);
        ctx.add_plugin(driver.get_as_plugin());

        if let Err(e) = ctx.prepare_machine(src.as_str()) {
            report(&src, PathBuf::from("/"), &e);
        }
        ctx.run_main();
        let runtimedata = {
            let ctxmut: &mut ExecContext = &mut ctx;
            RuntimeData::try_from(ctxmut).unwrap()
        };
        let iochannels = driver.init(
            runtimedata,
            Some(mimium_audiodriver::driver::SampleRate::from(
                self.config.sample_rate as u32,
            )),
        );
        let (ichannels, ochannels) = iochannels.map_or((0, 0), |io| (io.input, io.output));
        self.config.input_channels = ichannels;
        self.config.output_channels = ochannels;
        let out_ch = self.config.output_channels;
        let mut out_buf = vec![0.0; (out_ch * self.config.buffer_size) as usize];
        self.processor = Some(Box::new(move |_input, output: Output| -> u64 {
            if let (Some(vmdata), Ok(prog)) = (driver.vmdata.as_mut(), receiver.try_recv()) {
                vmdata.vm = vmdata.vm.new_resume(prog);
            };
            driver.play();
            driver
                .get_generated_samples()
                .iter()
                .map(|f| *f as f32)
                .enumerate()
                .for_each(|(i, f)| {
                    out_buf[i] = f;
                });
            output.copy_from_slice(&out_buf);
            0
        }));
    }
    #[wasm_bindgen]
    pub fn recompile(&mut self, src: String) {
        let mut ctx = get_default_context();
        let driver = LocalBufferDriver::new(self.config.buffer_size as usize);
        ctx.add_plugin(driver.get_as_plugin());
        match ctx.prepare_machine(src.as_str()) {
            Err(e) => {
                report(&src, PathBuf::from("/"), &e);
            }
            Ok(()) => {
                ctx.run_main();
                let prog = ctx.take_vm().unwrap().prog;
                self.config.input_channels = prog.iochannels.map_or(0, |io| io.input);
                self.config.output_channels = prog.iochannels.map_or(0, |io| io.output);
                self.swap_channel.as_mut().unwrap().send(prog).unwrap();
            }
        }
    }
    #[wasm_bindgen]
    pub fn get_input_channels(&self) -> u32 {
        self.config.input_channels
    }
    #[wasm_bindgen]
    pub fn get_output_channels(&self) -> u32 {
        self.config.output_channels
    }
    /// .
    ///
    /// # Safety
    /// Array size of input and output must be equal to `input_channels * buffer_size` and `output_channels * buffer_size` respectively.
    /// .
    #[wasm_bindgen]
    pub fn process(&mut self, input: &[f32], output: &mut [f32]) -> u64 {
        unsafe { self.processor.as_mut().unwrap_unchecked()(input, output) }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_iochannels() {
        let mut ctx = Context::new(Config::default());
        ctx.compile(
            r#"fn dsp(input:float){
        (0,input)
        }"#
            .to_string(),
        );
        assert_eq!(1, ctx.get_input_channels());
        assert_eq!(2, ctx.get_output_channels());
    }
}
