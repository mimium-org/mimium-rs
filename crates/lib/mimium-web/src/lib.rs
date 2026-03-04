#![cfg(target_arch = "wasm32")]

use std::path::PathBuf;
use std::sync::mpsc;

use mimium_audiodriver::backends::local_buffer::LocalBufferDriver;
use mimium_audiodriver::driver::{Driver, RuntimeData};
use mimium_lang::ExecContext;
use mimium_lang::log;
use mimium_lang::runtime::{ProgramPayload, vm};
use mimium_lang::utils::error::{dump_to_string, report};
use mimium_lang::utils::fileloader;
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
    module_base_url: Option<String>,
}

fn get_default_context() -> ExecContext {
    let mut ctx = ExecContext::new([].into_iter(), None, Default::default());
    ctx.add_system_plugin(mimium_scheduler::get_default_scheduler_plugin());
    log::warn!("Midi plugin is disabled on mimium-web.");
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
    fn compile_inner(&mut self, src: String) -> Result<(), JsValue> {
        let (sender, receiver) = mpsc::channel();
        let mut ctx = get_default_context();
        let mut driver = LocalBufferDriver::new(self.config.buffer_size as usize);
        ctx.add_plugin(driver.get_as_plugin());

        let prepare_result = ctx.prepare_machine(src.as_str());
        if let Err(e) = prepare_result {
            report(&src, PathBuf::from("/"), &e);
            let message = dump_to_string(&e);
            let message = if message.is_empty() {
                "Compilation failed".to_string()
            } else {
                message
            };
            return Err(JsValue::from_str(&message));
        }

        ctx.run_main();
        let runtimedata = {
            let ctxmut: &mut ExecContext = &mut ctx;
            RuntimeData::try_from(ctxmut).map_err(|e| JsValue::from_str(&e.to_string()))?
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
            if let Ok(prog) = receiver.try_recv()
                && let Some(vmdata) = driver.vmdata.as_mut()
            {
                vmdata.resume_with_program(ProgramPayload::VmProgram(prog));
            }
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
        self.swap_channel = Some(sender);
        Ok(())
    }

    #[wasm_bindgen]
    pub async fn compile(&mut self, src: String) -> Result<(), JsValue> {
        if fileloader::has_network_api() {
            self.init_github_lib_cache().await?;
            fileloader::preload_user_module_cache(src.as_str(), self.module_base_url.as_deref())
                .await
                .map_err(|e| JsValue::from_str(&e))?;
        }
        self.compile_inner(src)
    }

    #[wasm_bindgen]
    pub fn compile_direct(&mut self, src: String) -> Result<(), JsValue> {
        self.compile_inner(src)
    }

    #[wasm_bindgen]
    pub fn set_module_base_url(&mut self, base_url: String) -> Result<(), JsValue> {
        let normalized = if base_url.trim().is_empty() {
            None
        } else {
            Some(base_url)
        };
        fileloader::set_module_base_url(normalized.as_deref())
            .map_err(|e| JsValue::from_str(&e))?;
        self.module_base_url = normalized;
        Ok(())
    }

    #[wasm_bindgen]
    pub async fn init_github_lib_cache(&self) -> Result<(), JsValue> {
        mimium_lang::utils::fileloader::preload_github_stdlib_cache()
            .await
            .map_err(|e| JsValue::from_str(&e))
    }

    #[wasm_bindgen]
    pub async fn init_lib_cache_with_base_url(&self, base_url: String) -> Result<(), JsValue> {
        mimium_lang::utils::fileloader::preload_stdlib_cache_with_base_url(base_url.as_str())
            .await
            .map_err(|e| JsValue::from_str(&e))
    }

    #[wasm_bindgen]
    pub fn put_virtual_file_cache(&self, path: String, content: String) -> Result<(), JsValue> {
        fileloader::put_virtual_file_cache(path.as_str(), content.as_str())
            .map_err(|e| JsValue::from_str(&e))
    }

    #[wasm_bindgen]
    pub fn clear_virtual_file_cache(&self) -> Result<(), JsValue> {
        fileloader::clear_virtual_file_cache().map_err(|e| JsValue::from_str(&e))
    }

    #[wasm_bindgen]
    pub fn export_virtual_file_cache_json(&self) -> Result<String, JsValue> {
        fileloader::export_virtual_file_cache_json().map_err(|e| JsValue::from_str(&e))
    }

    #[wasm_bindgen]
    pub fn import_virtual_file_cache_json(&self, payload: String) -> Result<(), JsValue> {
        fileloader::import_virtual_file_cache_json(payload.as_str())
            .map_err(|e| JsValue::from_str(&e))
    }

    fn recompile_inner(&mut self, src: String) -> Result<(), JsValue> {
        let mut ctx = get_default_context();
        let driver = LocalBufferDriver::new(self.config.buffer_size as usize);
        ctx.add_plugin(driver.get_as_plugin());
        let prepare_result = ctx.prepare_machine(src.as_str());
        if let Err(e) = prepare_result {
            report(&src, PathBuf::from("/"), &e);
            let message = dump_to_string(&e);
            let message = if message.is_empty() {
                "Compilation failed".to_string()
            } else {
                message
            };
            return Err(JsValue::from_str(&message));
        }

        ctx.run_main();
        let prog = ctx
            .take_vm()
            .ok_or_else(|| JsValue::from_str("Failed to take VM while recompiling"))?
            .prog;
        self.config.input_channels = prog.iochannels.map_or(0, |io| io.input);
        self.config.output_channels = prog.iochannels.map_or(0, |io| io.output);
        self.swap_channel
            .as_mut()
            .ok_or_else(|| {
                JsValue::from_str("Compiler context is not initialized. Call compile() first.")
            })?
            .send(prog)
            .map_err(|e| {
                JsValue::from_str(&format!("Failed to send program to audio thread: {e}"))
            })?;
        Ok(())
    }

    #[wasm_bindgen]
    pub async fn recompile(&mut self, src: String) -> Result<(), JsValue> {
        if fileloader::has_network_api() {
            self.init_github_lib_cache().await?;
            fileloader::preload_user_module_cache(src.as_str(), self.module_base_url.as_deref())
                .await
                .map_err(|e| JsValue::from_str(&e))?;
        }
        self.recompile_inner(src)
    }

    #[wasm_bindgen]
    pub fn recompile_direct(&mut self, src: String) -> Result<(), JsValue> {
        self.recompile_inner(src)
    }
    #[wasm_bindgen]
    pub fn get_input_channels(&self) -> u32 {
        self.config.input_channels
    }
    #[wasm_bindgen]
    pub fn get_output_channels(&self) -> u32 {
        self.config.output_channels.max(1)
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
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test(async)]
    async fn test_iochannels() {
        let mut ctx = Context::new(Config::default());
        ctx.compile(
            r#"fn dsp(input:float){
        (0,input)
        }"#
            .to_string(),
        )
        .await
        .expect("compile should succeed");
        assert_eq!(1, ctx.get_input_channels());
        assert_eq!(2, ctx.get_output_channels());
    }

    #[wasm_bindgen_test(async)]
    async fn test_compile_error_returns_err() {
        let mut ctx = Context::new(Config::default());
        let result = ctx
            .compile("fn dsp(input:float){(0,input)".to_string())
            .await;
        assert!(
            result.is_err(),
            "compile should return Err on invalid source"
        );
    }

    #[wasm_bindgen_test]
    fn test_compile_inner_with_virtual_external_module() {
        let config = Config {
            sample_rate: 48_000.0,
            input_channels: 0,
            output_channels: 0,
            buffer_size: 1,
        };
        let mut ctx = Context::new(config);
        ctx.clear_virtual_file_cache()
            .expect("clear cache should succeed");
        ctx.put_virtual_file_cache(
            "module_external_math.mmm".to_string(),
            "pub fn sum(a,b){a+b}".to_string(),
        )
        .expect("put cache should succeed");

        ctx.compile_inner(
            r#"
mod module_external_math
use module_external_math::sum
fn dsp(){
    sum(5.0,7.0)
}
"#
            .to_string(),
        )
        .expect("compile should succeed with cached external module");

        let mut output = vec![0.0f32; 1];
        let input = vec![];
        ctx.process(input.as_slice(), output.as_mut_slice());
        assert_eq!(output[0], 12.0);
    }
}
