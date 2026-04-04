mod async_compiler;

use std::{
    env, fs,
    path::{Path, PathBuf},
    process::Command,
    sync::{Mutex, mpsc},
};

use crate::async_compiler::{CompileRequest, Errors, Response};
use clap::{Parser, ValueEnum};
use mimium_audiodriver::{
    AudioDriverOptions,
    backends::{csv::csv_driver, local_buffer::LocalBufferDriver},
    driver::{Driver, RuntimeData, SampleRate},
    load_runtime_with_options,
};
use mimium_lang::{
    Config, ExecContext,
    compiler::{
        self,
        bytecodegen::SelfEvalMode,
        emit_ast,
        parser::{self as cst_parser, parser_errors_to_reportable},
    },
    log,
    plugin::Plugin,
    runtime::ProgramPayload,
    utils::{
        error::{ReportableError, report},
        fileloader,
        miniprint::MiniPrint,
    },
};
#[cfg(target_os = "macos")]
use notify::event::{AccessKind, EventKind, ModifyKind};
#[cfg(all(not(target_os = "windows"), not(target_os = "macos")))]
use notify::event::{AccessKind, EventKind, ModifyKind};
use notify::{Event, RecursiveMode, Watcher};
use serde::{Deserialize, Serialize};

#[cfg(not(target_arch = "wasm32"))]
use mimium_lang::mir::StateType;
#[cfg(not(target_arch = "wasm32"))]
use mimium_lang::plugin::ExtFunTypeInfo;
#[cfg(not(target_arch = "wasm32"))]
use state_tree::StateStoragePatchPlan;
#[cfg(not(target_arch = "wasm32"))]
use state_tree::patch::CopyFromPatch;
#[cfg(not(target_arch = "wasm32"))]
use state_tree::tree::StateTreeSkeleton;

#[derive(clap::Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[command(flatten)]
    pub mode: Mode,

    /// File name
    #[clap(value_parser)]
    pub file: Option<String>,

    /// Write out the signal values to a file (e.g. out.csv).
    #[arg(long, short)]
    pub output: Option<PathBuf>,

    /// How many times to execute the code. This is only effective when --output
    /// is specified.
    #[arg(long, default_value_t = 10)]
    pub times: usize,

    /// Output format
    #[arg(long, value_enum)]
    pub output_format: Option<OutputFileFormat>,

    /// Don't launch GUI
    #[arg(long, default_value_t = false)]
    pub no_gui: bool,

    /// Execution backend (default: wasm).
    #[arg(long, value_enum, default_value_t = Backend::Wasm)]
    pub backend: Backend,

    /// Path to config.toml (default: ~/.mimium/config.toml)
    #[arg(long)]
    pub config: Option<PathBuf>,

    /// Change the behavior of `self` in the code. It this is set to true, `| | {self+1}` will return 0 at t=0, which normally returns 1.
    #[arg(long, default_value_t = false)]
    pub self_init_0: bool,
}

impl Args {
    pub fn to_execctx_config(self) -> mimium_lang::Config {
        mimium_lang::Config {
            compiler: mimium_lang::compiler::Config {
                self_eval_mode: if self.self_init_0 {
                    SelfEvalMode::ZeroAtInit
                } else {
                    SelfEvalMode::SimpleState
                },
            },
        }
    }
}

#[derive(Clone, Debug, ValueEnum)]
pub enum OutputFileFormat {
    Csv,
}

#[derive(Clone, Copy, Debug, ValueEnum, Eq, PartialEq)]
pub enum Backend {
    Vm,
    Wasm,
}

#[derive(Clone, Debug, Deserialize, Serialize, Default)]
pub struct CliConfig {
    #[serde(default)]
    pub audio_setting: AudioSetting,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(default)]
#[serde(rename_all = "kebab-case")]
pub struct AudioSetting {
    pub input_device: String,
    pub output_device: String,
    pub buffer_size: u32,
    pub sample_rate: u32,
}

impl Default for AudioSetting {
    fn default() -> Self {
        Self {
            input_device: String::new(),
            output_device: String::new(),
            buffer_size: 512,
            sample_rate: 48000,
        }
    }
}

impl AudioSetting {
    fn to_driver_options(&self) -> AudioDriverOptions {
        AudioDriverOptions {
            input_device: (!self.input_device.trim().is_empty())
                .then_some(self.input_device.clone()),
            output_device: (!self.output_device.trim().is_empty())
                .then_some(self.output_device.clone()),
            buffer_size: (self.buffer_size > 0).then_some(self.buffer_size as usize),
        }
    }

    fn effective_sample_rate(&self) -> u32 {
        if self.sample_rate > 0 {
            self.sample_rate
        } else {
            48000
        }
    }
}

fn home_dir() -> Option<PathBuf> {
    env::var_os("HOME")
        .map(PathBuf::from)
        .or_else(|| env::var_os("USERPROFILE").map(PathBuf::from))
}

fn default_config_path() -> Result<PathBuf, Box<dyn std::error::Error>> {
    home_dir()
        .map(|home| home.join(".mimium").join("config.toml"))
        .ok_or_else(|| "Could not resolve home directory for default config path".into())
}

fn expand_tilde(path: &Path) -> Result<PathBuf, Box<dyn std::error::Error>> {
    let raw = path.to_string_lossy();
    if raw == "~" {
        return home_dir().ok_or_else(|| "Could not resolve home directory".into());
    }
    if let Some(suffix) = raw.strip_prefix("~/").or_else(|| raw.strip_prefix("~\\")) {
        return home_dir()
            .map(|home| home.join(suffix))
            .ok_or_else(|| "Could not resolve home directory".into());
    }
    Ok(path.to_path_buf())
}

fn resolve_config_path(path: Option<&PathBuf>) -> Result<PathBuf, Box<dyn std::error::Error>> {
    path.map_or_else(default_config_path, |p| expand_tilde(p.as_path()))
}

fn load_or_create_cli_config(path: &Path) -> Result<CliConfig, Box<dyn std::error::Error>> {
    if !path.exists() {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let default_cfg = CliConfig::default();
        let serialized = toml::to_string_pretty(&default_cfg)?;
        fs::write(path, serialized)?;
        log::info!("Created default config at {}", path.display());
        return Ok(default_cfg);
    }

    let content = fs::read_to_string(path)?;
    let parsed: CliConfig = toml::from_str(&content)?;
    Ok(parsed)
}

#[derive(clap::Args, Debug, Clone, Copy)]
#[group(required = false, multiple = false)]
pub struct Mode {
    /// Print CST (Concrete Syntax Tree / GreenTree) and exit
    #[arg(long, default_value_t = false)]
    pub emit_cst: bool,

    /// Print AST and exit
    #[arg(long, default_value_t = false)]
    pub emit_ast: bool,

    /// Print MIR and exit
    #[arg(long, default_value_t = false)]
    pub emit_mir: bool,

    /// Print bytecode and exit
    #[arg(long, default_value_t = false)]
    pub emit_bytecode: bool,

    /// Generate WASM module and exit
    #[arg(long, default_value_t = false)]
    pub emit_wasm: bool,
}

pub enum RunMode {
    EmitCst,
    EmitAst,
    EmitMir,
    EmitByteCode,
    #[cfg(not(target_arch = "wasm32"))]
    EmitWasm {
        output: Option<PathBuf>,
    },
    NativeAudio,
    #[cfg(not(target_arch = "wasm32"))]
    WasmAudio,
    WriteCsv {
        times: usize,
        output: Option<PathBuf>,
    },
}

/// Execution options derived from CLI arguments.
pub struct RunOptions {
    mode: RunMode,
    with_gui: bool,
    /// Use the WASM backend instead of the native VM.
    use_wasm: bool,
    audio_setting: AudioSetting,
    config: Config,
}

impl RunOptions {
    /// Convert parsed command line arguments into [`RunOptions`].
    pub fn from_args(args: &Args, audio_setting: &AudioSetting) -> Self {
        let config = args.clone().to_execctx_config();
        #[cfg(not(target_arch = "wasm32"))]
        let use_wasm_backend = matches!(args.backend, Backend::Wasm);
        #[cfg(target_arch = "wasm32")]
        let use_wasm_backend = false;

        if args.mode.emit_cst {
            return Self {
                mode: RunMode::EmitCst,
                with_gui: false,
                use_wasm: false,
                audio_setting: audio_setting.clone(),
                config,
            };
        }

        if args.mode.emit_ast {
            return Self {
                mode: RunMode::EmitAst,
                with_gui: true,
                use_wasm: false,
                audio_setting: audio_setting.clone(),
                config,
            };
        }

        if args.mode.emit_mir {
            return Self {
                mode: RunMode::EmitMir,
                with_gui: true,
                use_wasm: false,
                audio_setting: audio_setting.clone(),
                config,
            };
        }

        if args.mode.emit_bytecode {
            return Self {
                mode: RunMode::EmitByteCode,
                with_gui: true,
                use_wasm: false,
                audio_setting: audio_setting.clone(),
                config,
            };
        }

        #[cfg(not(target_arch = "wasm32"))]
        if args.mode.emit_wasm {
            return Self {
                mode: RunMode::EmitWasm {
                    output: args.output.clone(),
                },
                with_gui: false,
                use_wasm: false,
                audio_setting: audio_setting.clone(),
                config,
            };
        }

        #[cfg(not(target_arch = "wasm32"))]
        if use_wasm_backend {
            // For WASM backend, respect output format
            let mode = match (&args.output_format, args.output.as_ref()) {
                (Some(OutputFileFormat::Csv), path) => RunMode::WriteCsv {
                    times: args.times,
                    output: path.cloned(),
                },
                (None, Some(output))
                    if output.extension().and_then(|x| x.to_str()) == Some("csv") =>
                {
                    RunMode::WriteCsv {
                        times: args.times,
                        output: Some(output.clone()),
                    }
                }
                _ => RunMode::WasmAudio,
            };

            let with_gui = match &mode {
                RunMode::WasmAudio => !args.no_gui,
                _ => false,
            };

            return Self {
                mode,
                with_gui,
                use_wasm: true,
                audio_setting: audio_setting.clone(),
                config,
            };
        }

        let mode = match (&args.output_format, args.output.as_ref()) {
            // if none of the output options is specified, make sounds.
            (None, None) => RunMode::NativeAudio,
            // When --output-format is explicitly specified, use it.
            (Some(OutputFileFormat::Csv), path) => RunMode::WriteCsv {
                times: args.times,
                output: path.cloned(),
            },
            // Otherwise, guess from the file extension.
            (None, Some(output)) => match output.extension() {
                Some(x) if &x.to_os_string() == "csv" => RunMode::WriteCsv {
                    times: args.times,
                    output: Some(output.clone()),
                },
                _ => panic!("cannot determine the output file format"),
            },
        };

        let with_gui = match &mode {
            // launch except when --no-gui is specified
            RunMode::NativeAudio => !args.no_gui,
            // do not launch in other mode
            _ => false,
        };

        Self {
            mode,
            with_gui,
            use_wasm: false,
            audio_setting: audio_setting.clone(),
            config,
        }
    }

    fn get_driver(&self) -> Box<dyn Driver<Sample = f64>> {
        match &self.mode {
            RunMode::NativeAudio => {
                load_runtime_with_options(&self.audio_setting.to_driver_options())
            }
            #[cfg(not(target_arch = "wasm32"))]
            RunMode::WasmAudio => {
                load_runtime_with_options(&self.audio_setting.to_driver_options())
            }
            RunMode::WriteCsv { times, output } => csv_driver(*times, output),
            _ => unreachable!(),
        }
    }
}

/// Construct an [`ExecContext`] with the default set of plugins.
pub fn get_default_context(
    path: Option<PathBuf>,
    with_gui: bool,
    use_wasm_backend: bool,
    config: Config,
) -> ExecContext {
    let plugins: Vec<Box<dyn Plugin>> = vec![];
    let mut ctx = ExecContext::new(plugins.into_iter(), path, config);

    // Load dynamic plugins
    #[cfg(not(target_arch = "wasm32"))]
    {
        ctx.init_plugin_loader();

        let mut loaded_count = 0;

        // Try to load all mimium_*.dylib/so/dll from the executable directory first
        if let Ok(exe_path) = std::env::current_exe()
            && let Some(exe_dir) = exe_path.parent()
            && let Some(loader) = ctx.get_plugin_loader_mut()
        {
            // Load all plugins except guitools when GUI is requested as SystemPlugin
            // (guitools will be loaded as SystemPlugin to avoid duplicates)
            loaded_count = if use_wasm_backend {
                loader
                    .load_plugins_from_dir_with_skip_substrings(exe_dir, &["symphonia"])
                    .unwrap_or(0)
            } else {
                loader.load_plugins_from_dir(exe_dir).unwrap_or(0)
            };

            if loaded_count > 0 {
                log::debug!("Loaded {loaded_count} plugin(s) from executable directory");

                // When GUI is requested, unload guitools if it was loaded dynamically
                // since we'll add it as a SystemPlugin instead
                if with_gui {
                    // Note: Currently we don't have unload functionality,
                    // but the SystemPlugin version will take precedence
                    log::debug!("GUI mode: guitools will be provided as SystemPlugin");
                }
            }
        }

        // If no plugins loaded from exe directory, try standard plugin directory
        if loaded_count == 0
            && let Err(e) = if let Some(loader) = ctx.get_plugin_loader_mut() {
                if use_wasm_backend {
                    loader.load_builtin_plugins_with_skip_substrings(&["symphonia"])
                } else {
                    loader.load_builtin_plugins()
                }
            } else {
                Ok(())
            }
        {
            log::debug!("No builtin dynamic plugins found: {e:?}");
        }
    }

    ctx.add_system_plugin(mimium_scheduler::get_default_scheduler_plugin());

    if use_wasm_backend {
        ctx.add_system_plugin(mimium_symphonia::SamplerPlugin::default());
    }

    // Always add guitools as SystemPlugin so Slider/Probe macros are available
    // on every backend. Use headless mode when GUI is disabled.
    if with_gui {
        ctx.add_system_plugin(mimium_guitools::GuiToolPlugin::default());
    } else {
        ctx.add_system_plugin(mimium_guitools::GuiToolPlugin::headless());
    }

    ctx
}

struct FileRunner {
    pub tx_compiler: mpsc::Sender<CompileRequest>,
    pub rx_compiler: mpsc::Receiver<Result<Response, Errors>>,
    pub tx_prog: Option<mpsc::Sender<ProgramPayload>>,
    pub fullpath: PathBuf,
    /// When true, recompilation targets the WASM backend instead of the native VM.
    pub use_wasm: bool,
    /// Last successfully prepared WASM program metadata.
    ///
    /// This is used on the non-RT thread to build deterministic
    /// state migration plans for the next hot-swap payload.
    #[cfg(not(target_arch = "wasm32"))]
    old_program: Mutex<Option<OldWasmProgram>>,
    /// Channel receiving old engines retired by the audio thread.
    ///
    /// Drained on the file-watcher (non-RT) thread so engine destruction does
    /// not block the real-time callback.
    #[cfg(not(target_arch = "wasm32"))]
    retired_engine_receiver: Option<mpsc::Receiver<mimium_lang::runtime::wasm::engine::WasmEngine>>,
}

#[cfg(not(target_arch = "wasm32"))]
#[derive(Clone)]
struct OldWasmProgram {
    /// DSP state structure of the previously active program.
    dsp_state_skeleton: Option<StateTreeSkeleton<StateType>>,
    /// External function signatures required to instantiate/prewarm the next module.
    ext_fns: Vec<ExtFunTypeInfo>,
    /// Frozen WASM plugin host handlers (e.g. ProbeValue intercepts).
    plugin_fns: Option<mimium_lang::runtime::wasm::WasmPluginFnMap>,
}

#[cfg(not(target_arch = "wasm32"))]
struct PreparedWasmSwapData {
    /// Global state snapshot taken after running `main` on non-RT thread.
    prewarmed_global_state: Vec<u64>,
    /// Fully loaded WASM engine prepared on non-RT thread.
    prepared_engine: Box<mimium_lang::runtime::wasm::engine::WasmEngine>,
}

struct FileWatcher {
    pub rx: mpsc::Receiver<notify::Result<Event>>,
    pub watcher: notify::RecommendedWatcher,
}

#[cfg(target_os = "macos")]
fn should_recompile_on_event(event: &Event) -> bool {
    matches!(
        event.kind,
        EventKind::Access(AccessKind::Close(notify::event::AccessMode::Write))
            | EventKind::Modify(ModifyKind::Data(_))
            | EventKind::Modify(ModifyKind::Any)
    )
}

#[cfg(all(not(target_os = "windows"), not(target_os = "macos")))]
fn should_recompile_on_event(event: &Event) -> bool {
    matches!(
        event.kind,
        EventKind::Access(AccessKind::Close(notify::event::AccessMode::Write))
            | EventKind::Modify(ModifyKind::Data(_))
            | EventKind::Modify(ModifyKind::Any)
    )
}

#[cfg(target_os = "windows")]
fn should_recompile_on_event(_event: &Event) -> bool {
    true
}

impl FileRunner {
    pub fn new(
        compiler: compiler::Context,
        path: PathBuf,
        prog_tx: Option<mpsc::Sender<ProgramPayload>>,
        use_wasm: bool,
        #[cfg(not(target_arch = "wasm32"))] old_program: Option<OldWasmProgram>,
        #[cfg(not(target_arch = "wasm32"))] retired_engine_receiver: Option<
            mpsc::Receiver<mimium_lang::runtime::wasm::engine::WasmEngine>,
        >,
    ) -> Self {
        let client = async_compiler::start_async_compiler_service(compiler);
        Self {
            tx_compiler: client.tx,
            rx_compiler: client.rx,
            tx_prog: prog_tx,
            fullpath: path,
            use_wasm,
            #[cfg(not(target_arch = "wasm32"))]
            old_program: Mutex::new(old_program),
            #[cfg(not(target_arch = "wasm32"))]
            retired_engine_receiver,
        }
    }
    fn try_new_watcher(&self) -> Result<FileWatcher, notify::Error> {
        let (tx, rx) = mpsc::channel::<notify::Result<Event>>();
        let mut watcher = notify::recommended_watcher(tx)?;
        watcher.watch(Path::new(&self.fullpath), RecursiveMode::NonRecursive)?;
        Ok(FileWatcher { rx, watcher })
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn try_compile_wasm_in_subprocess(&self) -> Result<Vec<u8>, String> {
        let exe = env::current_exe().map_err(|e| format!("failed to resolve current exe: {e}"))?;
        let output = Command::new(exe)
            .arg(self.fullpath.as_os_str())
            .arg("--backend=wasm")
            .arg("--emit-wasm")
            .output()
            .map_err(|e| format!("failed to spawn compiler subprocess: {e}"))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            return Err(format!(
                "subprocess compile failed (status: {:?}): {}",
                output.status.code(),
                stderr
            ));
        }

        if output.stdout.is_empty() {
            return Err("subprocess compile succeeded but produced empty wasm stdout".to_string());
        }

        Ok(output.stdout)
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn try_prewarm_wasm_global_state(
        wasm_bytes: &[u8],
        ext_fns: &[ExtFunTypeInfo],
        plugin_fns: Option<mimium_lang::runtime::wasm::WasmPluginFnMap>,
    ) -> Result<PreparedWasmSwapData, String> {
        use mimium_lang::runtime::wasm::engine::{WasmDspRuntime, WasmEngine};

        let mut engine = WasmEngine::new(ext_fns, plugin_fns)
            .map_err(|e| format!("failed to create prewarm wasm engine: {e}"))?;

        engine
            .load_module(wasm_bytes)
            .map_err(|e| format!("failed to load module for prewarm: {e}"))?;

        let mut runtime = WasmDspRuntime::new(engine, None, None);

        runtime
            .run_main()
            .map_err(|e| format!("failed to run main for prewarm: {e}"))?;

        let global_state = runtime
            .engine_mut()
            .get_global_state_data()
            .map(|data| data.to_vec())
            .ok_or_else(|| "missing global state after prewarm".to_string())?;

        let prepared_engine = runtime.into_engine();

        Ok(PreparedWasmSwapData {
            prewarmed_global_state: global_state,
            prepared_engine: Box::new(prepared_engine),
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn prepare_hot_swap_wasm_payload(
        &self,
        bytes: Vec<u8>,
        dsp_state_skeleton: Option<StateTreeSkeleton<StateType>>,
        ext_fns: Option<&[ExtFunTypeInfo]>,
    ) -> Result<ProgramPayload, String> {
        let old_program = self
            .old_program
            .lock()
            .ok()
            .and_then(|guard| (*guard).clone());
        let previous_skeleton = old_program
            .as_ref()
            .and_then(|program| program.dsp_state_skeleton.clone());
        let fallback_ext_fns: &[ExtFunTypeInfo] = old_program
            .as_ref()
            .map(|program| program.ext_fns.as_slice())
            .unwrap_or(&[]);
        let ext_fns = ext_fns.unwrap_or(fallback_ext_fns);
        let plugin_fns = old_program
            .as_ref()
            .and_then(|program| program.plugin_fns.clone());

        let prepared_swap_data =
            Self::try_prewarm_wasm_global_state(&bytes, ext_fns, plugin_fns.clone())?;

        let state_patch_plan = Self::build_required_state_patch_plan(
            previous_skeleton,
            dsp_state_skeleton.as_ref(),
            prepared_swap_data.prewarmed_global_state.len(),
        );
        let payload = ProgramPayload::WasmModule {
            bytes,
            prepared_engine: prepared_swap_data.prepared_engine,
            dsp_state_skeleton: dsp_state_skeleton.clone(),
            state_patch_plan,
            prewarmed_global_state: prepared_swap_data.prewarmed_global_state,
        };
        self.update_old_program(dsp_state_skeleton, ext_fns.to_vec(), plugin_fns);

        Ok(payload)
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn build_required_state_patch_plan(
        previous_skeleton: Option<StateTreeSkeleton<StateType>>,
        new_skeleton: Option<&StateTreeSkeleton<StateType>>,
        prewarmed_state_size: usize,
    ) -> StateStoragePatchPlan {
        if let (Some(old_skeleton), Some(new_skeleton)) = (previous_skeleton, new_skeleton.cloned())
        {
            let maybe_plan =
                state_tree::build_state_storage_patch_plan(old_skeleton, new_skeleton.clone());
            if let Some(plan) = maybe_plan {
                return plan;
            }
            let total_size = new_skeleton.total_size() as usize;
            return StateStoragePatchPlan {
                total_size,
                patches: vec![CopyFromPatch {
                    src_addr: 0,
                    dst_addr: 0,
                    size: total_size,
                }],
            };
        }

        StateStoragePatchPlan {
            total_size: prewarmed_state_size,
            patches: vec![],
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn update_old_program(
        &self,
        dsp_state_skeleton: Option<StateTreeSkeleton<StateType>>,
        ext_fns: Vec<ExtFunTypeInfo>,
        plugin_fns: Option<mimium_lang::runtime::wasm::WasmPluginFnMap>,
    ) {
        if let Ok(mut guard) = self.old_program.lock() {
            *guard = Some(OldWasmProgram {
                dsp_state_skeleton,
                ext_fns,
                plugin_fns,
            });
        }
    }

    fn recompile_file_inprocess(&self, new_content: String) {
        #[cfg(not(target_arch = "wasm32"))]
        let mode = RunMode::EmitByteCode;

        #[cfg(target_arch = "wasm32")]
        let mode = {
            let _ = self.use_wasm;
            RunMode::EmitByteCode
        };
        let _ = self.tx_compiler.send(CompileRequest {
            source: new_content.clone(),
            path: self.fullpath.clone(),
            option: RunOptions {
                mode,
                with_gui: true,
                use_wasm: self.use_wasm,
                audio_setting: AudioSetting::default(),
                config: Config::default(),
            },
        });
        let _ = self.rx_compiler.recv().map(|res| match res {
            Ok(Response::Ast(_)) | Ok(Response::Mir(_)) => {
                log::warn!("unexpected response: AST/MIR");
            }
            Ok(Response::ByteCode(prog)) => {
                log::info!("compiled successfully.");
                if let Some(tx) = &self.tx_prog {
                    let _ = tx.send(ProgramPayload::VmProgram(prog));
                }
            }
            #[cfg(not(target_arch = "wasm32"))]
            Ok(Response::WasmModule(output)) => {
                log::info!("WASM compiled successfully ({} bytes).", output.bytes.len());
                if let Some(tx) = &self.tx_prog {
                    match self.prepare_hot_swap_wasm_payload(
                        output.bytes,
                        output.dsp_state_skeleton,
                        Some(&output.ext_fns),
                    ) {
                        Ok(payload) => {
                            let _ = tx.send(payload);
                        }
                        Err(e) => {
                            log::error!("WASM prepare_hot_swap failed; skip hot-swap by spec: {e}");
                        }
                    }
                }
            }
            Err(errs) => {
                let errs = errs
                    .into_iter()
                    .map(|e| Box::new(e) as Box<dyn ReportableError>)
                    .collect::<Vec<_>>();
                report(&new_content, self.fullpath.clone(), &errs);
            }
        });
    }

    fn recompile_file(&self) {
        match fileloader::load(&self.fullpath.to_string_lossy()) {
            Ok(new_content) => {
                #[cfg(not(target_arch = "wasm32"))]
                {
                    if self.use_wasm {
                        match self.try_compile_wasm_in_subprocess() {
                            Ok(bytes) => {
                                log::info!(
                                    "WASM compiled in subprocess successfully ({} bytes).",
                                    bytes.len()
                                );
                                if let Some(tx) = &self.tx_prog {
                                    match self.prepare_hot_swap_wasm_payload(bytes, None, None) {
                                        Ok(payload) => {
                                            let _ = tx.send(payload);
                                        }
                                        Err(e) => {
                                            log::error!(
                                                "WASM prepare_hot_swap failed; skip hot-swap by spec: {e}"
                                            );
                                        }
                                    }
                                }
                            }
                            Err(e) => {
                                log::error!("{e}");
                            }
                        }
                    } else {
                        self.recompile_file_inprocess(new_content);
                    }
                }

                #[cfg(target_arch = "wasm32")]
                {
                    self.recompile_file_inprocess(new_content);
                }
            }
            Err(e) => {
                log::error!(
                    "failed to reload the file {}: {}",
                    self.fullpath.display(),
                    e
                );
            }
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn drain_retired_engines(&self) {
        if let Some(rx) = &self.retired_engine_receiver {
            let mut dropped_count = 0usize;
            while let Ok(_engine) = rx.try_recv() {
                dropped_count += 1;
            }
            if dropped_count > 0 {
                log::info!(
                    "WASM deferred drop: released {} retired engine(s) on non-RT thread",
                    dropped_count
                );
            }
        }
    }

    //this api never returns
    pub fn cli_loop(&self) {
        //watcher instance lives only this context
        let file_watcher = match self.try_new_watcher() {
            Ok(watcher) => watcher,
            Err(e) => {
                log::error!("Failed to watch file: {e}");
                return;
            }
        };

        loop {
            #[cfg(not(target_arch = "wasm32"))]
            self.drain_retired_engines();

            match file_watcher
                .rx
                .recv_timeout(std::time::Duration::from_millis(100))
            {
                Ok(Ok(event)) => {
                    if should_recompile_on_event(&event) {
                        log::info!("File event detected ({:?}), recompiling...", event.kind);
                        self.recompile_file();
                    } else {
                        log::debug!("Ignored file event: {:?}", event.kind);
                    }
                }
                Ok(Err(e)) => {
                    log::error!("watch error event: {e}");
                }
                Err(mpsc::RecvTimeoutError::Timeout) => {
                    continue;
                }
                Err(e) => {
                    log::error!("receiver error: {e}");
                }
            }
        }
    }
}

/// Compile and run a single source file according to the provided options.
pub fn run_file(
    options: RunOptions,
    content: &str,
    fullpath: &Path,
) -> Result<(), Vec<Box<dyn ReportableError>>> {
    log::debug!("Filename: {}", fullpath.display());

    let mut ctx = get_default_context(
        Some(PathBuf::from(fullpath)),
        options.with_gui,
        options.use_wasm || matches!(options.mode, RunMode::EmitWasm { .. }),
        options.config,
    );

    match options.mode {
        RunMode::EmitCst => {
            let tokens = cst_parser::tokenize(content);
            let preparsed = cst_parser::preparse(&tokens);
            let (green_id, arena, tokens, errors) = cst_parser::parse_cst(tokens, &preparsed);

            // Report errors to stderr if any
            if !errors.is_empty() {
                let reportable_errors =
                    parser_errors_to_reportable(content, fullpath.to_path_buf(), errors);
                report(content, fullpath.to_path_buf(), &reportable_errors);
            }

            // Print CST tree to stdout
            let tree_output = arena.print_tree(green_id, &tokens, content, 0);
            println!("{tree_output}");
            Ok(())
        }
        RunMode::EmitAst => {
            let ast = emit_ast(content, Some(PathBuf::from(fullpath)))?;
            println!("{}", ast.pretty_print());
            Ok(())
        }
        RunMode::EmitMir => {
            ctx.prepare_compiler();
            let res = ctx.get_compiler().unwrap().emit_mir(content);
            res.map(|r| {
                println!("{r}");
            })?;
            Ok(())
        }
        RunMode::EmitByteCode => {
            // need to prepare dummy audio plugin to link `now` and `samplerate`
            let localdriver = LocalBufferDriver::new(0);
            let plug = localdriver.get_as_plugin();
            ctx.add_plugin(plug);
            ctx.prepare_machine(content)?;
            println!("{}", ctx.get_vm().unwrap().prog);
            Ok(())
        }
        #[cfg(not(target_arch = "wasm32"))]
        RunMode::EmitWasm { output } => {
            use mimium_lang::utils::metadata::Location;
            use std::io::Write;
            use std::sync::Arc;

            ctx.prepare_compiler();
            let ext_fns = ctx.get_extfun_types();
            let mir = ctx.get_compiler().unwrap().emit_mir(content)?;

            // Generate WASM module
            let mut generator = compiler::wasmgen::WasmGenerator::new(Arc::new(mir), &ext_fns);
            let wasm_bytes = generator.generate().map_err(|e| {
                vec![Box::new(mimium_lang::utils::error::SimpleError {
                    message: e,
                    span: Location::default(),
                }) as Box<dyn ReportableError>]
            })?;

            if let Some(path) = output {
                std::fs::write(&path, &wasm_bytes).map_err(|e| {
                    vec![Box::new(mimium_lang::utils::error::SimpleError {
                        message: e.to_string(),
                        span: Location::default(),
                    }) as Box<dyn ReportableError>]
                })?;
                println!("Written to: {}", path.display());
            } else {
                let mut stdout = std::io::stdout().lock();
                stdout.write_all(&wasm_bytes).map_err(|e| {
                    vec![Box::new(mimium_lang::utils::error::SimpleError {
                        message: e.to_string(),
                        span: Location::default(),
                    }) as Box<dyn ReportableError>]
                })?;
                stdout.flush().map_err(|e| {
                    vec![Box::new(mimium_lang::utils::error::SimpleError {
                        message: e.to_string(),
                        span: Location::default(),
                    }) as Box<dyn ReportableError>]
                })?;
            }

            Ok(())
        }
        #[cfg(not(target_arch = "wasm32"))]
        RunMode::WasmAudio => {
            use mimium_lang::compiler::wasmgen::WasmGenerator;
            use mimium_lang::runtime::wasm::engine::{WasmDspRuntime, WasmEngine};
            use mimium_lang::utils::metadata::Location;
            use std::sync::Arc;

            ctx.prepare_compiler();
            let mut ext_fns = ctx.get_extfun_types();
            // Deduplicate ext_fns by name to avoid "defined twice" errors in WASM runtime
            // (can happen when same plugin is loaded both dynamically and as SystemPlugin)
            ext_fns.sort_by(|a, b| a.name.as_str().cmp(b.name.as_str()));
            ext_fns.dedup_by(|a, b| a.name == b.name);

            let mir = ctx.get_compiler().unwrap().emit_mir(content)?;

            let io_channels = mir.get_dsp_iochannels();
            let dsp_skeleton = mir.get_dsp_state_skeleton().cloned();

            // Generate WASM module
            let mut generator = WasmGenerator::new(Arc::new(mir), &ext_fns);
            let wasm_bytes = generator.generate().map_err(|e| {
                vec![Box::new(mimium_lang::utils::error::SimpleError {
                    message: e,
                    span: Location::default(),
                }) as Box<dyn ReportableError>]
            })?;

            log::info!("Generated WASM module ({} bytes)", wasm_bytes.len());

            // Collect WASM plugin functions from all system plugins.
            // freeze_wasm_plugin_fns() must be called before generate_wasm_audioworkers()
            // so that both share the same underlying scheduler state.
            let plugin_fns = ctx.freeze_wasm_plugin_fns();
            let plugin_fns_for_hotswap = plugin_fns.clone();

            // Collect per-sample audio workers from all plugins.
            let wasm_workers = ctx.generate_wasm_audioworkers();

            let mut wasm_engine = WasmEngine::new(&ext_fns, plugin_fns).map_err(|e| {
                vec![Box::new(mimium_lang::utils::error::SimpleError {
                    message: format!("Failed to create WASM engine: {e}"),
                    span: Location::default(),
                }) as Box<dyn ReportableError>]
            })?;

            wasm_engine.load_module(&wasm_bytes).map_err(|e| {
                vec![Box::new(mimium_lang::utils::error::SimpleError {
                    message: format!("Failed to load WASM module: {e}"),
                    span: Location::default(),
                }) as Box<dyn ReportableError>]
            })?;

            // Create WasmDspRuntime and wrap in RuntimeData
            let mut wasm_runtime =
                WasmDspRuntime::new(wasm_engine, io_channels, dsp_skeleton.clone());
            wasm_runtime.set_wasm_audioworkers(wasm_workers);
            let (retire_tx, retire_rx) = mpsc::channel();
            wasm_runtime.set_engine_retire_sender(retire_tx);
            ctx.run_wasm_on_init(wasm_runtime.engine_mut());
            let _ = wasm_runtime.run_main();
            ctx.run_wasm_after_main(wasm_runtime.engine_mut());

            let runtimedata = RuntimeData::new_from_runtime(Box::new(wasm_runtime));

            // Use the standard audio driver infrastructure
            let mut driver = options.get_driver();

            let with_gui = options.with_gui;
            let mainloop = ctx.try_get_main_loop().unwrap_or(Box::new(move || {
                if with_gui {
                    loop {
                        std::thread::sleep(std::time::Duration::from_millis(1000));
                    }
                }
            }));

            driver.init(
                runtimedata,
                Some(SampleRate::from(
                    options.audio_setting.effective_sample_rate(),
                )),
            );
            driver.play();

            // Set up file watcher for WASM hot-swap recompilation
            let compiler = ctx.take_compiler().unwrap();
            let frunner = FileRunner::new(
                compiler,
                fullpath.to_path_buf(),
                driver.get_program_channel(),
                true,
                Some(OldWasmProgram {
                    dsp_state_skeleton: dsp_skeleton,
                    ext_fns,
                    plugin_fns: plugin_fns_for_hotswap,
                }),
                Some(retire_rx),
            );
            if with_gui {
                std::thread::spawn(move || frunner.cli_loop());
            }

            mainloop();
            Ok(())
        }
        #[cfg(not(target_arch = "wasm32"))]
        _ if options.use_wasm => {
            // WASM backend with standard audio driver (WriteCsv or NativeAudio).
            use mimium_lang::compiler::wasmgen::WasmGenerator;
            use mimium_lang::runtime::wasm::engine::{WasmDspRuntime, WasmEngine};
            use mimium_lang::utils::metadata::Location;
            use std::sync::Arc;

            let mut driver = options.get_driver();

            ctx.prepare_compiler();
            let mut ext_fns = ctx.get_extfun_types();
            // Deduplicate ext_fns by name to avoid "defined twice" errors in WASM runtime
            // (can happen when same plugin is loaded both dynamically and as SystemPlugin)
            ext_fns.sort_by(|a, b| a.name.as_str().cmp(b.name.as_str()));
            ext_fns.dedup_by(|a, b| a.name == b.name);

            let mir = ctx.get_compiler().unwrap().emit_mir(content)?;
            let io_channels = mir.get_dsp_iochannels();
            let dsp_skeleton = mir.get_dsp_state_skeleton().cloned();

            let mut generator = WasmGenerator::new(Arc::new(mir), &ext_fns);
            let wasm_bytes = generator.generate().map_err(|e| {
                vec![Box::new(mimium_lang::utils::error::SimpleError {
                    message: e,
                    span: Location::default(),
                }) as Box<dyn ReportableError>]
            })?;

            log::info!("Generated WASM module ({} bytes)", wasm_bytes.len());

            // Collect WASM plugin functions from all system plugins.
            // freeze_wasm_plugin_fns() must be called before generate_wasm_audioworkers()
            // so that both share the same underlying scheduler state.
            let plugin_fns = ctx.freeze_wasm_plugin_fns();
            let plugin_fns_for_hotswap = plugin_fns.clone();

            // Collect per-sample audio workers from all plugins.
            let wasm_workers = ctx.generate_wasm_audioworkers();

            let mut wasm_engine = WasmEngine::new(&ext_fns, plugin_fns).map_err(|e| {
                vec![Box::new(mimium_lang::utils::error::SimpleError {
                    message: format!("Failed to create WASM engine: {e}"),
                    span: Location::default(),
                }) as Box<dyn ReportableError>]
            })?;

            wasm_engine.load_module(&wasm_bytes).map_err(|e| {
                vec![Box::new(mimium_lang::utils::error::SimpleError {
                    message: format!("Failed to load WASM module: {e}"),
                    span: Location::default(),
                }) as Box<dyn ReportableError>]
            })?;

            let mut wasm_runtime =
                WasmDspRuntime::new(wasm_engine, io_channels, dsp_skeleton.clone());
            wasm_runtime.set_wasm_audioworkers(wasm_workers);
            let (retire_tx, retire_rx) = mpsc::channel();
            wasm_runtime.set_engine_retire_sender(retire_tx);
            ctx.run_wasm_on_init(wasm_runtime.engine_mut());
            let _ = wasm_runtime.run_main();
            ctx.run_wasm_after_main(wasm_runtime.engine_mut());

            let runtimedata = RuntimeData::new_from_runtime(Box::new(wasm_runtime));

            // Get main loop from system plugins (e.g., GUI)
            let with_gui = options.with_gui;
            let mainloop = ctx.try_get_main_loop().unwrap_or(Box::new(move || {
                if with_gui {
                    loop {
                        std::thread::sleep(std::time::Duration::from_millis(1000));
                    }
                }
            }));

            driver.init(
                runtimedata,
                Some(SampleRate::from(
                    options.audio_setting.effective_sample_rate(),
                )),
            );
            driver.play();

            // Set up file watcher for WASM hot-swap recompilation
            let compiler = ctx.take_compiler().unwrap();
            let frunner = FileRunner::new(
                compiler,
                fullpath.to_path_buf(),
                driver.get_program_channel(),
                true,
                Some(OldWasmProgram {
                    dsp_state_skeleton: dsp_skeleton,
                    ext_fns,
                    plugin_fns: plugin_fns_for_hotswap,
                }),
                Some(retire_rx),
            );
            if with_gui {
                std::thread::spawn(move || frunner.cli_loop());
            }

            mainloop();
            Ok(())
        }
        _ => {
            let mut driver = options.get_driver();
            let audiodriver_plug = driver.get_as_plugin();

            ctx.add_plugin(audiodriver_plug);
            ctx.prepare_machine(content)?;
            let _res = ctx.run_main();

            let runtimedata = {
                let ctxmut: &mut ExecContext = &mut ctx;
                RuntimeData::try_from(ctxmut).unwrap()
            };

            let mainloop = ctx.try_get_main_loop().unwrap_or(Box::new(move || {
                if options.with_gui {
                    loop {
                        std::thread::sleep(std::time::Duration::from_millis(1000));
                    }
                }
            }));
            //this takes ownership of ctx
            driver.init(
                runtimedata,
                Some(SampleRate::from(
                    options.audio_setting.effective_sample_rate(),
                )),
            );
            driver.play();

            let compiler = ctx.take_compiler().unwrap();

            let frunner = FileRunner::new(
                compiler,
                fullpath.to_path_buf(),
                driver.get_program_channel(),
                false,
                None,
                None,
            );
            if options.with_gui {
                std::thread::spawn(move || frunner.cli_loop());
            }
            mainloop();
            Ok(())
        }
    }
}
pub fn lib_main() -> Result<(), Box<dyn std::error::Error>> {
    if cfg!(debug_assertions) | cfg!(test) {
        colog::default_builder()
            .filter_level(log::LevelFilter::Trace)
            .init();
    } else {
        colog::default_builder().init();
    }

    let args = Args::parse();
    let config_path = resolve_config_path(args.config.as_ref())?;
    let cli_config = load_or_create_cli_config(&config_path)?;

    match &args.file {
        Some(file) => {
            let fullpath = fileloader::get_canonical_path(".", file)?;
            let content = fileloader::load(fullpath.to_str().unwrap())?;
            let options = RunOptions::from_args(&args, &cli_config.audio_setting);
            match run_file(options, &content, &fullpath) {
                Ok(()) => {}
                Err(e) => {
                    // Note: I was hoping to implement std::error::Error for a
                    // struct around ReportableError and directly return it,
                    // however, std::error::Error cannot be so color-rich as
                    // ariadne because it just uses std::fmt::Display.
                    report(&content, fullpath, &e);
                    return Err(format!("Failed to process {file}").into());
                }
            }
        }
        None => {
            // repl::run_repl();
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::get_default_context;
    use mimium_lang::Config;
    use std::path::PathBuf;

    #[test]
    fn default_cli_context_compiles_lift_array_code_source() {
        let src = r#"
// @test {"times":1,"stereo":false,"expected":[31.0],"web":true}

#stage(macro)
fn mk_functions(){
   let funcs = [
      `|x| x + 1.0,
      `|x| x * 2.0,
   ]
   funcs |> lift_array_code
}

#stage(main)
fn dsp(){
  let funcs = mk_functions!()
  funcs[0](10.0) + funcs[1](10.0)
}
"#;
        let mut ctx = get_default_context(
            Some(PathBuf::from("tmp/lift_array_code_test.mmm")),
            false,
            false,
            Config::default(),
        );
        ctx.prepare_compiler();
        let result = ctx.get_compiler().unwrap().emit_mir(src);
        assert!(result.is_ok(), "emit_mir failed: {result:?}");
    }
}
