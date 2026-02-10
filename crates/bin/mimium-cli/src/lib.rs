mod async_compiler;

use std::{
    path::{Path, PathBuf},
    sync::mpsc,
};

use crate::async_compiler::{CompileRequest, Errors, Response};
use clap::{Parser, ValueEnum};
use mimium_audiodriver::{
    backends::{csv::csv_driver, local_buffer::LocalBufferDriver},
    driver::{Driver, RuntimeData, SampleRate},
    load_default_runtime,
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
use notify::{Event, RecursiveMode, Watcher};

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

    /// Run audio processing using WASM JIT compiler
    #[arg(long, default_value_t = false)]
    pub wasm: bool,
}

pub enum RunMode {
    EmitCst,
    EmitAst,
    EmitMir,
    EmitByteCode,
    EmitWasm,
    NativeAudio,
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
    config: Config,
}

impl RunOptions {
    /// Convert parsed command line arguments into [`RunOptions`].
    pub fn from_args(args: &Args) -> Self {
        let config = args.clone().to_execctx_config();
        if args.mode.emit_cst {
            return Self {
                mode: RunMode::EmitCst,
                with_gui: false,
                use_wasm: false,
                config,
            };
        }

        if args.mode.emit_ast {
            return Self {
                mode: RunMode::EmitAst,
                with_gui: true,
                use_wasm: false,
                config,
            };
        }

        if args.mode.emit_mir {
            return Self {
                mode: RunMode::EmitMir,
                with_gui: true,
                use_wasm: false,
                config,
            };
        }

        if args.mode.emit_bytecode {
            return Self {
                mode: RunMode::EmitByteCode,
                with_gui: true,
                use_wasm: false,
                config,
            };
        }

        if args.mode.emit_wasm {
            return Self {
                mode: RunMode::EmitWasm,
                with_gui: false,
                use_wasm: false,
                config,
            };
        }

        if args.mode.wasm {
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
            config,
        }
    }

    fn get_driver(&self) -> Box<dyn Driver<Sample = f64>> {
        match &self.mode {
            RunMode::NativeAudio => load_default_runtime(),
            RunMode::WasmAudio => load_default_runtime(),
            RunMode::WriteCsv { times, output } => csv_driver(*times, output),
            _ => unreachable!(),
        }
    }
}

/// Construct an [`ExecContext`] with the default set of plugins.
pub fn get_default_context(path: Option<PathBuf>, with_gui: bool, config: Config) -> ExecContext {
    let plugins: Vec<Box<dyn Plugin>> = vec![];
    let mut ctx = ExecContext::new(plugins.into_iter(), path, config);
    
    // Load dynamic plugins (e.g., mimium-symphonia)
    #[cfg(not(target_arch = "wasm32"))]
    {
        ctx.init_plugin_loader();
        
        // Try to load plugins from the same directory as the executable
        if let Ok(exe_path) = std::env::current_exe() {
            if let Some(exe_dir) = exe_path.parent() {
                let symphonia_path = exe_dir.join("mimium_symphonia");
                if let Err(e) = ctx.load_dynamic_plugin(&symphonia_path) {
                    log::debug!("Failed to load mimium-symphonia from executable directory: {:?}", e);
                }
            }
        }
        
        // Also try to load from standard plugin directory
        if let Err(e) = ctx.load_builtin_dynamic_plugins() {
            log::debug!("No builtin dynamic plugins found: {:?}", e);
        }
    }
    
    ctx.add_system_plugin(mimium_scheduler::get_default_scheduler_plugin());
    if let Some(midi_plug) = mimium_midi::MidiPlugin::try_new() {
        ctx.add_system_plugin(midi_plug);
    } else {
        log::warn!("Midi is not supported on this platform.")
    }

    if with_gui {
        #[cfg(not(target_arch = "wasm32"))]
        ctx.add_system_plugin(mimium_guitools::GuiToolPlugin::default());
    }

    ctx
}

struct FileRunner {
    pub tx_compiler: mpsc::Sender<CompileRequest>,
    pub rx_compiler: mpsc::Receiver<Result<Response, Errors>>,
    pub tx_prog: Option<mpsc::Sender<ProgramPayload>>,
    pub fullpath: PathBuf,
}

struct FileWatcher {
    pub rx: mpsc::Receiver<notify::Result<Event>>,
    pub watcher: notify::RecommendedWatcher,
}
impl FileRunner {
    pub fn new(
        compiler: compiler::Context,
        path: PathBuf,
        prog_tx: Option<mpsc::Sender<ProgramPayload>>,
    ) -> Self {
        let client = async_compiler::start_async_compiler_service(compiler);
        Self {
            tx_compiler: client.tx,
            rx_compiler: client.rx,
            tx_prog: prog_tx,
            fullpath: path,
        }
    }
    fn try_new_watcher(&self) -> Result<FileWatcher, notify::Error> {
        let (tx, rx) = mpsc::channel::<notify::Result<Event>>();
        let mut watcher = notify::recommended_watcher(tx)?;
        watcher.watch(Path::new(&self.fullpath), RecursiveMode::NonRecursive)?;
        Ok(FileWatcher { rx, watcher })
    }
    fn recompile_file(&self) {
        match fileloader::load(&self.fullpath.to_string_lossy()) {
            Ok(new_content) => {
                let _ = self.tx_compiler.send(CompileRequest {
                    source: new_content.clone(),
                    path: self.fullpath.clone(),
                    option: RunOptions {
                        mode: RunMode::EmitByteCode,
                        with_gui: true,
                        use_wasm: false,
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
                            let _ = tx.send(mimium_lang::runtime::ProgramPayload::VmProgram(prog));
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
            Err(e) => {
                log::error!(
                    "failed to reload the file {}: {}",
                    self.fullpath.display(),
                    e
                );
            }
        }
    }
    //this api never returns
    pub fn cli_loop(&self) {
        use notify::event::{EventKind, ModifyKind};
        //watcher instance lives only this context
        let file_watcher = match self.try_new_watcher() {
            Ok(watcher) => watcher,
            Err(e) => {
                log::error!("Failed to watch file: {e}");
                return;
            }
        };

        loop {
            match file_watcher.rx.recv() {
                Ok(Ok(Event {
                    kind: EventKind::Modify(ModifyKind::Data(_)),
                    ..
                })) => {
                    log::info!("File changed, recompiling...");
                    self.recompile_file();
                }
                Ok(Err(e)) => {
                    log::error!("watch error event: {e}");
                }
                Ok(_) => {}
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
        RunMode::EmitWasm => {
            use mimium_lang::utils::metadata::Location;
            use std::sync::Arc;

            ctx.prepare_compiler();
            let mir = ctx.get_compiler().unwrap().emit_mir(content)?;

            // Generate WASM module
            let mut generator = compiler::wasmgen::WasmGenerator::new(Arc::new(mir));
            let wasm_bytes = generator.generate().map_err(|e| {
                vec![Box::new(mimium_lang::utils::error::SimpleError {
                    message: e,
                    span: Location::default(),
                }) as Box<dyn ReportableError>]
            })?;

            // Output module info
            println!("Generated WASM module ({} bytes)", wasm_bytes.len());
            println!("Magic: {:?}", &wasm_bytes[0..4]);
            println!("Version: {:?}", &wasm_bytes[4..8]);

            // Write to file
            if let Some(parent) = fullpath.parent() {
                let wasm_path = parent
                    .join(fullpath.file_stem().unwrap())
                    .with_extension("wasm");
                std::fs::write(&wasm_path, &wasm_bytes).map_err(|e| {
                    vec![Box::new(mimium_lang::utils::error::SimpleError {
                        message: e.to_string(),
                        span: Location::default(),
                    }) as Box<dyn ReportableError>]
                })?;
                println!("Written to: {}", wasm_path.display());
            }

            Ok(())
        }
        RunMode::WasmAudio => {
            use mimium_lang::compiler::wasmgen::WasmGenerator;
            use mimium_lang::runtime::wasm::engine::{WasmDspRuntime, WasmEngine};
            use mimium_lang::utils::metadata::Location;
            use std::sync::Arc;

            ctx.prepare_compiler();
            let mir = ctx.get_compiler().unwrap().emit_mir(content)?;

            let io_channels = mir.get_dsp_iochannels();

            // Generate WASM module
            let mut generator = WasmGenerator::new(Arc::new(mir));
            let wasm_bytes = generator.generate().map_err(|e| {
                vec![Box::new(mimium_lang::utils::error::SimpleError {
                    message: e,
                    span: Location::default(),
                }) as Box<dyn ReportableError>]
            })?;

            log::info!("Generated WASM module ({} bytes)", wasm_bytes.len());

            // Create WASM engine and load module
            let mut wasm_engine = WasmEngine::new().map_err(|e| {
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
            let mut wasm_runtime = WasmDspRuntime::new(wasm_engine, io_channels);
            wasm_runtime.set_sample_rate(48000.0);
            let _ = wasm_runtime.run_main();

            let runtimedata = RuntimeData::new_from_runtime(Box::new(wasm_runtime));

            // Use the standard audio driver infrastructure
            let mut driver = options.get_driver();

            let mainloop = ctx.try_get_main_loop().unwrap_or(Box::new(move || {
                if options.with_gui {
                    loop {
                        std::thread::sleep(std::time::Duration::from_millis(1000));
                    }
                }
            }));

            driver.init(runtimedata, Some(SampleRate::from(48000)));
            driver.play();

            mainloop();
            Ok(())
        }
        _ if options.use_wasm => {
            // WASM backend with standard audio driver (WriteCsv or NativeAudio).
            use mimium_lang::compiler::wasmgen::WasmGenerator;
            use mimium_lang::runtime::wasm::engine::{WasmDspRuntime, WasmEngine};
            use mimium_lang::utils::metadata::Location;
            use std::sync::Arc;

            let mut driver = options.get_driver();

            ctx.prepare_compiler();
            let mir = ctx.get_compiler().unwrap().emit_mir(content)?;
            let io_channels = mir.get_dsp_iochannels();

            let mut generator = WasmGenerator::new(Arc::new(mir));
            let wasm_bytes = generator.generate().map_err(|e| {
                vec![Box::new(mimium_lang::utils::error::SimpleError {
                    message: e,
                    span: Location::default(),
                }) as Box<dyn ReportableError>]
            })?;

            log::info!("Generated WASM module ({} bytes)", wasm_bytes.len());

            let mut wasm_engine = WasmEngine::new().map_err(|e| {
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

            let mut wasm_runtime = WasmDspRuntime::new(wasm_engine, io_channels);
            wasm_runtime.set_sample_rate(48000.0);
            let _ = wasm_runtime.run_main();

            let runtimedata = RuntimeData::new_from_runtime(Box::new(wasm_runtime));

            driver.init(runtimedata, Some(SampleRate::from(48000)));
            driver.play();
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
            driver.init(runtimedata, Some(SampleRate::from(48000)));
            driver.play();

            let compiler = ctx.take_compiler().unwrap();

            let frunner = FileRunner::new(
                compiler,
                fullpath.to_path_buf(),
                driver.get_program_channel(),
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
    match &args.file {
        Some(file) => {
            let fullpath = fileloader::get_canonical_path(".", file)?;
            let content = fileloader::load(fullpath.to_str().unwrap())?;
            let options = RunOptions::from_args(&args);
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
