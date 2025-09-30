use std::{
    io::stdin,
    path::{Path, PathBuf},
};

use clap::{Parser, ValueEnum};
use mimium_audiodriver::{
    backends::{csv::csv_driver, local_buffer::LocalBufferDriver},
    driver::{Driver, RuntimeData, SampleRate},
    load_default_runtime,
};
use mimium_lang::{
    Config, ExecContext,
    compiler::{bytecodegen::SelfEvalMode, emit_ast},
    interner::{Symbol, ToSymbol},
    log,
    plugin::Plugin,
    utils::{
        error::{ReportableError, report},
        fileloader,
        miniprint::MiniPrint,
    },
};
use mimium_symphonia::SamplerPlugin;
use ringbuf::{HeapCons, traits::Producer};

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
    /// Print AST and exit
    #[arg(long, default_value_t = false)]
    pub emit_ast: bool,

    /// Print MIR and exit
    #[arg(long, default_value_t = false)]
    pub emit_mir: bool,

    /// Print bytecode and exit
    #[arg(long, default_value_t = false)]
    pub emit_bytecode: bool,
}

pub enum RunMode {
    EmitAst,
    EmitMir,
    EmitByteCode,
    NativeAudio,
    WriteCsv {
        times: usize,
        output: Option<PathBuf>,
    },
}

/// Execution options derived from CLI arguments.
pub struct RunOptions {
    mode: RunMode,
    with_gui: bool,
    config: Config,
}

impl RunOptions {
    /// Convert parsed command line arguments into [`RunOptions`].
    pub fn from_args(args: &Args) -> Self {
        let config = args.clone().to_execctx_config();
        if args.mode.emit_ast {
            return Self {
                mode: RunMode::EmitAst,
                with_gui: true,
                config,
            };
        }

        if args.mode.emit_mir {
            return Self {
                mode: RunMode::EmitMir,
                with_gui: false,
                config,
            };
        }

        if args.mode.emit_bytecode {
            return Self {
                mode: RunMode::EmitByteCode,
                with_gui: true,
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
            config,
        }
    }

    fn get_driver(&self) -> Box<dyn Driver<Sample = f64>> {
        match &self.mode {
            RunMode::NativeAudio => load_default_runtime(),
            RunMode::WriteCsv { times, output } => csv_driver(*times, output),
            _ => unreachable!(),
        }
    }
}

/// Construct an [`ExecContext`] with the default set of plugins.
pub fn get_default_context(path: Option<Symbol>, with_gui: bool, config: Config) -> ExecContext {
    let plugins: Vec<Box<dyn Plugin>> = vec![Box::new(SamplerPlugin)];
    let mut ctx = ExecContext::new(plugins.into_iter(), path, config);
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

/// Compile and run a single source file according to the provided options.
pub fn run_file(
    options: RunOptions,
    content: &str,
    fullpath: &Path,
) -> Result<(), Vec<Box<dyn ReportableError>>> {
    log::debug!("Filename: {}", fullpath.display());
    let path_sym = fullpath.to_string_lossy().to_symbol();
    let mut ctx = get_default_context(Some(path_sym), options.with_gui, options.config);

    match options.mode {
        RunMode::EmitAst => {
            let ast = emit_ast(content, Some(path_sym))?;
            println!("{}", ast.pretty_print());
            Ok(())
        }
        RunMode::EmitMir => {
            ctx.prepare_compiler();
            let res = ctx.get_compiler().unwrap().emit_mir(content);
            res.map(|r| {
                println!("{r}");
            })
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
        _ => {
            let mut driver = options.get_driver();
            let audiodriver_plug = driver.get_as_plugin();
            ctx.add_plugin(audiodriver_plug);
            ctx.prepare_machine(content)?;
            let _res = ctx.run_main();
            let runtimedata = RuntimeData::try_from(&mut ctx).unwrap();
            //this takes ownership of ctx
            driver.init(runtimedata, Some(SampleRate::from(48000)));
            driver.play();
            loop {
                let mut line = String::new();
                let _size = stdin().read_line(&mut line).expect("stdin read error.");
                match line.trim() {
                    // "p" | "play" => {
                    //     log::info!("play");
                    //     let _ = driver.play();
                    // }
                    // "s" | "stop" | "pause" => {
                    //     log::info!("pause");
                    //     let _ = driver.pause();
                    // }
                    "u" | "update" => {
                        log::info!("update");
                        match fileloader::load(fullpath.to_str().unwrap()) {
                            Ok(new_content) => match ctx.prepare_machine_resume(&new_content) {
                                Ok(prog) => {
                                    driver.renew_vm(prog);
                                }
                                Err(e) => {
                                    report(content, path_sym, &e);
                                }
                            },
                            Err(e) => {
                                log::error!(
                                    "failed to reload the file {}: {}",
                                    fullpath.display(),
                                    e
                                );
                            }
                        }
                    }
                    "q" | "quit" | "exit" | "" => {
                        eprintln!("exit");
                        break;
                    }
                    _ => {
                        println!("commands: play(p), pause(s), quit(q/empty)");
                    }
                }
            }
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
                Ok(_) => {}
                Err(e) => {
                    // Note: I was hoping to implement std::error::Error for a
                    // struct around ReportableError and directly return it,
                    // however, std::error::Error cannot be so color-rich as
                    // ariadne because it just uses std::fmt::Display.
                    report(&content, fullpath.to_string_lossy().to_symbol(), &e);
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
