use std::io::stdin;
use std::path::{Path, PathBuf};

// pub mod wcalculus;
use clap::{Parser, ValueEnum};
use mimium_audiodriver::backends::csv::csv_driver;
use mimium_audiodriver::backends::local_buffer::LocalBufferDriver;
use mimium_audiodriver::driver::{Driver, SampleRate};
use mimium_audiodriver::load_default_runtime;
use mimium_lang::compiler::emit_ast;
use mimium_lang::interner::{ExprNodeId, Symbol, ToSymbol};
use mimium_lang::log;
use mimium_lang::plugin::Plugin;
use mimium_lang::utils::error::ReportableError;
use mimium_lang::utils::miniprint::MiniPrint;
use mimium_lang::utils::{error::report, fileloader};
use mimium_lang::ExecContext;
use mimium_lang::{compiler::mirgen::convert_pronoun, repl};
use mimium_symphonia::{self, SamplerPlugin};
#[derive(clap::Parser, Debug)]
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

    /// Output format
    #[arg(long, value_enum)]
    pub output_format: Option<OutputFileFormat>,

    /// Don't launch GUI
    #[arg(long, default_value_t = false)]
    pub no_gui: bool,

    /// How many times to execute the code. This is only effective when --output
    /// is specified.
    #[arg(long, default_value_t = 10)]
    pub times: usize,
}

#[derive(Clone, Debug, ValueEnum)]
pub enum OutputFileFormat {
    Csv,
}

#[derive(clap::Args, Debug)]
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

enum RunMode {
    EmitAst,
    EmitMir,
    EmitByteCode,
    NativeAudio,
    WriteCsv {
        times: usize,
        output: Option<PathBuf>,
    },
}

struct RunOptions {
    mode: RunMode,
    with_gui: bool,
}

impl RunOptions {
    fn from_args(args: &Args) -> Self {
        if args.mode.emit_ast {
            return Self {
                mode: RunMode::EmitAst,
                with_gui: false,
            };
        }

        if args.mode.emit_mir {
            return Self {
                mode: RunMode::EmitMir,
                with_gui: false,
            };
        }

        if args.mode.emit_bytecode {
            return Self {
                mode: RunMode::EmitByteCode,
                with_gui: false,
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

        Self { mode, with_gui }
    }

    fn get_driver(&self) -> Box<dyn Driver<Sample = f64>> {
        match &self.mode {
            RunMode::NativeAudio => load_default_runtime(),
            RunMode::WriteCsv { times, output } => csv_driver(*times, output),
            _ => unreachable!(),
        }
    }
}

fn get_default_context(path: Option<Symbol>, with_gui: bool) -> ExecContext {
    let plugins: Vec<Box<dyn Plugin>> = vec![Box::new(SamplerPlugin)];
    let mut ctx = ExecContext::new(plugins.into_iter(), path);
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

fn emit_ast_local(src: &str, filepath: &Path) -> Result<ExprNodeId, Vec<Box<dyn ReportableError>>> {
    let path = filepath.to_str().unwrap().to_symbol();
    let ast1 = emit_ast(src, Some(path))?;

    let (ast, errs) = convert_pronoun::convert_pronoun(ast1, path);
    if errs.is_empty() {
        Ok(ast)
    } else {
        Err(errs
            .into_iter()
            .map(|e| -> Box<dyn ReportableError> { Box::new(e) })
            .collect())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    if cfg!(debug_assertions) | cfg!(test) {
        colog::default_builder()
            .filter_level(log::LevelFilter::Debug)
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
            repl::run_repl();
        }
    }
    Ok(())
}

fn run_file(
    options: RunOptions,
    content: &str,
    fullpath: &Path,
) -> Result<(), Vec<Box<dyn ReportableError>>> {
    log::debug!("Filename: {}", fullpath.display());
    let path_sym = fullpath.to_string_lossy().to_symbol();
    let mut ctx = get_default_context(Some(path_sym), options.with_gui);

    match options.mode {
        RunMode::EmitAst => {
            let ast = emit_ast_local(content, fullpath)?;
            Ok(println!("{}", ast.pretty_print()))
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
            Ok(println!("{}", ctx.get_vm().unwrap().prog))
        }
        _ => {
            let mut driver = options.get_driver();
            let audiodriver_plug = driver.get_as_plugin();
            ctx.add_plugin(audiodriver_plug);
            ctx.prepare_machine(content)?;
            let _res = ctx.run_main();
            let mainloop = ctx.try_get_main_loop().unwrap_or(Box::new(|| {
                //wait until input something
                let mut dummy = String::new();
                eprintln!("Press Enter to exit");
                let _size = stdin().read_line(&mut dummy).expect("stdin read error.");
            }));
            driver.init(ctx, Some(SampleRate::from(48000)));
            driver.play();
            mainloop();
            Ok(())
        }
    }
}
