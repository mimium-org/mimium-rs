use std::io::stdin;
use std::path::Path;

// pub mod wcalculus;
use clap::Parser;
use mimium_audiodriver::backends::mock::MockDriver;
use mimium_audiodriver::driver::load_default_runtime;
use mimium_lang::compiler::{emit_ast, emit_bytecode};
use mimium_lang::interner::ExprNodeId;
use mimium_lang::utils::error::ReportableError;
use mimium_lang::utils::miniprint::MiniPrint;
use mimium_lang::utils::{error::report, fileloader};
use mimium_lang::{compiler::emit_mir, compiler::mirgen::convert_pronoun, repl};
#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[command(flatten)]
    pub mode: Mode,

    /// File name
    #[clap(value_parser)]
    pub file: Option<String>,
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

    /// Write out the signal values to stdout in CSV format
    #[arg(long, default_value_t = 0)]
    pub emit_number: usize,
}

fn emit_ast_local(src: &str) -> Result<ExprNodeId, Vec<Box<dyn ReportableError>>> {
    let ast1 = emit_ast(src)?;

    convert_pronoun::convert_pronoun(ast1).map_err(|e| {
        let eb: Vec<Box<dyn ReportableError>> = vec![Box::new(e)];
        eb
    })
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
            let (content, fullpath) = fileloader::load(file.clone())?;
            match run_file(&args, &content, &fullpath) {
                Ok(_) => {}
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
            repl::run_repl();
        }
    }
    Ok(())
}

fn run_file(
    args: &Args,
    content: &str,
    fullpath: &Path,
) -> Result<(), Vec<Box<dyn ReportableError>>> {
    log::debug!("Filename: {}", fullpath.display());
    if args.mode.emit_ast {
        let ast = emit_ast_local(content)?;
        println!("{}", ast.pretty_print());
    } else if args.mode.emit_mir {
        let mir = emit_mir(content)?;
        println!("{mir}");
    } else {
        let prog = emit_bytecode(content)?;

        if args.mode.emit_bytecode {
            println!("{prog}");
            return Ok(());
        }

        if args.mode.emit_number > 0 {
            let mut driver = MockDriver::new(prog, None);
            let chunk_size = driver.get_ochannels();

            let header = (0..chunk_size)
                .map(|i| format!("ch{i}"))
                .collect::<Vec<_>>()
                .join(",");
            println!("{header}");

            for sample in driver
                .play_times(args.mode.emit_number as _)
                .chunks(chunk_size)
            {
                let line = sample
                    .iter()
                    .map(|x| format!("{x:?}")) // :? is to display "0" as "0.0"
                    .collect::<Vec<_>>()
                    .join(",");
                println!("{line}");
            }
        } else {
            let mut driver = load_default_runtime();
            driver.init(prog, None, 4096);
            let mut dummy = String::new();
            driver.play();
            //wait until input something
            let _size = stdin().read_line(&mut dummy).expect("stdin read error.");
        }
    }
    Ok(())
}
