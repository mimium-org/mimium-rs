//! Command line interface for the mimium language.
//!
//! This binary compiles and executes mimium programs using various audio
//! backends.  It can also emit intermediate representations such as the AST or
//! MIR for debugging purposes.

use clap::Parser;
use mimium_cli::{Args, RunOptions, run_file};
use mimium_lang::interner::ToSymbol;

use mimium_lang::log;
use mimium_lang::utils::{error::report, fileloader};

fn main() -> Result<(), Box<dyn std::error::Error>> {
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
