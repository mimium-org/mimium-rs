mod cst_print;
mod parser_print;
mod print;

use std::sync::LazyLock;
use std::sync::Mutex;
pub struct GlobalConfig {
    pub indent_size: usize,
}
impl Default for GlobalConfig {
    fn default() -> Self {
        Self { indent_size: 4 }
    }
}
pub static GLOBAL_DATA: LazyLock<Mutex<GlobalConfig>> =
    LazyLock::new(|| Mutex::new(GlobalConfig::default()));

/// CST-based pretty print (experimental - includes comments in width calculation)
pub use cst_print::pretty_print as pretty_print_cst;
pub use parser_print::pretty_print;

use clap::Parser;
use mimium_lang::utils::error::report;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// File to format(if not specified, reads from stdin)
    #[clap(value_parser)]
    file: Option<PathBuf>,
    /// Width of the editor
    #[arg(long, default_value = "80")]
    width: usize,
    /// Indentation size
    #[arg(long, default_value = "4")]
    indent_size: usize,
    /// Use CST-based formatter (experimental)
    #[arg(long)]
    cst: bool,
}

pub fn lib_main() {
    let args = Args::parse();

    if let Ok(mut gdata) = GLOBAL_DATA.try_lock() {
        gdata.indent_size = args.indent_size;
    }

    let file_path = args.file;
    let code = match file_path.as_ref() {
        Some(path) => fs::read_to_string(path).expect("Unable to read file"),
        None => {
            use std::io::Read;
            let mut buf = String::new();
            eprintln!("No file specified. Reading from stdin...");
            std::io::stdin()
                .read_to_string(&mut buf)
                .expect("Unable to read from stdin");
            buf
        }
    };

    let res = if args.cst {
        pretty_print_cst(code.as_str(), &file_path, args.width)
    } else {
        pretty_print(code.as_str(), &file_path, args.width)
    };

    match res {
        Ok(rendered) => {
            println!("{rendered}");
        }
        Err(errs) => {
            report(code.as_str(), file_path.unwrap_or_default(), &errs);
        }
    }
}
