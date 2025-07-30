mod pretty_print;

use clap::Parser;
use mimium_lang::compiler::parser::parse;
use mimium_lang::interner::ToSymbol;
use pretty::Arena;
use std::fs;
use std::path::PathBuf;

use crate::pretty_print::program;
use mimium_lang::utils::error::report;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// File to format(if not specified, reads from stdin)
    #[clap(value_parser)]
    file: Option<PathBuf>,
    /// Width of the editor
    #[arg(long, default_value = "80")]
    width: usize,
    /// Indentation size
    #[arg(long, default_value = "4")]
    indent_size: usize,
}
use std::sync::LazyLock;
use std::sync::Mutex;
struct GlobalConfig {
    indent_size: usize,
}
impl Default for GlobalConfig {
    fn default() -> Self {
        Self { indent_size: 4 }
    }
}
static GLOBAL_DATA: LazyLock<Mutex<GlobalConfig>> =
    LazyLock::new(|| Mutex::new(GlobalConfig::default()));

fn main() {
    let args = Args::parse();

    if let Ok(mut gdata) = GLOBAL_DATA.try_lock() {
        gdata.indent_size = args.indent_size;
    }

    let file_path = args.file;
    let code = match file_path.as_ref() {
        Some(path) => fs::read_to_string(path).expect("Unable to read file"),
        None => {
            let mut buf = String::new();
            eprintln!("No file specified. Reading from stdin...");
            std::io::stdin()
                .read_line(&mut buf)
                .expect("Unable to read from stdin");
            buf
        }
    };
    let (prog, errs) = parse(&code, file_path.clone());
    if !errs.is_empty() {
        report(
            code.as_str(),
            file_path.map_or("<stdin>".to_symbol(), |p| p.to_string_lossy().to_symbol()),
            &errs,
        );

        return;
    }

    let allocator = Arena::new();
    let doc = program::pretty::<_, ()>(prog, &allocator);
    let mut w = Vec::new();
    doc.render(args.width, &mut w).unwrap();
    println!("{}", String::from_utf8(w).unwrap());
}
