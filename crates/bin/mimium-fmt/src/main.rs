use clap::Parser;
use mimium_lang::interner::ToSymbol;
use mimium_lang::utils::error::report;
use std::fs;
use std::path::PathBuf;

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

fn main() {
    let args = Args::parse();

    if let Ok(mut gdata) = mimium_fmt::GLOBAL_DATA.try_lock() {
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
    let res = mimium_fmt::pretty_print(code.as_str(), &file_path, args.width);
    match res {
        Ok(rendered) => {
            println!("{rendered}");
        }
        Err(errs) => {
            report(
                code.as_str(),
                file_path.map_or("<stdin>".to_symbol(), |p| p.to_string_lossy().to_symbol()),
                &errs,
            );
        }
    }
}
