mod pretty_print;

use clap::Parser;
use mimium_lang::compiler::parser::parse;
use pretty::Arena;
use std::fs;
use std::path::PathBuf;

use crate::pretty_print::program;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// File to format
    #[arg(short, long)]
    file: PathBuf,
}

fn main() {
    let args = Args::parse();
    let file_path = args.file;
    let code = fs::read_to_string(&file_path).expect("Unable to read file");
    let (prog, errs) = parse(&code, Some(file_path));
    if !errs.is_empty() {
        for err in errs {
            eprintln!("{err}");
        }
        return;
    }

    let allocator = Arena::new();
    let doc = program::pretty::<_, ()>(prog, &allocator);
    let mut w = Vec::new();
    doc.render(80, &mut w).unwrap();
    println!("{}", String::from_utf8(w).unwrap());
}
