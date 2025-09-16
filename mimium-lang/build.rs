use std::path::Path;

fn main() {
    println!("cargo:rustc-env=TEST_ROOT={}", env!("CARGO_MANIFEST_DIR"));
    
    let dir = Path::new("tree-sitter-mimium");
    
    cc::Build::new()
        .include(&dir.join("src"))
        .file(dir.join("src/parser.c"))
        .compile("tree-sitter-mimium");
    
    println!("cargo:rerun-if-changed=tree-sitter-mimium/grammar.js");
    println!("cargo:rerun-if-changed=tree-sitter-mimium/src/parser.c");
}
