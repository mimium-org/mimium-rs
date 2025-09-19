fn main() {
    println!("cargo:rustc-env=TEST_ROOT={}", env!("CARGO_MANIFEST_DIR"));
}
