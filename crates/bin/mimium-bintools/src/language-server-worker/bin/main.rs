#[cfg(not(target_arch = "wasm32"))]
fn main() {
    mimium_language_server::worker_main();
}

#[cfg(target_arch = "wasm32")]
fn main() {}
