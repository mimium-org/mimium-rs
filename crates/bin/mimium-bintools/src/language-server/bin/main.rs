#[cfg(not(target_arch = "wasm32"))]
#[tokio::main]
async fn main() {
    mimium_language_server::lib_main().await;
}

#[cfg(target_arch = "wasm32")]
fn main() {}
