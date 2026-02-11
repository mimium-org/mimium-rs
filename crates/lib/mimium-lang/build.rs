use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    println!("cargo:rustc-env=TEST_ROOT={}", env!("CARGO_MANIFEST_DIR"));

    // When building tests (not on wasm32), ensure plugin cdylibs are built
    let target = env::var("TARGET").unwrap_or_default();
    let profile = env::var("PROFILE").unwrap_or_default();
    
    // Only build plugins for native tests (not for wasm32 or when building lib only)
    if !target.contains("wasm32") && profile == "debug" {
        // Check if we're building tests by looking for CARGO_CFG_TEST or checking features
        let building_tests = env::var("CARGO_CFG_TEST").is_ok()
            || env::var("CARGO_PRIMARY_PACKAGE").is_ok();
        
        if building_tests {
            println!("cargo:warning=Building plugin cdylibs for integration tests...");
            
            // Get the workspace root
            let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            let workspace_root = manifest_dir
                .parent()
                .and_then(|p| p.parent())
                .and_then(|p| p.parent())
                .expect("Failed to find workspace root");
            
            // Build mimium-symphonia as cdylib
            let status = Command::new("cargo")
                .arg("build")
                .arg("--package")
                .arg("mimium-symphonia")
                .arg("--lib")
                .current_dir(workspace_root)
                .status();
            
            match status {
                Ok(status) if status.success() => {
                    println!("cargo:warning=Successfully built mimium-symphonia plugin");
                }
                Ok(status) => {
                    println!("cargo:warning=Failed to build mimium-symphonia: exit code {:?}", status.code());
                }
                Err(e) => {
                    println!("cargo:warning=Failed to execute cargo build: {}", e);
                }
            }
            
            // Tell cargo to rerun if the plugin sources change
            println!("cargo:rerun-if-changed=../plugins/mimium-symphonia/src");
        }
    }
}
