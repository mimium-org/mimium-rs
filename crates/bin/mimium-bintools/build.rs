use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=../../lib/plugins/mimium-symphonia");
    println!("cargo:rerun-if-changed=../../lib/plugins/mimium-midi");
    println!("cargo:rerun-if-changed=../../lib/plugins/mimium-guitools");

    // Plugins are already built as dependencies and placed in target/release
    // We just verify they exist
    let target_dir = get_target_dir();
    let profile = env::var("PROFILE").unwrap_or_else(|_| "debug".to_string());
    let build_dir = target_dir.join(&profile);

    // Platform-specific library naming
    let (prefix, ext) = if cfg!(target_os = "windows") {
        ("", "dll")
    } else {
        (
            "lib",
            if cfg!(target_os = "macos") {
                "dylib"
            } else {
                "so"
            },
        )
    };

    let plugins = ["mimium_symphonia", "mimium_midi", "mimium_guitools"];

    // Verify plugins exist (they're already in the right place)
    for plugin in &plugins {
        let lib_name = format!("{prefix}{plugin}.{ext}");
        let plugin_path = build_dir.join(&lib_name);

        if plugin_path.exists() {
            println!("cargo:warning=Found plugin: {}", plugin_path.display());
        } else {
            println!("cargo:warning=Plugin not found: {}", plugin_path.display());
        }
    }
}

fn get_target_dir() -> PathBuf {
    env::var("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("../../../target")
        })
}
