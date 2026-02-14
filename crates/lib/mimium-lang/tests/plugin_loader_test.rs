//! Integration tests for dynamic plugin loading.

use std::path::PathBuf;

/// Helper function to get the plugin path and ensure it exists.
/// Returns None if the plugin is not built yet.
#[cfg(not(target_arch = "wasm32"))]
fn get_plugin_path(plugin_name: &str) -> Option<PathBuf> {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let base_path = PathBuf::from(manifest_dir);
    let workspace_root = base_path
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap();

    let plugin_path = workspace_root
        .join("target")
        .join("debug")
        .join(plugin_name);

    #[cfg(target_os = "windows")]
    let plugin_file = plugin_path.with_extension("dll");
    #[cfg(target_os = "macos")]
    let plugin_file = plugin_path.with_extension("dylib");
    #[cfg(all(not(target_os = "windows"), not(target_os = "macos")))]
    let plugin_file = plugin_path.with_extension("so");

    if plugin_file.exists() {
        Some(plugin_path)
    } else {
        eprintln!(
            "⚠️  Plugin not found at: {}\n   Run 'cargo build --package {}' first",
            plugin_file.display(),
            plugin_name
        );
        None
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn test_load_sampler_plugin() {
    use mimium_lang::plugin::loader::PluginLoader;

    let Some(plugin_path) = get_plugin_path("mimium_symphonia") else {
        eprintln!("Skipping test: plugin not built");
        return;
    };

    // Path to the compiled Sampler plugin
    let mut loader = PluginLoader::new();

    println!("Attempting to load plugin from: {}", plugin_path.display());

    // Try to load the plugin
    match loader.load_plugin(&plugin_path) {
        Ok(_) => {
            let loaded = loader.loaded_plugins();
            assert_eq!(loaded.len(), 1, "Should have loaded 1 plugin");

            let plugin = &loaded[0];
            assert_eq!(plugin.name(), "mimium-symphonia");
            assert!(plugin.metadata().capabilities.has_macros);
            assert!(plugin.metadata().capabilities.has_runtime_functions);
            assert!(!plugin.metadata().capabilities.has_audio_worker);

            println!(
                "✓ Successfully loaded plugin: {} v{}",
                plugin.name(),
                plugin.version()
            );
        }
        Err(e) => {
            panic!("Failed to load plugin: {:?}", e);
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn test_load_nonexistent_plugin() {
    use mimium_lang::plugin::loader::PluginLoader;
    use std::path::PathBuf;

    let mut loader = PluginLoader::new();
    let result = loader.load_plugin(PathBuf::from("/nonexistent/path/to/plugin"));

    assert!(result.is_err(), "Should fail to load nonexistent plugin");
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn test_call_plugin_function() {
    use mimium_lang::plugin::loader::PluginLoader;
    use mimium_lang::runtime::vm::{Machine, Program};

    let Some(plugin_path) = get_plugin_path("mimium_symphonia") else {
        eprintln!("Skipping test: plugin not built");
        return;
    };

    // Load the plugin
    let mut loader = PluginLoader::new();

    println!("Loading plugin from: {}", plugin_path.display());

    loader
        .load_plugin(&plugin_path)
        .expect("Failed to load plugin");

    let plugin = &loader.loaded_plugins()[0];
    println!("✓ Loaded plugin: {} v{}", plugin.name(), plugin.version());

    // Get the function
    let get_sampler_fn = plugin
        .get_function("__get_sampler")
        .expect("Failed to get __get_sampler function");

    println!("✓ Got function pointer for __get_sampler");

    // Create a minimal VM with a program
    let empty_program = Program::default();
    let mut machine = Machine::new(empty_program, std::iter::empty(), std::iter::empty());

    // Set up arguments on the stack
    // get_sampler expects: (pos: f64, sample_idx: f64)
    machine.set_stack(0, Machine::to_value(0.0)); // pos
    machine.set_stack(1, Machine::to_value(0.0)); // sample_idx

    // Call the plugin function
    let return_code = unsafe {
        get_sampler_fn(
            plugin.instance_ptr(),
            &mut machine as *mut _ as *mut std::ffi::c_void,
        )
    };

    println!("✓ Called function, return code: {}", return_code);

    // get_sampler returns 1 value (f64)
    assert_eq!(return_code, 1, "Should return 1 value");

    // Get the result from the stack
    let result = Machine::get_as::<f64>(machine.get_stack(0));
    println!("✓ Result: {}", result);

    // Since we didn't load any actual samples, it should return 0.0 (error fallback)
    assert_eq!(result, 0.0, "Should return 0.0 for invalid sample index");
}
