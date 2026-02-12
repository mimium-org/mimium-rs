# mimium Plugin Development Guide

This document describes how to develop plugins for mimium. Plugins extend mimium's capabilities by providing external functions, compile-time macros, and audio-thread callbacks that are callable from mimium source code.

mimium is transitioning to a WASM-based backend. Plugin development should use the procedural macros (`#[mimium_plugin_fn]`, `mimium_export_plugin!`) to write backend-agnostic code. Direct VM `Machine` manipulation is discouraged for new plugins.

## Plugin Categories

mimium supports three categories of plugins:

| Category | Trait | State Model | Use Case |
|----------|-------|------------|----------|
| **IO Plugin** | `Plugin` | Stateless, no per-instance state | Simple external functions (e.g., `print`) |
| **UGen Plugin** | `UGenPlugin` | Per-instance state, multiple instances | Native Unit Generators (e.g., sampler) |
| **System Plugin** | `SystemPlugin` | Singleton per VM | System-wide state with lifecycle hooks (e.g., scheduler, MIDI, GUI) |

Most practical plugins use the **System Plugin** interface.

## Evaluation Stages

Functions exposed by plugins operate at one of two stages:

- **Stage 0 (Macro)** — Executed at compile time. Receives AST values and returns transformed AST. Used for code generation.
- **Stage 1 (Runtime)** — Executed at runtime. Reads/writes `f64` values via the generated wrapper. Works on both the native VM and the WASM backend.

## Architecture Overview

A typical plugin consists of the following layers:

```
┌─────────────────────────────────────────────┐
│  mimium source code                         │
│   e.g.  let v = Slider!("vol",0.5,0.0,1.0) │
├─────────────────────────────────────────────┤
│  Stage 0 (compile-time macros)              │
│   Receives AST values, returns Code(...)    │
├─────────────────────────────────────────────┤
│  Stage 1 (runtime functions)                │
│   #[mimium_plugin_fn] methods               │
│   Works on both native VM & WASM backend    │
├─────────────────────────────────────────────┤
│  SystemPlugin trait                         │
│   Lifecycle hooks, interface registration   │
├─────────────────────────────────────────────┤
│  mimium_export_plugin! macro                │
│   Generates C ABI + WASM trampoline bridge  │
│   Enables dynamic loading as .dylib/.so     │
└─────────────────────────────────────────────┘
```

## Developing Plugins in Rust

### Dependencies

```toml
[package]
name = "mimium-myplugin"
edition = "2024"

[lib]
crate-type = ["cdylib", "rlib"]   # cdylib for dynamic loading, rlib for static linking

[dependencies]
mimium-lang = { workspace = true }
mimium-plugin-macros = { workspace = true }
bincode = { workspace = true }
```

### Step 1: Define Your Plugin Struct

```rust
use mimium_plugin_macros::{mimium_plugin_fn, mimium_export_plugin};
use mimium_lang::{
    function, numeric, unit,
    types::{PType, Type},
    plugin::{SysPluginSignature, SystemPlugin, SystemPluginFnType},
    runtime::vm::{Machine, ReturnCode},
};

pub struct MyPlugin {
    state: f64,
}

impl MyPlugin {
    pub fn new() -> Self {
        Self { state: 0.0 }
    }
}
```

### Step 2: Implement Runtime Functions with `#[mimium_plugin_fn]`

Use `#[mimium_plugin_fn]` to write runtime functions in idiomatic Rust. The macro automatically generates a VM-compatible wrapper that handles stack I/O for both the native VM and the WASM backend. **You never need to touch `Machine` directly.**

```rust
impl MyPlugin {
    /// Exposed to mimium as `my_gain(input, gain) -> float`
    #[mimium_plugin_fn]
    pub fn my_gain(&mut self, input: f64, gain: f64) -> f64 {
        self.state = input * gain;
        self.state
    }
}
```

The macro generates a companion wrapper method with the same name that has the low-level signature `fn(&mut Self, &mut Machine) -> ReturnCode`. You reference this wrapper name in `gen_interfaces()` and `mimium_export_plugin!`.

**Supported types:**

| Position | Supported Types |
|----------|----------------|
| Arguments | `f64` |
| Return | `f64`, `(f64, f64)`, `(f64, f64, f64)`, ... , `()` |

### Step 3: Implement `SystemPlugin`

```rust
impl SystemPlugin for MyPlugin {
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        let fun: SystemPluginFnType<Self> = Self::my_gain;
        let sig = SysPluginSignature::new(
            "my_gain",
            fun,
            function!(vec![numeric!(), numeric!()], numeric!()),
        );
        vec![sig]
    }
}
```

### Step 4: Export with `mimium_export_plugin!`

Add this at the bottom of your `lib.rs` to generate the C ABI and WASM bridge:

```rust
mimium_export_plugin! {
    plugin_type: MyPlugin,
    plugin_name: "mimium-myplugin",
    plugin_author: "your-name",
    constructor: "new",       // "try_new" for fallible constructors (-> Option<Self>)
    capabilities: {
        has_audio_worker: false,
        has_macros: false,
        has_runtime_functions: true,
    },
    runtime_functions: [
        ("my_gain", my_gain),   // (exported_name, method_name)
    ],
    macro_functions: [],
    type_infos: [
        {
            name: "my_gain",
            ty_expr: function!(vec![numeric!(), numeric!()], numeric!()),
            stage: 1
        },
    ],
}
```

### Registering a Plugin

In the host application (e.g., `mimium-cli`), register via `ExecContext::add_system_plugin`:

```rust
ctx.add_system_plugin(MyPlugin::new());
```

Dynamic plugins are discovered automatically from the executable directory or `$MIMIUM_PLUGIN_DIR` (see [Plugin Discovery](#plugin-discovery)).

---

## WASM Backend Support

The WASM backend compiles mimium code to WebAssembly and executes it via Wasmtime. Plugin runtime functions are bridged to WASM through **trampolines** — host-side closures that WASM code calls via `import`.

### Audio Handle and `wasm_audio_handle`

For plugins that provide runtime functions used on the audio thread, add a `wasm_audio_handle` section to `mimium_export_plugin!`. This generates an `into_wasm_plugin_fn_map()` method on your audio handle type, which converts it into a `WasmPluginFnMap` — a `HashMap<String, Arc<dyn Fn(&[f64]) -> Option<f64>>>` that the WASM runtime registers as host imports.

#### Pattern: Freeze + Audio Handle

The recommended pattern is:

1. **Plugin struct** — holds mutable setup state (populated by macros at compile time).
2. **Audio handle struct** — a lock-free, `Send`-able snapshot of the state for the audio thread.
3. **`freeze()`** — drains the plugin state into an audio handle.
4. **`freeze_for_wasm()`** — calls `freeze()` and converts the handle into `WasmPluginFnMap`.

```rust
/// Audio-thread handle (lock-free, Send-able).
pub struct MyAudioHandle {
    pub data: Box<[f64]>,
}

impl MyAudioHandle {
    pub fn get_value(&mut self, idx: f64) -> f64 {
        self.data.get(idx as usize).copied().unwrap_or(0.0)
    }
}

impl MyPlugin {
    pub fn freeze(&mut self) -> MyAudioHandle {
        MyAudioHandle {
            data: self.internal_data.drain(..).collect::<Vec<_>>().into_boxed_slice(),
        }
    }
}
```

Then in `mimium_export_plugin!`:

```rust
mimium_export_plugin! {
    // ... (plugin_type, plugin_name, etc.)
    runtime_functions: [
        ("__get_value", get_value),
    ],
    // ...
    wasm_audio_handle: {
        handle_type: MyAudioHandle,
        functions: [
            // (exported_name, method_name, number_of_f64_args)
            ("__get_value", get_value, 1),
        ],
    },
}
```

The macro generates:

```rust
impl MyAudioHandle {
    pub fn into_wasm_plugin_fn_map(self) -> WasmPluginFnMap {
        let handle = Arc::new(Mutex::new(self));
        let mut map = WasmPluginFnMap::new();
        // For each function entry:
        {
            let h = handle.clone();
            map.insert("__get_value".to_string(), Arc::new(move |args: &[f64]| -> Option<f64> {
                if args.len() >= 1 { Some(h.lock().ok()?.get_value(args[0])) } else { None }
            }));
        }
        map
    }
}
```

Finally, implement `freeze_for_wasm` on your `SystemPlugin`:

```rust
#[cfg(not(target_arch = "wasm32"))]
fn freeze_for_wasm(&mut self) -> Option<mimium_lang::runtime::wasm::WasmPluginFnMap> {
    if self.internal_data.is_empty() {
        return None;
    }
    Some(self.freeze().into_wasm_plugin_fn_map())
}
```

See `mimium-guitools` for a complete working example with `Slider` and `Probe` macros.

---

## Compile-Time Macros (Stage 0)

To define a macro that runs at compile time, use `SysPluginSignature::new_macro`. Macros receive literal values as `&[(Value, TypeNodeId)]` and return a `Value::Code(...)` containing the AST to splice into the program.

A common pattern is to allocate a unique ID at compile time and generate code that calls a runtime helper with that ID:

```rust
use mimium_lang::{
    ast::{Expr, Literal},
    interner::{ToSymbol, TypeNodeId},
    interpreter::Value,
    plugin::{SysPluginSignature, SystemPluginMacroType},
};

impl MyPlugin {
    fn my_macro(&mut self, args: &[(Value, TypeNodeId)]) -> Value {
        let uid = self.allocate_resource(/* ... */);

        // Generate code: __get_value(<uid>)
        Value::Code(
            Expr::Apply(
                Expr::Var("__get_value".to_symbol()).into_id_without_span(),
                vec![
                    Expr::Literal(Literal::Float(uid.to_string().to_symbol()))
                        .into_id_without_span(),
                ],
            )
            .into_id_without_span(),
        )
    }
}

// In gen_interfaces():
let macro_fn: SystemPluginMacroType<Self> = Self::my_macro;
let sig = SysPluginSignature::new_macro(
    "MyMacro",
    macro_fn,
    function!(vec![numeric!()], Type::Code(numeric!()).into_id()),
);
```

Register macros in `mimium_export_plugin!`:

```rust
macro_functions: [
    ("MyMacro", my_macro),
],
type_infos: [
    { name: "MyMacro", sig: MyPlugin::my_macro_signature(), stage: 0 },
],
```

For `type_infos` with stage 0, you can use either `ty_expr:` (inline type expression) or `sig:` (call a helper method that returns a `SysPluginSignature`). The `sig:` form is useful when the type is complex.

---

## Lifecycle Hooks

`SystemPlugin` provides optional lifecycle hooks:

| Hook | Timing | Typical Use |
|------|--------|-------------|
| `on_init(&mut self, &mut Machine)` | Before main function execution | VM initialization |
| `after_main(&mut self, &mut Machine)` | After main function completes | Connect to external devices, start I/O |
| `try_get_main_loop(&mut self)` | After compilation | Provide a GUI main loop (e.g., eframe) |
| `generate_audioworker(&mut self)` | During setup | Produce audio-thread worker (native VM only) |
| `freeze_audio_handle(&mut self)` | Before audio starts | Snapshot state for the native audio thread |
| `freeze_for_wasm(&mut self)` | Before WASM execution | Snapshot state as `WasmPluginFnMap` |

---

## Dynamic Loading

Plugins compiled as shared libraries (`.dylib` / `.so` / `.dll`) are loaded at runtime without recompiling the host. The `mimium_export_plugin!` macro generates all necessary C ABI exports.

### Generated C ABI Exports

| Export | Purpose |
|--------|---------|
| `mimium_plugin_metadata` | Returns plugin name, version, and capability flags |
| `mimium_plugin_create` | Constructs a plugin instance |
| `mimium_plugin_destroy` | Drops and deallocates the plugin instance |
| `mimium_plugin_set_interner` | Shares the host's symbol interner across the DLL boundary |
| `mimium_plugin_get_function` | Dispatches a runtime function call by name |
| `mimium_plugin_get_macro` | Dispatches a macro function call by name (bincode-serialized) |
| `mimium_plugin_get_type_infos` | Returns serialized type information for all exported functions |

### Plugin Discovery

The host searches for plugins in the following order:

1. The directory containing the mimium executable
2. The path specified by the `MIMIUM_PLUGIN_DIR` environment variable

Shared library files must follow the naming convention:

- macOS: `libmimium_<name>.dylib`
- Linux: `libmimium_<name>.so`
- Windows: `mimium_<name>.dll`

---

## Integrating C/C++ Libraries

While Rust is the primary plugin development language, you can integrate existing C/C++ code by wrapping it with a Rust plugin. This is the recommended approach — it lets you use the proc macros for safe ABI generation while calling into native C code via FFI.

```rust
// Link your C library
extern "C" {
    fn my_c_dsp_process(input: f64, param: f64) -> f64;
}

impl MyPlugin {
    #[mimium_plugin_fn]
    pub fn process(&mut self, input: f64, param: f64) -> f64 {
        unsafe { my_c_dsp_process(input, param) }
    }
}
```

Use the `cc` crate or `bindgen` in your `build.rs` to compile and link C sources. The `#[mimium_plugin_fn]` wrapper ensures compatibility with both native VM and WASM backends.

Writing a plugin entirely in C (implementing the C ABI exports directly) is possible but discouraged — it requires matching the exact layout of `mimium_plugin_*` functions, handling bincode serialization for macros, and managing the interner manually. The Rust wrapper approach avoids all of these pitfalls.

---

## Type System Helpers

mimium provides Rust macros for constructing type expressions used in `SysPluginSignature` and `type_infos`:

| Macro | Type | Description |
|-------|------|-------------|
| `numeric!()` | `Float` | 64-bit floating point number |
| `unit!()` | `()` | Unit / void |
| `string_t!()` | `String` | String type |
| `function!(args, ret)` | `(args...) -> ret` | Function type |
| `code!(ty)` | `Code<ty>` | Staged code type (for macros) |

Example: `function!(vec![numeric!(), numeric!()], numeric!())` represents `(float, float) -> float`.

## Reference Plugins

| Plugin | Key Features | Dynamic | WASM | Reference For |
|--------|-------------|---------|------|---------------|
| `mimium-scheduler` | Audio worker, lifecycle hooks | No | No | Minimal `SystemPlugin` impl |
| `mimium-midi` | `#[mimium_plugin_fn]`, macros, `mimium_export_plugin!` | Yes | No | Dynamic loading, macro + runtime combination |
| `mimium-guitools` | Macros, `wasm_audio_handle`, GUI main loop | Yes | Yes | Full WASM integration, freeze pattern |
| `mimium-symphonia` | UGen, sample playback | Yes | No | `UGenPlugin` pattern |

## Debugging Tips

- Use `log::info!`, `log::debug!`, and `log::error!` for plugin logging. The host captures these via the `log` crate.
- Set `RUST_LOG=debug` to see plugin loading diagnostics.
- Set `MIMIUM_PLUGIN_DIR` to override the plugin search path.
- Inspect intermediate representations with `--emit-cst`, `--emit-ast`, `--emit-mir`, `--emit-bytecode` CLI flags.
- Test output without audio by using `--output-format=csv` to write sample values to CSV.
