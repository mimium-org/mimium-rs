# mimium-test

Common integrated (or regression) test modules and basic integration tests of mimium.

## Testing with Different Backends

mimium-test supports running integration tests with both the **VM backend** (default) and the **WASM backend**.

### Running tests with VM backend (default)

```bash
cargo test --package mimium-test
```

### Running tests with WASM backend

Use the `MIMIUM_BACKEND` environment variable to switch to WASM backend:

**PowerShell (Windows):**
```powershell
$env:MIMIUM_BACKEND="wasm"; cargo test --package mimium-test
```

**Bash/Zsh (Unix/Linux/macOS):**
```bash
MIMIUM_BACKEND=wasm cargo test --package mimium-test
```

### Running specific tests with WASM backend

**PowerShell:**
```powershell
$env:MIMIUM_BACKEND="wasm"; cargo test --package mimium-test --test integration_test simple_arithmetic
```

**Bash:**
```bash
MIMIUM_BACKEND=wasm cargo test --package mimium-test --test integration_test simple_arithmetic
```

### Implementation Details

The test infrastructure automatically checks the `MIMIUM_BACKEND` environment variable. When set to `wasm`, the following functions automatically use the WASM backend:
- `run_source_test()` - Compiles and runs source code
- `run_file_test()` - Loads and runs test files
- `run_file_test_mono()` - Mono output tests
- `run_file_test_stereo()` - Stereo output tests

This means you can write tests once and run them on both backends without any code changes.

## When use test modules from external crate

When you write test for mimium-plugin, you can import this test crate as `dev-dependencies` to utilize integrated test modules.

When you use this crate from external crate, you must set OS environment variable `TEST_ROOT` to the same location of `Cargo.toml` in your crate. Typically, this can be set by making `build.rs` at your crate root with the content like this.

```rust
fn main() {
    println!("cargo:rustc-env=TEST_ROOT={}", env!("CARGO_MANIFEST_DIR"));
}
```

And you need to set the line to `Cargo.toml` in your crate.

```toml
...
build = "build.rs"
...
```

See `mimium-symphonia` crate for instance if you want to know more.