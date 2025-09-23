# mimium-test

Common integrated (or regression) test modules and basic integration tests of mimium.

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