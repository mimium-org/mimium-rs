mimium (MInimal Musical MedIUM) is a programming language specialized for describing and generating music.

mimium is based on a computation system called λ<sub>mmm</sub> (lambda-triple-m), which extends the Simply Typed Lambda Calculus (STLC). It uses a call-by-value evaluation strategy, the simplest evaluation strategy where all arguments are copied when a function is called, creating new values.

The language implements its own virtual machine (VM) and instruction set to execute λ<sub>mmm</sub> at practical speeds. While this approach may not be as performant as some other music programming environments like Faust, it covers most practical real-time use cases.

## Overall Directory Structure

- `crates/lib/mimium-lang` the main repository of the development.
- `crates/lib/mimium-test` repository for integration tests. 
- `crates/lib/mimium-web` Wrapper API for webassembly.
- `crates/lib/plugins` Builtin-plugins for mimium
  - `mimium-scheduler` core scheduling functionalities for `@` operator.
  - `mimium-midi` simple rmidi binding
  - `mimium-symphonia` Sampler implememntation using symphonia decoder
  - `mimium-guitools` GUI application tools for `Slider` and `Probe` macro
- `crates/bin/mimium-bintools` Wrapping 3 binary crates to distribute in one folder
 - `crates/bin/mimium-cli` A main frontend of mimium compiler.
 - `crates/bin/mimium-fmt` A code formatter(experimental, not compatible with handling code trivia such as comments)
 - `crates/bin/mimium-language-server` A language server for IDE, compatible with semantic highlighting and error reporting.
- `examples` basic examples of mimium codes.
- `lib` core mimium libraries that will be bundled with main binaries.

## Best practices of writing Rust

- Use iterator and functional method chains as possible. Try to use `.map()` instead using for loops, even if there' will be some cloning cost on iteration.
- Avoid using mutable variable as possible, even if it is local temporary variable.

- Use `cargo fmt` before commiting.
- Fix small warnings by utilizing `cargo clippy`.

- Write documentation comments `///` as possible for public API and the top of the file.

## Test Strategy

- If you are running on Linux, you need to install `libasound2-dev` from apt to compile codes.
- Integrated test files are in `crate/lib/mimium-test/integration_test`.
   - You can define new test by adding files in `mmm` directory, and add expected test result in `integration_test.rs`. Test result is the return value sequence of `dsp` function.
