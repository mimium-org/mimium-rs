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
- `crates/bin/mimium-bintools` Wrapping 3 binary crates to distribute in one folder. If you need to test those binaries, you need to rebuild this crate.
 - `crates/bin/mimium-cli` A main frontend of mimium compiler.
 - `crates/bin/mimium-fmt` A code formatter.
 - `crates/bin/mimium-language-server` A language server for IDE, compatible with semantic highlighting and error reporting.
- `examples` basic examples of mimium codes.
- `lib` core mimium libraries that will be bundled with main binaries.

## Best practices for general works

- If you need to create temporary files for testing, make it in `tmp` directory in the repository, which will be ignored from git. Do not try to write to system's `/tmp` or `/dev/null` which will require additional approval by author.

## Best practices of writing Rust

- Use iterator and functional method chains as possible. Try to use `.map()` instead using for loops, even if there' will be some cloning cost on iteration.
- Avoid using mutable variable as possible, even if it is local temporary variable.

- Use `cargo fmt` before commiting.
- Fix small warnings by utilizing `cargo clippy`.
- Write documentation comments `///`  for public API and `//!` the top of the file as possible.
- Write comments in English even if you asked in different languages.
- Do not use much emoji in the comments.
- Do not write too much comments when the behavior is apparent from the code.
- Do not leave the comment for just tracking history of changes. Always leave comment for those whom read the codes first time.
- You should use `debug_assert!` instead `assert!` for debugging purpose

## Test Strategy

- If you are running on Linux, you need to install `libasound2-dev` from apt to compile codes.
- Integrated test files are in `crate/lib/mimium-test/integration_test`.
   - You can define new test by adding files in `mmm` directory, and add expected test result in `integration_test.rs`. Test result is the return value sequence of `dsp` function.
