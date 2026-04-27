# Rust Codegen Current Status

This document summarizes the current implementation status of the backend that generates Rust source code from mimium MIR.

## Overview

- The input to the backend is MIR, not Expr.
- The public entry point is `mimium_lang::compiler::Context::emit_rust`.
- It is available from the CLI via `mimium-cli --emit-rust`.
- The generator implementation lives in `crates/lib/mimium-lang/src/compiler/rustgen.rs`.
- The fixed portion of the generated Rust is loaded from `crates/lib/mimium-lang/src/compiler/mimium_placeholder.rs.template` via `include_str!`, and only the variable sections are injected.

## Current Generation Strategy

- MIR basic blocks are lowered directly into a Rust state machine using `loop + match bb`.
- The generator does not try to reconstruct higher-level syntax. It preserves backend-oriented control flow as-is.
- Instead of embedding indentation directly in strings, `CodeWriter` manages indentation levels numerically.
- Output is not limited to `String`; the generator also provides `generate_to`, which writes to any `std::io::Write`.

## Implemented Runtime Pieces

The generated Rust includes a small supporting runtime.

- `Word = u64`-based value representation and `f64` / `i64` conversion helpers
- `MimiumHost` plus `PanicHost`, which fails when an external function is not wired up
- `StateStorage`
  - state for `self`
  - `mem`
  - `delay`
- `MemoryStore`
  - `alloc`
  - pointer-like `get_element`
  - `load` / `store`
- `ArrayStorage`
  - allocation, read, and write for fixed-length arrays
- `MimiumProgram`
  - `call_dsp`
  - `call_main`
  - function-handle dispatch

## MIR Instructions Currently Supported

At minimum, the following can currently be lowered to Rust.

- Constants
  - `Uinteger`
  - `Integer`
  - `Float`
- Memory and aggregate-value related
  - `Alloc`
  - `Load`
  - `Store`
  - `GetElement`
- Calls
  - `Call`
  - direct function call
  - ext function call
  - dispatch through function handles
- Globals
  - `GetGlobal`
  - `SetGlobal`
- State
  - `PushStateOffset`
  - `PopStateOffset`
  - `GetState`
  - `ReturnFeed`
  - `Delay`
  - `Mem`
- Control flow
  - `JmpIf`
  - `Jmp`
  - `Switch`
  - `Phi`
  - `PhiSwitch`
  - `Return`
- Numeric operations
  - `AddF`
  - `SubF`
  - `MulF`
  - `DivF`
  - `ModF`
  - `PowF`
  - `NegF`
  - `AbsF`
  - `SinF`
  - `CosF`
  - `LogF`
  - `SqrtF`
  - `AddI`
  - `SubI`
  - `MulI`
  - `DivI`
  - `ModI`
  - `NegI`
  - `AbsI`
- Comparison and boolean logic
  - `Gt`
  - `Ge`
  - `Lt`
  - `Le`
  - `Eq`
  - `Ne`
  - `And`
  - `Or`
  - `Not`
- Casts
  - `CastFtoI`
  - `CastItoF`
  - `CastItoB`
- Arrays
  - `Array`
  - `GetArrayElem`
  - `SetArrayElem`

## Current Transitional Behavior

Some parts still use transitional behavior because the representation has not yet been fully cleaned up.

- `call_main` is exposed not only when a literal `main` function exists, but also when only a global-initialization function exists, by treating the first function as the initialization entry point.
- Function dispatch accepts not only tagged handles but also raw function indices.
- `MemoryStore::load` includes a temporary path for reading a 1-word immediate value, to match cases where a scalar reaches `Load` directly in MIR.
- Transitions from `JmpIf` / `Switch` arms into merge blocks are supplemented on the Rust side to account for implicit MIR fallthrough.

These behaviors exist to get the initial implementation working. Long term, the value representation and calling convention should be cleaned up further.

## What Has Been Verified

At this point, the following has been verified.

- The generator can emit Rust source containing `MimiumProgram`, `call_dsp`, and each `func_N`
- The generated Rust can be compiled with `rustc`
- The generated Rust binary can be executed successfully
- Expected outputs have been verified for a scalar DSP program that includes a global array, `if`, user function calls, `self`, and `mem`
- `cargo check -p mimium-cli` passes

The relevant unit tests currently live in `crates/lib/mimium-lang/src/compiler.rs`.

- `emit_rust_generates_program_scaffold`
- `emit_rust_compiles_and_runs_generated_program`

## What Is Not Implemented Yet

The following are not supported yet. At the moment, they fail explicitly during code generation.

- Closure-related instructions
  - `Closure`
  - `CallCls`
  - `MakeClosure`
  - `CloseUpValues`
  - `CloseHeapClosure`
  - `CloneHeap`
  - `GetUpValue`
  - `SetUpValue`
- Indirect calls
  - `CallIndirect`
- Strings
  - `String`
- Tagged union / sum-related instructions
  - `TaggedUnionWrap`
  - `TaggedUnionGetTag`
  - `TaggedUnionGetValue`
  - `CloneUserSum`
  - `ReleaseUserSum`
- Box-related instructions
  - `BoxAlloc`
  - `BoxLoad`
  - `BoxClone`
  - `BoxRelease`
  - `BoxStore`
- Some integer operations
  - `PowI`
  - `LogI`
- Other
  - `Error`

## Known Limitations

- The compile-and-run test is currently biased toward scalar DSP output. Tuple returns and aggregate calling conventions are not fully sorted out yet.
- Normal ext function calls are supported, but cases where a function value ends up as `CallIndirect` are still unsupported.
- This is still an initial Rust backend. It does not yet cover the full language.

## Concrete Unsupported Example

For example, the following CLI invocation currently fails.

```sh
cargo run --bin mimium-cli --release examples/biquad.mmm --emit-rust
```

In this case, the `_mimium_getsamplerate` path appears in MIR as `CallIndirect`, and the backend reports the following error.

```text
instruction CallIndirect(...) is not supported by the initial Rust backend
```

In other words, practical examples that include sample-rate access or closure-like execution paths are still outside the current Rust backend's supported subset.

## Next Areas to Tackle

The highest-priority remaining items are the following.

1. Implement closure support and `CallIndirect`
2. Clean up aggregate-value representation
3. Clean up calling conventions for tuple returns and aggregate arguments
4. Implement boxed values and tagged unions
5. Expand compile-and-run tests based on real examples