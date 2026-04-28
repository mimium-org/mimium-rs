# Rust Codegen Current Status

This document summarizes the current state of the backend that emits Rust source from mimium MIR.

## Overview

- The Rust backend lowers from MIR, not Expr.
- The public entry point is `mimium_lang::compiler::Context::emit_rust`.
- It is available from the CLI via `mimium-cli --emit-rust`.
- The generator implementation lives in `crates/lib/mimium-lang/src/compiler/rustgen.rs`.
- The fixed runtime scaffold is loaded from `crates/lib/mimium-lang/src/compiler/mimium_placeholder.rs.template` via `include_str!`, and the generator injects the variable sections around it.
- Generated Rust now uses MIR-derived function names where possible instead of anonymous `func_N` names.

## Generation Strategy

- MIR basic blocks are lowered directly into a Rust state machine using `loop + match bb`.
- The backend preserves MIR-oriented control flow instead of reconstructing higher-level syntax.
- `CodeWriter` manages indentation structurally instead of embedding indentation directly into ad-hoc strings.
- The backend can write either to `String` or to any `std::io::Write` via `generate_to`.
- Generated arguments, locals, and most transient values are materialized as fixed-size word arrays.

## Embedded Runtime Model

The emitted Rust includes a small runtime that is sufficient to execute generated programs without depending on the VM implementation.

- `Word = u64` value representation
- conversion helpers for `f64` and `i64`
- `MimiumHost` and `PanicHost`
- host hooks for `current_time` and `sample_rate`
- `StateStorage`
  - feed and `self` state
  - `mem`
  - `delay`
- `MemoryStore`
  - aggregate temporary allocation
  - pointer-like `get_element`
  - `load` / `store`
  - shared within a single generated call chain so nested calls can see the same aggregate temporaries
- `ArrayStorage`
  - allocation and element access for array values
- `ClosureStorage`
  - heap-allocated closures
  - upvalue storage
  - closure-local state storage
- `MimiumProgram`
  - `call_dsp`
  - `call_main`
  - function-handle dispatch
  - closure-handle dispatch
  - self-managed access to current state storage, current closure, and runtime memory

## MIR Coverage

For ordinary supported mimium programs, the Rust backend now covers the MIR that the current frontend emits in practice.

Implemented categories include:

- Constants
  - `Uinteger`
  - `Integer`
  - `Float`
  - `String`
- Memory and aggregate operations
  - `Alloc`
  - `Load`
  - `Store`
  - `GetElement`
- Calls and closures
  - `Call`
  - direct function calls
  - static default-argument expansion through synthesized `__default_*` helpers
  - external function calls through the generated host
  - function-handle dispatch
  - `CallIndirect`
  - `MakeClosure`
  - `CloseHeapClosure`
  - `CloneHeap`
  - `GetUpValue`
  - `SetUpValue`
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
- Tagged unions and user sums
  - `TaggedUnionWrap`
  - `TaggedUnionGetTag`
  - `TaggedUnionGetValue`
  - `CloneUserSum`
  - `ReleaseUserSum`

## Current Transitional Behavior

Some behavior is still intentionally transitional because the ABI and runtime representation are not fully finalized yet.

- `call_main` is exposed not only when a literal `main` function exists, but also when only a global-initialization function exists, by treating the first function as the initialization entry point.
- Function dispatch accepts not only tagged handles but also raw function indices.
- `MemoryStore::load` still includes a fallback path for reading a 1-word immediate value, to match cases where a scalar reaches `Load` directly in MIR.
- Aggregate temporaries still flow through `MemoryStore`; the current Rust backend keeps that storage scoped to a single generated call chain instead of treating it as fully program-global state.
- Missing default arguments are currently synthesized only when the callee can be resolved statically during Rust generation.
- Transitions from `JmpIf` and `Switch` arms into merge blocks are supplemented on the Rust side to account for implicit MIR fallthrough.

These behaviors exist to keep the backend practical while the representation and calling convention continue to settle.

## What Has Been Verified

At this point, the following has been verified.

- The generator emits Rust source containing `MimiumProgram`, `call_dsp`, and MIR-derived generated function names.
- The generated Rust compiles with plain `rustc --edition=2024`.
- The generated Rust binary executes successfully.
- Dedicated compile-and-run tests cover generated programs with globals, arrays, branches, user functions, `self`, `mem`, runtime literals, indirect calls, and mutable upvalues.
- Annotated fixture programs are executed through Rust code generation in the regression suite.
- CI runs the Rust codegen compile-and-run tests as part of the normal cargo test workflow.

Relevant tests currently live in:

- `crates/lib/mimium-lang/src/compiler.rs`
- `crates/lib/mimium-test/tests/rust_codegen_test.rs`

Representative tests include:

- `emit_rust_generates_program_scaffold`
- `emit_rust_compiles_and_runs_generated_program`
- `emit_rust_supports_runtime_literals_from_host`
- `emit_rust_supports_indirect_calls_via_no_capture_closures`
- `emit_rust_supports_mutable_upvalues`
- `run_all_annotated_fixtures_via_rust_codegen`

## Practical Unsupported Area

The main user-visible unsupported area is no longer ordinary MIR lowering. The remaining practical gap is plugin-backed functionality.

In particular, Rust codegen does not yet provide runtime host integration for plugin-dependent macros and externals such as:

- `Control`
- `Probe`

These features depend on plugin-side runtime wiring, and the generated Rust host currently does not provide that integration. In the current Rust codegen regression suite, plugin-backed fixtures are explicitly skipped for this reason.

This means:

- ordinary core-language Rust emission is broadly covered
- generated Rust can be compiled and executed for a large supported subset of real programs
- plugin-driven GUI and interception features are still outside the supported subset
- `Control!` and `Probe!` style programs should still be documented as unsupported in the Rust backend

## Known Limitations

- The generated Rust host is intentionally minimal and does not yet load or emulate the plugin runtime environment.
- Aggregate arguments and returns still use a transitional runtime representation internally, even though the external generated ABI is much cleaner than the initial version.
- Default arguments are only expanded for statically resolvable calls.
- The Rust backend is now practical for a broad subset of the language, but it is still a dedicated alternate backend rather than a full replacement for the VM and plugin runtime stack.

## Notes on Legacy MIR Variants

The MIR enum still contains a small number of legacy or backend-specific variants that are not the main practical limitation described above. The important user-facing status is that the Rust backend now covers the MIR exercised by the currently supported Rust-codegen test and fixture paths, while plugin-backed features such as `Control` and `Probe` remain unsupported.

## Next Areas to Tackle

The highest-priority remaining items are the following.

1. Add host/runtime integration for plugin-backed features such as `Control` and `Probe`
2. Continue cleaning up aggregate-value representation and calling conventions
3. Expand compile-and-run coverage using more real examples, especially examples that currently rely on plugin infrastructure
