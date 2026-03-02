# Implementation Plan: Bracket/Escape to Code-Generation Combinators — Eliminating interpreter.rs

## Background and Motivation

The current mimium macro system (stage-0 evaluation) uses a tree-walk interpreter (`interpreter.rs`, ~1011 lines) to evaluate bracket/escape expressions. This is fundamentally slow because:

1. Every AST node is dispatched through `eval_expr` match arms at runtime
2. The `rebuild` function traverses and reconstructs the entire AST for code construction
3. Complex patterns (e.g., mininotation parser, pattern library) involve deep recursive array operations at stage-0, all interpreted node-by-node
4. `Value` ↔ `ExprNodeId` conversions (via `TryInto`) add overhead for every splice

The goal is to **unify the execution backend** by compiling stage-0 code to VM bytecode (or WASM), eliminating the interpreter entirely. The approach is based on the MetaOCaml technique described in [arXiv:2309.08207](https://arxiv.org/pdf/2309.08207): translating bracket/escape into **code-generation combinators** during compilation.

## Core Idea

Instead of:
1. Wrapping the whole program in `Bracket`
2. Running a tree-walk interpreter that builds `Value::Code(ast)` via `rebuild`
3. Unwrapping `Value::Code` back to AST via `expand_macro_rec`

We will:
1. Translate bracket/escape into calls to **code-generation combinator functions** during/after type checking
2. These combinators produce MIR instructions (or a serialized IR representation) when executed
3. Stage-0 code (including the combinators) is compiled to VM bytecode and executed on the VM
4. The VM execution produces the MIR for stage-1 code (instead of producing `Value::Code(ast)`)

## Key Design Decisions

### What do code-generation combinators produce?

**Option A: Combinators produce AST (ExprNodeId)**
- Combinators are compiled functions that build AST nodes when executed on the VM
- After VM execution, the resulting AST goes through the normal pipeline (MIR gen → bytecode gen)
- Pros: Minimal changes to existing pipeline; AST is already well-understood
- Cons: Still requires two compilation passes (AST → MIR → bytecode twice)

**Option B: Combinators produce MIR directly**
- Combinators build MIR instructions when executed
- After VM execution, the MIR is ready for bytecode generation
- Pros: Skips one compilation pass
- Cons: MIR is harder to compose; requires careful SSA register management in combinators

**Option C: Combinators produce a simplified IR ("CodeIR")**
- Define a new lightweight intermediate representation specifically for code construction
- Combinators produce CodeIR, which is then lowered to MIR
- Pros: Clean separation, purpose-built for composition
- Cons: Another IR to maintain

**Recommended: Option A (AST-producing combinators)**

This is the most pragmatic choice and closest to the MetaOCaml approach. The performance bottleneck is the *evaluation* of stage-0 code (array operations, pattern matching, recursion), not the final AST→MIR compilation. By executing stage-0 code on the VM, we get the speed improvement where it matters most. The final AST→MIR pass is a one-time cost and already fast.

### How are combinators represented in the type system?

The `Code(T)` type already exists. After translation, a value of type `Code(T)` becomes a runtime value in the VM that holds a representation of an AST fragment of type `T`.

In the VM, `Value::Code(ExprNodeId)` can be represented as a **boxed heap value** containing a serialized AST fragment. The combinators are ordinary VM functions that allocate and compose these heap values.

## Phase-by-Phase Implementation Plan

### Phase 0: Preparation and Infrastructure (Non-breaking)

#### 0.1: Define `CodeValue` runtime representation

Create a new runtime value type for code fragments that can live in the VM's heap:

```rust
// In runtime/vm/mod.rs or a new file runtime/codegen_combinator.rs
/// A runtime representation of a code fragment (AST) that lives in the VM heap.
/// This replaces `interpreter::Value::Code(ExprNodeId)` in the new system.
pub enum CodeValue {
    Literal(Literal),           // number, string literals
    Var(Symbol),                // variable reference
    Apply(Box<CodeValue>, Vec<CodeValue>),
    Lambda(Vec<Symbol>, Option<TypeNodeId>, Box<CodeValue>),
    Let(Symbol, Box<CodeValue>, Box<CodeValue>),
    LetRec(Symbol, Box<CodeValue>, Box<CodeValue>),
    If(Box<CodeValue>, Box<CodeValue>, Option<Box<CodeValue>>),
    Tuple(Vec<CodeValue>),
    Array(Vec<CodeValue>),
    Record(Vec<(Symbol, CodeValue)>),
    Block(Vec<CodeValue>),
    // ... mirror relevant Expr variants
}
```

Or alternatively, reuse `ExprNodeId` directly by making the interned AST storage accessible from the VM. This is simpler but requires the AST interner to be thread-safe or passed into the VM.

**Recommended**: Reuse `ExprNodeId` directly. The AST interner is already global (uses `SessionGlobals`). The VM can create new `ExprNodeId` values via combinator functions.

#### 0.2: Register AST construction functions as VM external functions

Create a set of external functions that build AST nodes:

```rust
// New file: src/plugin/codegen_combinators.rs

/// code_lit_f(x: float) -> Code(float)
/// Wraps a float literal into a Code value
fn code_lit_f(machine: &mut Machine) -> ReturnCode { ... }

/// code_lit_s(x: string) -> Code(string)
fn code_lit_s(machine: &mut Machine) -> ReturnCode { ... }

/// code_var(name: string) -> Code(a)
fn code_var(machine: &mut Machine) -> ReturnCode { ... }

/// code_app(f: Code(a->b), args: [Code(a)]) -> Code(b)
fn code_app(machine: &mut Machine) -> ReturnCode { ... }

/// code_lam(param_names: [string], body_fn: (Code(a) -> Code(b))) -> Code(a->b)
/// This is the key combinator — uses HOAS (Higher-Order Abstract Syntax)
fn code_lam(machine: &mut Machine) -> ReturnCode { ... }

/// code_let(name: string, val: Code(a), body_fn: (Code(a) -> Code(b))) -> Code(b)
fn code_let(machine: &mut Machine) -> ReturnCode { ... }

/// code_if(cond: Code(bool), then: Code(a), else: Code(a)) -> Code(a)
fn code_if(machine: &mut Machine) -> ReturnCode { ... }

/// code_tuple(elems: [Code(...)]) -> Code((...))
fn code_tuple(machine: &mut Machine) -> ReturnCode { ... }

/// code_array(elems: [Code(a)]) -> Code([a])
fn code_array(machine: &mut Machine) -> ReturnCode { ... }

/// code_then(a: Code(a), b: Code(b)) -> Code(b)
fn code_then(machine: &mut Machine) -> ReturnCode { ... }

/// code_assign(lhs: Code(a), rhs: Code(a)) -> Code(())
fn code_assign(machine: &mut Machine) -> ReturnCode { ... }

/// code_proj(val: Code((a,b,...)), idx: int) -> Code(a)
fn code_proj(machine: &mut Machine) -> ReturnCode { ... }

/// code_record(fields: [(string, Code(...))]) -> Code({...})
fn code_record(machine: &mut Machine) -> ReturnCode { ... }

/// code_field_access(val: Code({...}), field: string) -> Code(a)
fn code_field_access(machine: &mut Machine) -> ReturnCode { ... }

/// code_array_access(arr: Code([a]), idx: Code(int)) -> Code(a)
fn code_array_access(machine: &mut Machine) -> ReturnCode { ... }

/// code_feed(name: string, body_fn: (Code(a) -> Code(a))) -> Code(a)
fn code_feed(machine: &mut Machine) -> ReturnCode { ... }

/// code_match(scrutinee: Code(a), arms: [...]) -> Code(b)
fn code_match(machine: &mut Machine) -> ReturnCode { ... }
```

**Key point about `code_lam` (HOAS approach):**

In MetaOCaml's approach:
```
.<fun y -> .~x * y + 1>.
```
becomes:
```
build_fun "y" (fun y -> mkApp "*" [x; y] |> mkApp "+" [_; mkConst 1])
```

The function body is a **host-language closure** that receives the generated variable as a `Code` value. This is the HOAS trick. In mimium's VM, this means `code_lam` receives a **VM closure** as its body argument. When called, it:
1. Creates a fresh variable name/symbol
2. Creates a `Code` value representing that variable (`Expr::Var`)
3. Calls the VM closure with this `Code` value
4. Gets back a `Code` value for the body
5. Wraps everything in `Expr::Lambda`

**Representation in VM:**
- `Code` values are stored as **boxed heap values** (using the existing `BoxAlloc`/`BoxLoad` mechanism or a new dedicated storage)
- Each `Code` value is essentially a `RawVal` that is an index into a side table of `ExprNodeId` values

#### 0.3: Extend VM heap to store `ExprNodeId`

Add a new storage area in the VM `Machine` for code values:

```rust
// In runtime/vm.rs, Machine struct:
pub struct Machine {
    // ... existing fields ...
    /// Storage for code values (AST fragments) produced by codegen combinators
    code_values: Vec<ExprNodeId>,
}

impl Machine {
    pub fn alloc_code(&mut self, expr: ExprNodeId) -> RawVal {
        let idx = self.code_values.len();
        self.code_values.push(expr);
        RawVal::from(idx as u64 | CODE_TAG)  // use a tag to distinguish from other values
    }

    pub fn get_code(&self, val: RawVal) -> ExprNodeId {
        let idx = (val.to_u64() & !CODE_TAG) as usize;
        self.code_values[idx]
    }
}
```

Alternatively, serialize `ExprNodeId` as a plain `u64` (since `ExprNodeId` is already an interned index). This might be simpler — `ExprNodeId` is `Copy` and can be transmuted to/from `u64`.

### Phase 1: Bracket/Escape Translation Pass (The Core Change)

#### 1.1: New compiler pass — `translate_staging`

Create a new AST-to-AST transformation pass that runs **after type checking** (which validates staging correctness) but **before MIR generation**. This pass translates `Bracket`/`Escape` into calls to codegen combinators.

Location: `src/compiler/translate_staging.rs`

The translation rules (for 2-stage system, stage 0 and stage 1):

**Rule 1: Bracket at stage 0**
```
Bracket(e)  →  translate_to_combinators(e)
```
Where `translate_to_combinators` converts each syntactic form inside the bracket into combinator calls.

**Rule 2: Escape inside Bracket**
```
Inside translate_to_combinators:
  Escape(e)  →  e    (the expression is already at stage 0, evaluate directly)
```

**Rule 3: Literals inside Bracket**
```
Inside translate_to_combinators:
  Literal(lit)  →  code_lit_f(lit)   or   code_lit_s(lit)
```

**Rule 4: Variables inside Bracket**

Two cases:
- **Stage-0 variable** (bound at stage 0 or persistent): This is a cross-stage persistent value. Needs `lift`:
  ```
  x  →  lift(x)
  ```
- **Stage-1 variable** (bound by a lambda/let inside the bracket): Already a `Code` value:
  ```
  x  →  x    (it's already Code(T) from the enclosing code_lam/code_let)
  ```

**Rule 5: Lambda inside Bracket**
```
Inside translate_to_combinators:
  Lambda([p1, p2, ...], body)  →
    code_lam(fun (p1_code, p2_code, ...) ->
      translate_to_combinators(body[p1 → p1_code, p2 → p2_code, ...]))
```

**Rule 6: Application inside Bracket**
```
Inside translate_to_combinators:
  Apply(f, [a1, a2, ...])  →
    code_app(translate_to_combinators(f),
             [translate_to_combinators(a1), ...])
```

**Rule 7: Let inside Bracket**
```
Inside translate_to_combinators:
  Let(x, val, body)  →
    code_let("x", translate_to_combinators(val),
             fun (x_code) -> translate_to_combinators(body[x → x_code]))
```

**Rule 8: If inside Bracket**
```
Inside translate_to_combinators:
  If(cond, then, else)  →
    code_if(translate_to_combinators(cond),
            translate_to_combinators(then),
            translate_to_combinators(else))
```

**Rule 9: Then/Block inside Bracket**
```
Inside translate_to_combinators:
  Then(a, b)  →
    code_then(translate_to_combinators(a),
              translate_to_combinators(b))
```

And similarly for all other expression forms (`Tuple`, `Proj`, `Match`, `Feed`, `ArrayLiteral`, `RecordLiteral`, etc.).

#### 1.2: Handle variable staging in the translation

The translator needs to track which variables are **stage-0 bound** (cross-stage persistent, need lifting) vs **stage-1 bound** (introduced by `code_lam`/`code_let`, already `Code` values).

```rust
struct StagingTranslator {
    /// Variables that are code values (bound inside brackets by code_lam/code_let)
    code_variables: HashSet<Symbol>,
}

impl StagingTranslator {
    fn translate(&mut self, expr: ExprNodeId) -> ExprNodeId {
        match expr.to_expr() {
            Expr::Var(name) => {
                if self.code_variables.contains(&name) {
                    expr  // already a Code value
                } else {
                    // CSP: lift the stage-0 value
                    make_apply(make_var("lift"), vec![expr])
                }
            }
            Expr::Lambda(params, ret_ty, body) => {
                // Add params to code_variables
                for p in params { self.code_variables.insert(p.name); }
                let translated_body = self.translate(body);
                // Remove params
                for p in params { self.code_variables.remove(&p.name); }
                make_apply(make_var("code_lam"),
                    vec![make_lambda(params_as_code_type, translated_body)])
            }
            // ... etc.
        }
    }
}
```

#### 1.3: Handle `MacroExpand` desugaring

Currently `MacroExpand(f, args)` desugars to `Escape(Apply(f, args))`. This should continue to work naturally:

```
MacroExpand(f, args)
  → Escape(Apply(f, args))    // existing desugaring
  → Apply(f, args)            // escape inside bracket = evaluate at stage 0
```

This means macro functions that return `Value::Code(...)` now need to return a `Code`-typed value that the VM can handle. Since we're using `ExprNodeId` as the code representation (stored as a tagged `u64` in the VM), the existing `MacroFunction` implementations (Slider, Probe, etc.) need to be adapted to work as VM external functions.

### Phase 2: Adapt Existing Macro Functions

#### 2.1: Convert `MacroFunction` to `MachineFunction`

Currently, macro functions like `Slider`, `Probe`, `lift` are `MacroFunction` implementations that work with `interpreter::Value`. They need to become `MachineFunction` implementations that work with `RawVal` on the VM.

For each existing `MacroFunction`:

**`lift`**: Simplifies to: take a `RawVal`, wrap it in an `ExprNodeId` literal, store in code_values, return the code index.

**`Slider`**: Currently creates a GUI slider at macro-time and returns `Value::Code(Expr::Apply("__get_slider", [idx]))`. In the new system, it creates the GUI slider (side effect during VM execution), then constructs `ExprNodeId` for the apply expression and returns it as a code value.

**`Probe`**: Similar — creates ring buffer, returns code for the intercept function.

#### 2.2: Adapt `CommonFunction` 

`CommonFunction` already has both macro and VM implementations. In the new system, only the VM implementation is needed. The macro implementation becomes redundant since stage-0 code also runs on the VM.

However, array operations (`map`, `split_head`, `split_tail`, `prepend`, `length_array`, etc.) that currently have `CommonFunction` implementations will "just work" — they're already VM functions.

### Phase 3: Two-Pass Compilation

#### 3.1: Split compilation into two phases

The new compilation pipeline:

```
Source → Parse → AST
  → Type Check (with staging) → Typed AST with Code types
  → translate_staging → AST without Bracket/Escape (pure stage-0 code that uses combinators)
  → MIR Generation (stage-0)
  → Bytecode Generation (stage-0)
  → VM Execution (stage-0) → produces ExprNodeId (the stage-1 AST)
  → MIR Generation (stage-1, on the produced AST)
  → Bytecode Generation (stage-1)
  → VM/WASM Runtime (stage-1)
```

This is a **two-pass compilation**: the first pass compiles and executes stage-0 code, which produces stage-1 AST, which is then compiled in the second pass.

The key change in `compiler.rs` / `mirgen.rs`:

```rust
pub fn compile_with_module_info(
    root_expr_id: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    macro_env: &[Box<dyn MacroFunction>],
    file_path: Option<PathBuf>,
    module_info: ModuleInfo,
) -> Result<Mir, Vec<Box<dyn ReportableError>>> {
    let expr = root_expr_id.wrap_to_staged_expr();

    // Step 1: Type check
    let (expr, mut infer_ctx, errors) =
        typecheck_with_module_info(expr, builtin_types, file_path.clone(), module_info);
    if !errors.is_empty() { return Err(errors); }

    let top_type = infer_ctx.infer_type(expr).unwrap();

    // Step 2: Translate brackets/escapes to combinators (NEW)
    let stage0_expr = translate_staging::translate(expr, top_type, &infer_ctx);

    // Step 3: Compile stage-0 code to bytecode
    let stage0_mir = mirgen::compile_stage0(stage0_expr, &infer_ctx)?;
    let stage0_program = bytecodegen::gen_bytecode(stage0_mir);

    // Step 4: Execute stage-0 code on VM → get stage-1 AST
    let mut vm = Machine::new(stage0_program, macro_env_as_ext_funs);
    let result = vm.execute_main();
    let stage1_ast = vm.get_code(result);  // extract the ExprNodeId from the result

    // Step 5: Compile stage-1 code normally
    let stage1_mir = mirgen::compile(stage1_ast, &infer_ctx)?;
    Ok(stage1_mir)
}
```

### Phase 4: Handle Edge Cases and Special Patterns

#### 4.1: `#stage(macro)` / `#stage(main)` transitions

Currently, `#stage(macro)` → `#stage(main)` inserts `Bracket`, and the reverse inserts `Escape`. After the translation pass, these become combinator calls at the boundary. This should continue to work because the translation operates on the AST that already has the `Bracket`/`Escape` nodes inserted.

#### 4.2: Nested macros and recursive staging

The `run()` function in `lib/pattern.mmm` is a macro function that itself creates brackets with escapes. This pattern works because:
1. The function is defined at stage 0
2. When called inside a bracket (via `MacroExpand`), the escape evaluates it at stage 0
3. The function returns `Value::Code(...)` which gets spliced in

In the new system:
1. `run()` is compiled to VM bytecode as a stage-0 function  
2. It uses codegen combinators internally to build stage-1 code
3. When called, it returns a code value (ExprNodeId) that gets composed with the surrounding combinators

#### 4.3: `MacroExpand(f, args)` with external macro functions (Slider, Probe)

These are function calls at stage 0 that return `Code` values. In the new system, they're just VM external function calls that return code values. The translation naturally handles this:

```
Inside translate_to_combinators:
  Escape(Apply(Slider, [name, min, max, init]))
  → Apply(Slider, [name, min, max, init])   // escape removes bracket context
```

Since `Slider` is a function `(string, float, float, float) -> Code(float)`, calling it at stage 0 returns a code value. The combinator translation doesn't touch the `Apply(Slider, ...)` because it's an escape — it evaluates at stage 0.

#### 4.4: `lift` becomes implicit for CSP

In the current system, users explicitly write `$(value |> lift)` to embed stage-0 values in stage-1 code. In the combinator approach, CSP handling can be made **automatic**: every stage-0 variable reference inside a bracket is automatically lifted.

However, we should keep `lift` as an explicit function for backward compatibility and clarity. The translator can insert `lift` calls where needed:

```rust
// In translate_to_combinators:
Expr::Var(name) if is_stage0_variable(name) => {
    make_apply(make_var("lift"), vec![expr])
}
```

#### 4.5: Pattern matching inside brackets

`code_match` is complex because it involves patterns. The combinator needs to construct `ExprNodeId` for match expressions:

```rust
fn code_match(machine: &mut Machine) -> ReturnCode {
    let scrutinee_code = machine.get_code(machine.get_arg(0));
    let arms = machine.get_array(machine.get_arg(1));
    // Each arm is a (pattern_code, body_code) pair
    let expr = Expr::Match(scrutinee_code, arms_as_expr).into_id();
    machine.set_return(machine.alloc_code(expr));
    ReturnCode::Continue
}
```

Since patterns in mimium's AST are separate from expressions, the combinator must handle pattern construction too (or patterns can be passed through as data).

#### 4.6: Feed/Self/Delay inside brackets

These stateful constructs need dedicated combinators:
- `code_feed(name, body_fn)`: Constructs `Expr::Feed`
- Direct `self` references: Translated to `code_var("self")` inside feed context
- `delay(x, n)`: Normal function application, handled by `code_app`

### Phase 5: Remove interpreter.rs

#### 5.1: Remove interpreter module

Once all tests pass with the new combinator-based approach:
1. Delete `src/interpreter.rs`  
2. Remove `interpreter::expand_macro` call from `mirgen.rs`
3. Remove `interpreter::Value` enum
4. Remove `MacroFunction` trait (replace with `MachineFunction` for all macro-time functions)
5. Remove `MacroFunType` and related types from `plugin.rs`
6. Clean up `Context<V>` generic parameter (was needed for interpreter values)

#### 5.2: Simplify plugin API

The `Plugin` trait can be simplified:
- `get_macro_functions()` → removed
- `get_ext_closures()` → now handles both stage-0 and stage-1 functions
- The distinction between `MacroFunction` and `MachineFunction` disappears — everything is a `MachineFunction`

#### 5.3: Clean up type system

- `EvalStage` may still be needed for the type checker but the interpreter-specific usage is removed
- `Value::Code(ExprNodeId)` in the interpreter is replaced by the VM's code value storage

### Phase 6: Optimize (Optional)

#### 6.1: Direct MIR generation (skip AST)

Instead of combinators producing `ExprNodeId`, they could produce MIR directly. This eliminates the second AST→MIR pass. However, this is significantly more complex and should only be done if profiling shows the second compilation pass is a bottleneck.

#### 6.2: Incremental compilation

With VM-based stage-0 execution, we can cache the stage-0 bytecode for unchanged macro code and only re-execute when macro definitions change.

#### 6.3: Use WASM for stage-0 too

Since WASM uses Cranelift JIT, stage-0 execution could potentially be even faster than the custom VM. The infrastructure is already there.

## Migration Strategy

### Step 1: Parallel Implementation (Weeks 1-3)
- Implement `codegen_combinators.rs` (Phase 0)
- Implement `translate_staging.rs` (Phase 1)
- Add the two-pass compilation path as an **alternative** behind a feature flag
- Run existing tests against both paths

### Step 2: Feature Parity (Weeks 3-5)
- Ensure all integration tests pass with the new path
- Adapt plugin macros (Slider, Probe, etc.) to work as VM functions
- Handle edge cases (nested staging, match, feed, etc.)

### Step 3: Switchover (Week 6)
- Make the new path the default
- Run full test suite with `--test-threads=1`
- Test both VM and WASM backends

### Step 4: Cleanup (Week 7)
- Remove `interpreter.rs`
- Simplify plugin API
- Update documentation

## Risk Analysis

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| HOAS code_lam causes scoping issues | Medium | High | Careful alpha-renaming with fresh symbols; test with nested lambdas |
| VM bootstrapping circular dependency | Low | High | Stage-0 VM only needs basic ops (no audio, no state); modular VM init |
| Performance regression in code construction | Low | Medium | Profile before/after; AST interner is fast |
| Breaking existing .mmm code | Medium | Medium | Keep `lift` API, auto-insert CSP lifting |
| Pattern matching complexity in combinators | Medium | Medium | Reuse existing pattern AST construction; don't decompose patterns |
| Plugin compatibility (Slider, Probe) | Medium | High | Adapter layer wrapping old MacroFunType as MachineFunction |

## Files to Create/Modify

### New Files
- `src/compiler/translate_staging.rs` — The bracket/escape → combinator translation
- `src/plugin/codegen_combinators.rs` — VM external functions for AST construction

### Modified Files
- `src/compiler.rs` — Two-pass compilation pipeline
- `src/compiler/mirgen.rs` — Remove `expand_macro` call, add stage-0 compilation path
- `src/runtime/vm.rs` — Add `code_values` storage, related methods
- `src/plugin.rs` — Simplify traits (eventually remove `MacroFunction`)
- `src/plugin/builtin_functins.rs` — Rewrite `lift` as a VM function
- `src/plugin/system_plugin.rs` — Adapt Slider/Probe to MachineFunction
- `crates/lib/plugins/mimium-guitools/src/lib.rs` — Adapt macro implementations

### Deleted Files (Phase 5)
- `src/interpreter.rs` — The entire tree-walk interpreter
