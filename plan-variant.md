# mimium Variant type implementation

This document for agent.

we are gonna implement variant (enum / sum) type for mimium.

The plan should be separated into the following steps.

---

## Implementation Plan Overview

### Phase Dependencies

```
Phase 1: Integer Match     (foundation)  ✅ COMPLETED
    ↓
Phase 2: Sum Type `A|B`    (type system extension)  ✅ COMPLETED
    ↓
Phase 3: Type Declaration  (custom types)  ✅ COMPLETED
    ↓
Phase 4: Constructor w/ values  ← NEXT
    ↓
Phase 5-6: Complex patterns
```

### Key Files to Modify (by phase)

| Phase | Parser | Type System | MIR | VM/Bytecode |
|-------|--------|-------------|-----|-------------|
| 1 ✅ | `green.rs`, `cst_parser.rs`, `lower.rs` | `typing.rs` | `mirgen.rs` | `bytecodegen.rs`, `vm.rs` |
| 2 ✅ | (type annotation parser) | `types/mod.rs`, `typing.rs`, `unification.rs` | `mirgen.rs` | `bytecodegen.rs`, `vm.rs` |
| 3 ✅ | `cst_parser.rs` (type decl) | `typing.rs`, `unification.rs` | `mirgen.rs` | - |
| 4+ | pattern parser | pattern typing | pattern MIR | - |

### Current Test Status (2026-02-05)

- `match_int`: ✅ PASSED (integer literal matching with wildcard default)
- `sum_type_basic`: ✅ PASSED (Union type with constructor patterns)
- `enum_basic`: ✅ PASSED (User-defined enum with constructor patterns)
- All other tests (96 integration + 7 scheduler): ✅ PASSED
- `multistage` tests: ⏳ Skipped (pre-existing hang issue, unrelated to variant types)

### Phase 1 Implementation Summary ✅

**Completed: 2026-02-05**

#### Implementation Details

1. **Parser**: Added `match` keyword, `=>` token (FatArrow), `MatchExpr` CST parsing
2. **AST**: Defined `Expr::Match`, `MatchArm`, `MatchPattern` (Literal/Wildcard)
3. **Lower**: CST → AST conversion for match expressions
4. **Typing**: Type inference for match expressions (all arms must have same type)
5. **MIR**: Added `Switch` instruction with cases and `PhiSwitch` for merge blocks
6. **Bytecode**: Added `JmpTable(Reg, u8)` instruction with dense jump table
7. **VM**: O(1) jump table lookup using `clamp` for index bounds

#### Key Data Structures

```rust
// MIR
Instruction::Switch { 
    scrutinee: VPtr, 
    cases: Vec<(i64, Bbindex)>, 
    default_block: Option<Bbindex>,  // None for exhaustive matches
    merge_block: Bbindex 
}
Instruction::PhiSwitch(Vec<VPtr>)

// Bytecode
Instruction::JmpTable(Reg, u8)  // 16 bits: scrutinee register + table index

// VM Program
pub struct JumpTable {
    pub min: i64,           // minimum case value
    pub offsets: Vec<i16>,  // dense array, last element is default
}
```

#### Test Result
- `match_int` test: ✅ PASSED (returns 100.0 + 200.0 + 300.0 + 300.0 = 900.0)

---

## step.1 integer match expression ✅ COMPLETED

First, we add `match` syntax. In the first implementation, it will be match only to the numeric type.

To prevent from mismatch by decimal rounding error, in the match pattern, the number will be rounded into integer.

This test should be passed.

```mimium
fn test(num){
  match num {
    0 => 100,
    1 => 200,
    _ => 300,
  }
}

fn dsp(){
    test(0)+ test(1)+test(2) +test(100) //the result should be 900
}
```

The developer should add 

- parser and formatter for match expression
- type inference for match expression
- add switch operation for MIR
- add jump table mechanism to bytecode and VM

## step 2. primitive sum type ✅ COMPLETED

The variant type with explicit type annotation should be added.

```mimium
fn test(num_or_str: float | string ){
  match num_or_str {
    string(x) => 1,
    float(x) => 2,
  }
}

let x = test("tstring")
let y = test(99)


fn dsp(){
    x+y //the result should be 3
}
```

### Phase 2 Implementation Status ✅ COMPLETED (2026-02-05)

**All features working:**
- Union type parsing (`float | string`)
- Type inference for union types
- Subtype-based coercion: values are automatically wrapped when passed to union type parameters
- Functions with union type parameters compile and execute correctly
- Constructor patterns parse and work correctly (`float(x)`, `string(_)`)
- Tagged union memory layout implemented (tag + value)
- Match expressions with constructor patterns and variable bindings work correctly

**Test Result:**
- `sum_type_basic` test: ✅ PASSED (returns 3.0)

### Phase 2 Implementation Steps

1. **Type System Extension** ✅ COMPLETED
   - ✅ Add `Type::Union(Vec<TypeNodeId>)` variant to `types.rs`
   - ✅ Parser for `A | B` type syntax in type annotations (`parse_type_union`)
   - ✅ `SyntaxKind::UnionType` added to `green.rs`
   - ✅ `lower_type` handles `UnionType` in `lower.rs`
   - ✅ `Display` implementation for union types (formats as `A | B`)
   - ✅ Updated `contains_*` methods for union types
   - ✅ `to_mangled_string` for union types
   - ✅ Subtype relationship in `unification.rs`: `A` is subtype of `A | B`
   - ✅ Union-to-Union unification support

2. **Constructor Pattern Parsing** ✅ COMPLETED
   - ✅ Add `MatchPattern::Constructor(Symbol, Option<Symbol>)` to AST
   - ✅ `SyntaxKind::ConstructorPattern` added to `green.rs`
   - ✅ Parser for `Float(x)`, `String(s)` patterns in `cst_parser.rs`
   - ✅ Support for type keywords as constructor names (float, string, int)
   - ✅ `lower_constructor_pattern` in `lower.rs`
   - ✅ Formatter support in `cst_print.rs`

3. **Type Inference for Constructor Patterns** ✅ COMPLETED
   - ✅ `get_constructor_type_from_union` helper function
   - ✅ `type_constructor_name` maps primitive types to constructor names
   - ✅ Variable binding scope in match arms
   - ✅ Error types: `ConstructorNotInUnion`, `ExpectedUnionType`

4. **MIR Generation** ✅ COMPLETED
   - ✅ `Instruction::TaggedUnionWrap` - wrap value with tag
   - ✅ `Instruction::TaggedUnionGetTag` - extract tag
   - ✅ `Instruction::TaggedUnionGetValue` - extract value
   - ✅ Constructor pattern variable bindings in `eval_union_match`
   - ✅ Subtype-based coercion in `coerce_value()` and `coerce_args_for_call()`
   - ✅ Float-to-int cast for numeric scrutinees in match expressions
   - ✅ `Switch.default_block` changed to `Option<Bbindex>` (None for exhaustive matches)

5. **VM/Bytecode Changes** ✅ COMPLETED
   - ✅ Tagged union memory layout: `[tag (1 word), value (N words)]`
   - ✅ `word_size_for_type(Type::Union)` = `1 + max(variant_sizes)`
   - ✅ Bytecode instructions: `TaggedUnionWrap`, `GetTag`, `GetValue`
   - ✅ VM execution: SetTag, Move/MoveRange for values
   - ✅ `CastFtoI` / `CastItoF` bytecode generation
   - ✅ JmpTable simplified to always read as i64

---

- parser and formatter for sum type with `|` operator should be added.
- Primnitive type constructor(String and Float) pattern should be added.
- Tagged Union representation should be added in MIR and VM.

## step 3. Explicit Constructor pattern without value ← NEXT

```mimium
type MyEnum = One | Two | Three 
fn test(myenum: MyEnum){
  match myenum {
    One => 1,
    Two => 2,
    Three => 3
  }
}

let x = test(One)
let y = test(Two)
let z = test(Three)

fn dsp(){
    x+y+z //the result should be 6
}
```

### Phase 3 Implementation Status ✅ COMPLETED (2026-02-05)

#### Goal
Enable user-defined sum types with explicit constructor names (without payload values).

#### Implementation Steps

1. **Type Declaration Parsing** ✅ COMPLETED
   - Added `SyntaxKind::TypeDecl` and `SyntaxKind::VariantDef` to `green.rs`
   - Added `TokenKind::Type` to `token.rs` and `tokenizer.rs`
   - Parse `type Name = Constructor1 | Constructor2 | ...` in `cst_parser.rs`
   - CST structure: `TypeDecl { name: Ident, variants: Vec<VariantDef> }`
   - Lower to AST in `lower.rs` (`lower_type_decl`, `lower_variant_def`)

2. **AST for Type Declarations** ✅ COMPLETED
   - Added `ProgramStatement::TypeDeclaration { visibility, name, variants }`
   - Added `VariantDef { name: Symbol, payload: Option<TypeNodeId> }`
   - Added `Type::UserSum { name: Symbol, variants: Vec<Symbol> }` to type system
   - Added `TypeDeclarationMap` to `ModuleInfo`

3. **Formatter Support** ✅ COMPLETED
   - Updated `mimium-fmt/src/print.rs` for TypeDeclaration and UserSum
   - Updated `mimium-fmt/src/cst_print.rs` for TypeDecl and VariantDef
   - Updated `mimium-language-server` semantic tokens for Type keyword

4. **Type Environment Extension** ✅ COMPLETED
   - Added `ConstructorInfo` and `ConstructorEnv` types to `typing.rs`
   - Added `constructor_env` field to `InferContext`
   - `register_type_declarations()` method creates UserSum type and registers constructors
   - `infer_root_with_type_decls()` accepts optional type declarations

5. **Constructor as Value** ✅ COMPLETED
   - Constructors without payload are integer tags
   - Type of `One` is `MyEnum` (the UserSum type)
   - In MIR: `Integer(tag_index)` - simple integer value

6. **Pattern Matching Enhancement** ✅ COMPLETED
   - Bare identifiers in patterns treated as constructor patterns
   - Updated `lower_match_pattern` to handle `SyntaxKind::Identifier`
   - `get_constructor_type_from_union` handles UserSum variants
   - `eval_union_match` generates Switch on integer tag values

7. **Type Unification** ✅ COMPLETED
   - Added UserSum type matching in `unification.rs`
   - Same UserSum types unify as Identical

#### Key Files Modified
- `crates/lib/mimium-lang/src/compiler/parser/token.rs` - Type keyword
- `crates/lib/mimium-lang/src/compiler/parser/tokenizer.rs` - type keyword recognition
- `crates/lib/mimium-lang/src/compiler/parser/green.rs` - TypeDecl, VariantDef SyntaxKinds
- `crates/lib/mimium-lang/src/compiler/parser/cst_parser.rs` - parse_type_decl, parse_variant_def
- `crates/lib/mimium-lang/src/compiler/parser/lower.rs` - lower_type_decl, lower_variant_def
- `crates/lib/mimium-lang/src/ast/program.rs` - VariantDef, TypeDeclaration, TypeDeclarationMap
- `crates/lib/mimium-lang/src/types.rs` - Type::UserSum variant
- `crates/lib/mimium-lang/src/compiler/typing.rs` - ConstructorInfo, constructor_env, register_type_declarations
- `crates/lib/mimium-lang/src/compiler/typing/unification.rs` - UserSum unification
- `crates/lib/mimium-lang/src/compiler/mirgen.rs` - constructor handling in eval_rvar, eval_union_match
- `crates/bin/mimium-fmt/src/print.rs` - TypeDeclaration formatting
- `crates/bin/mimium-fmt/src/cst_print.rs` - TypeDecl CST printing
- `crates/bin/mimium-language-server/src/semantic_token.rs` - Type keyword highlighting

#### Test Result
- `enum_basic` test: ✅ PASSED (returns 6.0 = 1 + 2 + 3)
- All other tests (96 integration + 7 scheduler): ✅ PASSED
- `multistage` tests: ⏳ Skipped (pre-existing hang issue, unrelated to variant types)

---

## step 4. Explicit Constructor with value
4. Generate MIR for constructor values

#### Key Design Decisions

- **Constructor Names**: Uppercase convention distinguishes constructors from variables
- **Constructor Types**: Constructors without payload have UserSum type directly
- **Memory Representation**: Uses integer tag only (no payload for Phase 3)

#### Test File Location
- `crates/lib/mimium-test/tests/mmm/enum_basic.mmm`

---

## step 4. Explicit Constructor with value

```mimium
type MyEnum = One(float) 
            | Two(float)
            | Three(float)
fn test(myenum: MyEnum){
  match myenum {
    One(v) => v*1,
    Two(v) => v*2,
    Three(v) => v*3
  }
}

let x = test(One(4))
let y = test(Two(5))
let z = test(Three(6))

fn dsp(){
    x+y+z //the result should be 32
}
```

- Add constructor parser
- add match pattern with proper variable binding
- ensure tagged union representation

## step 4. Explicit Constructor with complex value

```mimium
type MyEnum = One(float) 
            | Two((float,float))
            | Three((float,float,float))
fn test(myenum: MyEnum){
  match myenum {
    One(v) => v*1,
    Two((x,y)) => x+y,
    Three((x,y,z)) => x+y+z
  }
}

let x = test(One(4))
let y = test(Two((5,6)))
let z = test(Three((7,8,9)))

fn dsp(){
    x+y+z //the result should be 39
}
```

- Add tuple pattern capture

## step 5. combination with record

(create proper test case here)

## step 6. complex pattern capture matrix

```mimium
type MyEnum = One(float) 
            | Two((float,float))
            | Three((float,float,float))
fn test(myenum: MyEnum, myenum2:MyEnum){
  match myenum {
    (One(v), One(v2)) => v1+v2,
    (Two((x,y)),Two((x2,y2))) => x+y+x2+y2,
    (Three((x,y,z)),Three(x2,y2,z2)) => x+y+z+x2+y2+z2
    _ => 100
  }
}

let x = test(One(4),One(5))
let y = test(Two((5,6)),Two(7,8))
let z = test(Three((7,8,9)),Three(10,11,12))
let a = test(One(0),Two(2,2))

fn dsp(){
    x+y+z+a //the result should be 184
}
```

