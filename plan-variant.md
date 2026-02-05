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
Phase 2: Sum Type `A|B`    (type system extension)  ← NEXT
    ↓
Phase 3: Type Declaration  (custom types)
    ↓
Phase 4: Constructor w/ values
    ↓
Phase 5-6: Complex patterns
```

### Key Files to Modify (by phase)

| Phase | Parser | Type System | MIR | VM/Bytecode |
|-------|--------|-------------|-----|-------------|
| 1 ✅ | `green.rs`, `cst_parser.rs`, `lower.rs` | `typing.rs` | `mirgen.rs` | `bytecodegen.rs`, `vm.rs` |
| 2 | (type annotation parser) | `types/mod.rs`, `typing.rs` | `mirgen.rs` | `vm.rs` (tagged union) |
| 3 | `cst_parser.rs` (type decl) | type environment | - | - |
| 4+ | pattern parser | pattern typing | pattern MIR | - |

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
Instruction::Switch { scrutinee: VPtr, cases: Vec<(i64, u64)>, default_block: u64, merge_block: u64 }
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

## step 2. primitive sum type ← PARTIALLY COMPLETE

The variant type with explicit type annotation should be added.

```mimium
fn test(num_or_str: float | string ){
  match num_or_str {
    String(x) => 1,
    Float(x) => 2,
  }
}

let x = test("tstring")
let y = test(99)


fn dsp(){
    x+y //the result should be 3
}
```

### Phase 2 Implementation Status (Updated: 2025-01-XX)

**Working:**
- Union type parsing (`float | string`)
- Type inference for union types
- Subtype relationship: values can be passed to union type parameters
- Functions with union type parameters compile successfully
- Constructor patterns parse correctly (`float(x)`, `string(_)`)

**Known Limitations:**
- Match expressions with constructor patterns and variable bindings cause bytecode generation errors due to cross-basic-block register scoping issues
- Tagged union memory layout not yet implemented (values passed as-is for now)

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

4. **MIR Generation** ⚠️ PARTIAL
   - ✅ `Instruction::TaggedUnionWrap` - wrap value with tag (defined)
   - ✅ `Instruction::TaggedUnionGetTag` - extract tag (defined)
   - ✅ `Instruction::TaggedUnionGetValue` - extract value (defined)
   - ✅ Constructor pattern variable bindings in `eval_match`
   - ⚠️ Cross-block register references cause bytecode gen issues
   - ⏳ Need phi-node style value propagation for match blocks

5. **VM/Bytecode Changes** ⏳ TODO
   - Tagged union memory layout implementation
   - Bytecode instructions for tagged union operations
   - VM execution support

### Known Issues

- **Register scoping**: When constructor patterns reference the scrutinee value from a previous basic block, the bytecode generator fails to find the register because `VRegister::find` removes entries after first use. This requires SSA-style value propagation or block arguments for match expressions.

---

- parser and formatter for sum type with `|` operator should be added.
- Primnitive type constructor(String and Float) pattern should be added.
- Tagged Union representation should be added in MIR and VM.

## step 3. Explicit Constructor pattern without value

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

- Type declaration statement and binding environemnt should be added.
- Variable environemnt for type constructors should be added.
- Variable match pattern should be added.
- Conversion to the internal integer representation in MIR and VM

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

