# mimium Variant type implementation

This document for agent.

we are gonna implement variant (enum / sum) type for mimium.

The plan should be separated into the following steps.

---

## Implementation Plan Overview

### Phase Dependencies

```
Phase 1: Integer Match     (foundation)
    ↓
Phase 2: Sum Type `A|B`    (type system extension)
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
| 1 | `green.rs`, `cst_parser.rs`, `lower.rs` | `typing.rs` | `mirgen.rs` | `bytecodegen.rs`, `vm.rs` |
| 2 | (type annotation parser) | `types/mod.rs`, `typing.rs` | `mirgen.rs` | `vm.rs` (tagged union) |
| 3 | `cst_parser.rs` (type decl) | type environment | - | - |
| 4+ | pattern parser | pattern typing | pattern MIR | - |

### Phase 1 Implementation Steps

1. **Tokenizer**: Add `match` keyword and `=>` token
2. **Parser**: Add `MatchExpr` CST parsing  
3. **AST**: Define `Expr::Match` and `MatchPattern`
4. **Lower**: CST → AST conversion
5. **Typing**: Type inference for match (reference if-else)
6. **MIR**: Extend `JmpIf` pattern to implement Switch
7. **Bytecode/VM**: Sequential comparison implementation
8. **Test**: Verify with `match_int.mmm`

---

## step.1 integer match expression

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

## step 2. primitive sum type

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

