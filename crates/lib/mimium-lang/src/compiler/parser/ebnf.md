# mimium Syntax Specification (EBNF)

This document describes the formal syntax of the mimium programming language using Extended Backus-Naur Form (EBNF).

## Notation Conventions

- `::=` defines a production rule
- `|` denotes alternatives
- `[ ... ]` denotes optional elements (zero or one)
- `{ ... }` denotes repetition (zero or more)
- `( ... )` groups elements
- `"..."` denotes terminal symbols (keywords/operators)
- `'...'` denotes single character terminals
- `/* ... */` denotes comments about the grammar

## Lexical Elements

### Comments

```ebnf
Comment         ::= SingleLineComment | MultiLineComment
SingleLineComment ::= "//" { AnyCharExceptNewline } Newline
MultiLineComment  ::= "/*" { AnyChar } "*/"
```

### Literals

```ebnf
IntLiteral      ::= Digit { Digit }
FloatLiteral    ::= Digit { Digit } "." Digit { Digit }
StringLiteral   ::= '"' { AnyCharExceptQuote } '"'

Digit           ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
```

### Identifiers

```ebnf
Ident           ::= Letter { Letter | Digit | '_' }
MacroIdent      ::= Ident "!"

Letter          ::= 'a'..'z' | 'A'..'Z' | '_'
```

## Grammar

### Program Structure

```ebnf
Program         ::= { Separator } { TopLevelStatement Separator } { Separator }

TopLevelStatement ::= FunctionDefinition
                    | GlobalStatement
                    | ImportStatement
                    | StageDeclaration

Separator       ::= Newline | ";"
```

### Stage Declaration

```ebnf
StageDeclaration ::= "#" "stage" "(" StageKind ")"
StageKind        ::= "macro" | "main"
```

### Import Statement

```ebnf
ImportStatement ::= "include" "(" StringLiteral ")"
```

### Function Definition

```ebnf
FunctionDefinition ::= "fn" Ident FunctionParams [ "->" Type ] Block

FunctionParams     ::= "(" [ ParamList ] ")"
ParamList          ::= Parameter { "," Parameter } [ "," ]
Parameter          ::= TypedIdent [ "=" Expr ]
```

### Statements

```ebnf
Statement       ::= LetStatement
                  | LetRecStatement
                  | AssignStatement
                  | SingleStatement

GlobalStatement ::= Statement

LetStatement    ::= "let" TypedPattern "=" ExprGroup
LetRecStatement ::= "letrec" TypedIdent "=" ExprGroup
AssignStatement ::= LValue "=" ExprGroup
SingleStatement ::= ExprGroup

LValue          ::= Ident
                  | LValue "." Ident        /* field access */
                  | LValue "[" Expr "]"     /* array access */
```

### Expressions

```ebnf
ExprGroup       ::= Block
                  | IfExpr
                  | Expr

/* Expression with operator precedence (lowest to highest) */
Expr            ::= PipeExpr

/* Precedence 0: Pipe (left associative) */
PipeExpr        ::= ScheduleExpr { "|>" ScheduleExpr }

/* Precedence 1: Schedule (left associative) */
ScheduleExpr    ::= OrExpr { "@" OrExpr }

/* Precedence 2: Logical OR (left associative) */
OrExpr          ::= AndExpr { "||" AndExpr }

/* Precedence 3: Logical AND (left associative) */
AndExpr         ::= EqExpr { "&&" EqExpr }

/* Precedence 4: Equality (left associative) */
EqExpr          ::= RelExpr { ("==" | "!=") RelExpr }

/* Precedence 5: Relational (left associative) */
RelExpr         ::= AddExpr { ("<" | "<=" | ">" | ">=") AddExpr }

/* Precedence 6: Additive (left associative) */
AddExpr         ::= MultExpr { ("+" | "-") MultExpr }

/* Precedence 7: Multiplicative (left associative) */
MultExpr        ::= ExponentExpr { ("*" | "/" | "%") ExponentExpr }

/* Precedence 8: Exponentiation (right associative) */
ExponentExpr    ::= UnaryExpr [ "^" ExponentExpr ]

/* Precedence 9: Unary operators */
UnaryExpr       ::= { "-" | "`" | "$" } DotExpr

/* Precedence 10: Field access (left associative) */
DotExpr         ::= ApplyExpr { "." DotField }
DotField        ::= Ident | IntLiteral

/* Function application and array indexing */
ApplyExpr       ::= AtomExpr { CallSuffix | IndexSuffix }

CallSuffix      ::= "(" [ ArgList ] ")"
IndexSuffix     ::= "[" Expr "]"

ArgList         ::= Expr { "," Expr } [ "," ]
```

### Atomic Expressions

```ebnf
AtomExpr        ::= Literal
                  | Ident
                  | LambdaExpr
                  | MacroExpand
                  | ParenExpr
                  | TupleExpr
                  | ArrayLiteral
                  | RecordExpr

Literal         ::= IntLiteral
                  | FloatLiteral
                  | StringLiteral
                  | "self"
                  | "now"
                  | "samplerate"
                  | "_"                     /* placeholder */
```

### Lambda Expression

```ebnf
LambdaExpr      ::= "|" [ LambdaParams ] "|" [ "->" Type ] ExprGroup

LambdaParams    ::= TypedIdent { "," TypedIdent }
```

### Macro Expansion

```ebnf
MacroExpand     ::= MacroIdent "(" [ ArgList ] ")"
```

### Parenthesized and Tuple Expressions

```ebnf
ParenExpr       ::= "(" Expr ")"
TupleExpr       ::= "(" Expr { "," Expr } [ "," ] ")"
```

### Array Literal

```ebnf
ArrayLiteral    ::= "[" [ ArgList ] "]"
```

### Record Expressions

```ebnf
RecordExpr      ::= RecordLiteral | RecordUpdate | ImcompleteRecord

RecordLiteral   ::= "{" [ RecordFields ] "}"
ImcompleteRecord ::= "{" RecordFields ".." "}"
RecordUpdate    ::= "{" Expr "<-" RecordFields "}"

RecordFields    ::= RecordField { "," RecordField } [ "," ]
RecordField     ::= Ident "=" Expr
```

### Block Expression

```ebnf
Block           ::= { "`" | "$" } "{" Statements "}"

Statements      ::= { Separator } [ Statement { Separator Statement } ] { Separator }
```

Note: Quote (`` ` ``) and Escape (`$`) operators can be applied to:
- Any expression as unary operators (see `UnaryExpr`)
- Block expressions as shown above

### If Expression

```ebnf
IfExpr          ::= "if" "(" Expr ")" ExprGroup [ "else" ExprGroup ]
```

## Types

```ebnf
Type            ::= PrimitiveType
                  | TupleType
                  | RecordType
                  | ArrayType
                  | FunctionType
                  | CodeType

PrimitiveType   ::= "float" | "int" | "string"

TupleType       ::= "(" Type { "," Type } [ "," ] ")"

RecordType      ::= "{" [ RecordTypeFields ] "}"
RecordTypeFields ::= RecordTypeField { "," RecordTypeField } [ "," ]
RecordTypeField  ::= Ident ":" Type

ArrayType       ::= "[" Type "]"

FunctionType    ::= "(" [ TypeList ] ")" "->" Type
TypeList        ::= Type { "," Type }

CodeType        ::= "`" Type
```

### Type Annotations

```ebnf
TypedIdent      ::= Ident [ ":" Type ]
TypedPattern    ::= Pattern [ ":" Type ]
```

## Patterns

```ebnf
Pattern         ::= SinglePattern
                  | TuplePattern
                  | RecordPattern

SinglePattern   ::= Ident | "_"

TuplePattern    ::= "(" Pattern { "," Pattern } [ "," ] ")"

RecordPattern   ::= "{" RecordPatternFields "}"
RecordPatternFields ::= RecordPatternField { "," RecordPatternField } [ "," ]
RecordPatternField  ::= Ident "=" Pattern
```

## Multi-Stage Programming

mimium supports multi-stage programming with quote and escape operators.

Quote (`` ` ``) creates a code representation (lifts a value to the next stage),
while Escape/Splice (`$`) splices code into a quoted context (drops to the previous stage).

These operators are defined as:
- Unary operators in `UnaryExpr` (applicable to any expression)
- Prefix operators for `Block` expressions

They can be chained, e.g., ``` ``expr ``` or `$$expr`.

## Examples

### Simple Function Definition

```mimium
fn add(x: float, y: float) -> float {
    x + y
}
```

Parsed as:
```
FunctionDefinition
├── Ident: "add"
├── FunctionParams
│   ├── Parameter: x: float
│   └── Parameter: y: float
├── ReturnType: float
└── Block
    └── BinaryOp(+)
        ├── Var: x
        └── Var: y
```

### Lambda Expression

```mimium
let f = |x, y| x + y
```

### Record Literal and Update

```mimium
let r = { a = 1, b = 2 }
let r2 = { r <- a = 10 }
```

### Scheduled Expression

```mimium
fn dsp() {
    osc(440) @ (now + 1000)
}
```

### Macro Expansion

```mimium
Slider!(freq, 440, 20, 20000)
```

---

*This specification is derived from the parser implementation in mimium-rs.*
