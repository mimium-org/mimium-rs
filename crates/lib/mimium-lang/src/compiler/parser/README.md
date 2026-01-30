# Parser for mimium

This directory contains the parser implementation for mimium, based on the Red-Green Syntax Tree pattern.

For the formal syntax specification, see [ebnf.md](./ebnf.md).

## Overview

The parser preserves all information from the source code, including comments, whitespace, and exact positions. This makes it suitable for IDE features like:

- Code formatting
- Refactoring
- Semantic highlighting
- Code completion with context
- Comment preservation during transformations

## Architecture

The parser consists of four main stages:

### 1. Tokenizer (`tokenizer.rs`)

Converts source text into position-aware tokens using chumsky parser combinators.

- **Input**: Source text (`&str`)
- **Output**: Sequence of `Token`s
- Each token stores:
  - `kind`: Type of token (identifier, keyword, literal, etc.)
  - `start`: Byte offset in source
  - `length`: Length in bytes
- Literals store only position information, not the actual value (values can be reconstructed from source + position)
- **Error Recovery**: Uses chumsky's error recovery to continue parsing after encountering invalid characters
  - Invalid characters are marked with `TokenKind::Error`
  - Parser continues collecting valid tokens after errors

### 2. Pre-parser (`preparser.rs`)

Separates trivia (comments, whitespace) from syntax tokens.

- **Input**: Token sequence
- **Output**: `PreParsedTokens` containing:
  - `token_indices`: Indices of non-trivia tokens
  - `leading_trivia_map`: Map from token index to leading trivia
  - `trailing_trivia_map`: Map from token index to trailing trivia
- Comments and whitespace are preserved in trivia maps
- Linebreaks act as separators between leading and trailing trivia

### 3. CST Parser (`cst_parser.rs`, `green.rs`)

Parses token indices into a Green Tree (Concrete Syntax Tree).

- **Input**: Token indices from pre-parser
- **Output**: Green Tree (position-independent, immutable CST)
- Green nodes use **SlotMap-based interning** for efficient memory management
  - Nodes stored in `GreenNodeArena` with O(1) lookups
  - Referenced via lightweight `GreenNodeId` handles
  - Better cache locality compared to Arc-based sharing
  - No reference counting overhead
- Green nodes represent the complete syntactic structure
- Can be shared and cached across multiple uses
- Implements recursive descent parsing
- **Lookahead Disambiguation**:
  - Tuples `(a, b)` vs parenthesized expressions `(a)`
  - Records `{a = 1}` vs blocks `{stmt}`
  - Tuples require a comma (single element tuples need trailing comma)
  - Records require field assignment pattern `ident = expr`

### 4. Red Tree / AST (`red.rs`, `lower.rs`)

Converts Green Tree to Red Tree and AST.

- **Input**: Green Tree
- **Output**: 
  - Red Tree: Position-aware nodes with absolute offsets
  - AST: Traditional abstract syntax tree without trivia
- Red nodes are created on-demand from Green nodes
- **Let-body-then Chain Transformation**: When converting to AST, flat statement lists are transformed into nested Let structures
  - Green/Red Tree: Statements stored as a flat list for full-fidelity representation
  - AST: Let bindings nested as `Let(x, value, Let(y, value2, body))` for semantic analysis
  - Matches the structure expected by the main mimium compiler

## Usage

```rust
use mimium_lang::compiler::parser;

let source = "fn dsp() { 42 }";

// Complete pipeline
let (ast, tokens, preparsed, arena, errors) = parser::parse(source);

// Or step by step:
let tokens = parser::tokenize(source);
let preparsed = parser::preparse(&tokens);
let (green_id, arena, tokens, errors) = parser::parse_cst(tokens, &preparsed);
let red = parser::green_to_red(green_id, 0);
let ast = parser::red_to_ast(&red, source, &tokens, &arena);
```

### Error Recovery

The tokenizer can recover from invalid characters and continue parsing:

```rust
let source = "fn dsp() { let x = 42 § let y = 3.14 © }";
let tokens = parser::tokenize(source);

// The parser continues and marks invalid characters as Error tokens
// Valid tokens: fn, dsp, (, ), {, let, x, =, 42, let, y, =, 3.14, }
// Error tokens: §, ©
```

## Red-Green Syntax Tree Pattern

This implementation follows the Red-Green Syntax Tree pattern used in rust-analyzer and Roslyn:

- **Green Tree**: Immutable, position-independent nodes that represent the syntactic structure
  - Can be shared and cached
  - Efficient for incremental parsing
  - Contains all tokens including trivia

- **Red Tree**: Position-aware wrapper around Green nodes
  - Created on-demand with absolute positions
  - Used for queries and navigation
  - Efficient as Green nodes are reused

## Testing

```bash
cargo test -p mimium-lang parser
```

## References

- [ebnf.md](./ebnf.md) - Formal syntax specification in EBNF
- [rust-analyzer's rowan library](https://github.com/rust-lang/rust-analyzer/tree/master/crates/rowan)
- [Roslyn's Red-Green Trees](https://ericlippert.com/2012/06/08/red-green-trees/)
- [Simple but Powerful Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
