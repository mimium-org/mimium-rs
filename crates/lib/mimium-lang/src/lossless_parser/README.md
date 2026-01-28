# Lossless Parser for mimium Language Server

This directory contains a prototype implementation of a lossless parser for the mimium Language Server, based on the Red-Green Syntax Tree pattern.

## Overview

The lossless parser preserves all information from the source code, including comments, whitespace, and exact positions. This makes it suitable for IDE features like:

- Code formatting
- Refactoring
- Semantic highlighting
- Code completion with context
- Comment preservation during transformations

## Architecture

The parser consists of four main stages:

### 1. Tokenizer (`tokenizer.rs`)

Converts source text into position-aware tokens using chumsky parser combinators.

- **Input**: Source text (String)
- **Output**: Sequence of `LosslessToken`s
- Each token stores:
  - `kind`: Type of token (identifier, keyword, literal, etc.)
  - `start`: Byte offset in source
  - `length`: Length in bytes
- Literals store only position information, not the actual value (values can be reconstructed from source + position)
- **Error Recovery**: Uses chumsky's error recovery to continue parsing after encountering invalid characters
  - Invalid characters are marked with `TokenKind::Error`
  - Parser continues collecting valid tokens after errors
  - All errors are logged for debugging

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
- **Tuple and Record Support**:
  - Uses lookahead to distinguish tuples `(a, b)` from parenthesized expressions `(a)`
  - Uses lookahead to distinguish records `{a = 1}` from blocks `{stmt}`
  - Tuples require a comma (single element tuples need trailing comma)
  - Records require field assignment pattern `ident = expr`

### 4. AST Parser (`red.rs`)

Converts Green Tree to Red Tree and AST.

- **Input**: Green Tree
- **Output**: 
  - Red Tree: Position-aware nodes with absolute offsets and parent references
  - AST: Traditional abstract syntax tree without trivia
- Red nodes are created on-demand from Green nodes
- **Parent References**: Red nodes maintain weak references to parent nodes for bottom-up traversal
  - Enables efficient upward navigation (e.g., finding enclosing function, scope)
  - Avoids circular references using `Weak<RedNode>`
  - Provides `parent()`, `ancestors()`, and `is_descendant_of()` methods
- **Let-body-then Chain Transformation**: When converting to AST, flat statement lists are transformed into nested Let structures
  - Green/Red Tree: Statements stored as a flat list for lossless representation
  - AST: Let bindings nested as `Let(x, value, Let(y, value2, body))` for semantic analysis
  - Matches the structure expected by the main mimium compiler
- AST is suitable for semantic analysis

## Usage Example

```rust
use mimium_language_server::lossless_parser;

let source = "fn dsp() { 42 }";

// Complete pipeline
let (ast, tokens, preparsed, arena) = lossless_parser::parse(source);

// Or step by step:
let tokens = lossless_parser::tokenize(source);
let preparsed = lossless_parser::preparse(&tokens);
let (cst, arena, annotated_tokens) = lossless_parser::parse_cst(tokens, &preparsed);
let red = lossless_parser::green_to_red(cst, &arena, 0);
let ast = lossless_parser::red_to_ast(&red, source, &annotated_tokens, &arena);
```

Run the demos:

```bash
# Main demo showing the full pipeline
cargo run --package mimium-language-server --example lossless_parser_demo

# Error recovery demo showing continued parsing after errors
cargo run --package mimium-language-server --example error_recovery_demo
```

### Error Recovery Example

The tokenizer can recover from invalid characters and continue parsing:

```rust
let source = "fn dsp() { let x = 42 § let y = 3.14 © }";
let tokens = lossless_parser::tokenize(source);

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

The implementation includes comprehensive unit tests:

```bash
cargo test --package mimium-language-server --lib
```

All tests (28 total) should pass:
- Tokenizer tests (8 tests including 2 error recovery tests)
- Pre-parser tests (5 tests)
- Green Tree tests (4 tests)
- CST parser tests (3 tests)
- Red Tree tests (3 tests)
- Integration tests (5 tests)

## Future Enhancements

This is a prototype implementation. Future work could include:

1. **More complete grammar**: Currently handles basic statements and expressions
2. **Error recovery**: Better handling of syntax errors
3. **Incremental parsing**: Reuse unchanged parts of the tree
4. **Performance optimization**: Profile and optimize hot paths
5. **Source maps**: Better tracking of macro expansions and includes
6. **IDE features**: Implement code actions, refactorings using the lossless tree

## References

- [rust-analyzer's rowan library](https://github.com/rust-lang/rust-analyzer/tree/master/crates/rowan)
- [Roslyn's Red-Green Trees](https://ericlippert.com/2012/06/08/red-green-trees/)
- [Simple but Powerful Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
