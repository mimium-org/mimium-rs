# Technical Context: mimium-rs

## Language Implementation

The mimium language is implemented in Rust with a focus on performance and safety. The implementation follows a traditional compiler pipeline:

1. **Lexing/Parsing**: Using the `chumsky` parser combinator library
2. **Type Checking**: Hindley-Milner type inference system
3. **MIR Generation**: Mid-level Intermediate Representation
4. **Bytecode Generation**: For the virtual machine
5. **Execution**: Via a custom virtual machine

## Key Technologies

### Core Language (mimium-lang)

- **Parser**: `chumsky` for parser combinators
- **Error Reporting**: `ariadne` for beautiful error messages
- **Symbol Handling**: `string-interner` for efficient symbol management

### Audio Processing (mimium-audiodriver)

- **Audio Output**: Multiple backends including CPAL for cross-platform audio
- **Real-time Processing**: Low-latency audio processing

### Language Server Protocol (mimium-lsp)

- **LSP Implementation**: `tower-lsp` for the LSP server
- **Concurrency**: `tokio` for async runtime
- **Document Management**: `dashmap` for concurrent document storage

## File Structure

The mimium language source files use the `.mmm` extension. The language has a functional programming style with support for:

- First-class functions
- Closures
- Pattern matching
- Reactive programming constructs

## Development Environment

The project is developed as a Rust workspace with multiple crates, each responsible for a specific aspect of the language. The build system uses Cargo, Rust's package manager.

## Testing Strategy

The project includes:

- Unit tests for individual components
- Integration tests for the language features
- Example programs to demonstrate functionality

## Code Management

- Before finishiing working, you must run `cargo fmt`, and `cargo clippy`, then fix the warnings raised by clippy (You need to fix warnings, only the part you modified or newly added in your tasks).

## Performance Considerations

As an audio programming language, mimium has strict performance requirements:

- Low-latency processing
- Efficient memory usage
- Deterministic Latency
    - For example, garbage collection is allowed but non-deterministic time consumption like stop-the-world should not be allowed. That's shy we choose reference-count GC for now.
