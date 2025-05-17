# System Patterns: mimium-rs

## Architecture Overview

The mimium-rs project follows a modular architecture with clear separation of concerns:

```
mimium-rs/
├── mimium-lang/        # Core language implementation
├── mimium-audiodriver/ # Audio output
├── mimium-cli/         # Command-line interface
├── mimium-midi/        # MIDI support
├── mimium-symphonia/   # Audio file handling
├── mimium-scheduler/   # Scheduler for audio processing
├── mimium-guitools/    # GUI tools
├── mimium-web/         # Web support
├── mimium-test/        # Testing utilities
└── mimium-lsp/         # Language Server Protocol implementation
```

## Key Design Patterns

### Compiler Pipeline

The compiler follows a traditional pipeline pattern:

1. **Source Text** → **Lexer** → **Tokens**
2. **Tokens** → **Parser** → **AST**
3. **AST** → **Type Checker** → **Typed AST**
4. **Typed AST** → **MIR Generator** → **MIR**
5. **MIR** → **Bytecode Generator** → **Bytecode**
6. **Bytecode** → **VM** → **Execution**

### Language Server Architecture

The LSP implementation follows a client-server architecture:

```
Editor (Client) ←→ LSP Protocol ←→ mimium-lsp (Server) ←→ mimium-lang
```

The server is structured with the following components:

1. **Server**: Main LSP server implementation
2. **Document Manager**: Tracks open documents and their content
3. **Capabilities**: Modules for specific LSP features
   - **Diagnostics**: Syntax error reporting
   - **Hover**: Type information and documentation
   - **Completion**: Code completion suggestions

### Audio Processing

The audio processing follows a real-time processing pattern:

1. **Audio Thread**: High-priority thread for audio processing
2. **Lock-free Communication**: Between audio and control threads
3. **Sample-by-Sample Processing**: For minimal latency

## Data Flow

### Compilation Process

```
Source Code → Parser → AST → Type Checker → MIR → Bytecode → VM → Audio Output
```

### LSP Data Flow

```
Editor Change → LSP Server → Document Update → Parse → Analyze → LSP Response → Editor Update
```

## Error Handling

The project uses a consistent error handling approach:

1. **Recoverable Errors**: Return `Result` types
2. **Unrecoverable Errors**: Panic (in development) or graceful shutdown (in production)
3. **Error Reporting**: Use `ReportableError` trait for user-friendly error messages

## Concurrency Model

The project uses different concurrency models for different components:

1. **Audio Processing**: Real-time thread with lock-free communication
2. **LSP Server**: Async/await with Tokio runtime
3. **Compiler**: Single-threaded for simplicity and determinism

## Extension Points

The system is designed with several extension points:

1. **Plugin System**: For extending language functionality
2. **Audio Backends**: For supporting different audio APIs
3. **LSP Capabilities**: For adding new IDE features
