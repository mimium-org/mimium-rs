# mimium-lsp

Language Server Protocol (LSP) implementation for the mimium programming language.

## Features

The mimium Language Server provides the following features:

- **Syntax Error Reporting**: Highlights syntax errors in your mimium code.
- **Hover Information**: Shows type information and documentation when hovering over symbols.
- **Code Completion**: Suggests keywords, functions, and variables as you type.

## Installation

### Building from Source

To build the mimium Language Server from source, you need to have Rust and Cargo installed. Then, run:

```bash
cargo build --release -p mimium-lsp
```

The binary will be available at `target/release/mimium-lsp`.

## Usage

### With Visual Studio Code

1. Install the mimium extension for VS Code (coming soon).
2. The extension will automatically start the language server when you open a `.mmm` file.

### With Other Editors

For other editors that support the Language Server Protocol, you need to configure them to use the `mimium-lsp` binary as the language server for `.mmm` files.

## Development

### Architecture

The mimium Language Server is built using the `tower-lsp` crate and consists of the following components:

- **Server**: The main LSP server implementation.
- **Capabilities**: Modules implementing specific LSP features:
  - **Diagnostics**: Syntax error reporting.
  - **Hover**: Type information and documentation.
  - **Completion**: Code completion suggestions.

### Adding New Features

To add a new LSP feature:

1. Create a new module in the `capabilities` directory.
2. Implement the feature using the mimium language APIs.
3. Update the server to use the new capability.

## License

This project is licensed under the MPL-2.0 License - see the LICENSE file for details.
