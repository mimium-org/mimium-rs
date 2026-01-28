# Lossless Parser Integration with Language Server

This document describes how the lossless parser is integrated into the mimium Language Server.

## Architecture Overview

The Language Server now maintains two parallel parsing systems:

1. **Existing Parser** (mimium-lang): Used for type checking and semantic analysis
2. **Lossless Parser**: Preserves all source information for IDE features

## Integration Points

### Backend State

The `Backend` struct stores lossless parser results per document (URI):

```rust
struct Backend {
    // ... existing fields ...
    
    // Lossless parser state
    lossless_arena_map: DashMap<SrcUri, GreenNodeArena>,
    lossless_root_map: DashMap<SrcUri, GreenNodeId>,
    lossless_tokens_map: DashMap<SrcUri, Vec<LosslessToken>>,
}
```

### Document Processing Flow

When a document changes (`did_open`, `did_change`, `did_save`), the `compile` method:

1. Runs the **lossless parser**:
   - Tokenizes the source
   - Pre-parses to separate trivia
   - Parses to CST (Green Tree)
   - Stores arena, root ID, and tokens

2. Runs the **existing parser**:
   - Parses for type checking
   - Generates diagnostics
   - Produces semantic tokens

Both parsers run on every document change, ensuring both systems have up-to-date information.

## Data Structures

### GreenNodeArena
- Stores all Green nodes using SlotMap for efficient interning
- Nodes are position-independent and immutable
- Shared across the entire document

### GreenNodeId
- Lightweight handle to a node in the arena
- Used to reference nodes without copying

### LosslessToken
- Contains token kind, start position, and length
- Preserves exact source positions
- Literals reference source text (not parsed values)

## Usage for IDE Features

### Accessing Lossless Parser Data

```rust
// In a Language Server handler:
let uri = document_uri.to_string();

// Get the tokens
if let Some(tokens) = self.lossless_tokens_map.get(&uri) {
    // Use tokens for position lookups
}

// Get the Green Tree
if let Some(root_id) = self.lossless_root_map.get(&uri) {
    if let Some(arena) = self.lossless_arena_map.get(&uri) {
        // Create Red node for position-aware queries
        let red = lossless_parser::green_to_red(*root_id, &arena, 0);
        
        // Use Red node for:
        // - Finding node at position
        // - Getting parent nodes
        // - Traversing syntax tree
    }
}
```

### Potential IDE Features

The lossless parser data can be used for:

1. **Semantic Highlighting**
   - Use tokens with accurate positions
   - Preserve all syntax information

2. **Hover Information**
   - Find node at cursor position using Red nodes
   - Get parent context using `parent()` method
   - Display relevant information

3. **Go-to-Definition**
   - Navigate using parent references
   - Find enclosing scope
   - Locate definition sites

4. **Code Folding**
   - Use block structure from Green Tree
   - Preserve comments in folded regions

5. **Code Formatting**
   - Reconstruct source exactly using tokens
   - Preserve trivia (comments, whitespace)

## Performance Considerations

### Current Implementation

- **Dual Parsing**: Both parsers run on every change
- **Memory**: Each document stores two parse trees
- **Time**: Adds lossless parsing overhead to compilation

### Potential Optimizations

1. **Incremental Parsing**: Only re-parse changed portions
2. **Lazy Evaluation**: Only create Red nodes when needed
3. **Caching**: Share Green nodes across similar structures
4. **Conditional Parsing**: Skip lossless parsing for large files

## Testing

All integration is covered by tests:

- **34 unit tests** for lossless parser components
- **1 doctest** for API usage
- **Integration tests** verify dual parsing doesn't break existing functionality

## Future Enhancements

1. **Incremental Parsing**: Update only changed portions of Green Tree
2. **Query API**: High-level API for IDE features
3. **Performance Metrics**: Track parsing time and memory usage
4. **LSP Features**: Implement hover, definition, references using lossless parser

## References

- **Red-Green Trees**: [rust-analyzer syntax documentation](https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/syntax.md)
- **Lossless Parsing**: Preserving all source information including trivia
- **SlotMap Interning**: Efficient node storage and sharing
