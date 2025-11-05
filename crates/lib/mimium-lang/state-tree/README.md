# state-tree

A state tree utility library that provides functionality for preserving and managing internal DSP (Digital Signal Processing) state between two different compiled source codes.

## Overview

In mimium, to implement hot-reload functionality, it's necessary to transfer the state of compiled code (delay line buffers, memory values, feedback values, etc.) to newly compiled code. This crate provides a mechanism for efficiently migrating data from an old state tree to a new state tree.

## Key Features

### 1. State Tree Representation (`tree` module)

**StateTree** - A data structure for hierarchically representing DSP internal state

- **Delay**: Represents delay lines (read index, write index, buffer data)
- **Mem**: Memory nodes (list of values)
- **Feed**: Feedback values (control signals, etc.)
- **FnCall**: Function call nodes with multiple child nodes

**StateTreeSkeleton** - Type-safe representation of state tree "structure"

- Holds only size information for each node, not actual values
- Used to define what the new code structure will look like
- Generic type `T: SizedType` specifies how to calculate size for each node

### 2. Diff Detection (`tree_diff` module)

**take_diff** - Detects differences between old and new trees

Diff detection algorithm:
- Uses **Longest Common Subsequence (LCS)** algorithm to detect correspondence between nodes
- Matches nodes by verifying they are the "same type" and "same size"
- Identifies inserted, deleted, and shared nodes

Detection results are returned as a list of `CopyFromPatch`

### 3. Patch Application (`patch` module)

**CopyFromPatch** - Instructions for copying data from old tree to new tree

- `old_path`: Path (array of indices) indicating node position in the old tree
- `new_path`: Path (array of indices) indicating node position in the new tree

**apply_patches** - Applies patches to transfer old data to the new tree

- Copies data from corresponding nodes in the old tree to the new tree
- Appropriately copies Delay's readidx/writeidx and Mem/Feed data

### 4. State Storage Update (`lib.rs`)

**update_state_storage** - Integrated update function

```rust
pub fn update_state_storage<T: SizedType + PartialEq>(
    old: &[u64],                              // Flattened binary representation of old state
    old_state_skeleton: StateTreeSkeleton<T>, // Old structure
    new_state_skeleton: StateTreeSkeleton<T>, // New structure
) -> Result<Option<Vec<u64>>, Box<dyn std::error::Error>>
```

Processing flow:
1. Deserialize old binary data with old structure
2. Create an empty tree with new structure
3. Detect differences (`take_diff`)
4. Apply patches (`apply_patches`)
5. Serialize the new tree to binary and return

Returns `None` if the structure hasn't changed (optimization)

## Usage Examples

### Basic Usage

```rust
use state_tree::{update_state_storage, tree::*};

// Hold old state in binary
let old_state_bytes: Vec<u64> = vec![/* ... */];

// Structure obtained from old compilation result
let old_skeleton = StateTreeSkeleton::FnCall(vec![
    Box::new(StateTreeSkeleton::Delay { len: 2 }),
    Box::new(StateTreeSkeleton::Mem(SizeInfo { size: 100 })),
]);

// Structure of newly compiled code
let new_skeleton = StateTreeSkeleton::FnCall(vec![
    Box::new(StateTreeSkeleton::Mem(SizeInfo { size: 100 })),  // Order changed
    Box::new(StateTreeSkeleton::Delay { len: 2 }),
    Box::new(StateTreeSkeleton::Feed(SizeInfo { size: 1 })),   // Newly added
]);

// Adapt state to new structure
match update_state_storage(&old_state_bytes, &old_skeleton, &new_skeleton) {
    Ok(Some(new_state_bytes)) => {
        // Use state adapted to new structure
    }
    Ok(None) => {
        // Structure hasn't changed
    }
    Err(e) => {
        // Error handling
    }
}
```

## Implementation Details

### Serialization/Deserialization

- `serialize_tree_untagged`: Flattens StateTree to `Vec<u64>`
  - Does not include tag information (structure information managed separately by skeleton)
  - Efficient memory usage

- `deserialize_tree_untagged`: Restores `Vec<u64>` to StateTree using skeleton

### Node Matching Strategy

By using the LCS algorithm:
- Handles node reordering
- Automatically detects node insertion/deletion
- Minimizes data transfer between corresponding nodes

### Path Specification

Access to each node is specified by path (array of indices):
```
old_path: [2, 1, 0]  // Child [2] → its child [1] → its child [0]
```

## Testing

### Unit Tests (`tests/diff.rs`)

- Simple diff detection
- Node insertion/deletion
- LCS algorithm correctness

### Integration Tests (`tests/end2end.rs`)

- Node reordering
- Complex structure changes
- Integration of serialization/deserialization and diff application

## Design Considerations

### Performance

- LCS algorithm has $O(mn)$ time complexity (m, n are node counts)
- Practical for actual DSP structures which typically have few nodes

### Type Safety

- `SizedType` trait generalizes size calculation for each node
- Representing skeleton as types reduces runtime type mismatch errors

### Error Handling

- Returns `Result` on deserialization failure
- Panics on invalid path specification during patch application (validated with debug_assert)

## Related Modules

- **crates/lib/mimium-lang**: Main lang crate. Uses state-tree as part of language processing
- **plugins/mimium-scheduler**: Requires state management for scheduling functionality

## License

Follows the common license of the workspace
Ï