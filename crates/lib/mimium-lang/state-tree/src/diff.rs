use crate::patch::Patch;
use crate::tree::{ArchivedStateTree, StateTree};
use ArchivedStateTree::*;
use rkyv::deserialize;
use rkyv::rancor::Error;

/// Calculate the difference between two StateTrees and return a list of patches
/// This is the new functional-style API with optimization applied
pub fn diff(old: &ArchivedStateTree, new: &ArchivedStateTree, path: &[usize]) -> Vec<Patch> {
    let patches = diff_unoptimized(old, new, path);
    optimize_patches(patches)
}

/// Calculate the difference between two StateTrees without optimization
/// This is the core diff algorithm
fn diff_unoptimized(
    old: &ArchivedStateTree,
    new: &ArchivedStateTree,
    path: &[usize],
) -> Vec<Patch> {
    // Create a patch to replace nodes when types or values are different

    match (old, new) {
        // If both are FnCall, compare their contents using advanced algorithms
        (ArchivedStateTree::FnCall(old_children), ArchivedStateTree::FnCall(new_children)) => {
            diff_children_unoptimized(old_children, new_children, path)
        }
        _ if old == new => {
            // Do nothing if the nodes are completely identical
            Vec::new()
        }

        (Mem { data: data1 }, Mem { data: data2 })
        | (Feed { data: data1 }, Feed { data: data2 })
        | (Delay { data: data1, .. }, Delay { data: data2, .. })
            if data1.len() == data2.len() =>
        {
            // Reuse the same object for Mem, Feed, and Delay if they have the same size
            Vec::new()
        }
        // For all other cases (different types or values), use simple Replace
        _ if old != new => {
            let deserialized_new = deserialize::<StateTree, Error>(new).unwrap();
            vec![Patch::Replace {
                path: path.to_vec(),
                new_tree: deserialized_new,
            }]
        }
        _ => {
            // Do nothing if the contents of Delay or FnCall are completely identical
            Vec::new()
        }
    }
}

/// Efficiently compare children of FnCall nodes without optimization
fn diff_children_unoptimized(
    old_children: &[ArchivedStateTree],
    new_children: &[ArchivedStateTree],
    parent_path: &[usize],
) -> Vec<Patch> {
    // 1. Scan common prefix - find the length using take_while and enumerate
    let prefix_len = old_children
        .iter()
        .zip(new_children.iter())
        .take_while(|(old_child, new_child)| are_compatible(old_child, new_child))
        .count();

    // Process prefix recursively
    let prefix_patches: Vec<Patch> = old_children
        .iter()
        .zip(new_children.iter())
        .take(prefix_len)
        .enumerate()
        .flat_map(|(index, (old_child, new_child))| {
            let mut current_path = parent_path.to_vec();
            current_path.push(index);
            diff(old_child, new_child, &current_path)
        })
        .collect();

    // 2. Scan common suffix - find the length using iterators
    let suffix_len = old_children[prefix_len..]
        .iter()
        .rev()
        .zip(new_children[prefix_len..].iter().rev())
        .take_while(|(old_child, new_child)| old_child == new_child)
        .count();

    // Process suffix recursively
    let suffix_patches: Vec<Patch> = old_children[prefix_len..]
        .iter()
        .rev()
        .zip(new_children[prefix_len..].iter().rev())
        .take(suffix_len)
        .enumerate()
        .flat_map(|(rev_index, (old_child, new_child))| {
            let new_idx = new_children.len() - 1 - rev_index;
            let mut current_path = parent_path.to_vec();
            current_path.push(new_idx);
            diff(old_child, new_child, &current_path)
        })
        .collect();

    // 3. Execute LCS on the remaining middle part
    let old_middle = &old_children[prefix_len..old_children.len() - suffix_len];
    let new_middle = &new_children[prefix_len..new_children.len() - suffix_len];

    if old_middle.is_empty() && new_middle.is_empty() {
        return [prefix_patches, suffix_patches].concat(); // No differences in the middle part
    }

    // Generate Replace if the middle part is a single element
    let middle_patches = if old_middle.len() == 1 && new_middle.len() == 1 {
        let old_node = &old_middle[0];
        let new_node = &new_middle[0];

        if old_node != new_node {
            if are_compatible(old_node, new_node) {
                // If compatible but different, perform deep comparison
                let mut current_path = parent_path.to_vec();
                current_path.push(prefix_len);
                diff(old_node, new_node, &current_path)
            } else {
                // If not compatible, generate a Replace patch
                let deserialized_new = deserialize::<StateTree, Error>(new_node).unwrap();
                vec![Patch::Replace {
                    path: {
                        let mut path = parent_path.to_vec();
                        path.push(prefix_len);
                        path
                    },
                    new_tree: deserialized_new,
                }]
            }
        } else {
            Vec::new()
        }
    } else {
        // Create LCS table and generate differences functionally
        diff_middle_with_lcs(old_middle, new_middle, prefix_len, parent_path)
    };

    [prefix_patches, middle_patches, suffix_patches].concat()
}

/// Process middle part using LCS algorithm in functional style
fn diff_middle_with_lcs(
    old_middle: &[ArchivedStateTree],
    new_middle: &[ArchivedStateTree],
    prefix_len: usize,
    parent_path: &[usize],
) -> Vec<Patch> {
    let lcs_table = lcs(old_middle, new_middle);

    backtrack_lcs(
        old_middle,
        new_middle,
        &lcs_table,
        prefix_len,
        parent_path,
        old_middle.len(),
        new_middle.len(),
    )
}
// Convert the backtracking while loop to a recursive functional approach
fn backtrack_lcs(
    old_middle: &[ArchivedStateTree],
    new_middle: &[ArchivedStateTree],
    lcs_table: &[Vec<usize>],
    prefix_len: usize,
    parent_path: &[usize],
    i: usize,
    j: usize,
) -> Vec<Patch> {
    if i == 0 && j == 0 {
        return Vec::new();
    }

    if i > 0 && j > 0 && old_middle[i - 1] == new_middle[j - 1] {
        // Common element -> recursively diff
        let current_idx = prefix_len + j - 1;
        let mut current_path = parent_path.to_vec();
        current_path.push(current_idx);
        let mut patches = backtrack_lcs(
            old_middle,
            new_middle,
            lcs_table,
            prefix_len,
            parent_path,
            i - 1,
            j - 1,
        );
        patches.extend(diff(&old_middle[i - 1], &new_middle[j - 1], &current_path));
        patches
    } else if j > 0 && (i == 0 || lcs_table[i][j - 1] >= lcs_table[i - 1][j]) {
        // Only exists in new -> Insert
        let deserialized_new = deserialize::<StateTree, Error>(&new_middle[j - 1]).unwrap();
        let mut patches = backtrack_lcs(
            old_middle,
            new_middle,
            lcs_table,
            prefix_len,
            parent_path,
            i,
            j - 1,
        );
        patches.push(Patch::Insert {
            parent_path: parent_path.to_vec(),
            index: prefix_len + j - 1,
            new_tree: deserialized_new,
        });
        patches
    } else if i > 0 && (j == 0 || lcs_table[i][j - 1] < lcs_table[i - 1][j]) {
        // Only exists in old -> Remove
        let mut patches = backtrack_lcs(
            old_middle,
            new_middle,
            lcs_table,
            prefix_len,
            parent_path,
            i - 1,
            j,
        );
        patches.push(Patch::Remove {
            parent_path: parent_path.to_vec(),
            index: prefix_len + i - 1,
        });
        patches
    } else {
        Vec::new()
    }
}
/// Calculate DP table for LCS (Longest Common Subsequence) length using functional style
fn lcs(old: &[ArchivedStateTree], new: &[ArchivedStateTree]) -> Vec<Vec<usize>> {
    let mut table = vec![vec![0; new.len() + 1]; old.len() + 1];

    // Fill the table using functional approach with enumerate and fold
    (1..=old.len()).for_each(|i| {
        (1..=new.len()).for_each(|j| {
            table[i][j] = if old[i - 1] == new[j - 1] {
                1 + table[i - 1][j - 1]
            } else {
                table[i - 1][j].max(table[i][j - 1])
            };
        });
    });

    table
}

/// Check if two nodes are compatible (same type and same data size)
fn are_compatible(old: &ArchivedStateTree, new: &ArchivedStateTree) -> bool {
    use ArchivedStateTree::*;
    match (old, new) {
        (Mem { data: data1 }, Mem { data: data2 })
        | (Feed { data: data1 }, Feed { data: data2 }) => data1.len() == data2.len(),
        (Delay { .. }, Delay { .. }) => true, // Always consider Delay as compatible (handled by Replace)
        (FnCall(_), FnCall(_)) => true,       // FnCall always has compatibility
        _ => false,
    }
}

/// Optimize a sequence of patches by merging Remove-Insert pairs into Replace operations
/// This function identifies consecutive Remove and Insert operations at the same path and index
/// and merges them into a single Replace operation for better efficiency.
pub fn optimize_patches(patches: Vec<Patch>) -> Vec<Patch> {
    let mut optimized = Vec::new();
    let mut i = 0;

    while i < patches.len() {
        // Check if current patch is Remove and next patch is Insert at the same location
        if i + 1 < patches.len() {
            match (&patches[i], &patches[i + 1]) {
                (
                    Patch::Remove {
                        parent_path: remove_path,
                        index: remove_idx,
                    },
                    Patch::Insert {
                        parent_path: insert_path,
                        index: insert_idx,
                        new_tree,
                    },
                ) if remove_path == insert_path && remove_idx == insert_idx => {
                    // Merge Remove + Insert into Replace
                    let replace_path = {
                        let mut path = remove_path.clone();
                        path.push(*remove_idx);
                        path
                    };

                    optimized.push(Patch::Replace {
                        path: replace_path,
                        new_tree: new_tree.clone(),
                    });

                    // Skip both Remove and Insert patches
                    i += 2;
                    continue;
                }
                _ => {}
            }
        }

        // If no optimization possible, keep the original patch
        optimized.push(patches[i].clone());
        i += 1;
    }

    optimized
}
