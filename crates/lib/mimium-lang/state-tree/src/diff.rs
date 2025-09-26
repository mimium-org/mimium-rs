use crate::patch::{Patch, Path};
use crate::tree::{ArchivedStateTree, StateTree};
use ArchivedStateTree::*;
use rkyv::deserialize;
use rkyv::rancor::Error;
/// Calculate the difference between two StateTrees and return a list of patches
pub fn diff(
    old: &ArchivedStateTree,
    new: &ArchivedStateTree,
    path: &mut Path,
    patches: &mut Vec<Patch>,
) {
    // Create a patch to replace nodes when types or values are different

    match (old, new) {
        // If both are FnCall, compare their contents using advanced algorithms
        (ArchivedStateTree::FnCall(old_children), ArchivedStateTree::FnCall(new_children)) => {
            diff_children(old_children, new_children, path, patches);
        }
        _ if old == new => {
            // Do nothing if the nodes are completely identical
        }
        (Mem { data: data1 }, Mem { data: data2 })
        | (Feed { data: data1 }, Feed { data: data2 })
        | (Delay { data: data1, .. }, Delay { data: data2, .. })
            if data1.len() == data2.len() =>
        {
            // Reuse the same object for Mem, Feed, and Delay if they have the same size
        }
        // For all other cases (different types or values), use simple Replace
        _ if old != new => {
            let deserialized_new = deserialize::<StateTree, Error>(new).unwrap();
            patches.push(Patch::Replace {
                path: path.clone(),
                new_tree: deserialized_new,
            });
        }
        _ => {
            // Do nothing if the contents of Delay or FnCall are completely identical
        }
    }
}
/// Efficiently compare children of FnCall nodes
fn diff_children(
    old_children: &[ArchivedStateTree],
    new_children: &[ArchivedStateTree],
    parent_path: &mut Path,
    patches: &mut Vec<Patch>,
) {
    // 1. Scan common prefix
    let mut prefix_len = 0;
    for (old_child, new_child) in old_children.iter().zip(new_children.iter()) {
        if are_compatible(old_child, new_child) {
            // For compatible nodes (same type and size), recursively check internal differences
            parent_path.push(prefix_len);
            diff(old_child, new_child, parent_path, patches);
            parent_path.pop();
            prefix_len += 1;
        } else {
            break;
        }
    }

    // 2. Scan common suffix
    let mut suffix_len = 0;
    for (old_child, new_child) in old_children[prefix_len..]
        .iter()
        .rev()
        .zip(new_children[prefix_len..].iter().rev())
    {
        if old_child == new_child {
            // Recursively check suffix elements as well
            let old_idx = old_children.len() - 1 - suffix_len;
            let new_idx = new_children.len() - 1 - suffix_len;
            // Create path based on new index
            parent_path.push(new_idx);
            diff(
                &old_children[old_idx],
                &new_children[new_idx],
                parent_path,
                patches,
            );
            parent_path.pop();
            suffix_len += 1;
        } else {
            break;
        }
    }

    // 3. Execute LCS on the remaining middle part
    let old_middle = &old_children[prefix_len..old_children.len() - suffix_len];
    let new_middle = &new_children[prefix_len..new_children.len() - suffix_len];

    if old_middle.is_empty() && new_middle.is_empty() {
        return; // No differences in the middle part
    }

    // Generate Replace if the middle part is a single element and compatible
    if old_middle.len() == 1 && new_middle.len() == 1 {
        let old_node = &old_middle[0];
        let new_node = &new_middle[0];

        if are_compatible(old_node, new_node) && old_node != new_node {
            // If compatible but different, perform deep comparison
            parent_path.push(prefix_len);
            diff(old_node, new_node, parent_path, patches);
            parent_path.pop();
            return;
        }
    }

    // Create LCS table
    let lcs_table = lcs(old_middle, new_middle);

    // Generate differences based on LCS table (backtrack)
    let mut i = old_middle.len();
    let mut j = new_middle.len();

    while i > 0 || j > 0 {
        if i > 0 && j > 0 && old_middle[i - 1] == new_middle[j - 1] {
            // Common element -> recursively diff
            let current_idx = prefix_len + j - 1;
            parent_path.push(current_idx);
            diff(&old_middle[i - 1], &new_middle[j - 1], parent_path, patches);
            parent_path.pop();
            i -= 1;
            j -= 1;
        } else if j > 0 && (i == 0 || lcs_table[i][j - 1] >= lcs_table[i - 1][j]) {
            // Only exists in new -> Insert
            let deserialized_new = deserialize::<StateTree, Error>(&new_middle[j - 1]).unwrap();
            patches.push(Patch::Insert {
                parent_path: parent_path.clone(),
                index: prefix_len + j - 1,
                new_tree: deserialized_new,
            });
            j -= 1;
        } else if i > 0 && (j == 0 || lcs_table[i][j - 1] < lcs_table[i - 1][j]) {
            // Only exists in old -> Remove
            patches.push(Patch::Remove {
                parent_path: parent_path.clone(),
                index: prefix_len + i - 1,
            });
            i -= 1;
        } else {
            break; // End of loop
        }
    }
}

/// Calculate DP table for LCS (Longest Common Subsequence) length
fn lcs(old: &[ArchivedStateTree], new: &[ArchivedStateTree]) -> Vec<Vec<usize>> {
    let mut table = vec![vec![0; new.len() + 1]; old.len() + 1];
    for i in 1..=old.len() {
        for j in 1..=new.len() {
            if old[i - 1] == new[j - 1] {
                table[i][j] = 1 + table[i - 1][j - 1];
            } else {
                table[i][j] = table[i - 1][j].max(table[i][j - 1]);
            }
        }
    }
    table
}

/// Check if two nodes are compatible (same type and same data size)
fn are_compatible(old: &ArchivedStateTree, new: &ArchivedStateTree) -> bool {
    use ArchivedStateTree::*;
    match (old, new) {
        (Mem { data: data1 }, Mem { data: data2 }) => data1.len() == data2.len(),
        (Feed { data: data1 }, Feed { data: data2 }) => data1.len() == data2.len(),
        (Delay { .. }, Delay { .. }) => true, // Always consider Delay as compatible (handled by Replace)
        (FnCall(_), FnCall(_)) => true,       // FnCall always has compatibility
        _ => false,
    }
}
