use std::collections::HashSet;

use crate::patch::CopyFromPatch;
use crate::tree::{SizedType, StateTreeSkeleton};

pub fn take_diff<T: SizedType>(
    old_skeleton: &StateTreeSkeleton<T>,
    new_skeleton: &StateTreeSkeleton<T>,
) -> HashSet<CopyFromPatch> {
    build_patches_recursive(old_skeleton, new_skeleton, vec![], vec![])
}

/// Enum representing the result of LCS algorithm
#[derive(Debug)]
pub enum DiffResult {
    /// Element that exists in both sequences
    Common { old_index: usize, new_index: usize },
    /// Element that exists only in the old sequence (deleted)
    Delete { old_index: usize },
    /// Element that exists only in the new sequence (inserted)
    Insert { new_index: usize },
}

/// Compare two slices and return optimal matching based on scores.
/// The `score_fn` closure calculates the match score (0.0 to 1.0).
pub fn lcs_by_score<T>(
    old: &[T],
    new: &[T],
    mut score_fn: impl FnMut(&T, &T) -> f64,
) -> Vec<DiffResult> {
    let old_len = old.len();
    let new_len = new.len();

    // DP table: dp[i][j] = cumulative score
    let mut dp = vec![vec![0.0; new_len + 1]; old_len + 1];

    for i in 1..=old_len {
        for j in 1..=new_len {
            let score = score_fn(&old[i - 1], &new[j - 1]);

            if score > 0.0 {
                // If matched: diagonal value + match score
                dp[i][j] = (dp[i - 1][j - 1] + score).max(dp[i - 1][j].max(dp[i][j - 1]));
            } else {
                // If not matched: take the maximum value
                dp[i][j] = dp[i - 1][j].max(dp[i][j - 1]);
            }
        }
    }
    // Backtrack to restore the result
    let mut results = Vec::new();
    let (mut i, mut j) = (old_len, new_len);

    while i > 0 || j > 0 {
        if i > 0 && j > 0 {
            let score = score_fn(&old[i - 1], &new[j - 1]);

            if score > 0.0 {
                // Likely matched
                results.push(DiffResult::Common {
                    old_index: i - 1,
                    new_index: j - 1,
                });
                i -= 1;
                j -= 1;
            } else if j > 0 && (i == 0 || dp[i][j - 1] >= dp[i - 1][j]) {
                results.push(DiffResult::Insert { new_index: j - 1 });
                j -= 1;
            } else if i > 0 {
                results.push(DiffResult::Delete { old_index: i - 1 });
                i -= 1;
            }
        } else if j > 0 {
            results.push(DiffResult::Insert { new_index: j - 1 });
            j -= 1;
        } else if i > 0 {
            results.push(DiffResult::Delete { old_index: i - 1 });
            i -= 1;
        }
    }

    results.reverse(); // Reverse to get the correct order
    results
}

fn nodes_match<T: SizedType>(old: &StateTreeSkeleton<T>, new: &StateTreeSkeleton<T>) -> bool {
    match (old, new) {
        (StateTreeSkeleton::Delay { len: len1 }, StateTreeSkeleton::Delay { len: len2 }) => {
            len1 == len2
        }
        (StateTreeSkeleton::Mem(t1), StateTreeSkeleton::Mem(t2)) => {
            t1.word_size() == t2.word_size()
        }
        (StateTreeSkeleton::Feed(t1), StateTreeSkeleton::Feed(t2)) => {
            t1.word_size() == t2.word_size()
        }
        (StateTreeSkeleton::FnCall(c1), StateTreeSkeleton::FnCall(c2)) => {
            c1.len() == c2.len() && c1.iter().zip(c2.iter()).all(|(a, b)| nodes_match(a, b))
        }
        _ => false,
    }
}

/// Retrieve a node from a Skeleton using a path
fn get_node_at_path<'a, T: SizedType>(
    skeleton: &'a StateTreeSkeleton<T>,
    path: &[usize],
) -> Option<&'a StateTreeSkeleton<T>> {
    if path.is_empty() {
        return Some(skeleton);
    }
    
    match skeleton {
        StateTreeSkeleton::FnCall(children) => {
            let child = children.get(path[0])?;
            get_node_at_path(child, &path[1..])
        }
        _ => None,
    }
}

fn build_patches_recursive<T: SizedType>(
    old_skeleton: &StateTreeSkeleton<T>,
    new_skeleton: &StateTreeSkeleton<T>,
    old_path: Vec<usize>,
    new_path: Vec<usize>,
) -> HashSet<CopyFromPatch> {
    // Retrieve the current node from the path
    let old_node = get_node_at_path(old_skeleton, &old_path).expect("Invalid old_path");
    let new_node = get_node_at_path(new_skeleton, &new_path).expect("Invalid new_path");
    
    // If the nodes are completely matched, return a single patch
    if nodes_match(old_node, new_node) {
        // Convert path to address
        let (src_addr, size) = old_skeleton
            .path_to_address(&old_path)
            .expect("Invalid old_path");
        let (dst_addr, dst_size) = new_skeleton
            .path_to_address(&new_path)
            .expect("Invalid new_path");

        debug_assert_eq!(
            size, dst_size,
            "Size mismatch between matched nodes at old_path {old_path:?} and new_path {new_path:?}"
        );

        return [CopyFromPatch {
            src_addr,
            dst_addr,
            size,
        }]
        .into_iter()
        .collect();
    }

    match (old_node, new_node) {
        (StateTreeSkeleton::FnCall(old_children), StateTreeSkeleton::FnCall(new_children)) => {
            // First, calculate patches for all child nodes (to avoid side effects in score calculation)
            let mut child_patches_map = Vec::new();
            for old_idx in 0..old_children.len() {
                for new_idx in 0..new_children.len() {
                    let child_old_path = [old_path.clone(), vec![old_idx]].concat();
                    let child_new_path = [new_path.clone(), vec![new_idx]].concat();
                    let patches = build_patches_recursive(
                        old_skeleton,
                        new_skeleton,
                        child_old_path,
                        child_new_path,
                    );
                    let score = if patches.is_empty() {
                        0.0
                    } else {
                        patches.len() as f64
                    };
                    child_patches_map.push(((old_idx, new_idx), patches, score));
                }
            }

            // Find matching using LCS
            let old_c_with_id: Vec<_> = old_children.iter().enumerate().collect();
            let new_c_with_id: Vec<_> = new_children.iter().enumerate().collect();

            let lcs_results = lcs_by_score(
                &old_c_with_id,
                &new_c_with_id,
                |(oid, _old), (nid, _new)| {
                    child_patches_map
                        .iter()
                        .find(|((o, n), _, _)| o == oid && n == nid)
                        .map(|(_, _, score)| *score)
                        .unwrap_or(0.0)
                },
            );

            // Collect patches based on LCS results
            let mut c_patches = HashSet::new();
            for result in &lcs_results {
                if let DiffResult::Common {
                    old_index,
                    new_index,
                } = result
                    && let Some((_, patches, _)) = child_patches_map
                        .iter()
                        .find(|((o, n), _, _)| o == old_index && n == new_index)
                {
                    c_patches.extend(patches.iter().cloned());
                }
            }

            c_patches
        }
        _ => HashSet::new(),
    }
}
