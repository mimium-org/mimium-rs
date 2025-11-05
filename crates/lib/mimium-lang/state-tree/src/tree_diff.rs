use std::collections::HashSet;

use crate::patch::CopyFromPatch;
use crate::tree::{SizedType, StateTreeSkeleton};

pub fn take_diff<T: SizedType>(
    old_skeleton: &StateTreeSkeleton<T>,
    new_skeleton: &StateTreeSkeleton<T>,
) -> HashSet<CopyFromPatch> {
    let patchset = build_patches_recursive(old_skeleton, new_skeleton, vec![], vec![]);
    patchset.into_iter().collect()
}

/// LCSアルゴリズムの結果を表すEnum
#[derive(Debug)]
pub enum DiffResult {
    /// 両方のシーケンスに共通して存在する要素
    Common { old_index: usize, new_index: usize },
    /// 古いシーケンスにのみ存在する要素（削除された）
    Delete { old_index: usize },
    /// 新しいシーケンスにのみ存在する要素（挿入された）
    Insert { new_index: usize },
}

/// 2つのスライスを比較し、スコアに基づいて最適なマッチングを返す
/// `score_fn`クロージャでマッチスコア（0.0～1.0）を計算する
pub fn lcs_by_score<T>(
    old: &[T],
    new: &[T],
    mut score_fn: impl FnMut(&T, &T) -> f64,
) -> Vec<DiffResult> {
    let old_len = old.len();
    let new_len = new.len();

    // DP テーブル: dp[i][j] = 累積スコア
    let mut dp = vec![vec![0.0; new_len + 1]; old_len + 1];

    for i in 1..=old_len {
        for j in 1..=new_len {
            let score = score_fn(&old[i - 1], &new[j - 1]);

            if score > 0.0 {
                // マッチした場合: 対角線の値 + マッチスコア
                dp[i][j] = (dp[i - 1][j - 1] + score).max(dp[i - 1][j].max(dp[i][j - 1]));
            } else {
                // マッチしなかった場合: 最大値を取る
                dp[i][j] = dp[i - 1][j].max(dp[i][j - 1]);
            }
        }
    }
    // バックトラックして結果を復元
    let mut results = Vec::new();
    let (mut i, mut j) = (old_len, new_len);

    while i > 0 || j > 0 {
        if i > 0 && j > 0 {
            let score = score_fn(&old[i - 1], &new[j - 1]);

            if score > 0.0 {
                // マッチしている可能性が高い
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

    results.reverse(); // 結果を正しい順序にする
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

fn build_patches_recursive<T: SizedType>(
    old_node: &StateTreeSkeleton<T>,
    new_node: &StateTreeSkeleton<T>,
    old_path: Vec<usize>,
    new_path: Vec<usize>,
) -> HashSet<CopyFromPatch> {
    // ノードが完全に一致する場合は、単一のパッチを返す
    if nodes_match(old_node, new_node) {
        return [CopyFromPatch { old_path, new_path }].into_iter().collect();
    }

    match (old_node, new_node) {
        (StateTreeSkeleton::FnCall(old_children), StateTreeSkeleton::FnCall(new_children)) => {
            // 最初に全ての子ノードのパッチを計算（スコア計算の副作用を避けるため）
            let mut child_patches_map = Vec::new();
            for (old_idx, old_child) in old_children.iter().enumerate() {
                for (new_idx, new_child) in new_children.iter().enumerate() {
                    let child_old_path = [old_path.clone(), vec![old_idx]].concat();
                    let child_new_path = [new_path.clone(), vec![new_idx]].concat();
                    let patches = build_patches_recursive(
                        old_child,
                        new_child,
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

            // LCSでマッチングを見つける
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

            // LCS結果に基づいてパッチを収集
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
