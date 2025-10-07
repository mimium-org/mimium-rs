use crate::patch::CopyFromPatch;
use crate::tree::StateTree;

pub fn take_diff(old_tree: &StateTree, new_tree: &StateTree) -> Vec<CopyFromPatch> {
    let mut patches = Vec::new();
    build_patches_recursive(old_tree, new_tree, Vec::new(), Vec::new(), &mut patches);
    patches
}

/// 2つのノードが「同じ種類」であるかを判定する
fn nodes_match(old: &StateTree, new: &StateTree) -> bool {
    match (old, new) {
        (StateTree::Delay { data: d1, .. }, StateTree::Delay { data: d2, .. }) => {
            d1.len() == d2.len()
        }
        (StateTree::Mem { data: d1 }, StateTree::Mem { data: d2 }) => d1.len() == d2.len(),
        (StateTree::Feed { data: d1 }, StateTree::Feed { data: d2 }) => d1.len() == d2.len(),
        (StateTree::FnCall(_), StateTree::FnCall(_)) => true,
        _ => false,
    }
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

/// 2つのスライスを比較し、LCSの結果を返す
/// `compare`クロージャで要素の比較方法を指定する
pub fn lcs_by<T>(old: &[T], new: &[T], compare: impl Fn(&T, &T) -> bool) -> Vec<DiffResult> {
    let old_len = old.len();
    let new_len = new.len();

    // DPテーブルを作成
    let mut dp = vec![vec![0; new_len + 1]; old_len + 1];
    for i in 1..=old_len {
        for j in 1..=new_len {
            if compare(&old[i - 1], &new[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = dp[i - 1][j].max(dp[i][j - 1]);
            }
        }
    }

    // DPテーブルをバックトラックして差分情報を復元
    let mut results = Vec::new();
    let (mut i, mut j) = (old_len, new_len);
    while i > 0 || j > 0 {
        if i > 0 && j > 0 && compare(&old[i - 1], &new[j - 1]) {
            // Common
            results.push(DiffResult::Common {
                old_index: i - 1,
                new_index: j - 1,
            });
            i -= 1;
            j -= 1;
        } else if j > 0 && (i == 0 || dp[i][j - 1] >= dp[i - 1][j]) {
            // Insert
            results.push(DiffResult::Insert { new_index: j - 1 });
            j -= 1;
        } else if i > 0 {
            // Delete
            results.push(DiffResult::Delete { old_index: i - 1 });
            i -= 1;
        }
    }

    results.reverse(); // 結果を正しい順序にする
    results
}

fn build_patches_recursive(
    old_node: &StateTree,
    new_node: &StateTree,
    old_path: Vec<usize>,
    new_path: Vec<usize>,
    patches: &mut Vec<CopyFromPatch>,
) {
    if !nodes_match(old_node, new_node) {
        return;
    }

    if !matches!(new_node, StateTree::FnCall(_)) {
        patches.push(CopyFromPatch { old_path, new_path });
        return;
    }

    if let (StateTree::FnCall(old_children), StateTree::FnCall(new_children)) = (old_node, new_node)
    {
        // 自作のlcs_by関数を使用
        let lcs_results = lcs_by(old_children, new_children, nodes_match);

        let mut unmatched_old = Vec::new();
        let mut unmatched_new = Vec::new();

        for result in &lcs_results {
            match *result {
                DiffResult::Common {
                    old_index,
                    new_index,
                } => {
                    let old_child = &old_children[old_index];
                    let new_child = &new_children[new_index];

                    let mut next_old_path = old_path.clone();
                    next_old_path.push(old_index);
                    let mut next_new_path = new_path.clone();
                    next_new_path.push(new_index);

                    build_patches_recursive(
                        old_child,
                        new_child,
                        next_old_path,
                        next_new_path,
                        patches,
                    );
                }
                DiffResult::Delete { old_index } => {
                    unmatched_old.push(old_index);
                }
                DiffResult::Insert { new_index } => {
                    unmatched_new.push(new_index);
                }
            }
        }

        for new_index in unmatched_new {
            if let Some(position) = unmatched_old.iter().position(|&old_index| {
                nodes_match(&old_children[old_index], &new_children[new_index])
            }) {
                let old_index = unmatched_old.remove(position);

                let mut next_old_path = old_path.clone();
                next_old_path.push(old_index);
                let mut next_new_path = new_path.clone();
                next_new_path.push(new_index);

                build_patches_recursive(
                    &old_children[old_index],
                    &new_children[new_index],
                    next_old_path,
                    next_new_path,
                    patches,
                );
            }
        }
    }
}
