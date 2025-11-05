use crate::patch::CopyFromPatch;
use crate::tree::StateTree;

pub fn take_diff(old_tree: &StateTree, new_tree: &StateTree) -> Vec<CopyFromPatch> {
    let mut patches = Vec::new();
    build_patches_recursive(old_tree, new_tree, Vec::new(), Vec::new(), &mut patches);
    patches
}

/// 2つのノードの完全一致度をスコアリングする（0.0～1.0）
///
/// # Returns
/// - 1.0: ノードが完全に一致（同じ種類かつ同じサイズ）
/// - 0.0: ノードが異なる種類、またはサイズが異なる
/// - 0.0～1.0: FnCallの場合は、再帰的にマッチしたスコアを正規化
fn score_node_match(old: &StateTree, new: &StateTree) -> f64 {
    match (old, new) {
        // Delayノード: 同じサイズなら1.0、異なれば0.0
        (StateTree::Delay { data: d1, .. }, StateTree::Delay { data: d2, .. })
            if d1.len() == d2.len() =>
        {
            1.0
        }
        // Memノード: 同じサイズなら1.0、異なれば0.0
        (StateTree::Mem { data: d1 }, StateTree::Mem { data: d2 }) if d1.len() == d2.len() => 1.0,
        // Feedノード: 同じサイズなら1.0、異なれば0.0
        (StateTree::Feed { data: d1 }, StateTree::Feed { data: d2 }) if d1.len() == d2.len() => 1.0,
        // FnCallノード: ネストした呼び出しの場合、サイズの類似度でスコアを計算
        (StateTree::FnCall(children_old), StateTree::FnCall(children_new)) => {
            if children_old.is_empty() && children_new.is_empty() {
                return 1.0;
            }

            let old_len = children_old.len() as f64;
            let new_len = children_new.len() as f64;

            // 子ノード数の類似度でスコアを計算（完全一致なら1.0）
            let scores_child = lcs_by_score(children_old, children_new, score_node_match)
                .iter()
                .map(|res| match res {
                    DiffResult::Common {
                        old_index,
                        new_index,
                    } => 1.0,
                    _ => 0.0,
                })
                .sum::<f64>();
            let max_len = old_len.max(new_len);
            scores_child / max_len
        }
        // 異なるノード種別
        _ => 0.0,
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

/// 2つのスライスを比較し、スコアに基づいて最適なマッチングを返す
/// `score_fn`クロージャでマッチスコア（0.0～1.0）を計算する
pub fn lcs_by_score<T>(old: &[T], new: &[T], score_fn: impl Fn(&T, &T) -> f64) -> Vec<DiffResult> {
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
    println!("DP Table: {:?}", dp);
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

/// 後方互換性のための古い関数
pub fn lcs_by<T>(old: &[T], new: &[T], compare: impl Fn(&T, &T) -> bool) -> Vec<DiffResult> {
    lcs_by_score(old, new, |a, b| if compare(a, b) { 1.0 } else { 0.0 })
}

fn build_patches_recursive(
    old_node: &StateTree,
    new_node: &StateTree,
    old_path: Vec<usize>,
    new_path: Vec<usize>,
    patches: &mut Vec<CopyFromPatch>,
) {
    match score_node_match(old_node, new_node) {
        0.0 => {
            return;
        }
        1.0 => {
            patches.push(CopyFromPatch { old_path, new_path });
            return;
        }
        _ => {}
    };

    if let (StateTree::FnCall(old_children), StateTree::FnCall(new_children)) = (old_node, new_node)
    {
        // スコア付きのLCS アルゴリズムを使用
        let lcs_results = lcs_by_score(old_children, new_children, score_node_match);

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

        // for new_index in unmatched_new {
        //     if let Some(position) = unmatched_old.iter().position(|&old_index| {
        //         score_node_match(&old_children[old_index], &new_children[new_index]) > 0.0
        //     }) {
        //         let old_index = unmatched_old.remove(position);

        //         let mut next_old_path = old_path.clone();
        //         next_old_path.push(old_index);
        //         let mut next_new_path = new_path.clone();
        //         next_new_path.push(new_index);

        //         build_patches_recursive(
        //             &old_children[old_index],
        //             &new_children[new_index],
        //             next_old_path,
        //             next_new_path,
        //             patches,
        //         );
        //     }
        // }
    }
}
