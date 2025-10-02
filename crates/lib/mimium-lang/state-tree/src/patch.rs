use crate::tree::StateTree;

/// A patch to be applied to a Empty StateTree from old tree.
#[derive(Debug, PartialEq, Clone)]
pub struct CopyFromPatch {
    pub new_path: Vec<usize>,
    pub old_path: Vec<usize>,
}


/// パッチを新しい木に適用する
///
/// # Arguments
/// * `new_tree` - データのコピー先となる、構造が新しい木（中身は0で初期化済み）
/// * `old_tree` - データのコピー元となる、古い木
/// * `patches` - `diff`関数で生成されたパッチのリスト
///
/// # Panics
/// パッチに含まれるパスが無効な場合、またはコピー元とコピー先のノードの型が一致しない場合にパニックする可能性があります。
/// （`diff`が正しく実装されていれば、通常は起こりません）
pub fn apply_patches(new_tree: &mut StateÏTree, old_tree: &StateTree, patches: &[CopyFromPatch]) {
    for patch in patches {
        let source_node = old_tree
            .get_node(&patch.old_path)
            .expect("Invalid old_path in patch");
        let dest_node = new_tree
            .get_node_mut(&patch.new_path)
            .expect("Invalid new_path in patch");

        // `diff`の`nodes_match`によって型とlenが一致していることは保証されているはず
        // ここでは、その前提に基づいてデータをコピーする
        match (source_node, dest_node) {
            (
                StateTree::Delay { readidx: r_src, writeidx: w_src, data: d_src },
                StateTree::Delay { readidx: r_dest, writeidx: w_dest, data: d_dest },
            ) => {
                *r_dest = *r_src;
                *w_dest = *w_src;
                d_dest.copy_from_slice(d_src);
            }
            (
                StateTree::Mem { data: d_src },
                StateTree::Mem { data: d_dest },
            ) => {
                d_dest.copy_from_slice(d_src);
            }
            (
                StateTree::Feed { data: d_src },
                StateTree::Feed { data: d_dest },
            ) => {
                d_dest.copy_from_slice(d_src);
            }
            // FnCallはデータを持たないので何もしない
            (StateTree::FnCall(_), StateTree::FnCall(_)) => (),
            // 型が一致しない場合はロジックエラー
            _ => panic!("Mismatched node types during patch application. This should not happen."),
        }
    }
}

