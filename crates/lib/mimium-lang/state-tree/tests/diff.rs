use state_tree::{
    patch::{CopyFromPatch, apply_patches},
    tree::StateTree,
    tree_diff::take_diff,
};

// --- 既存のテスト ... ---
#[test]
fn test_simple_diff() {
    let old_tree = StateTree::Delay {
        readidx: 1,
        writeidx: 1,
        data: vec![1, 2],
    };
    let new_tree = StateTree::Delay {
        readidx: 0,
        writeidx: 0,
        data: vec![0, 0],
    };

    let patches = take_diff(&old_tree, &new_tree);
    assert_eq!(
        patches,
        vec![CopyFromPatch {
            old_path: vec![],
            new_path: vec![]
        }]
    );
}

#[test]
fn test_node_insertion() {
    let old_tree = StateTree::FnCall(vec![
        StateTree::Delay {
            readidx: 1,
            writeidx: 1,
            data: vec![1],
        }, // path: [0]
        StateTree::Mem { data: vec![3] }, // path: [1]
    ]);
    let new_tree = StateTree::FnCall(vec![
        StateTree::Delay {
            readidx: 0,
            writeidx: 0,
            data: vec![0],
        }, // path: [0]
        StateTree::Feed { data: vec![0] }, // (新規) path: [1]
        StateTree::Mem { data: vec![0] },  // path: [2]
    ]);

    let mut patches = take_diff(&old_tree, &new_tree);
    patches.sort_by_key(|p| p.new_path.clone()); // 順序を安定させる

    assert_eq!(
        patches,
        vec![
            CopyFromPatch {
                old_path: vec![0],
                new_path: vec![0]
            },
            CopyFromPatch {
                old_path: vec![1],
                new_path: vec![2]
            },
        ]
    );
}

// --- `apply_patches`のテストを追加 ---
#[test]
fn test_apply_simple_patch() {
    let old_tree = StateTree::Delay {
        readidx: 42,
        writeidx: 84,
        data: vec![1, 2, 3],
    };
    let mut new_tree = StateTree::Delay {
        readidx: 0,
        writeidx: 0,
        data: vec![0, 0, 0],
    };

    let patches = take_diff(&old_tree, &new_tree);
    apply_patches(&mut new_tree, &old_tree, &patches);

    assert_eq!(new_tree, old_tree);
}

#[test]
fn test_apply_with_structural_changes() {
    let old_tree = StateTree::FnCall(vec![
        StateTree::Delay {
            readidx: 10,
            writeidx: 11,
            data: vec![1],
        }, // old: [0]
        StateTree::Mem { data: vec![2, 3] }, // old: [1]
        StateTree::Feed { data: vec![4] },   // (削除される)
    ]);

    let mut new_tree = StateTree::FnCall(vec![
        StateTree::Mem { data: vec![0, 0] }, // new: [0] <- old: [1]
        StateTree::Delay {
            readidx: 0,
            writeidx: 0,
            data: vec![0],
        }, // new: [1] <- old: [0]
        StateTree::Mem {
            data: vec![0, 0, 0],
        }, // (新規ノード)
    ]);

    // 期待される最終状態
    let expected_tree = StateTree::FnCall(vec![
        StateTree::Mem { data: vec![2, 3] }, // old: [1]からコピー
        StateTree::Delay {
            readidx: 10,
            writeidx: 11,
            data: vec![1],
        }, // old: [0]からコピー
        StateTree::Mem {
            data: vec![0, 0, 0],
        }, // 新規なのでゼロのまま
    ]);

    let patches = take_diff(&old_tree, &new_tree);
    assert_eq!(
        patches,
        vec![
            CopyFromPatch {
                old_path: vec![1],
                new_path: vec![0]
            },
            CopyFromPatch {
                old_path: vec![0],
                new_path: vec![1]
            },
        ]
    );
    apply_patches(&mut new_tree, &old_tree, &patches);

    assert_eq!(new_tree, expected_tree);
}
