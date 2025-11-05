use std::collections::HashSet;

use state_tree::{
    patch::{CopyFromPatch, apply_patches},
    tree::{StateTree, StateTreeSkeleton},
    tree_diff::take_diff,
};

// ヘルパー関数: StateTreeからStateTreeSkeletonを経由してtake_diffを呼び出す
fn take_diff_from_trees(old_tree: &StateTree, new_tree: &StateTree) -> HashSet<CopyFromPatch> {
    let old_skeleton = old_tree.to_skeleton();
    let new_skeleton = new_tree.to_skeleton();
    take_diff(&old_skeleton, &new_skeleton)
}

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

    let patches = take_diff_from_trees(&old_tree, &new_tree);
    assert_eq!(
        patches,
        [CopyFromPatch {
            old_path: vec![],
            new_path: vec![]
        }]
        .into_iter()
        .collect()
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

    let patches = take_diff_from_trees(&old_tree, &new_tree);

    assert_eq!(
        patches,
        [
            CopyFromPatch {
                old_path: vec![0],
                new_path: vec![0]
            },
            CopyFromPatch {
                old_path: vec![1],
                new_path: vec![2]
            },
        ]
        .into_iter()
        .collect()
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

    let patches = take_diff_from_trees(&old_tree, &new_tree)
        .into_iter()
        .collect::<Vec<_>>();
    apply_patches(&mut new_tree, &old_tree, patches.as_slice());

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
        StateTree::Mem { data: vec![2, 3] }, //削除
        StateTree::Feed { data: vec![4] },   // (削除される)
    ]);

    let mut new_tree = StateTree::FnCall(vec![
        StateTree::Mem { data: vec![0, 0] },
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

    let patches = take_diff_from_trees(&old_tree, &new_tree);
    assert_eq!(
        patches,
        [CopyFromPatch {
            old_path: vec![1],
            new_path: vec![0]
        }]
        .into_iter()
        .collect()
    );
    apply_patches(
        &mut new_tree,
        &old_tree,
        &patches.into_iter().collect::<Vec<_>>(),
    );

    assert_eq!(new_tree, expected_tree);
}
type Skeleton = StateTreeSkeleton<usize>;
#[test]
fn test_complex() {
    //structure taken from examples/fbdelay.mmm
    let old_tree = Skeleton::FnCall(vec![
        Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::Feed(1))])),
        Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::FnCall(vec![
            Box::new(Skeleton::Feed(1)),
        ]))])),
        Box::new(Skeleton::FnCall(vec![
            Box::new(Skeleton::Delay { len: 4 }),
            Box::new(Skeleton::Feed(1)),
        ])),
    ]);
    let new_tree = Skeleton::FnCall(vec![
        Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::Feed(1))])), // new node
        Box::new(Skeleton::FnCall(vec![
            Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::Feed(1))])), // new node
            //new node
            Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::FnCall(vec![
                Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::Feed(1))])),
            ]))])),
        ])),
        Box::new(Skeleton::FnCall(vec![
            Box::new(Skeleton::Delay { len: 4 }),
            Box::new(Skeleton::Feed(1)),
        ])),
        Box::new(Skeleton::FnCall(vec![
            //new node
            Box::new(Skeleton::Delay { len: 4 }),
            Box::new(Skeleton::Feed(1)),
        ])),
    ]);
    let patches = take_diff(&old_tree, &new_tree);
    let answer = [
        CopyFromPatch {
            old_path: vec![1, 0],
            new_path: vec![1, 0],
        },
        CopyFromPatch {
            old_path: vec![0],
            new_path: vec![0],
        },
        CopyFromPatch {
            old_path: vec![2],
            new_path: vec![3],
        },
    ]
    .into_iter()
    .collect();
    assert_eq!(patches, answer);
}
