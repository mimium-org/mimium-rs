use std::collections::HashSet;

use state_tree::{
    patch::{CopyFromPatch, apply_patches},
    tree::{StateTree, StateTreeSkeleton, serialize_tree_untagged, deserialize_tree_untagged},
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
    // Delay全体のサイズは readidx(1) + writeidx(1) + data(2) = 4
    assert_eq!(
        patches,
        [CopyFromPatch {
            src_addr: 0,
            dst_addr: 0,
            size: 4,
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
        }, // Delayサイズ: 2 + 1 = 3, addr: 0
        StateTree::Mem { data: vec![3] }, // Memサイズ: 1, addr: 3
    ]);
    let new_tree = StateTree::FnCall(vec![
        StateTree::Delay {
            readidx: 0,
            writeidx: 0,
            data: vec![0],
        }, // Delayサイズ: 3, addr: 0
        StateTree::Feed { data: vec![0] }, // Feedサイズ: 1, addr: 3 (新規)
        StateTree::Mem { data: vec![0] },  // Memサイズ: 1, addr: 4
    ]);

    let patches = take_diff_from_trees(&old_tree, &new_tree);

    assert_eq!(
        patches,
        [
            CopyFromPatch {
                src_addr: 0,
                dst_addr: 0,
                size: 3,
            },
            CopyFromPatch {
                src_addr: 3,
                dst_addr: 4,
                size: 1,
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
    let new_tree = StateTree::Delay {
        readidx: 0,
        writeidx: 0,
        data: vec![0, 0, 0],
    };
    
    let old_storage = serialize_tree_untagged(old_tree.clone());
    let mut new_storage = serialize_tree_untagged(new_tree.clone());

    let patches = take_diff_from_trees(&old_tree, &new_tree)
        .into_iter()
        .collect::<Vec<_>>();
    apply_patches(&mut new_storage, &old_storage, patches.as_slice());

    assert_eq!(new_storage, old_storage);
}

#[test]
fn test_apply_with_structural_changes() {
    let old_tree = StateTree::FnCall(vec![
        StateTree::Delay {
            readidx: 10,
            writeidx: 11,
            data: vec![1],
        }, // Delayサイズ: 3, addr: 0
        StateTree::Mem { data: vec![2, 3] }, // Memサイズ: 2, addr: 3
        StateTree::Feed { data: vec![4] },   // Feedサイズ: 1, addr: 5
    ]);

    let new_tree = StateTree::FnCall(vec![
        StateTree::Mem { data: vec![0, 0] }, // Memサイズ: 2, addr: 0
        StateTree::Delay {
            readidx: 0,
            writeidx: 0,
            data: vec![0],
        }, // Delayサイズ: 3, addr: 2
        StateTree::Mem {
            data: vec![0, 0, 0],
        }, // Memサイズ: 3, addr: 5 (新規ノード)
    ]);

    // 期待される最終状態
    let expected_tree = StateTree::FnCall(vec![
        StateTree::Mem { data: vec![2, 3] }, // old: addr 3からコピー
        StateTree::Delay {
            readidx: 0,
            writeidx: 0,
            data: vec![0],
        },
        StateTree::Mem {
            data: vec![0, 0, 0],
        }, // 新規なのでゼロのまま
    ]);

    let old_storage = serialize_tree_untagged(old_tree.clone());
    let mut new_storage = serialize_tree_untagged(new_tree.clone());
    
    let patches = take_diff_from_trees(&old_tree, &new_tree);
    assert_eq!(
        patches,
        [CopyFromPatch {
            src_addr: 3,
            dst_addr: 0,
            size: 2,
        }]
        .into_iter()
        .collect()
    );
    apply_patches(
        &mut new_storage,
        &old_storage,
        &patches.into_iter().collect::<Vec<_>>(),
    );

    let result_tree = deserialize_tree_untagged(&new_storage, &new_tree.to_skeleton())
        .expect("Failed to deserialize result tree");
    assert_eq!(result_tree, expected_tree);
}
type Skeleton = StateTreeSkeleton<usize>;
#[test]
fn test_complex() {
    //structure taken from examples/fbdelay.mmm
    let old_tree = Skeleton::FnCall(vec![
        Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::Feed(1))])), // Feed(1), addr: 0, size: 1
        Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::FnCall(vec![
            Box::new(Skeleton::Feed(1)), // Feed(1), addr: 1, size: 1
        ]))])),
        Box::new(Skeleton::FnCall(vec![
            Box::new(Skeleton::Delay { len: 4 }), // Delay, addr: 2, size: 6 (2 + 4)
            Box::new(Skeleton::Feed(1)),           // Feed(1), addr: 8, size: 1
        ])),
    ]);
    // 旧構造の合計サイズ: 1 + 1 + 6 + 1 = 9
    
    let new_tree = Skeleton::FnCall(vec![
        Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::Feed(1))])), // Feed(1), addr: 0, size: 1
        Box::new(Skeleton::FnCall(vec![
            Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::Feed(1))])), // Feed(1), addr: 1, size: 1
            Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::FnCall(vec![
                Box::new(Skeleton::FnCall(vec![Box::new(Skeleton::Feed(1))])), // Feed(1), addr: 2, size: 1
            ]))])),
        ])),
        Box::new(Skeleton::FnCall(vec![
            Box::new(Skeleton::Delay { len: 4 }), // Delay, addr: 3, size: 6
            Box::new(Skeleton::Feed(1)),           // Feed(1), addr: 9, size: 1
        ])),
        Box::new(Skeleton::FnCall(vec![
            Box::new(Skeleton::Delay { len: 4 }), // Delay, addr: 10, size: 6
            Box::new(Skeleton::Feed(1)),           // Feed(1), addr: 16, size: 1
        ])),
    ]);
    
    let patches = take_diff(&old_tree, &new_tree);
    
    // 期待されるパッチ:
    // 1. old[1, 0] (old addr: 1, size: 1) -> new[1, 0] (new addr: 1, size: 1)
    // 2. old[0] (old addr: 0, size: 1) -> new[0] (new addr: 0, size: 1)
    // 3. old[2] (old addr: 2, size: 7) -> new[3] (new addr: 10, size: 7)
    let answer = [
        CopyFromPatch {
            src_addr: 1,
            dst_addr: 1,
            size: 1,
        },
        CopyFromPatch {
            src_addr: 0,
            dst_addr: 0,
            size: 1,
        },
        CopyFromPatch {
            src_addr: 2,
            dst_addr: 10,
            size: 7,
        },
    ]
    .into_iter()
    .collect();
    assert_eq!(patches, answer);
}
