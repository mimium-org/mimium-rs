use state_tree::{
    apply_state_storage_patch_plan, build_state_storage_patch_plan,
    tree::{SizedType, StateTree, StateTreeSkeleton, serialize_tree_untagged},
    update_state_storage,
};

#[derive(Debug, PartialEq, Clone, Copy)]
struct DummyType(pub u64);

impl SizedType for DummyType {
    fn word_size(&self) -> u64 {
        self.0
    }
}

fn boxed<T: SizedType>(node: StateTreeSkeleton<T>) -> Box<StateTreeSkeleton<T>> {
    Box::new(node)
}

#[test]
fn test_update_state_storage_reorders_nodes() {
    let delay_node = StateTree::Delay {
        readidx: 5,
        writeidx: 6,
        data: vec![10, 11],
    };
    let mem_node = StateTree::Mem {
        data: vec![100, 101, 102],
    };
    let feed_node = StateTree::Feed { data: vec![7] };

    let old_tree = StateTree::FnCall(vec![
        delay_node.clone(),
        mem_node.clone(),
        feed_node.clone(),
    ]);
    let old_skeleton = StateTreeSkeleton::FnCall(vec![
        boxed(StateTreeSkeleton::Delay { len: 2 }),
        boxed(StateTreeSkeleton::Mem(DummyType(3))),
        boxed(StateTreeSkeleton::Feed(DummyType(1))),
    ]);

    let flat_input = serialize_tree_untagged(old_tree.clone());

    let new_skeleton = StateTreeSkeleton::FnCall(vec![
        boxed(StateTreeSkeleton::Mem(DummyType(3))),
        boxed(StateTreeSkeleton::Delay { len: 2 }),
        boxed(StateTreeSkeleton::Feed(DummyType(1))),
    ]);

    let expected_len = new_skeleton.total_size() as usize;

    let updated = update_state_storage(&flat_input, old_skeleton, new_skeleton)
        .expect("update_state_storage should succeed")
        .expect("tree structures differ, patches should be generated");

    assert_eq!(updated.len(), expected_len);

    let expected_flat = vec![100, 101, 102, 0, 0, 0, 0, 7];
    assert_eq!(updated, expected_flat);
}

#[test]
fn test_update_state_storage_nested_child_insertion() {
    let delay_node = StateTree::Delay {
        readidx: 21,
        writeidx: 22,
        data: vec![1, 2],
    };
    let mem_left = StateTree::Mem { data: vec![8, 9] };
    let feed_existing = StateTree::Feed { data: vec![77] };
    let mem_right = StateTree::Mem {
        data: vec![5, 6, 7],
    };

    let old_tree = StateTree::FnCall(vec![StateTree::FnCall(vec![
        delay_node.clone(),
        mem_left.clone(),
        feed_existing.clone(),
        mem_right.clone(),
    ])]);

    let old_skeleton = StateTreeSkeleton::FnCall(vec![boxed(StateTreeSkeleton::FnCall(vec![
        boxed(StateTreeSkeleton::Delay { len: 2 }),
        boxed(StateTreeSkeleton::Mem(DummyType(2))),
        boxed(StateTreeSkeleton::Feed(DummyType(1))),
        boxed(StateTreeSkeleton::Mem(DummyType(3))),
    ]))]);

    let flat_input = serialize_tree_untagged(old_tree.clone());

    let new_skeleton = StateTreeSkeleton::FnCall(vec![boxed(StateTreeSkeleton::FnCall(vec![
        boxed(StateTreeSkeleton::Mem(DummyType(2))),
        boxed(StateTreeSkeleton::Feed(DummyType(2))),
        boxed(StateTreeSkeleton::Delay { len: 2 }),
        boxed(StateTreeSkeleton::Feed(DummyType(1))),
        boxed(StateTreeSkeleton::Mem(DummyType(3))),
    ]))]);

    let updated = update_state_storage(&flat_input, old_skeleton, new_skeleton)
        .expect("update_state_storage should succeed")
        .expect("structural changes should generate patches");

    let expected_flat = vec![8, 9, 0, 0, 0, 0, 0, 0, 77, 5, 6, 7];
    assert_eq!(updated, expected_flat);
}

#[test]
fn test_patch_plan_then_apply_matches_update_state_storage() {
    let old_tree = StateTree::FnCall(vec![StateTree::FnCall(vec![
        StateTree::Delay {
            readidx: 3,
            writeidx: 4,
            data: vec![1, 2],
        },
        StateTree::Mem { data: vec![8, 9] },
        StateTree::Feed { data: vec![99] },
    ])]);

    let old_skeleton = StateTreeSkeleton::FnCall(vec![boxed(StateTreeSkeleton::FnCall(vec![
        boxed(StateTreeSkeleton::Delay { len: 2 }),
        boxed(StateTreeSkeleton::Mem(DummyType(2))),
        boxed(StateTreeSkeleton::Feed(DummyType(1))),
    ]))]);
    let new_skeleton = StateTreeSkeleton::FnCall(vec![boxed(StateTreeSkeleton::FnCall(vec![
        boxed(StateTreeSkeleton::Mem(DummyType(2))),
        boxed(StateTreeSkeleton::Delay { len: 2 }),
        boxed(StateTreeSkeleton::Feed(DummyType(1))),
    ]))]);

    let old_flat = serialize_tree_untagged(old_tree);

    let patch_plan = build_state_storage_patch_plan(old_skeleton.clone(), new_skeleton.clone())
        .expect("patch plan should be generated");
    let by_plan = apply_state_storage_patch_plan(&old_flat, &patch_plan);

    let by_legacy = update_state_storage(&old_flat, old_skeleton, new_skeleton)
        .expect("legacy update_state_storage should succeed")
        .expect("legacy update_state_storage should produce migrated storage");

    assert_eq!(by_plan, by_legacy);
}
