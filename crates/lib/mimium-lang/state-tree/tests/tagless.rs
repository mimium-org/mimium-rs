use rkyv::{access_mut, to_bytes};
use state_tree::diff::diff;
use state_tree::patch::{Patch, apply};
use state_tree::tree::ArchivedStateTree;
use state_tree::tree::{
    StateTree, StateTreeSkeleton, deserialize_tree_untagged, serialize_tree_untagged,
};

struct DummyType(pub u64);
impl state_tree::tree::SizedType for DummyType {
    fn word_size(&self) -> u64 {
        self.0
    }
}

/// Comprehensive scenario test: Raw data -> Deserialize -> Edit -> Diff -> Patch -> Serialize -> Verify
#[test]
fn test_serde_append_delay() {
    println!("🔄 === Full Scenario Test: append delay ===");

    // Step 1: Start with raw data (Vec<u64>) and skeleton
    let original_data = vec![42, 1, 2, 3]; // Mem{data: 42} + Feed{data: [1, 2, 3]}
    let skeleton = StateTreeSkeleton::<DummyType>::FnCall(vec![
        Box::new(StateTreeSkeleton::Mem(DummyType(1))),
        Box::new(StateTreeSkeleton::Feed(DummyType(3))),
    ]);

    println!("📊 Original raw data: {original_data:?}");

    // Step 2: Deserialize to StateTree
    let original_tree = deserialize_tree_untagged(&original_data, &skeleton)
        .expect("Should deserialize successfully");

    println!("🌳 Deserialized tree: {original_tree:#?}");

    let modified_skeleton = StateTreeSkeleton::FnCall(vec![
        Box::new(StateTreeSkeleton::Mem(DummyType(1))),
        Box::new(StateTreeSkeleton::Feed(DummyType(3))),
        Box::new(StateTreeSkeleton::Delay { len: 2 }),
    ]);
    let modified_tree = StateTree::from(modified_skeleton);

    println!("🔧 Modified tree: {modified_tree:#?}");

    // Step 4: Calculate diff patches
    let mut original_bytes =
        to_bytes::<rkyv::rancor::Error>(&original_tree).expect("Should serialize original tree");
    let mut modified_bytes =
        to_bytes::<rkyv::rancor::Error>(&modified_tree).expect("Should serialize modified tree");

    let archived_original =
        access_mut::<ArchivedStateTree, rkyv::rancor::Error>(&mut original_bytes)
            .expect("Should access archived original");
    let archived_modified =
        access_mut::<ArchivedStateTree, rkyv::rancor::Error>(&mut modified_bytes)
            .expect("Should access archived modified");

    let patches = diff(&archived_original, &archived_modified, &[]);
    let expected_patches = vec![Patch::Insert {
        parent_path: vec![],
        index: 2,
        new_tree: StateTree::Delay {
            readidx: 0,
            writeidx: 0,
            data: vec![0, 0],
        },
    }];
    println!("🔍 Generated patches: {patches:#?}");
    assert_eq!(patches, expected_patches);

    // Step 5: Apply patches to original tree
    let mut patched_tree = original_tree.clone();
    apply(&mut patched_tree, &patches).expect("Should apply patches successfully");

    // Step 6: Serialize patched tree back to raw data
    let patched_raw_data = serialize_tree_untagged(patched_tree);

    println!("📤 Patched raw data: {patched_raw_data:?}");

    // Step 7: Create expected raw data for verification
    let expected_raw_data = vec![42, 1, 2, 3, 0, 0, 0, 0]; // Mem{data: 42} + Feed{data: [1,2,3]} + Delay{readidx:0, writeidx:0, data:[0,0]}

    assert_eq!(patched_raw_data, expected_raw_data);
    println!("✅ Expected raw data: {expected_raw_data:?}");

    println!("🎉 Full scenario test completed!");
}
