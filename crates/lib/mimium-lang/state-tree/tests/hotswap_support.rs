use state_tree::{
    build_state_storage_patch_plan, codec, feed_ranges,
    patch::{CopyFromPatch, apply_patches},
    tree::StateTreeSkeleton,
    StateStoragePatchPlan,
};

fn fncall(c: Vec<StateTreeSkeleton<u64>>) -> StateTreeSkeleton<u64> {
    StateTreeSkeleton::FnCall(c.into_iter().map(Box::new).collect())
}

#[test]
fn apply_patches_clamps_to_short_storage() {
    // A runtime may grow storage lazily, so the old storage can be shorter than its skeleton.
    let old = vec![1, 2, 3];
    let mut new = vec![0; 6];
    apply_patches(&mut new, &old, &[CopyFromPatch { src_addr: 1, dst_addr: 4, size: 4 }]);
    assert_eq!(new, vec![0, 0, 0, 0, 2, 3]);
    apply_patches(&mut new, &old, &[CopyFromPatch { src_addr: 9, dst_addr: 0, size: 2 }]);
    assert_eq!(new, vec![0, 0, 0, 0, 2, 3]);
}

#[test]
fn feed_patches_follow_inserted_nodes() {
    // old: [Feed1, Delay4, Feed1]  new: [Delay2, Feed1, Delay4, Feed1]
    let old = fncall(vec![StateTreeSkeleton::Feed(1), StateTreeSkeleton::Delay { len: 4 }, StateTreeSkeleton::Feed(1)]);
    let new = fncall(vec![
        StateTreeSkeleton::Delay { len: 2 },
        StateTreeSkeleton::Feed(1),
        StateTreeSkeleton::Delay { len: 4 },
        StateTreeSkeleton::Feed(1),
    ]);
    assert_eq!(feed_ranges(&new), vec![(4, 1), (11, 1)]);
    let plan = build_state_storage_patch_plan(old, new).unwrap();
    let mut f = plan.feed_patches.clone();
    f.sort_by_key(|p| p.dst_addr);
    assert_eq!(
        f,
        vec![
            CopyFromPatch { src_addr: 0, dst_addr: 4, size: 1 },
            CopyFromPatch { src_addr: 7, dst_addr: 11, size: 1 },
        ]
    );
}

#[test]
fn identity_plan_covers_all_feeds() {
    let s = fncall(vec![StateTreeSkeleton::Feed(2), StateTreeSkeleton::Delay { len: 3 }, fncall(vec![StateTreeSkeleton::Feed(1)])]);
    let plan = StateStoragePatchPlan::identity(&s);
    assert_eq!(plan.total_size, 8);
    assert_eq!(
        plan.feed_patches,
        vec![
            CopyFromPatch { src_addr: 0, dst_addr: 0, size: 2 },
            CopyFromPatch { src_addr: 7, dst_addr: 7, size: 1 },
        ]
    );
}

#[test]
fn codec_roundtrip_and_rejects_garbage() {
    let s = fncall(vec![
        StateTreeSkeleton::Delay { len: 192000 },
        StateTreeSkeleton::Mem(1),
        fncall(vec![StateTreeSkeleton::Feed(28), fncall(vec![])]),
    ]);
    let b = codec::encode(&s);
    assert_eq!(codec::decode(&b, |v| v), Some(s));
    assert_eq!(codec::decode::<u64>(&b[..b.len() - 1], |v| v), None);
    assert_eq!(codec::decode::<u64>(&[3, 255, 255, 255, 255, 255, 255, 255, 255], |v| v), None);
    assert_eq!(codec::decode::<u64>(&[], |v| v), None);
}
