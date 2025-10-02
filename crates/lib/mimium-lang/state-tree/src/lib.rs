pub mod patch;
pub mod tree;
pub mod tree_diff;

use crate::tree::{
    SizedType, StateTree, StateTreeSkeleton, deserialize_tree_untagged, serialize_tree_untagged,
};

pub fn update_state_storage<T: SizedType>(
    old: &[u64],
    old_state_skeleton: StateTreeSkeleton<T>,
    new_state_skeleton: StateTreeSkeleton<T>,
) -> Result<Option<Vec<u64>>, Box<dyn std::error::Error>> {
    let target_tree = deserialize_tree_untagged(old, &old_state_skeleton)
        .ok_or("Failed to deserialize old state tree")?;
    let totalsize = new_state_skeleton.total_size();
    let mut new_empty_tree = StateTree::from(new_state_skeleton);
    let patches = tree_diff::take_diff(&target_tree, &new_empty_tree);
    if patches.is_empty() {
        Ok(None)
    } else {
        for patch in &patches {
            log::info!("Patch: {patch:?}");
        }
        patch::apply_patches(&mut new_empty_tree, &target_tree, &patches);

        let res = serialize_tree_untagged(target_tree);
        debug_assert_eq!(res.len(), totalsize as usize);

        Ok(Some(res))
    }
}
