pub mod patch;
pub mod tree;
pub mod tree_diff;

use crate::tree::{
    SizedType, StateTree, StateTreeSkeleton, deserialize_tree_untagged, serialize_tree_untagged,
};

pub fn update_state_storage<T: SizedType + PartialEq>(
    old: &[u64],
    old_state_skeleton: StateTreeSkeleton<T>,
    new_state_skeleton: StateTreeSkeleton<T>,
) -> Result<Option<Vec<u64>>, Box<dyn std::error::Error>> {
    if old_state_skeleton == new_state_skeleton {
        return Ok(None);
    }
    let target_tree = deserialize_tree_untagged(old, &old_state_skeleton)
        .ok_or("Failed to deserialize old state tree")?;
    let totalsize = new_state_skeleton.total_size();
    let mut new_tree = StateTree::from(new_state_skeleton);
    let patches = tree_diff::take_diff(&target_tree, &new_tree);

    for patch in &patches {
        log::debug!("Patch: {patch:?}");
    }
    patch::apply_patches(&mut new_tree, &target_tree, &patches);

    let res = serialize_tree_untagged(new_tree);
    debug_assert_eq!(res.len(), totalsize as usize);

    Ok(Some(res))
}
