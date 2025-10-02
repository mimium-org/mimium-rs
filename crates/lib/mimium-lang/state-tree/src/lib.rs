pub mod diff;
pub mod patch;
pub mod tree;

use rkyv::{access_mut, to_bytes};

use crate::tree::{
    ArchivedStateTree, SizedType, StateTree, StateTreeSkeleton, deserialize_tree_untagged,
    serialize_tree_untagged,
};

pub fn update_state_storage<T: SizedType>(
    old: &[u64],
    old_state_skeleton: StateTreeSkeleton<T>,
    new_state_skeleton: StateTreeSkeleton<T>,
) -> Result<Option<Vec<u64>>, Box<dyn std::error::Error>> {
    let mut target_tree = deserialize_tree_untagged(old, &old_state_skeleton)
        .ok_or("Failed to deserialize old state tree")?;
    let totalsize = new_state_skeleton.total_size();
    let new_empty_tree = StateTree::from(new_state_skeleton);
    let patches = diff::diff_live(&target_tree, &new_empty_tree);
    if patches.is_empty() {
        Ok(None)
    } else {
        for patch in &patches {
            log::info!("Patch: {:?}", patch);
        }
        patch::apply(&mut target_tree, &patches)
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        let res = serialize_tree_untagged(target_tree);
        debug_assert_eq!(res.len(), totalsize as usize);

        Ok(Some(res))
    }
}
