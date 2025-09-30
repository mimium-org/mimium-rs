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
    let new_empty_tree = StateTree::from(new_state_skeleton);
    let mut original_bytes = to_bytes::<rkyv::rancor::Error>(&target_tree)
        .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
    let mut modified_bytes = to_bytes::<rkyv::rancor::Error>(&new_empty_tree)
        .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
    let archived_original =
        access_mut::<ArchivedStateTree, rkyv::rancor::Error>(&mut original_bytes)
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
    let archived_modified =
        access_mut::<ArchivedStateTree, rkyv::rancor::Error>(&mut modified_bytes)
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
    let patches = diff::diff(&archived_original, &archived_modified, &[]);
    if patches.is_empty() {
        Ok(None)
    } else {
        patch::apply(&mut target_tree, &patches)
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        Ok(Some(serialize_tree_untagged(target_tree)))
    }
}
