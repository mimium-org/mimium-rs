pub mod patch;
pub mod tree;
pub mod tree_diff;

use crate::patch::CopyFromPatch;
use crate::tree::{SizedType, StateTreeSkeleton};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StateStoragePatchPlan {
    pub total_size: usize,
    pub patches: Vec<CopyFromPatch>,
}

pub fn build_state_storage_patch_plan<T: SizedType + PartialEq>(
    old_state_skeleton: StateTreeSkeleton<T>,
    new_state_skeleton: StateTreeSkeleton<T>,
) -> Option<StateStoragePatchPlan> {
    if old_state_skeleton == new_state_skeleton {
        return None;
    }

    let total_size = new_state_skeleton.total_size() as usize;
    let patches = tree_diff::take_diff(&old_state_skeleton, &new_state_skeleton)
        .into_iter()
        .collect::<Vec<_>>();

    Some(StateStoragePatchPlan {
        total_size,
        patches,
    })
}

pub fn apply_state_storage_patch_plan(
    old_storage: &[u64],
    patch_plan: &StateStoragePatchPlan,
) -> Vec<u64> {
    let mut new_storage = vec![0u64; patch_plan.total_size];
    patch::apply_patches(&mut new_storage, old_storage, patch_plan.patches.as_slice());

    debug_assert_eq!(new_storage.len(), patch_plan.total_size);

    new_storage
}

pub fn update_state_storage<T: SizedType + PartialEq>(
    old: &[u64],
    old_state_skeleton: StateTreeSkeleton<T>,
    new_state_skeleton: StateTreeSkeleton<T>,
) -> Result<Option<Vec<u64>>, Box<dyn std::error::Error>> {
    let patch_plan = build_state_storage_patch_plan(old_state_skeleton, new_state_skeleton);
    if let Some(plan) = patch_plan {
        for patch in &plan.patches {
            log::debug!("Patch: {patch:?}");
        }

        Ok(Some(apply_state_storage_patch_plan(old, &plan)))
    } else {
        Ok(None)
    }
}
