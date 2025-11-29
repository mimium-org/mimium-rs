pub mod patch;
pub mod tree;
pub mod tree_diff;

use crate::tree::{SizedType, StateTreeSkeleton};

pub fn update_state_storage<T: SizedType + PartialEq>(
    old: &[u64],
    old_state_skeleton: StateTreeSkeleton<T>,
    new_state_skeleton: StateTreeSkeleton<T>,
) -> Result<Option<Vec<u64>>, Box<dyn std::error::Error>> {
    if old_state_skeleton == new_state_skeleton {
        return Ok(None);
    }

    let totalsize = new_state_skeleton.total_size();
    let patches = tree_diff::take_diff(&old_state_skeleton, &new_state_skeleton)
        .into_iter()
        .collect::<Vec<_>>();

    // 新しいストレージを0で初期化
    let mut new_storage = vec![0u64; totalsize as usize];

    for patch in &patches {
        log::debug!("Patch: {patch:?}");
    }

    // フラットな配列から配列へパッチを適用
    patch::apply_patches(&mut new_storage, old, patches.as_slice());

    debug_assert_eq!(new_storage.len(), totalsize as usize);

    Ok(Some(new_storage))
}
