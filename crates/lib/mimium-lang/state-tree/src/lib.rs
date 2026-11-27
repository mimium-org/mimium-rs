pub mod codec;
pub mod patch;
pub mod tree;
pub mod tree_diff;

use crate::patch::CopyFromPatch;
use crate::tree::{SizedType, StateTreeSkeleton};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StateStoragePatchPlan {
    pub total_size: usize,
    pub patches: Vec<CopyFromPatch>,
    /// The same migration restricted to `Feed` leaves (sorted by destination).
    ///
    /// Backends that keep `self` (Feed) values in a storage separate from delay/mem
    /// buffers (the WASM backend keeps them in linear memory) apply these to that
    /// storage, without touching the large delay ranges.
    pub feed_patches: Vec<CopyFromPatch>,
}

impl StateStoragePatchPlan {
    /// A plan that keeps every word in place (old and new layouts are identical).
    pub fn identity<T: SizedType>(skeleton: &StateTreeSkeleton<T>) -> Self {
        let total_size = skeleton.total_size() as usize;
        let patches = vec![CopyFromPatch {
            src_addr: 0,
            dst_addr: 0,
            size: total_size,
        }];
        let feed_patches = restrict_to_feed(&patches, skeleton);
        Self {
            total_size,
            patches,
            feed_patches,
        }
    }
}

/// Address ranges `(addr, size)` of all `Feed` leaves, in address order.
pub fn feed_ranges<T: SizedType>(skeleton: &StateTreeSkeleton<T>) -> Vec<(usize, usize)> {
    fn rec<T: SizedType>(s: &StateTreeSkeleton<T>, base: usize, out: &mut Vec<(usize, usize)>) {
        match s {
            StateTreeSkeleton::Feed(t) => {
                let n = t.word_size() as usize;
                if n > 0 {
                    out.push((base, n));
                }
            }
            StateTreeSkeleton::FnCall(children) => {
                let mut addr = base;
                for c in children {
                    rec(c, addr, out);
                    addr += c.total_size() as usize;
                }
            }
            StateTreeSkeleton::Delay { .. } | StateTreeSkeleton::Mem(_) => {}
        }
    }
    let mut out = vec![];
    rec(skeleton, 0, &mut out);
    out
}

/// Cut `patches` down to the parts that land on `Feed` leaves of `new_skeleton`.
///
/// A patch copies a subtree whose old and new shapes match, so a Feed leaf inside the
/// destination range comes from the same relative position in the source range.
pub fn restrict_to_feed<T: SizedType>(
    patches: &[CopyFromPatch],
    new_skeleton: &StateTreeSkeleton<T>,
) -> Vec<CopyFromPatch> {
    let ranges = feed_ranges(new_skeleton);
    let mut sorted = patches.to_vec();
    sorted.sort_by_key(|p| p.dst_addr);
    let mut out = vec![];
    let mut r = 0;
    for p in &sorted {
        let p_end = p.dst_addr + p.size;
        while r < ranges.len() && ranges[r].0 + ranges[r].1 <= p.dst_addr {
            r += 1;
        }
        let mut k = r;
        while k < ranges.len() && ranges[k].0 < p_end {
            let start = ranges[k].0.max(p.dst_addr);
            let end = (ranges[k].0 + ranges[k].1).min(p_end);
            if start < end {
                out.push(CopyFromPatch {
                    src_addr: p.src_addr + (start - p.dst_addr),
                    dst_addr: start,
                    size: end - start,
                });
            }
            k += 1;
        }
    }
    out
}

/// Build the patch plan that migrates old state storage into a new layout.
///
/// Returns `None` when both skeletons are identical: the old storage can then be
/// reused as is (see [`StateStoragePatchPlan::identity`] for an explicit plan).
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
    let feed_patches = restrict_to_feed(&patches, &new_state_skeleton);

    Some(StateStoragePatchPlan {
        total_size,
        patches,
        feed_patches,
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
