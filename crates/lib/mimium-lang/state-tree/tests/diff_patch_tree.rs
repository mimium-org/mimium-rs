use rkyv::{access_mut, to_bytes};
use state_tree::diff::diff;
use state_tree::patch::Patch;
use state_tree::tree::ArchivedStateTree;
use state_tree::{patch::apply, tree::StateTree};

#[test]
fn test_main() {
    // --- 1. Create old version of the tree ---
    let old_tree = StateTree::FnCall(vec![
        StateTree::Mem {
            data: vec![10, 20].into_boxed_slice(),
        },
        StateTree::FnCall(vec![StateTree::Delay {
            typesize: 8,
            readidx: 0,
            writeidx: 1,
            data: vec![100, 200].into_boxed_slice(),
        }]),
    ]);

    // --- 2. Serialize old tree with rkyv ---
    let mut old_bytes =
        to_bytes::<rkyv::rancor::Error>(&old_tree).expect("Failed to serialize old_tree");

    // --- 3. Create new version of the tree ---
    let new_tree = StateTree::FnCall(vec![
        StateTree::Mem {
            data: vec![0, 0].into_boxed_slice(), // Same data size
        },
        StateTree::FnCall(vec![StateTree::Delay {
            typesize: 8,
            readidx: 1,
            writeidx: 2,
            data: vec![0, 0, 0].into_boxed_slice(), // Size changed, so replace
        }]),
        StateTree::Mem {
            // Newly inserted
            data: vec![0].into_boxed_slice(),
        },
    ]);
    let mut new_bytes =
        to_bytes::<rkyv::rancor::Error>(&new_tree).expect("Failed to serialize new_tree");

    // --- 4. Detect differences from two byte arrays with zero-copy ---
    let archived_old = access_mut::<ArchivedStateTree, rkyv::rancor::Error>(&mut old_bytes)
        .expect("Failed to access archived_old");
    let archived_new = access_mut::<ArchivedStateTree, rkyv::rancor::Error>(&mut new_bytes)
        .expect("Failed to access archived_new");

    let mut patches = Vec::new();
    let path = Vec::new();
    patches.extend(diff(&archived_old, &archived_new, &path));

    println!("✅ Detected differences (Patches):");
    for patch in &patches {
        println!("  - {:?}", patch);
    }
    let patch_answer = vec![
        Patch::Replace {
            path: vec![1, 0],
            new_tree: StateTree::Delay {
                typesize: 8,
                readidx: 1,
                writeidx: 2,
                data: vec![0, 0, 0].into_boxed_slice(), // Size changed, so replace
            },
        },
        Patch::Insert {
            parent_path: vec![],
            index: 2,
            new_tree: StateTree::Mem {
                data: vec![0].into_boxed_slice(),
            },
        },
    ];
    assert_eq!(patches, patch_answer);
    // --- 5. Apply patches to copy of old tree for verification ---
    let mut tree_to_patch = old_tree.clone();
    apply(&mut tree_to_patch, &patches).expect("Failed to apply patches");

    println!("\n✅ Tree after patch application:");
    println!("{:#?}", tree_to_patch);

    let ans = StateTree::FnCall(vec![
        StateTree::Mem {
            data: vec![10, 20].into_boxed_slice(), // Data content was replaced with old
        },
        StateTree::FnCall(vec![StateTree::Delay {
            typesize: 8,
            readidx: 1,
            writeidx: 2,
            data: vec![0, 0, 0].into_boxed_slice(),
        }]),
        StateTree::Mem {
            // Newly inserted
            data: vec![0].into_boxed_slice(),
        },
    ]);
    // --- 6. Check if the patched tree matches the new tree ---
    assert_eq!(tree_to_patch, ans);
    println!("\n✅ Verification successful: The patched tree matches the new tree.");
}
