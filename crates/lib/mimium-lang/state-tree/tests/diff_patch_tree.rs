use rkyv::{access_mut, to_bytes};
use state_tree::diff::diff;
use state_tree::patch::Patch;
use state_tree::tree::ArchivedStateTree;
use state_tree::{patch::apply, tree::StateTree};

#[test]
fn test_main() {
    // --- 1. 古いバージョンのツリーを作成 ---
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

    // --- 2. 古いツリーをrkyvでシリアライズ ---
    let mut old_bytes =
        to_bytes::<rkyv::rancor::Error>(&old_tree).expect("Failed to serialize old_tree");

    // --- 3. 新しいバージョンのツリーを作成 ---
    let new_tree = StateTree::FnCall(vec![
        StateTree::Mem {
            data: vec![0, 0].into_boxed_slice(), // データのサイズはおなじ
        },
        StateTree::FnCall(vec![StateTree::Delay {
            typesize: 8,
            readidx: 1,
            writeidx: 2,
            data: vec![0, 0, 0].into_boxed_slice(), //サイズ変更なので差し替え
        }]),
        StateTree::Mem {
            // 新しく挿入
            data: vec![0].into_boxed_slice(),
        },
    ]);
    let mut new_bytes =
        to_bytes::<rkyv::rancor::Error>(&new_tree).expect("Failed to serialize new_tree");

    // --- 4. 2つのバイト列からゼロコピーで差分を検出 ---
    let archived_old = access_mut::<ArchivedStateTree, rkyv::rancor::Error>(&mut old_bytes)
        .expect("Failed to access archived_old");
    let archived_new = access_mut::<ArchivedStateTree, rkyv::rancor::Error>(&mut new_bytes)
        .expect("Failed to access archived_new");

    let mut patches = Vec::new();
    let mut path = Vec::new();
    diff(&archived_old, &archived_new, &mut path, &mut patches);

    println!("✅ 検出された差分 (Patches):");
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
                data: vec![0, 0, 0].into_boxed_slice(), //サイズ変更なので差し替え
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
    // --- 5. 古いツリーのコピーにパッチを適用して検証 ---
    let mut tree_to_patch = old_tree.clone();
    apply(&mut tree_to_patch, &patches).expect("Failed to apply patches");

    println!("\n✅ パッチ適用後のツリー:");
    println!("{:#?}", tree_to_patch);

    let ans = StateTree::FnCall(vec![
        StateTree::Mem {
            data: vec![10, 20].into_boxed_slice(), //データの中身がoldで差し替えられている
        },
        StateTree::FnCall(vec![StateTree::Delay {
            typesize: 8,
            readidx: 1,
            writeidx: 2,
            data: vec![0, 0, 0].into_boxed_slice(),
        }]),
        StateTree::Mem {
            // 新しく挿入
            data: vec![0].into_boxed_slice(),
        },
    ]);
    // --- 6. パッチ適用後のツリーが、新しいツリーと一致するか確認 ---
    assert_eq!(tree_to_patch, ans);
    println!("\n✅ 検証成功: パッチ適用後のツリーは新しいツリーと一致します。");
}
