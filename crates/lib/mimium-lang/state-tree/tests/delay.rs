use rkyv::{access_mut, to_bytes};
use state_tree::tree::{ArchivedStateTree, StateTree};

#[test]
fn archived_delay_update() {
    let tree = StateTree::Delay {
        readidx: 0,
        writeidx: 0,
        data: vec![0, 0, 0, 0],
    };
    let mut new_bytes =
        to_bytes::<rkyv::rancor::Error>(&tree).expect("Failed to serialize new_tree");
    let archived_new = access_mut::<ArchivedStateTree, rkyv::rancor::Error>(&mut new_bytes)
        .expect("Failed to access archived_new");
    let hoge = unsafe { archived_new.unseal_unchecked() };
    match hoge {
        ArchivedStateTree::Delay {
            readidx, writeidx, ..
        } => {
            *readidx = (u64::from(*readidx) + 1u64).into();
            *writeidx = (u64::from(*writeidx) + 1u64).into();
        }
        _ => panic!(),
    };
    let readidx = *match hoge {
        ArchivedStateTree::Delay { readidx, .. } => readidx,
        _ => panic!(),
    };
    let writeidx = *match hoge {
        ArchivedStateTree::Delay { writeidx, .. } => writeidx,
        _ => panic!(),
    };
    assert_eq!(readidx, 1);
    assert_eq!(writeidx, 1);
    //read again from same memory and check values is correctly updated
    let archived_new = unsafe {
        access_mut::<ArchivedStateTree, rkyv::rancor::Error>(&mut new_bytes)
            .expect("Failed to access archived_new")
            .unseal_unchecked()
    };
    let readidx = *match archived_new {
        ArchivedStateTree::Delay { readidx, .. } => readidx,
        _ => panic!(),
    };
    let writeidx = *match archived_new {
        ArchivedStateTree::Delay { writeidx, .. } => writeidx,
        _ => panic!(),
    };
    assert_eq!(readidx, 1);
    assert_eq!(writeidx, 1);
}
