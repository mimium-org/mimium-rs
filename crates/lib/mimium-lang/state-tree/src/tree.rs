use rkyv::{Archive, Deserialize, Serialize};

/// State Tree structure.
/// This data represents just a memory layout on a flat array, do not own actual data.

//on attributes, see https://github.com/rkyv/rkyv/blob/main/rkyv/examples/json_like_schema.rs
#[derive(Archive, Deserialize, Serialize, Debug, PartialEq, Clone)]
// Use `attr` to make rkyv implement traits on the generated Archived type
#[rkyv(attr(derive(Debug, PartialEq)))]
#[rkyv(serialize_bounds(
    __S: rkyv::ser::Writer + rkyv::ser::Allocator,
    __S::Error: rkyv::rancor::Source,
))]
#[rkyv(deserialize_bounds(__D::Error: rkyv::rancor::Source))]
#[rkyv(bytecheck(
    bounds(
        __C: rkyv::validation::ArchiveContext,
    )
))]
pub enum StateTree {
    Delay {
        readidx: u64,
        writeidx: u64,
        data: Vec<u64>, //assume we are using only mono f64 data
    },
    Mem {
        data: u64, //assume we are using only mono f64 data
    },
    Feed {
        data: Vec<u64>, //assume we are using generic data, might be tuple of float
    },
    FnCall(#[rkyv(omit_bounds)] Vec<StateTree>),
}

fn serialize_tree_untagged(tree: &StateTree) -> Vec<u64> {
    match tree {
        StateTree::Delay {
            readidx,
            writeidx,
            data,
        } => {
            let mut v = Vec::with_capacity(2 + data.len());
            v.push(*readidx);
            v.push(*writeidx);
            v.extend(data.iter());
            v
        }
        StateTree::Mem { data } => vec![*data],
        StateTree::Feed { data } => {
            let mut v = Vec::with_capacity(data.len());
            v.extend(data.iter());
            v
        }
        StateTree::FnCall(state_trees) => state_trees
            .iter()
            .flat_map(serialize_tree_untagged)
            .collect(),
    }
}

pub enum StateTreeSkeleton {
    Delay {
        len: u64, //assume we are using only mono f64 data
    },
    Mem,
    Feed {
        size: u64, //assume we are using generic data, might be tuple of float
    },
    FnCall(Vec<Box<StateTreeSkeleton>>),
}

fn deserialize_tree_untagged_rec(
    data: &[u64],
    data_layout: &StateTreeSkeleton,
) -> Option<(StateTree, usize)> {
    match data_layout {
        StateTreeSkeleton::Delay { len } => {
            if data.len() < 2 + (*len as usize) {
                return None;
            }
            let readidx = data[0];
            let writeidx = data[1];
            let d = data[2..2 + (*len as usize)].to_vec();
            Some((
                StateTree::Delay {
                    readidx,
                    writeidx,
                    data: d,
                },
                2 + (*len as usize),
            ))
        }
        StateTreeSkeleton::Mem => {
            if data.is_empty() {
                return None;
            }
            Some((StateTree::Mem { data: data[0] }, 1))
        }
        StateTreeSkeleton::Feed { size } => {
            if data.len() < (*size as usize) {
                return None;
            }
            let d = data[0..(*size as usize)].to_vec();
            Some((StateTree::Feed { data: d }, *size as usize))
        }
        StateTreeSkeleton::FnCall(children_layout) => {
            let mut offset = 0;
            let mut children = Vec::with_capacity(children_layout.len());
            for child_layout in children_layout {
                if let Some((child, used)) =
                    deserialize_tree_untagged_rec(&data[offset..], child_layout)
                {
                    children.push(child);
                    offset += used;
                } else {
                    return None;
                }
            }
            Some((StateTree::FnCall(children), offset))
        }
    }
}

fn deserialize_tree_untagged(data: &[u64], data_layout: &StateTreeSkeleton) -> Option<StateTree> {
    if let Some((tree, used)) = deserialize_tree_untagged_rec(data, data_layout) {
        if used == data.len() { Some(tree) } else { None }
    } else {
        None
    }
}
