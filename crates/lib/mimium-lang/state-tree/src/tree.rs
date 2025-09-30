use rkyv::{Archive, Deserialize, Serialize};

/// State Tree structure.

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
        data: Vec<u64>, //assume we are using only mono f64 data
    },
    Feed {
        data: Vec<u64>, //assume we are using generic data, might be tuple of float
    },
    FnCall(#[rkyv(omit_bounds)] Vec<StateTree>),
}

impl<T: SizedType> From<StateTreeSkeleton<T>> for StateTree {
    //create empty StateTree from StateTreeSkeleton
    fn from(skeleton: StateTreeSkeleton<T>) -> Self {
        match skeleton {
            StateTreeSkeleton::Delay { len } => StateTree::Delay {
                readidx: 0,
                writeidx: 0,
                data: vec![0; len as usize],
            },
            StateTreeSkeleton::Mem(t) => StateTree::Mem {
                data: vec![0; t.word_size() as usize],
            },
            StateTreeSkeleton::Feed(t) => StateTree::Feed {
                data: vec![0; t.word_size() as usize],
            },
            StateTreeSkeleton::FnCall(children_layout) => StateTree::FnCall(
                children_layout
                    .into_iter()
                    .map(|child_layout| StateTree::from(*child_layout))
                    .collect(),
            ),
        }
    }
}

pub fn serialize_tree_untagged(tree: StateTree) -> Vec<u64> {
    match tree {
        StateTree::Delay {
            readidx,
            writeidx,
            data,
        } => itertools::concat([vec![readidx, writeidx], data]),
        StateTree::Mem { data } | StateTree::Feed { data } => data,
        StateTree::FnCall(state_trees) => {
            itertools::concat(state_trees.into_iter().map(serialize_tree_untagged))
        }
    }
}

pub trait SizedType {
    fn word_size(&self) -> u64;
}

/// This data represents just a memory layout on a flat array, do not own actual data.
#[derive(Debug, Clone, PartialEq)]
pub enum StateTreeSkeleton<T: SizedType> {
    Delay {
        len: u64, //assume we are using only mono f64 data
    },
    Mem(T),
    Feed(T),
    FnCall(Vec<Box<StateTreeSkeleton<T>>>),
}

fn deserialize_tree_untagged_rec<T: SizedType>(
    data: &[u64],
    data_layout: &StateTreeSkeleton<T>,
) -> Option<(StateTree, usize)> {
    match data_layout {
        StateTreeSkeleton::Delay { len } => {
            let readidx = data.first().copied()?;
            let writeidx = data.get(1).copied()?;
            let d = data.get(2..2 + (*len as usize))?.to_vec();
            Some((
                StateTree::Delay {
                    readidx,
                    writeidx,
                    data: d,
                },
                2 + (*len as usize),
            ))
        }
        StateTreeSkeleton::Mem(t) => {
            let size = t.word_size() as usize;
            let data = data.get(0..size)?.to_vec();
            Some((StateTree::Mem { data }, size))
        }
        StateTreeSkeleton::Feed(t) => {
            let size = t.word_size() as usize;
            let data = data.get(0..size)?.to_vec();
            Some((StateTree::Feed { data }, size))
        }
        StateTreeSkeleton::FnCall(children_layout) => {
            let (children, used) =
                children_layout
                    .iter()
                    .try_fold((vec![], 0), |(v, last_used), child_layout| {
                        let (child, used) =
                            deserialize_tree_untagged_rec(&data[last_used..], child_layout)?;

                        Some(([v, vec![child]].concat(), last_used + used))
                    })?;

            Some((StateTree::FnCall(children), used))
        }
    }
}

pub fn deserialize_tree_untagged<T: SizedType>(
    data: &[u64],
    data_layout: &StateTreeSkeleton<T>,
) -> Option<StateTree> {
    deserialize_tree_untagged_rec(data, data_layout)
        .and_then(|(tree, used)| (used == data.len()).then_some(tree))
}
