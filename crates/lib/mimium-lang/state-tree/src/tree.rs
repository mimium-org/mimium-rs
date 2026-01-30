pub const DELAY_ADDITIONAL_OFFSET: usize = 2;

/// State Tree structure.

//on attributes, see https://github.com/rkyv/rkyv/blob/main/rkyv/examples/json_like_schema.rs
#[derive(Clone, PartialEq, Eq)]
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
    FnCall(Vec<StateTree>),
}

impl std::fmt::Display for StateTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StateTree::Delay {
                readidx,
                writeidx,
                data,
            } => write!(
                f,
                "Delay(readidx: {}, writeidx: {}, data: {:?} ...)",
                readidx,
                writeidx,
                data.iter().take(10).collect::<Vec<&u64>>()
            ),
            StateTree::Mem { data } => write!(f, "Mem(data: {data:?})"),
            StateTree::Feed { data } => write!(f, "Feed(data: {data:?})"),
            StateTree::FnCall(children) => {
                let children_str: Vec<String> = children.iter().map(|c| format!("{c}")).collect();
                write!(f, "FnCall([{}])", children_str.join(", "))
            }
        }
    }
}
impl std::fmt::Debug for StateTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StateTree::Delay {
                readidx,
                writeidx,
                data,
            } => write!(
                f,
                "Delay(readidx: {}, writeidx: {}, data: {:?} ...)",
                readidx,
                writeidx,
                data.iter().take(10).collect::<Vec<&u64>>()
            ),
            StateTree::Mem { data } => write!(f, "Mem(data: {data:?})"),
            StateTree::Feed { data } => write!(f, "Feed(data: {data:?})"),
            StateTree::FnCall(children) => {
                let children_str: Vec<String> = children.iter().map(|c| format!("{c:?}")).collect();
                write!(f, "FnCall([{}])", children_str.join(", "))
            }
        }
    }
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

impl StateTree {
    /// パスを指定して、イミュータブルなノードへの参照を取得する
    pub fn get_node(&self, path: &[usize]) -> Option<&StateTree> {
        let mut current = self;
        for &index in path {
            if let StateTree::FnCall(children) = current {
                current = children.get(index)?;
            } else {
                // パスが深すぎるか、FnCallではないノードを指している
                return None;
            }
        }
        Some(current)
    }

    /// パスを指定して、ミュータブルなノードへの参照を取得する
    pub fn get_node_mut(&mut self, path: &[usize]) -> Option<&mut StateTree> {
        let mut current = self;
        for &index in path {
            if let StateTree::FnCall(children) = current {
                current = children.get_mut(index)?;
            } else {
                // パスが深すぎるか、FnCallではないノードを指している
                return None;
            }
        }
        Some(current)
    }

    /// StateTree から StateTreeSkeleton への変換（データを除いた構造のみ）
    pub fn to_skeleton(&self) -> StateTreeSkeleton<u64> {
        match self {
            StateTree::Delay { data, .. } => StateTreeSkeleton::Delay {
                len: data.len() as u64,
            },
            StateTree::Mem { data } => StateTreeSkeleton::Mem(data.len() as u64),
            StateTree::Feed { data } => StateTreeSkeleton::Feed(data.len() as u64),
            StateTree::FnCall(children) => StateTreeSkeleton::FnCall(
                children
                    .iter()
                    .map(|child| Box::new(child.to_skeleton()))
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

pub trait SizedType: std::fmt::Debug {
    fn word_size(&self) -> u64;
}

impl SizedType for u64 {
    fn word_size(&self) -> u64 {
        *self
    }
}

impl SizedType for usize {
    fn word_size(&self) -> u64 {
        *self as u64
    }
}

/// This data represents just a memory layout on a flat array, do not own actual data.
#[derive(Debug, Clone)]
pub enum StateTreeSkeleton<T: SizedType> {
    Delay {
        len: u64, //assume we are using only mono f64 data
    },
    Mem(T),
    Feed(T),
    FnCall(Vec<Box<StateTreeSkeleton<T>>>),
}
impl<T: SizedType> StateTreeSkeleton<T> {
    pub fn total_size(&self) -> u64 {
        match self {
            StateTreeSkeleton::Delay { len } => DELAY_ADDITIONAL_OFFSET as u64 + *len,
            StateTreeSkeleton::Mem(t) | StateTreeSkeleton::Feed(t) => t.word_size(),
            StateTreeSkeleton::FnCall(children_layout) => children_layout
                .iter()
                .map(|child_layout| child_layout.total_size())
                .sum(),
        }
    }

    /// Convert a path (position in the tree) to an address (offset) in a flat array.
    ///
    /// # Arguments
    /// * `path` - Path in the tree. Empty means root, [0] is the first child, [0, 1] is the second child of the first child.
    ///
    /// # Returns
    /// Returns the start address of the node pointed to by the path and the size of that node.
    /// Returns None if the path is invalid.
    pub fn path_to_address(&self, path: &[usize]) -> Option<(usize, usize)> {
        if path.is_empty() {
            // Root node case
            return Some((0, self.total_size() as usize));
        }

        match self {
            StateTreeSkeleton::FnCall(children) => {
                let child_idx = path[0];
                if child_idx >= children.len() {
                    return None;
                }

                // Calculate offset to the child node
                let offset: u64 = children
                    .iter()
                    .take(child_idx)
                    .map(|child| child.total_size())
                    .sum();

                // Recursively resolve the path within the child node
                let (child_offset, size) = children[child_idx].path_to_address(&path[1..])?;
                Some((offset as usize + child_offset, size))
            }
            // Error if path remains on a leaf node
            _ => None,
        }
    }
}
impl<T: SizedType> PartialEq for StateTreeSkeleton<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Delay { len: l_len }, Self::Delay { len: r_len }) => l_len == r_len,
            (Self::Mem(l0), Self::Mem(r0)) => l0.word_size() == r0.word_size(),
            (Self::Feed(l0), Self::Feed(r0)) => l0.word_size() == r0.word_size(),
            (Self::FnCall(l0), Self::FnCall(r0)) => l0 == r0,
            _ => false,
        }
    }
}

fn deserialize_tree_untagged_rec<T: SizedType>(
    data: &[u64],
    data_layout: &StateTreeSkeleton<T>,
) -> Option<(StateTree, usize)> {
    match data_layout {
        StateTreeSkeleton::Delay { len } => {
            let readidx = data.first().copied()?;
            let writeidx = data.get(1).copied()?;
            let d = data
                .get(DELAY_ADDITIONAL_OFFSET..DELAY_ADDITIONAL_OFFSET + (*len as usize))?
                .to_vec();
            Some((
                StateTree::Delay {
                    readidx,
                    writeidx,
                    data: d,
                },
                DELAY_ADDITIONAL_OFFSET + (*len as usize),
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
    log::trace!("Deserializing  with layout: {data_layout:?}");
    if let Some((tree, used)) = deserialize_tree_untagged_rec(data, data_layout) {
        if used == data.len() { Some(tree) } else { None }
    } else {
        None
    }
}
