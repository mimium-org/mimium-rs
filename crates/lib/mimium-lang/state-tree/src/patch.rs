use crate::tree::StateTree;

/// Path to a node in the tree
pub type Path = Vec<usize>;

/// Enum representing change operations
#[derive(Debug, PartialEq, Clone)]
pub enum Patch {
    /// Replace an existing node
    Replace { path: Path, new_tree: StateTree },
    /// Insert a new node into the child list of a FnCall node
    Insert {
        parent_path: Path,
        index: usize,
        new_tree: StateTree,
    },
    /// Remove a node from the child list of a FnCall node
    Remove { parent_path: Path, index: usize },
}

#[derive(Debug)]
pub enum ApplyError {
    PathNotFound,
    InvalidIndex,
    NotFnCall, // Error when parent is not a FnCall
}

/// Apply patches to StateTree
pub fn apply(root: &mut StateTree, patches: &[Patch]) -> Result<(), ApplyError> {
    for patch in patches {
        match patch {
            Patch::Replace { path, new_tree } => {
                let node_to_replace = find_node_mut(root, path.iter())?;
                *node_to_replace = new_tree.clone();
            }
            Patch::Insert {
                parent_path,
                index,
                new_tree,
            } => {
                let parent_node = find_node_mut(root, parent_path.iter())?;
                if let StateTree::FnCall(children) = parent_node {
                    if *index > children.len() {
                        return Err(ApplyError::InvalidIndex);
                    }
                    children.insert(*index, new_tree.clone());
                } else {
                    return Err(ApplyError::NotFnCall);
                }
            }
            Patch::Remove { parent_path, index } => {
                let parent_node = find_node_mut(root, parent_path.iter())?;
                if let StateTree::FnCall(children) = parent_node {
                    if *index >= children.len() {
                        return Err(ApplyError::InvalidIndex);
                    }
                    children.remove(*index);
                } else {
                    return Err(ApplyError::NotFnCall);
                }
            }
        }
    }
    Ok(())
}

// Helper to find a mutable reference to a StateTree node
fn find_node_mut<'a>(
    tree: &'a mut StateTree,
    mut path_i: impl Iterator<Item = &'a usize>,
) -> Result<&'a mut StateTree, ApplyError> {
    if let Some(i) = path_i.next() {
        if let StateTree::FnCall(children) = tree {
            let c = children.get_mut(*i).ok_or(ApplyError::PathNotFound)?;
            find_node_mut(c, path_i)
        } else {
            // Hit a non-FnCall node in the middle of the path
            Err(ApplyError::PathNotFound)
        }
    } else {
        Ok(tree) //empty path means the root
    }
}
