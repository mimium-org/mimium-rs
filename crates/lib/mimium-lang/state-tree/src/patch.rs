use crate::tree::StateTree;

/// ツリー内のノードへのパス
pub type Path = Vec<usize>;

/// 変更操作を表すenum
#[derive(Debug, PartialEq, Clone)]
pub enum Patch {
    /// 既存のノードを置き換える
    Replace { path: Path, new_tree: StateTree },
    /// FnCallノードの子リストに新しいノードを挿入する
    Insert {
        parent_path: Path,
        index: usize,
        new_tree: StateTree,
    },
    /// FnCallノードの子リストからノードを削除する
    Remove { parent_path: Path, index: usize },
}

#[derive(Debug)]
pub enum ApplyError {
    PathNotFound,
    InvalidIndex,
    NotFnCall, // 親がFnCallではない場合のエラー
}

/// StateTreeにPatchを適用する
pub fn apply(root: &mut StateTree, patches: &[Patch]) -> Result<(), ApplyError> {
    for patch in patches {
        match patch {
            Patch::Replace { path, new_tree } => {
                let node_to_replace = find_node_mut(root, path)?;
                *node_to_replace = new_tree.clone();
            }
            Patch::Insert {
                parent_path,
                index,
                new_tree,
            } => {
                let parent_node = find_node_mut(root, parent_path)?;
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
                let parent_node = find_node_mut(root, parent_path)?;
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

// 可変なStateTreeノードへの参照を探すヘルパー
fn find_node_mut<'a>(
    root: &'a mut StateTree,
    path: &Path,
) -> Result<&'a mut StateTree, ApplyError> {
    let mut current = root;
    for &index in path {
        if let StateTree::FnCall(children) = current {
            current = children.get_mut(index).ok_or(ApplyError::PathNotFound)?;
        } else {
            // パスの途中でFnCallでないノードに行き当たった
            return Err(ApplyError::PathNotFound);
        }
    }
    Ok(current)
}
