use anyhow::{anyhow, Result};
use mimium_lang::ExecContext;
use mimium_lang::mir::StateType;
use state_tree::patch::CopyFromPatch;
use state_tree::tree::{StateTree, StateTreeSkeleton};
use state_tree::tree_diff::take_diff;
use std::collections::BTreeMap;
use std::env;
use std::fs;

/// A simplified representation of a memory region in the StateTree.
#[derive(Debug, Clone)]
struct MemRegion {
    path: Vec<usize>,
    group_path: Vec<usize>,
    len: usize,
    label: String,
}

/// Builds a memory map by traversing the StateTree in pre-order.
fn build_memory_map(root: &StateTree) -> Vec<MemRegion> {
    let mut regions = Vec::new();
    let mut offset = 0usize;

    fn walk(
        node: &StateTree,
        path: &mut Vec<usize>,
        group_path: &mut Vec<usize>,
        offset: &mut usize,
        regions: &mut Vec<MemRegion>,
    ) {
        match node {
            StateTree::Delay { data, .. } => {
                let len = data.len();
                regions.push(MemRegion {
                    path: path.clone(),
                    group_path: group_path.clone(),
                    len,
                    label: format!("Delay@{} (len={})", *offset, len),
                });
                *offset += len;
            }
            StateTree::Mem { data } => {
                let len = data.len();
                regions.push(MemRegion {
                    path: path.clone(),
                    group_path: group_path.clone(),
                    len,
                    label: format!("Mem@{} (len={})", *offset, len),
                });
                *offset += len;
            }
            StateTree::Feed { data } => {
                let len = data.len();
                regions.push(MemRegion {
                    path: path.clone(),
                    group_path: group_path.clone(),
                    len,
                    label: format!("Feed@{} (len={})", *offset, len),
                });
                *offset += len;
            }
            StateTree::FnCall(children) => {
                // When entering a FnCall, update the group_path to the current path
                let prev_group_path = group_path.clone();
                *group_path = path.clone();
                
                for (i, child) in children.iter().enumerate() {
                    path.push(i);
                    walk(child, path, group_path, offset, regions);
                    path.pop();
                }
                
                // Restore the previous group_path when exiting the FnCall
                *group_path = prev_group_path;
            }
        }
    }

    let mut path = Vec::new();
    let mut group_path = Vec::new();
    walk(root, &mut path, &mut group_path, &mut offset, &mut regions);
    regions
}

fn generate_subgraph(
    prefix: &str,
    regions: &[MemRegion],
    current_group_path: &[usize],
) -> String {
    let mut out = String::new();
    let mut direct_children_indices = Vec::new();
    let mut nested_subgroups = BTreeMap::new();

    // Collect all regions that belong to the current group or its subgroups
    for (i, region) in regions.iter().enumerate() {
        // Check if the region's group_path starts with the current_group_path
        if region.group_path.len() >= current_group_path.len()
            && region.group_path[..current_group_path.len()] == current_group_path[..]
        {
            // If the region's group_path is exactly the same, it's a direct child memory region
            if region.group_path.len() == current_group_path.len() {
                direct_children_indices.push(i);
            }
            // If the region's group_path is longer, it belongs to a nested subgroup
            else if region.group_path.len() > current_group_path.len() {
                let next_group_segment = region.group_path[current_group_path.len()];
                let mut next_group_path = current_group_path.to_vec();
                next_group_path.push(next_group_segment);
                nested_subgroups.insert(next_group_path, true);
            }
        }
    }

    // Render direct memory regions (Delay, Mem, Feed)
    for &idx in &direct_children_indices {
        let node_id = format!("{}mem{}", prefix, idx);
        out.push_str(&format!(
            "    {}[\"{}\"]\n",
            node_id, regions[idx].label
        ));
    }

    // Render nested subgroups (FnCalls)
    for (subgroup_path, _) in &nested_subgroups {
        let subgroup_id = format!(
            "{}{}",
            prefix,
            subgroup_path
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join("_")
        );
        out.push_str(&format!(
            "  subgraph {subgroup_id} [\"FnCall {subgroup_path:?}\"]\n"
        ));
        out.push_str(&generate_subgraph(
            prefix,
            regions,
            subgroup_path,
        ));
        out.push_str("  end\n");
    }

    out
}

/// Converts the patches to a Mermaid flowchart string.
fn patches_to_mermaid(
    old_map: &[MemRegion],
    new_map: &[MemRegion],
    patches: &[CopyFromPatch],
) -> String {
    let mut by_old_path: BTreeMap<Vec<usize>, &MemRegion> = BTreeMap::new();
    for r in old_map {
        by_old_path.insert(r.path.clone(), r);
    }
    let mut by_new_path: BTreeMap<Vec<usize>, &MemRegion> = BTreeMap::new();
    for r in new_map {
        by_new_path.insert(r.path.clone(), r);
    }

    let mut out = String::new();
    out.push_str("flowchart LR\n");

    eprintln!("\nOld memory map:");
    for (i, region) in old_map.iter().enumerate() {
        eprintln!("  [{}] path: {:?}, group_path: {:?}, label: {}", i, region.path, region.group_path, region.label);
    }
    eprintln!("\nNew memory map:");
    for (i, region) in new_map.iter().enumerate() {
        eprintln!("  [{}] path: {:?}, group_path: {:?}, label: {}", i, region.path, region.group_path, region.label);
    }

    out.push_str("  subgraph OldMemory\n");
    out.push_str(&generate_subgraph("O", old_map, &[]));
    out.push_str("  end\n");

    out.push_str("  subgraph NewMemory\n");
    out.push_str(&generate_subgraph("N", new_map, &[]));
    out.push_str("  end\n\n");

    for p in patches {
        if let (Some(src), Some(dst)) = (by_old_path.get(&p.old_path), by_new_path.get(&p.new_path))
        {
            if let (Some(src_idx), Some(dst_idx)) = (
                old_map.iter().position(|x| x.path == src.path),
                new_map.iter().position(|x| x.path == dst.path),
            ) {
                out.push_str(&format!(
                    "  Omem{src_idx} -->|copy {}| Nmem{dst_idx}\n",
                    src.len
                ));
            }
        } else {
            out.push_str(&format!(
                "  %% missing mapping for {:?} -> {:?}\n",
                p.old_path, p.new_path
            ));
        }
    }

    out
}

/// Compiles a mimium file and returns its StateTree.
fn get_state_tree_from_file(file_path: &str) -> Result<StateTreeSkeleton<StateType>> {
    let source = fs::read_to_string(file_path)?;
    let mut context = ExecContext::new([].into_iter(), Some(file_path.into()), Default::default());
    context.prepare_compiler();
    let compiler = context
        .get_compiler_mut()
        .ok_or(anyhow!("compiler not found"))?;
    let bytecode = compiler.emit_bytecode(&source).map_err(|errors| {
        anyhow!(errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n"))
    })?;
    let dspfn = bytecode.get_dsp_fn().ok_or(anyhow!("dsp not found"))?;
    Ok(dspfn.state_skeleton.clone())
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} <old_file.mmm> <new_file.mmm>", args[0]);
        std::process::exit(1);
    }

    let old_file = &args[1];
    let new_file = &args[2];

    let old_state_skeleton = get_state_tree_from_file(old_file)?;
    let new_state_skeleton = get_state_tree_from_file(new_file)?;
    let old_tree = StateTree::from(old_state_skeleton);
    let new_tree = StateTree::from(new_state_skeleton);
    let patches = take_diff(&old_tree, &new_tree);
    eprintln!("Computed patches:\n{patches:?}");
    let old_map = build_memory_map(&old_tree);
    let new_map = build_memory_map(&new_tree);
    let mermaid = patches_to_mermaid(&old_map, &new_map, &patches);

    println!("{mermaid}");

    Ok(())
}
