use anyhow::{anyhow, Result};
use mimium_lang::mir::StateType;
use mimium_lang::ExecContext;
use state_tree::patch::CopyFromPatch;
use state_tree::tree::{StateTree, StateTreeSkeleton};
use state_tree::tree_diff::take_diff;
use std::env;
use std::fs;

/// A simplified representation of a memory region in the StateTree.
#[derive(Debug, Clone)]
enum MemRegion {
    Leaf {
        label: String,
    },
    Group {
        label: String,
        children: Vec<MemRegion>,
    },
}
/// Builds a hierarchical memory map by traversing the StateTree.
fn build_memory_map(root: &StateTree) -> MemRegion {
    fn build_region(node: &StateTree, label_prefix: &str) -> MemRegion {
        match node {
            StateTree::Delay { data, .. } => MemRegion::Leaf {
                label: format!("{label_prefix}Delay(len={})", data.len()),
            },
            StateTree::Mem { data } => MemRegion::Leaf {
                label: format!("{label_prefix}Mem(len={})", data.len()),
            },
            StateTree::Feed { data } => MemRegion::Leaf {
                label: format!("{label_prefix}Feed(len={})", data.len()),
            },
            StateTree::FnCall(children) => {
                let child_regions = children
                    .iter()
                    .enumerate()
                    .map(|(i, child)| build_region(child, &format!("{label_prefix}[{i}].")))
                    .collect();
                MemRegion::Group {
                    label: format!("{label_prefix}FnCall"),
                    children: child_regions,
                }
            }
        }
    }

    build_region(root, "")
}

/// Generate Mermaid subgraph representation for a MemRegion
fn region_to_mermaid(region: &MemRegion, prefix: &str, indent: usize) -> String {
    let indent_str = "  ".repeat(indent);
    match region {
        MemRegion::Leaf { label } => {
            format!("{indent_str}{prefix}[\"{label}\"]\n")
        }
        MemRegion::Group { label, children } => {
            let mut out = String::new();
            out.push_str(&format!("{indent_str}subgraph {prefix} [\"{label}\"]\n"));
            out.push_str(&format!("{}direction TB\n", "  ".repeat(indent + 1)));

            for (i, child) in children.iter().enumerate() {
                let child_prefix = format!("{prefix}_{i}");
                out.push_str(&region_to_mermaid(child, &child_prefix, indent + 1));
            }

            out.push_str(&format!("{indent_str}end\n"));
            out
        }
    }
}

/// Find a node in the region tree by path
fn find_node_by_path<'a>(region: &'a MemRegion, path: &[usize]) -> Option<&'a MemRegion> {
    if path.is_empty() {
        return Some(region);
    }

    match region {
        MemRegion::Leaf { .. } => None,
        MemRegion::Group { children, .. } => {
            let first = path[0];
            children
                .get(first)
                .and_then(|child| find_node_by_path(child, &path[1..]))
        }
    }
}

/// Generate a node ID from a path
fn path_to_id(prefix: &str, path: &[usize]) -> String {
    if path.is_empty() {
        prefix.to_string()
    } else {
        format!(
            "{}_{}",
            prefix,
            path.iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join("_")
        )
    }
}

/// Converts the patches to a Mermaid flowchart string.
fn patches_to_mermaid(
    old_map: &MemRegion,
    new_map: &MemRegion,
    patches: impl Iterator<Item = CopyFromPatch>,
) -> String {
    let mut out = String::new();
    out.push_str("flowchart LR\n");

    out.push_str("  subgraph OldMemory\n");
    out.push_str("  direction TB\n");
    out.push_str(&region_to_mermaid(old_map, "O", 2));
    out.push_str("  end\n");

    out.push_str("  subgraph NewMemory\n");
    out.push_str("  direction TB\n");
    out.push_str(&region_to_mermaid(new_map, "N", 2));
    out.push_str("  end\n\n");

    for p in patches {
        let old_node = find_node_by_path(old_map, &p.old_path);
        let new_node = find_node_by_path(new_map, &p.new_path);

        match (old_node, new_node) {
            (Some(MemRegion::Leaf { .. }), Some(MemRegion::Leaf { .. })) => {
                let old_id = path_to_id("O", &p.old_path);
                let new_id = path_to_id("N", &p.new_path);
                out.push_str(&format!("  {old_id} -->|copy| {new_id}\n"));
            }
            (Some(_), Some(_)) => {
                // Both are groups - still generate a link
                let old_id = path_to_id("O", &p.old_path);
                let new_id = path_to_id("N", &p.new_path);
                out.push_str(&format!("  {old_id} -.->|copy| {new_id}\n"));
            }
            _ => {
                out.push_str(&format!(
                    "  %% missing mapping for {:?} -> {:?}\n",
                    p.old_path, p.new_path
                ));
            }
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
    let patches = take_diff(&old_state_skeleton, &new_state_skeleton);
    let old_tree = StateTree::from(old_state_skeleton);
    let new_tree = StateTree::from(new_state_skeleton);
    eprintln!(
        "Computed patches:\n{}",
        patches
            .iter()
            .map(|p| format!("{p:?}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
    let old_map = build_memory_map(&old_tree);
    let new_map = build_memory_map(&new_tree);
    let mermaid = patches_to_mermaid(&old_map, &new_map, patches.into_iter());

    println!("{mermaid}");

    Ok(())
}
