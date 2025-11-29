use anyhow::{anyhow, Result};
use mimium_lang::mir::StateType;
use mimium_lang::ExecContext;
use state_tree::patch::CopyFromPatch;
use state_tree::tree::StateTreeSkeleton;
use state_tree::tree_diff::take_diff;
use std::env;
use std::fs;

/// A memory block representation with address and size information.
#[derive(Debug, Clone)]
struct MemBlock {
    addr: usize,
    size: usize,
    label: String,
    path: Vec<usize>,
    node_type: String,
}

/// Builds a flat list of memory blocks from StateTreeSkeleton.
fn build_memory_blocks(skeleton: &StateTreeSkeleton<StateType>) -> Vec<MemBlock> {
    fn traverse(
        skeleton: &StateTreeSkeleton<StateType>,
        path: &[usize],
        root_skeleton: &StateTreeSkeleton<StateType>,
        blocks: &mut Vec<MemBlock>,
    ) {
        let (addr, size) = root_skeleton
            .path_to_address(path)
            .expect("Invalid path during traversal");

        let path_str = if path.is_empty() {
            "root".to_string()
        } else {
            format!(
                "[{}]",
                path.iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        match skeleton {
            StateTreeSkeleton::Delay { len } => {
                blocks.push(MemBlock {
                    addr,
                    size,
                    label: format!("{path_str} Delay(len={len})"),
                    path: path.to_vec(),
                    node_type: "Delay".to_string(),
                });
            }
            StateTreeSkeleton::Mem(_) => {
                blocks.push(MemBlock {
                    addr,
                    size,
                    label: format!("{path_str} Mem(size={size})"),
                    path: path.to_vec(),
                    node_type: "Mem".to_string(),
                });
            }
            StateTreeSkeleton::Feed(_) => {
                blocks.push(MemBlock {
                    addr,
                    size,
                    label: format!("{path_str} Feed(size={size})"),
                    path: path.to_vec(),
                    node_type: "Feed".to_string(),
                });
            }
            StateTreeSkeleton::FnCall(children) => {
                for (i, child) in children.iter().enumerate() {
                    let mut child_path = path.to_vec();
                    child_path.push(i);
                    traverse(child, &child_path, root_skeleton, blocks);
                }
            }
        }
    }

    let mut blocks = Vec::new();
    traverse(skeleton, &[], skeleton, &mut blocks);
    blocks.sort_by_key(|b| b.addr);
    blocks
}

/// Group blocks by their common path prefix to create hierarchical structure
fn group_blocks_by_path(blocks: &[MemBlock]) -> Vec<(Vec<usize>, Vec<MemBlock>)> {
    use std::collections::BTreeMap;
    
    let mut groups: BTreeMap<Vec<usize>, Vec<MemBlock>> = BTreeMap::new();
    
    for block in blocks {
        // For paths with length > 2, group by taking first N-2 elements
        // This creates more meaningful hierarchical groups
        let prefix = if block.path.len() > 2 {
            block.path[..block.path.len() - 2].to_vec()
        } else if block.path.len() == 2 {
            vec![block.path[0]]
        } else {
            vec![]
        };
        groups.entry(prefix).or_default().push(block.clone());
    }
    
    groups.into_iter().collect()
}

/// Generate Mermaid representation with hierarchical subgraphs based on path
fn blocks_to_mermaid_hierarchical(blocks: &[MemBlock], prefix: &str) -> String {
    let mut out = String::new();
    let groups = group_blocks_by_path(blocks);
    
    for (group_path, group_blocks) in groups {
        let group_name = if group_path.is_empty() {
            "root".to_string()
        } else {
            format!(
                "[{}]",
                group_path
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        
        let subgraph_id = format!(
            "{}_group_{}",
            prefix,
            group_path
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join("_")
        );
        
        if group_blocks.len() > 1 {
            out.push_str(&format!("    subgraph {subgraph_id} [\"{group_name}\"]\n"));
            for block in &group_blocks {
                let id = format!("{}_{}", prefix, block.addr);
                let label = format!("Addr: {}\\nSize: {}\\n{}", block.addr, block.size, block.node_type);
                out.push_str(&format!("      {id}[\"{label}\"]\n"));
            }
            out.push_str("    end\n");
        } else {
            // Single block, no subgraph needed
            for block in &group_blocks {
                let id = format!("{}_{}", prefix, block.addr);
                let label = format!(
                    "{}\\nAddr: {}\\nSize: {}",
                    block.label, block.addr, block.size
                );
                out.push_str(&format!("    {id}[\"{label}\"]\n"));
            }
        }
    }
    
    out
}

/// Converts the patches to a Mermaid flowchart string with horizontal layout.
fn patches_to_mermaid(
    old_blocks: &[MemBlock],
    new_blocks: &[MemBlock],
    patches: impl Iterator<Item = CopyFromPatch>,
) -> String {
    let mut out = String::new();
    out.push_str("%%{init: {'theme':'base', 'themeVariables': {'fontSize':'14px'}}}%%\n");
    out.push_str("flowchart LR\n");

    out.push_str("  subgraph OldMemory [\"🗂️ Old Memory Layout\"]\n");
    out.push_str("    direction TB\n");
    out.push_str(&blocks_to_mermaid_hierarchical(old_blocks, "O"));
    out.push_str("  end\n\n");

    out.push_str("  subgraph NewMemory [\"🗂️ New Memory Layout\"]\n");
    out.push_str("    direction TB\n");
    out.push_str(&blocks_to_mermaid_hierarchical(new_blocks, "N"));
    out.push_str("  end\n\n");

    out.push_str("  %% Copy operations (patches)\n");
    let patches_vec: Vec<_> = patches.collect();
    for p in &patches_vec {
        let old_id = format!("O_{}", p.src_addr);
        let new_id = format!("N_{}", p.dst_addr);
        let label = format!("{} words", p.size);
        out.push_str(&format!("  {old_id} ==>|{label}| {new_id}\n"));
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
    
    eprintln!(
        "Computed patches:\n{}",
        patches
            .iter()
            .map(|p| format!("{p:?}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
    
    let old_blocks = build_memory_blocks(&old_state_skeleton);
    let new_blocks = build_memory_blocks(&new_state_skeleton);
    let mermaid = patches_to_mermaid(&old_blocks, &new_blocks, patches.into_iter());

    println!("{mermaid}");

    Ok(())
}
