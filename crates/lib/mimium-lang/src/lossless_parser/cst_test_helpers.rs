//! CST Test Helpers - Utilities for testing the CST parser
//!
//! Provides helper functions to validate CST structure and content,
//! enabling reuse of original parser tests with minimal adaptation.

use super::green::{GreenNodeArena, GreenNodeId, SyntaxKind};
use super::token::{Token, TokenKind};

/// Search the CST tree for the first node with the given kind.
///
/// Returns the node ID if found, otherwise None.
pub fn find_node_by_kind(
    arena: &GreenNodeArena,
    node_id: GreenNodeId,
    target_kind: SyntaxKind,
) -> Option<GreenNodeId> {
    if arena.kind(node_id) == Some(target_kind) {
        return Some(node_id);
    }

    if let Some(children) = arena.children(node_id) {
        for &child_id in children {
            if let Some(found) = find_node_by_kind(arena, child_id, target_kind) {
                return Some(found);
            }
        }
    }

    None
}

/// Collect all nodes of a specific kind in the CST subtree.
///
/// Returns a vector of node IDs matching the target kind.
pub fn collect_nodes_by_kind(
    arena: &GreenNodeArena,
    node_id: GreenNodeId,
    target_kind: SyntaxKind,
) -> Vec<GreenNodeId> {
    let mut results = Vec::new();

    fn collect_recursive(
        arena: &GreenNodeArena,
        node_id: GreenNodeId,
        target_kind: SyntaxKind,
        results: &mut Vec<GreenNodeId>,
    ) {
        if arena.kind(node_id) == Some(target_kind) {
            results.push(node_id);
        }

        if let Some(children) = arena.children(node_id) {
            for &child_id in children {
                collect_recursive(arena, child_id, target_kind, results);
            }
        }
    }

    collect_recursive(arena, node_id, target_kind, &mut results);
    results
}

/// Assert that a node or its subtree contains at least one child of the given kind.
///
/// Panics if not found, with a descriptive message.
pub fn assert_cst_contains_kind(arena: &GreenNodeArena, node_id: GreenNodeId, kind: SyntaxKind) {
    if find_node_by_kind(arena, node_id, kind).is_none() {
        panic!(
            "Expected to find {:?} in subtree rooted at {:?}",
            kind, node_id
        );
    }
}

/// Assert that a node contains a direct child of the given kind.
///
/// Panics if the child does not exist.
pub fn assert_cst_has_child_kind(
    arena: &GreenNodeArena,
    node_id: GreenNodeId,
    expected_kind: SyntaxKind,
) {
    let has_child = arena
        .children(node_id)
        .map(|children| {
            children
                .iter()
                .any(|&child_id| arena.kind(child_id) == Some(expected_kind))
        })
        .unwrap_or(false);

    if !has_child {
        panic!(
            "Expected direct child of kind {:?} in node {:?}",
            expected_kind, node_id
        );
    }
}

/// Count nodes of a specific kind in a subtree.
pub fn count_nodes_by_kind(
    arena: &GreenNodeArena,
    node_id: GreenNodeId,
    target_kind: SyntaxKind,
) -> usize {
    collect_nodes_by_kind(arena, node_id, target_kind).len()
}

/// Assert that a specific number of nodes of a given kind exist in the subtree.
pub fn assert_node_count(
    arena: &GreenNodeArena,
    node_id: GreenNodeId,
    kind: SyntaxKind,
    expected_count: usize,
) {
    let actual_count = count_nodes_by_kind(arena, node_id, kind);
    assert_eq!(
        actual_count, expected_count,
        "Expected {} nodes of kind {:?}, found {}",
        expected_count, kind, actual_count
    );
}

/// Check if a token list contains at least one token of the given kind.
pub fn assert_tokens_present(tokens: &[Token], expected_kinds: &[TokenKind]) {
    for expected_kind in expected_kinds {
        if !tokens.iter().any(|t| t.kind == *expected_kind) {
            panic!(
                "Expected token kind {:?} not found in token list",
                expected_kind
            );
        }
    }
}

/// Get the depth (maximum nesting level) of a CST subtree.
pub fn get_cst_depth(arena: &GreenNodeArena, node_id: GreenNodeId) -> usize {
    if let Some(children) = arena.children(node_id) {
        if children.is_empty() {
            1
        } else {
            1 + children
                .iter()
                .map(|&child_id| get_cst_depth(arena, child_id))
                .max()
                .unwrap_or(0)
        }
    } else {
        1
    }
}

/// Assert that CST structure has a specific minimum depth.
pub fn assert_cst_depth_at_least(arena: &GreenNodeArena, node_id: GreenNodeId, min_depth: usize) {
    let actual_depth = get_cst_depth(arena, node_id);
    assert!(
        actual_depth >= min_depth,
        "Expected CST depth >= {}, found {}",
        min_depth,
        actual_depth
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser_internal::cst_parser::parse_cst;
    use crate::parser_internal::preparser::preparse;
    use crate::parser_internal::tokenizer::tokenize;

    #[test]
    fn test_find_node_by_kind() {
        let source = "fn test(x) { 42 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, _errors) = parse_cst(tokens, &preparsed);

        // Should find Program node
        let program = find_node_by_kind(&arena, root_id, SyntaxKind::Program);
        assert!(program.is_some());

        // Should find IntLiteral node
        let int_lit = find_node_by_kind(&arena, root_id, SyntaxKind::IntLiteral);
        assert!(int_lit.is_some());
    }

    #[test]
    fn test_collect_nodes_by_kind() {
        let source = "(1, 2, 3)";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, _errors) = parse_cst(tokens, &preparsed);

        // Should find three IntLiteral nodes
        let int_lits = collect_nodes_by_kind(&arena, root_id, SyntaxKind::IntLiteral);
        assert_eq!(int_lits.len(), 3);
    }

    #[test]
    fn test_assert_cst_contains_kind() {
        let source = "42";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, _errors) = parse_cst(tokens, &preparsed);

        assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    }

    #[test]
    #[should_panic]
    fn test_assert_cst_contains_kind_fails() {
        let source = "42";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, _errors) = parse_cst(tokens, &preparsed);

        assert_cst_contains_kind(&arena, root_id, SyntaxKind::StringLiteral);
    }

    #[test]
    fn test_count_nodes_by_kind() {
        let source = "fn f() { 1 } fn g() { 2 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, _errors) = parse_cst(tokens, &preparsed);

        let func_count = count_nodes_by_kind(&arena, root_id, SyntaxKind::FunctionDecl);
        assert_eq!(func_count, 2);
    }

    #[test]
    fn test_get_cst_depth() {
        let source = "1";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, _errors) = parse_cst(tokens, &preparsed);

        let depth = get_cst_depth(&arena, root_id);
        assert!(depth >= 2); // At least Program -> Statement -> IntLiteral
    }
}
