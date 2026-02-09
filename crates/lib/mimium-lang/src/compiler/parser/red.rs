/// Red Tree - Abstract Syntax Tree (AST) without trivia
/// Based on the Red-Green Syntax Tree pattern
///
/// Red nodes have absolute positions and are created from Green nodes.
/// They represent the actual AST without comments and whitespace.
/// Red nodes maintain parent references for bottom-up traversal.
use super::green::{GreenNodeArena, GreenNodeId, SyntaxKind};
use super::token::{Token, TokenKind};
use std::sync::{Arc, Weak};

/// Red node - represents an AST node with position information
/// This is the "Red" part of Red-Green Syntax Tree
#[derive(Debug, Clone)]
pub struct RedNode {
    /// The underlying green node ID
    green_id: GreenNodeId,
    /// Absolute position in the source
    offset: usize,
    /// Parent node (weak reference to avoid cycles)
    parent: Option<Weak<RedNode>>,
}

impl RedNode {
    /// Create a new red node from a green node ID
    pub fn new(green_id: GreenNodeId, offset: usize) -> Arc<Self> {
        Arc::new(RedNode {
            green_id,
            offset,
            parent: None,
        })
    }

    /// Create a new red node with a parent reference
    pub fn new_with_parent(
        green_id: GreenNodeId,
        offset: usize,
        parent: Weak<RedNode>,
    ) -> Arc<Self> {
        Arc::new(RedNode {
            green_id,
            offset,
            parent: Some(parent),
        })
    }

    /// Get the absolute position of this node
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// Get the width (length) of this node
    pub fn width(&self, arena: &GreenNodeArena) -> usize {
        arena.width(self.green_id)
    }

    /// Get the end position of this node
    pub fn end(&self, arena: &GreenNodeArena) -> usize {
        self.offset + self.width(arena)
    }

    /// Get the syntax kind of this node
    pub fn kind(&self, arena: &GreenNodeArena) -> Option<SyntaxKind> {
        arena.kind(self.green_id)
    }

    /// Get the underlying green node ID
    pub fn green_id(&self) -> GreenNodeId {
        self.green_id
    }

    /// Get the parent node if it exists
    pub fn parent(&self) -> Option<Arc<RedNode>> {
        self.parent.as_ref().and_then(|weak| weak.upgrade())
    }

    /// Get children as red nodes with parent references
    pub fn children(self: &Arc<Self>, arena: &GreenNodeArena) -> Vec<Arc<RedNode>> {
        if let Some(green_children) = arena.children(self.green_id) {
            let mut offset = self.offset;

            green_children
                .iter()
                .map(|&child_id| {
                    let child = RedNode::new_with_parent(child_id, offset, Arc::downgrade(self));
                    offset += arena.width(child_id);
                    child
                })
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Get ancestors of this node (bottom-up traversal)
    pub fn ancestors(&self) -> Vec<Arc<RedNode>> {
        let mut result = Vec::new();
        let mut current = self.parent();

        while let Some(node) = current {
            result.push(node.clone());
            current = node.parent();
        }

        result
    }

    /// Check if this node is a descendant of another node
    pub fn is_descendant_of(&self, ancestor: &RedNode) -> bool {
        self.ancestors()
            .iter()
            .any(|node| node.green_id == ancestor.green_id && node.offset == ancestor.offset)
    }

    /// Get the text of this node from the source
    pub fn text<'a>(&self, source: &'a str, arena: &GreenNodeArena) -> &'a str {
        &source[self.offset..self.end(arena)]
    }
}

/// AST representation - simplified from Red Tree
#[derive(Debug, Clone)]
pub enum AstNode {
    Program {
        statements: Vec<AstNode>,
    },

    FunctionDecl {
        name: String,
        params: Vec<String>,
        body: Box<AstNode>,
    },

    LetDecl {
        name: String,
        value: Box<AstNode>,
    },

    LetRecDecl {
        name: String,
        value: Box<AstNode>,
    },

    BinaryExpr {
        op: String,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },

    CallExpr {
        callee: Box<AstNode>,
        args: Vec<AstNode>,
    },

    IfExpr {
        condition: Box<AstNode>,
        then_branch: Box<AstNode>,
        else_branch: Option<Box<AstNode>>,
    },

    BlockExpr {
        statements: Vec<AstNode>,
    },

    TupleExpr {
        elements: Vec<AstNode>,
    },

    RecordExpr {
        fields: Vec<(String, AstNode)>,
    },

    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Identifier(String),

    Error,
}

/// Convert Red Tree to AST
pub fn red_to_ast(
    red: &Arc<RedNode>,
    source: &str,
    tokens: &[Token],
    arena: &GreenNodeArena,
) -> AstNode {
    match red.kind(arena) {
        Some(SyntaxKind::Program) => {
            let children = red.children(arena);
            let mut statements: Vec<AstNode> = children
                .iter()
                .map(|child| red_to_ast(child, source, tokens, arena))
                .collect();

            // Transform flat statement list into Let-body-then chain
            statements = vec![transform_let_chain(statements)];

            AstNode::Program { statements }
        }

        Some(SyntaxKind::Statement) => {
            let children = red.children(arena);
            if let Some(first) = children.first() {
                red_to_ast(first, source, tokens, arena)
            } else {
                AstNode::Error
            }
        }

        Some(SyntaxKind::FunctionDecl) => {
            let children = red.children(arena);
            let mut name = String::new();
            let mut params = Vec::new();
            let mut body = None;

            for (i, child) in children.iter().enumerate() {
                let green = arena.get(child.green_id());
                match green {
                    super::green::GreenNode::Token { token_index, .. } => {
                        if let Some(token) = tokens.get(*token_index)
                            && matches!(token.kind, TokenKind::Ident | TokenKind::IdentFunction)
                            && i == 1
                        {
                            name = token.text(source).to_string();
                        }
                    }
                    _ => {
                        if child.kind(arena) == Some(SyntaxKind::ParamList) {
                            params = extract_params(child, source, tokens, arena);
                        } else if child.kind(arena) == Some(SyntaxKind::BlockExpr) {
                            body = Some(Box::new(red_to_ast(child, source, tokens, arena)));
                        }
                    }
                }
            }

            AstNode::FunctionDecl {
                name,
                params,
                body: body.unwrap_or_else(|| Box::new(AstNode::Error)),
            }
        }

        Some(SyntaxKind::LetDecl) => {
            let children = red.children(arena);
            let mut name = String::new();
            let mut value = None;

            for (i, child) in children.iter().enumerate() {
                let green = arena.get(child.green_id());
                match green {
                    super::green::GreenNode::Token { token_index, .. } => {
                        if let Some(token) = tokens.get(*token_index)
                            && matches!(token.kind, TokenKind::Ident | TokenKind::IdentVariable)
                            && i == 1
                        {
                            name = token.text(source).to_string();
                        }
                    }
                    _ => {
                        // Attempt to extract identifier from pattern subtree if name is empty
                        if name.is_empty()
                            && matches!(
                                child.kind(arena),
                                Some(SyntaxKind::Pattern)
                                    | Some(SyntaxKind::SinglePattern)
                                    | Some(SyntaxKind::TuplePattern)
                                    | Some(SyntaxKind::RecordPattern)
                            )
                        {
                            // DFS over subtree to find first identifier token
                            let mut stack = vec![child.clone()];
                            while let Some(node) = stack.pop() {
                                let g = arena.get(node.green_id());
                                if let super::green::GreenNode::Token { token_index, .. } = g {
                                    if let Some(tok) = tokens.get(*token_index)
                                        && matches!(
                                            tok.kind,
                                            TokenKind::Ident | TokenKind::IdentVariable
                                        )
                                    {
                                        name = tok.text(source).to_string();
                                        break;
                                    }
                                } else {
                                    stack.extend(node.children(arena));
                                }
                            }
                        } else if value.is_none() {
                            value = Some(Box::new(red_to_ast(child, source, tokens, arena)));
                        }
                    }
                }
            }

            AstNode::LetDecl {
                name,
                value: value.unwrap_or_else(|| Box::new(AstNode::Error)),
            }
        }

        Some(SyntaxKind::BlockExpr) => {
            let children = red.children(arena);
            let statements = children
                .iter()
                .filter_map(|child| {
                    let green = arena.get(child.green_id());
                    if matches!(green, super::green::GreenNode::Token { .. }) {
                        None
                    } else {
                        Some(red_to_ast(child, source, tokens, arena))
                    }
                })
                .collect();
            AstNode::BlockExpr { statements }
        }

        Some(SyntaxKind::IntLiteral) => {
            let children = red.children(arena);
            if let Some(child) = children.first() {
                let green = arena.get(child.green_id());
                if let super::green::GreenNode::Token { token_index, .. } = green
                    && let Some(token) = tokens.get(*token_index)
                {
                    let text = token.text(source);
                    if let Ok(value) = text.parse::<i64>() {
                        return AstNode::IntLiteral(value);
                    }
                }
            }
            AstNode::Error
        }

        Some(SyntaxKind::FloatLiteral) => {
            let children = red.children(arena);
            if let Some(child) = children.first() {
                let green = arena.get(child.green_id());
                if let super::green::GreenNode::Token { token_index, .. } = green
                    && let Some(token) = tokens.get(*token_index)
                {
                    let text = token.text(source);
                    if let Ok(value) = text.parse::<f64>() {
                        return AstNode::FloatLiteral(value);
                    }
                }
            }
            AstNode::Error
        }

        Some(SyntaxKind::StringLiteral) => {
            let children = red.children(arena);
            if let Some(child) = children.first() {
                let green = arena.get(child.green_id());
                if let super::green::GreenNode::Token { token_index, .. } = green
                    && let Some(token) = tokens.get(*token_index)
                {
                    let text = token.text(source);
                    // Remove quotes
                    let unquoted = text.trim_matches('"');
                    return AstNode::StringLiteral(unquoted.to_string());
                }
            }
            AstNode::Error
        }

        Some(SyntaxKind::Identifier) => {
            let children = red.children(arena);
            if let Some(child) = children.first() {
                let green = arena.get(child.green_id());
                if let super::green::GreenNode::Token { token_index, .. } = green
                    && let Some(token) = tokens.get(*token_index)
                {
                    return AstNode::Identifier(token.text(source).to_string());
                }
            }
            AstNode::Error
        }

        Some(SyntaxKind::TupleExpr) => {
            let children = red.children(arena);
            let elements = children
                .iter()
                .filter_map(|child| {
                    // Skip tokens (parens, commas), only process expressions
                    if child.kind(arena).is_some() {
                        Some(red_to_ast(child, source, tokens, arena))
                    } else {
                        None
                    }
                })
                .collect();
            AstNode::TupleExpr { elements }
        }

        Some(SyntaxKind::RecordExpr) => {
            let children = red.children(arena);
            let mut fields = Vec::new();
            let mut current_field_name = None;

            for child in children.iter() {
                let green = arena.get(child.green_id());
                match green {
                    super::green::GreenNode::Token { token_index, .. } => {
                        if let Some(token) = tokens.get(*token_index)
                            && matches!(token.kind, TokenKind::Ident | TokenKind::IdentVariable)
                        {
                            current_field_name = Some(token.text(source).to_string());
                        }
                    }
                    _ => {
                        // This is an expression node
                        if let Some(field_name) = current_field_name.take() {
                            let value = red_to_ast(child, source, tokens, arena);
                            fields.push((field_name, value));
                        }
                    }
                }
            }
            AstNode::RecordExpr { fields }
        }

        _ => AstNode::Error,
    }
}

/// Extract parameter names from ParamList node
fn extract_params(
    red: &Arc<RedNode>,
    source: &str,
    tokens: &[Token],
    arena: &GreenNodeArena,
) -> Vec<String> {
    let mut params = Vec::new();

    for child in red.children(arena) {
        let green = arena.get(child.green_id());
        if let super::green::GreenNode::Token { token_index, .. } = green
            && let Some(token) = tokens.get(*token_index)
            && matches!(token.kind, TokenKind::Ident | TokenKind::IdentParameter)
        {
            params.push(token.text(source).to_string());
        }
    }

    params
}

/// Transform a flat list of statements into a Let-body-then chain
///
/// For example:
/// ```text
/// [LetDecl(x, 1), LetDecl(y, 2), Expr(x+y)]
/// ```
/// becomes:
/// ```text
/// LetDecl(x, 1, LetDecl(y, 2, Expr(x+y)))
/// ```
fn transform_let_chain(statements: Vec<AstNode>) -> AstNode {
    if statements.is_empty() {
        return AstNode::Error;
    }

    // Work backwards through the statements, building the chain from the inside out
    let result = statements.into_iter().rev().reduce(|body, stmt| {
        match stmt {
            AstNode::LetDecl { name, value } => {
                // Transform LetDecl into a nested structure with body
                // We destructure the Box, so value is AstNode, not Box<AstNode>
                AstNode::LetDecl {
                    name,
                    value: Box::new(AstNode::BlockExpr {
                        statements: vec![*value, body],
                    }),
                }
            }
            AstNode::LetRecDecl { name, value } => {
                // Same for LetRecDecl
                AstNode::LetRecDecl {
                    name,
                    value: Box::new(AstNode::BlockExpr {
                        statements: vec![*value, body],
                    }),
                }
            }
            _ => {
                // For non-Let statements, wrap with the body in a block
                AstNode::BlockExpr {
                    statements: vec![stmt, body],
                }
            }
        }
    });

    result.unwrap_or(AstNode::Error)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::cst_parser::parse_cst;
    use crate::compiler::parser::preparser::preparse;
    use crate::compiler::parser::tokenizer::tokenize;

    #[test]
    fn test_red_node_creation() {
        let source = "42";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);
        let red = RedNode::new(root_id, 0);

        assert_eq!(red.offset(), 0);
        assert!(red.width(&arena) > 0);
        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }

    #[test]
    fn test_red_to_ast_simple() {
        let source = "42";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, annotated_tokens, errors) = parse_cst(tokens, &preparsed);
        let red = RedNode::new(root_id, 0);
        let ast = red_to_ast(&red, source, &annotated_tokens, &arena);

        match ast {
            AstNode::Program { .. } => {} // Expected
            _ => panic!("Expected Program node"),
        }

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }

    #[test]
    fn test_red_to_ast_function() {
        let source = "fn add(x, y) { 42 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, annotated_tokens, errors) = parse_cst(tokens, &preparsed);
        let red = RedNode::new(root_id, 0);
        let ast = red_to_ast(&red, source, &annotated_tokens, &arena);

        match ast {
            AstNode::Program { statements } => {
                assert!(!statements.is_empty());
            }
            _ => panic!("Expected Program node"),
        }

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }

    #[test]
    fn test_parent_references() {
        let source = "fn add(x, y) { 42 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);
        let root = RedNode::new(root_id, 0);

        // Root should have no parent
        assert!(root.parent().is_none());

        // Get children with parent references
        let children = root.children(&arena);

        // Children should have parent references pointing to root
        for child in children.iter() {
            let parent = child.parent();
            assert!(parent.is_some(), "Child should have a parent reference");

            let parent = parent.unwrap();
            assert_eq!(parent.offset(), root.offset());
            assert_eq!(parent.green_id(), root.green_id());
        }

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }

    #[test]
    fn test_ancestors() {
        let source = "fn add(x, y) { let z = 42 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);
        let root = RedNode::new(root_id, 0);

        // Get first child (statement)
        let children = root.children(&arena);
        if let Some(statement) = children.first() {
            // Get ancestors
            let ancestors = statement.ancestors();

            // Should have at least the root as ancestor
            assert!(!ancestors.is_empty(), "Statement should have ancestors");

            // First ancestor should be the root
            if let Some(first_ancestor) = ancestors.first() {
                assert_eq!(first_ancestor.offset(), root.offset());
            }
        }

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }

    #[test]
    fn test_is_descendant_of() {
        let source = "fn add(x, y) { 42 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);
        let root = RedNode::new(root_id, 0);

        let children = root.children(&arena);
        if let Some(child) = children.first() {
            // Child should be descendant of root
            assert!(
                child.is_descendant_of(&root),
                "Child should be descendant of root"
            );

            // Root should not be descendant of child
            assert!(
                !root.is_descendant_of(child),
                "Root should not be descendant of child"
            );
        }

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }

    #[test]
    fn test_let_chain_transformation() {
        let source = "let x = 1\nlet y = 2\nx";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, annotated_tokens, errors) = parse_cst(tokens, &preparsed);
        let red = RedNode::new(root_id, 0);
        let ast = red_to_ast(&red, source, &annotated_tokens, &arena);

        // The AST should have transformed the flat statement list into a chain
        match ast {
            AstNode::Program { statements } => {
                assert_eq!(statements.len(), 1);
                // The single statement should be a nested structure
                // of Let bindings, not a flat list
            }
            _ => panic!("Expected Program node"),
        }

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }
}
