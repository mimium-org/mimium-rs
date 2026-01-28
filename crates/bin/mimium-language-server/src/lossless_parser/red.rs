/// Red Tree - Abstract Syntax Tree (AST) without trivia
/// Based on the Red-Green Syntax Tree pattern
/// 
/// Red nodes have absolute positions and are created from Green nodes.
/// They represent the actual AST without comments and whitespace.

use super::green::{GreenNodeArena, GreenNodeId, SyntaxKind};
use super::token::{LosslessToken, TokenKind};
use std::sync::Arc;

/// Red node - represents an AST node with position information
/// This is the "Red" part of Red-Green Syntax Tree
#[derive(Debug, Clone)]
pub struct RedNode {
    /// The underlying green node ID
    green_id: GreenNodeId,
    /// Absolute position in the source
    offset: usize,
}

impl RedNode {
    /// Create a new red node from a green node ID
    pub fn new(green_id: GreenNodeId, offset: usize) -> Arc<Self> {
        Arc::new(RedNode {
            green_id,
            offset,
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
    
    /// Get children as red nodes
    pub fn children(&self, arena: &GreenNodeArena) -> Vec<Arc<RedNode>> {
        if let Some(green_children) = arena.children(self.green_id) {
            let mut offset = self.offset;
            
            green_children
                .iter()
                .map(|&child_id| {
                    let child = RedNode::new(child_id, offset);
                    offset += arena.width(child_id);
                    child
                })
                .collect()
        } else {
            Vec::new()
        }
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
    
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Identifier(String),
    
    Error,
}

/// Convert Red Tree to AST
pub fn red_to_ast(red: &RedNode, source: &str, tokens: &[LosslessToken], arena: &GreenNodeArena) -> AstNode {
    match red.kind(arena) {
        Some(SyntaxKind::Program) => {
            let children = red.children(arena);
            let statements = children
                .iter()
                .map(|child| red_to_ast(child, source, tokens, arena))
                .collect();
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
                        if let Some(token) = tokens.get(*token_index) {
                            if token.kind == TokenKind::Ident && i == 1 {
                                name = token.text(source).to_string();
                            }
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
                        if let Some(token) = tokens.get(*token_index) {
                            if token.kind == TokenKind::Ident && i == 1 {
                                name = token.text(source).to_string();
                            }
                        }
                    }
                    _ => {
                        if value.is_none() {
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
                if let super::green::GreenNode::Token { token_index, .. } = green {
                    if let Some(token) = tokens.get(*token_index) {
                        let text = token.text(source);
                        if let Ok(value) = text.parse::<i64>() {
                            return AstNode::IntLiteral(value);
                        }
                    }
                }
            }
            AstNode::Error
        }
        
        Some(SyntaxKind::FloatLiteral) => {
            let children = red.children(arena);
            if let Some(child) = children.first() {
                let green = arena.get(child.green_id());
                if let super::green::GreenNode::Token { token_index, .. } = green {
                    if let Some(token) = tokens.get(*token_index) {
                        let text = token.text(source);
                        if let Ok(value) = text.parse::<f64>() {
                            return AstNode::FloatLiteral(value);
                        }
                    }
                }
            }
            AstNode::Error
        }
        
        Some(SyntaxKind::StringLiteral) => {
            let children = red.children(arena);
            if let Some(child) = children.first() {
                let green = arena.get(child.green_id());
                if let super::green::GreenNode::Token { token_index, .. } = green {
                    if let Some(token) = tokens.get(*token_index) {
                        let text = token.text(source);
                        // Remove quotes
                        let unquoted = text.trim_matches('"');
                        return AstNode::StringLiteral(unquoted.to_string());
                    }
                }
            }
            AstNode::Error
        }
        
        Some(SyntaxKind::Identifier) => {
            let children = red.children(arena);
            if let Some(child) = children.first() {
                let green = arena.get(child.green_id());
                if let super::green::GreenNode::Token { token_index, .. } = green {
                    if let Some(token) = tokens.get(*token_index) {
                        return AstNode::Identifier(token.text(source).to_string());
                    }
                }
            }
            AstNode::Error
        }
        
        _ => AstNode::Error,
    }
}

/// Extract parameter names from ParamList node
fn extract_params(red: &RedNode, source: &str, tokens: &[LosslessToken], arena: &GreenNodeArena) -> Vec<String> {
    let mut params = Vec::new();
    
    for child in red.children(arena) {
        let green = arena.get(child.green_id());
        if let super::green::GreenNode::Token { token_index, .. } = green {
            if let Some(token) = tokens.get(*token_index) {
                if token.kind == TokenKind::Ident {
                    params.push(token.text(source).to_string());
                }
            }
        }
    }
    
    params
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lossless_parser::cst_parser::parse_cst;
    use crate::lossless_parser::preparser::preparse;
    use crate::lossless_parser::tokenizer::tokenize;
    
    #[test]
    fn test_red_node_creation() {
        let source = "42";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena) = parse_cst(&tokens, &preparsed);
        let red = RedNode::new(root_id, 0);
        
        assert_eq!(red.offset(), 0);
        assert!(red.width(&arena) > 0);
    }
    
    #[test]
    fn test_red_to_ast_simple() {
        let source = "42";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena) = parse_cst(&tokens, &preparsed);
        let red = RedNode::new(root_id, 0);
        let ast = red_to_ast(&red, source, &tokens, &arena);
        
        match ast {
            AstNode::Program { .. } => {}, // Expected
            _ => panic!("Expected Program node"),
        }
    }
    
    #[test]
    fn test_red_to_ast_function() {
        let source = "fn add(x, y) { 42 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena) = parse_cst(&tokens, &preparsed);
        let red = RedNode::new(root_id, 0);
        let ast = red_to_ast(&red, source, &tokens, &arena);
        
        match ast {
            AstNode::Program { statements } => {
                assert!(!statements.is_empty());
            }
            _ => panic!("Expected Program node"),
        }
    }
}
