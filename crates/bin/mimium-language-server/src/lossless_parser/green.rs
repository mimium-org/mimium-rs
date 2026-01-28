/// Green Tree - Concrete Syntax Tree (CST) for lossless representation
/// Based on the Red-Green Syntax Tree pattern
/// 
/// Green nodes are immutable and position-independent. They represent
/// the structure of the syntax tree without absolute positions.
/// Green trees can be shared and cached.

use std::sync::Arc;

/// Green node - represents a CST node without position information
/// This is the "Green" part of Red-Green Syntax Tree
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GreenNode {
    /// A token node (leaf)
    Token {
        token_index: usize,
        width: usize, // Length in bytes
    },
    
    /// An internal node with children
    Internal {
        kind: SyntaxKind,
        children: Vec<Arc<GreenNode>>,
        width: usize, // Total width of all children
    },
}

impl GreenNode {
    /// Create a new token node
    pub fn new_token(token_index: usize, width: usize) -> Arc<Self> {
        Arc::new(GreenNode::Token { token_index, width })
    }
    
    /// Create a new internal node
    pub fn new_internal(kind: SyntaxKind, children: Vec<Arc<GreenNode>>) -> Arc<Self> {
        let width = children.iter().map(|c| c.width()).sum();
        Arc::new(GreenNode::Internal { kind, children, width })
    }
    
    /// Get the width (length in bytes) of this node
    pub fn width(&self) -> usize {
        match self {
            GreenNode::Token { width, .. } => *width,
            GreenNode::Internal { width, .. } => *width,
        }
    }
    
    /// Get the kind of this node
    pub fn kind(&self) -> Option<SyntaxKind> {
        match self {
            GreenNode::Token { .. } => None,
            GreenNode::Internal { kind, .. } => Some(*kind),
        }
    }
    
    /// Get children if this is an internal node
    pub fn children(&self) -> Option<&[Arc<GreenNode>]> {
        match self {
            GreenNode::Token { .. } => None,
            GreenNode::Internal { children, .. } => Some(children),
        }
    }
}

/// Syntax kinds - types of CST nodes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxKind {
    // Top-level
    Program,
    Statement,
    
    // Declarations
    FunctionDecl,
    LetDecl,
    LetRecDecl,
    
    // Expressions
    BinaryExpr,
    UnaryExpr,
    CallExpr,
    LambdaExpr,
    IfExpr,
    BlockExpr,
    
    // Literals
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    
    // Names and paths
    Identifier,
    
    // Types
    TypeAnnotation,
    PrimitiveType,
    FunctionType,
    TupleType,
    
    // Patterns
    Pattern,
    
    // Lists and sequences
    ParamList,
    ArgList,
    ExprList,
    
    // Other
    Error, // For error recovery
}

impl std::fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SyntaxKind::Program => write!(f, "Program"),
            SyntaxKind::Statement => write!(f, "Statement"),
            SyntaxKind::FunctionDecl => write!(f, "FunctionDecl"),
            SyntaxKind::LetDecl => write!(f, "LetDecl"),
            SyntaxKind::LetRecDecl => write!(f, "LetRecDecl"),
            SyntaxKind::BinaryExpr => write!(f, "BinaryExpr"),
            SyntaxKind::UnaryExpr => write!(f, "UnaryExpr"),
            SyntaxKind::CallExpr => write!(f, "CallExpr"),
            SyntaxKind::LambdaExpr => write!(f, "LambdaExpr"),
            SyntaxKind::IfExpr => write!(f, "IfExpr"),
            SyntaxKind::BlockExpr => write!(f, "BlockExpr"),
            SyntaxKind::IntLiteral => write!(f, "IntLiteral"),
            SyntaxKind::FloatLiteral => write!(f, "FloatLiteral"),
            SyntaxKind::StringLiteral => write!(f, "StringLiteral"),
            SyntaxKind::Identifier => write!(f, "Identifier"),
            SyntaxKind::TypeAnnotation => write!(f, "TypeAnnotation"),
            SyntaxKind::PrimitiveType => write!(f, "PrimitiveType"),
            SyntaxKind::FunctionType => write!(f, "FunctionType"),
            SyntaxKind::TupleType => write!(f, "TupleType"),
            SyntaxKind::Pattern => write!(f, "Pattern"),
            SyntaxKind::ParamList => write!(f, "ParamList"),
            SyntaxKind::ArgList => write!(f, "ArgList"),
            SyntaxKind::ExprList => write!(f, "ExprList"),
            SyntaxKind::Error => write!(f, "Error"),
        }
    }
}

/// A builder for constructing Green Trees
pub struct GreenTreeBuilder {
    stack: Vec<(SyntaxKind, Vec<Arc<GreenNode>>)>,
}

impl GreenTreeBuilder {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }
    
    /// Start a new internal node
    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.stack.push((kind, Vec::new()));
    }
    
    /// Add a token as a child
    pub fn add_token(&mut self, token_index: usize, width: usize) {
        let token = GreenNode::new_token(token_index, width);
        if let Some((_, children)) = self.stack.last_mut() {
            children.push(token);
        }
    }
    
    /// Finish the current node and add it to its parent
    pub fn finish_node(&mut self) -> Option<Arc<GreenNode>> {
        if let Some((kind, children)) = self.stack.pop() {
            let node = GreenNode::new_internal(kind, children);
            
            // Add to parent if exists
            if let Some((_, parent_children)) = self.stack.last_mut() {
                parent_children.push(node.clone());
            }
            
            Some(node)
        } else {
            None
        }
    }
    
    /// Check if the builder is at the root level
    pub fn is_root(&self) -> bool {
        self.stack.len() <= 1
    }
}

impl Default for GreenTreeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_green_node_token() {
        let token = GreenNode::new_token(0, 5);
        assert_eq!(token.width(), 5);
        assert_eq!(token.kind(), None);
    }
    
    #[test]
    fn test_green_node_internal() {
        let token1 = GreenNode::new_token(0, 5);
        let token2 = GreenNode::new_token(1, 3);
        let internal = GreenNode::new_internal(
            SyntaxKind::BinaryExpr,
            vec![token1, token2]
        );
        
        assert_eq!(internal.width(), 8);
        assert_eq!(internal.kind(), Some(SyntaxKind::BinaryExpr));
        assert_eq!(internal.children().unwrap().len(), 2);
    }
    
    #[test]
    fn test_builder() {
        let mut builder = GreenTreeBuilder::new();
        
        builder.start_node(SyntaxKind::Program);
        builder.start_node(SyntaxKind::IntLiteral);
        builder.add_token(0, 2); // "42"
        builder.finish_node();
        let root = builder.finish_node().unwrap();
        
        assert_eq!(root.kind(), Some(SyntaxKind::Program));
        assert_eq!(root.width(), 2);
    }
    
    #[test]
    fn test_nested_nodes() {
        let mut builder = GreenTreeBuilder::new();
        
        builder.start_node(SyntaxKind::BinaryExpr);
        builder.add_token(0, 1); // "1"
        builder.add_token(1, 1); // "+"
        builder.add_token(2, 1); // "2"
        let node = builder.finish_node().unwrap();
        
        assert_eq!(node.width(), 3);
        assert_eq!(node.children().unwrap().len(), 3);
    }
}
