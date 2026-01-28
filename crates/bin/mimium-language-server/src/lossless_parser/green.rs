/// Green Tree - Concrete Syntax Tree (CST) for lossless representation
/// Based on the Red-Green Syntax Tree pattern
///
/// Green nodes are immutable and position-independent. They represent
/// the structure of the syntax tree without absolute positions.
/// Green trees use interning via SlotMap for efficient sharing and caching.
use slotmap::{SlotMap, new_key_type};

new_key_type! {
    /// A handle to an interned GreenNode
    pub struct GreenNodeId;
}

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
        children: Vec<GreenNodeId>,
        width: usize, // Total width of all children
    },
}

impl GreenNode {
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
    pub fn children(&self) -> Option<&[GreenNodeId]> {
        match self {
            GreenNode::Token { .. } => None,
            GreenNode::Internal { children, .. } => Some(children),
        }
    }
}

/// Arena for interning Green nodes
#[derive(Debug, Default)]
pub struct GreenNodeArena {
    nodes: SlotMap<GreenNodeId, GreenNode>,
}

impl GreenNodeArena {
    /// Create a new arena
    pub fn new() -> Self {
        Self {
            nodes: SlotMap::with_key(),
        }
    }

    /// Intern a token node
    pub fn alloc_token(&mut self, token_index: usize, width: usize) -> GreenNodeId {
        self.nodes.insert(GreenNode::Token { token_index, width })
    }

    /// Intern an internal node
    pub fn alloc_internal(&mut self, kind: SyntaxKind, children: Vec<GreenNodeId>) -> GreenNodeId {
        let width = children.iter().map(|&id| self.nodes[id].width()).sum();
        self.nodes.insert(GreenNode::Internal {
            kind,
            children,
            width,
        })
    }

    /// Get a node by its ID
    pub fn get(&self, id: GreenNodeId) -> &GreenNode {
        &self.nodes[id]
    }

    /// Get the width of a node
    pub fn width(&self, id: GreenNodeId) -> usize {
        self.nodes[id].width()
    }

    /// Get the kind of a node
    pub fn kind(&self, id: GreenNodeId) -> Option<SyntaxKind> {
        self.nodes[id].kind()
    }

    /// Get children of a node
    pub fn children(&self, id: GreenNodeId) -> Option<&[GreenNodeId]> {
        self.nodes[id].children()
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
    TupleExpr,
    RecordExpr,

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
            SyntaxKind::TupleExpr => write!(f, "TupleExpr"),
            SyntaxKind::RecordExpr => write!(f, "RecordExpr"),
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

/// A builder for constructing Green Trees with an arena
pub struct GreenTreeBuilder {
    arena: GreenNodeArena,
    stack: Vec<(SyntaxKind, Vec<GreenNodeId>)>,
}

impl GreenTreeBuilder {
    pub fn new() -> Self {
        Self {
            arena: GreenNodeArena::new(),
            stack: Vec::new(),
        }
    }

    /// Get a reference to the arena
    pub fn arena(&self) -> &GreenNodeArena {
        &self.arena
    }

    /// Take ownership of the arena
    pub fn into_arena(self) -> GreenNodeArena {
        self.arena
    }

    /// Start a new internal node
    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.stack.push((kind, Vec::new()));
    }

    /// Add a token as a child
    pub fn add_token(&mut self, token_index: usize, width: usize) {
        let token_id = self.arena.alloc_token(token_index, width);
        if let Some((_, children)) = self.stack.last_mut() {
            children.push(token_id);
        }
    }

    /// Finish the current node and add it to its parent
    pub fn finish_node(&mut self) -> Option<GreenNodeId> {
        if let Some((kind, children)) = self.stack.pop() {
            let node_id = self.arena.alloc_internal(kind, children);

            // Add to parent if exists
            if let Some((_, parent_children)) = self.stack.last_mut() {
                parent_children.push(node_id);
            }

            Some(node_id)
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
        let mut arena = GreenNodeArena::new();
        let token_id = arena.alloc_token(0, 5);
        assert_eq!(arena.width(token_id), 5);
        assert_eq!(arena.kind(token_id), None);
    }

    #[test]
    fn test_green_node_internal() {
        let mut arena = GreenNodeArena::new();
        let token1 = arena.alloc_token(0, 5);
        let token2 = arena.alloc_token(1, 3);
        let internal = arena.alloc_internal(SyntaxKind::BinaryExpr, vec![token1, token2]);

        assert_eq!(arena.width(internal), 8);
        assert_eq!(arena.kind(internal), Some(SyntaxKind::BinaryExpr));
        assert_eq!(arena.children(internal).unwrap().len(), 2);
    }

    #[test]
    fn test_builder() {
        let mut builder = GreenTreeBuilder::new();

        builder.start_node(SyntaxKind::Program);
        builder.start_node(SyntaxKind::IntLiteral);
        builder.add_token(0, 2); // "42"
        builder.finish_node();
        let root_id = builder.finish_node().unwrap();

        let arena = builder.arena();
        assert_eq!(arena.kind(root_id), Some(SyntaxKind::Program));
        assert_eq!(arena.width(root_id), 2);
    }

    #[test]
    fn test_nested_nodes() {
        let mut builder = GreenTreeBuilder::new();

        builder.start_node(SyntaxKind::BinaryExpr);
        builder.add_token(0, 1); // "1"
        builder.add_token(1, 1); // "+"
        builder.add_token(2, 1); // "2"
        let node_id = builder.finish_node().unwrap();

        let arena = builder.arena();
        assert_eq!(arena.width(node_id), 3);
        assert_eq!(arena.children(node_id).unwrap().len(), 3);
    }
}
