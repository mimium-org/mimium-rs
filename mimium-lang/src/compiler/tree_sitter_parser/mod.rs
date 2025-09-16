use crate::ast::*;
use crate::interner::{ExprNodeId, Symbol, ToSymbol};
use crate::utils::error::{ReportableError, SimpleError};
use crate::utils::metadata::*;
use crate::ast::operators::Op;

use tree_sitter::{Parser, Tree, Node};

mod bindings;
pub use bindings::language;

/// Tree-sitter based incremental parser for mimium
pub struct TreeSitterParser {
    parser: Parser,
    tree: Option<Tree>,
}

impl TreeSitterParser {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut parser = Parser::new();
        parser.set_language(&language())?;
        Ok(Self { parser, tree: None })
    }

    pub fn parse(&mut self, source: &str, old_tree: Option<&Tree>) -> Result<Option<Tree>, Box<dyn std::error::Error>> {
        let tree = self.parser.parse(source, old_tree);
        self.tree = tree.clone();
        Ok(tree)
    }

    pub fn get_tree(&self) -> Option<&Tree> {
        self.tree.as_ref()
    }
}

/// Convert tree-sitter CST to mimium AST - minimal implementation
pub struct ASTConverter {
    source: String,
    file_path: Symbol,
}

impl ASTConverter {
    pub fn new(source: String, file_path: Symbol) -> Self {
        Self { source, file_path }
    }

    fn error(&self, message: &str, loc: Location) -> Box<dyn ReportableError> {
        Box::new(SimpleError {
            message: message.to_string(),
            span: loc,
        })
    }

    pub fn convert_tree(&self, tree: &Tree) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let root_node = tree.root_node();
        self.convert_source_file(root_node)
    }

    /// Parse source using tree-sitter - minimal implementation for testing
    pub fn parse_to_expr_treesitter(
        source: &str, 
        file_path: Option<std::path::PathBuf>
    ) -> (ExprNodeId, Vec<Box<dyn ReportableError>>) {
        let mut parser = match TreeSitterParser::new() {
            Ok(p) => p,
            Err(e) => {
                let error = SimpleError {
                    message: format!("Failed to create parser: {}", e),
                    span: Location::default(),
                };
                return (Expr::Error.into_id_without_span(), vec![Box::new(error)]);
            }
        };

        let tree = match parser.parse(source, None) {
            Ok(Some(tree)) => tree,
            Ok(None) => {
                let error = SimpleError {
                    message: "Failed to parse source".to_string(),
                    span: Location::default(),
                };
                return (Expr::Error.into_id_without_span(), vec![Box::new(error)]);
            }
            Err(e) => {
                let error = SimpleError {
                    message: format!("Parse error: {}", e),
                    span: Location::default(),
                };
                return (Expr::Error.into_id_without_span(), vec![Box::new(error)]);
            }
        };

        let file_symbol = file_path
            .map(|p| p.to_string_lossy().to_symbol())
            .unwrap_or_else(|| "<unknown>".to_symbol());
        
        let converter = ASTConverter::new(source.to_string(), file_symbol);
        
        match converter.convert_tree(&tree) {
            Ok(expr) => (expr, vec![]),
            Err(e) => (Expr::Error.into_id_without_span(), vec![e]),
        }
    }

    /// Parse source using tree-sitter and return Program structure for compatibility
    pub fn parse_treesitter(
        source: &str,
        file_path: Option<std::path::PathBuf>
    ) -> (crate::ast::program::Program, Vec<Box<dyn ReportableError>>) {
        // For now, create an empty program since our tree-sitter parser works at expression level
        // This is a compatibility wrapper for include resolution
        // TODO: Implement full program-level parsing if needed
        (crate::ast::program::Program::default(), vec![])
    }

    /// Add global context wrapper - moved from old parser to maintain compatibility
    pub fn add_global_context(ast: ExprNodeId, file_path: Symbol) -> ExprNodeId {
        use crate::utils::metadata::GLOBAL_LABEL;
        let span = ast.to_span();
        let loc = Location {
            span: span.clone(),
            path: file_path,
        };
        let res = Expr::Let(
            crate::pattern::TypedPattern::new(
                crate::pattern::Pattern::Single(GLOBAL_LABEL.to_symbol()),
                crate::types::Type::Unknown.into_id_with_location(loc.clone()),
            ),
            Expr::Lambda(vec![], None, ast).into_id(loc.clone()),
            None,
        );
        res.into_id(loc)
    }

    fn convert_source_file(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        assert_eq!(node.kind(), "source_file");
        
        // For now, create a simple placeholder
        let loc = self.node_location(node);
        Ok(Expr::Literal(Literal::PlaceHolder).into_id(loc))
    }

    fn node_text(&self, node: Node) -> &str {
        node.utf8_text(self.source.as_bytes()).unwrap()
    }

    fn node_location(&self, node: Node) -> Location {
        Location {
            span: node.start_byte()..node.end_byte(),
            path: self.file_path,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_creation() {
        let parser = TreeSitterParser::new();
        assert!(parser.is_ok());
    }

    #[test]
    fn test_simple_parse() {
        let mut parser = TreeSitterParser::new().unwrap();
        let source = "fn test() { 42 }";
        let tree = parser.parse(source, None);
        assert!(tree.is_ok());
    }

    #[test]
    fn test_parse_to_expr_integration() {
        let source = "42";
        let (expr, errors) = ASTConverter::parse_to_expr_treesitter(source, None);
        
        // Should parse successfully with no errors
        assert!(errors.is_empty());
        
        // For now, just verify we get a valid expression ID
        assert!(expr.to_expr() != Expr::Error);
    }
}