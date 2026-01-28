/// Lossless Parser for mimium Language Server
/// 
/// This module implements a lossless parser based on the Red-Green Syntax Tree pattern.
/// The parser preserves all information from the source code including comments and whitespace.
/// 
/// ## Architecture
/// 
/// 1. **Tokenizer** (`tokenizer.rs`): Converts source text into position-aware tokens
///    - Input: Source text (String)
///    - Output: Token sequence with position information
///    - Tokens store kind, start position, and length (not the actual value)
/// 
/// 2. **Pre-parser** (`preparser.rs`): Separates trivia from syntax tokens
///    - Input: Token sequence
///    - Output: Index sequence with trivia maps
///    - Comments and whitespace are stored in leading/trailing trivia maps
/// 
/// 3. **CST Parser** (`cst_parser.rs`): Parses into Green Tree (Concrete Syntax Tree)
///    - Input: Token indices from pre-parser
///    - Output: Green Tree (position-independent, immutable CST)
///    - Represents the complete syntactic structure
/// 
/// 4. **AST Parser** (`red.rs`): Converts Green Tree to Red Tree/AST
///    - Input: Green Tree
///    - Output: Red Tree (position-aware) or AST (without trivia)
///    - Red nodes have absolute positions
///    - AST removes trivia for semantic analysis
/// 
/// ## Usage Example
/// 
/// ```rust
/// use mimium_language_server::lossless_parser;
/// 
/// let source = "fn dsp() { 42 }";
/// 
/// // Step 1: Tokenize
/// let tokens = lossless_parser::tokenize(source);
/// 
/// // Step 2: Pre-parse (separate trivia)
/// let preparsed = lossless_parser::preparse(&tokens);
/// 
/// // Step 3: Parse to CST (Green Tree) and get annotated tokens
/// let (green_id, arena, tokens) = lossless_parser::parse_cst(tokens, &preparsed);
/// 
/// // Step 4: Convert to AST (Red Tree)
/// let red = lossless_parser::green_to_red(green_id, &arena, 0);
/// let ast = lossless_parser::red_to_ast(&red, source, &tokens, &arena);
/// ```

pub mod token;
pub mod tokenizer;
pub mod preparser;
pub mod green;
pub mod cst_parser;
pub mod red;

// Re-export main types and functions
pub use token::{LosslessToken, TokenKind};
pub use tokenizer::tokenize;
pub use preparser::{PreParsedTokens, preparse};
pub use green::{GreenNodeArena, GreenNodeId, SyntaxKind};
pub use cst_parser::parse_cst;
pub use red::{RedNode, AstNode, red_to_ast};

/// Convenience function to create a Red node from a Green node
pub fn green_to_red(green_id: GreenNodeId, arena: &GreenNodeArena, offset: usize) -> std::sync::Arc<RedNode> {
    RedNode::new(green_id, offset)
}

/// Complete parsing pipeline from source to AST
pub fn parse(source: &str) -> (AstNode, Vec<LosslessToken>, PreParsedTokens, GreenNodeArena) {
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (green_id, arena, tokens) = parse_cst(tokens, &preparsed);
    let red = green_to_red(green_id, &arena, 0);
    let ast = red_to_ast(&red, source, &tokens, &arena);
    
    (ast, tokens, preparsed, arena)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_full_pipeline() {
        let source = "fn dsp() { 42 }";
        let (ast, tokens, _preparsed, _arena) = parse(source);
        
        match ast {
            AstNode::Program { statements } => {
                assert!(!statements.is_empty());
            }
            _ => panic!("Expected Program node"),
        }
        
        assert!(!tokens.is_empty());
    }
    
    #[test]
    fn test_with_comments() {
        let source = r#"
            // This is a comment
            fn dsp() {
                /* multi-line
                   comment */
                42
            }
        "#;
        
        let (ast, tokens, _preparsed, _arena) = parse(source);
        
        // Check that comments are in the token stream
        let has_comments = tokens.iter().any(|t| 
            matches!(t.kind, TokenKind::SingleLineComment | TokenKind::MultiLineComment)
        );
        assert!(has_comments);
        
        // Check that AST is still valid
        match ast {
            AstNode::Program { .. } => {},
            _ => panic!("Expected Program node"),
        }
    }
    
    #[test]
    fn test_let_binding() {
        let source = "let x = 42";
        let (ast, _tokens, _preparsed, _arena) = parse(source);
        
        match ast {
            AstNode::Program { statements } => {
                assert_eq!(statements.len(), 1);
                match &statements[0] {
                    AstNode::LetDecl { name, value } => {
                        assert_eq!(name, "x");
                        match value.as_ref() {
                            AstNode::IntLiteral(n) => assert_eq!(*n, 42),
                            _ => panic!("Expected IntLiteral"),
                        }
                    }
                    _ => panic!("Expected LetDecl"),
                }
            }
            _ => panic!("Expected Program node"),
        }
    }
    
    #[test]
    fn test_function_with_params() {
        let source = "fn add(x, y) { x }";
        let (ast, _tokens, _preparsed, _arena) = parse(source);
        
        match ast {
            AstNode::Program { statements } => {
                assert_eq!(statements.len(), 1);
                match &statements[0] {
                    AstNode::FunctionDecl { name, params, body: _ } => {
                        assert_eq!(name, "add");
                        assert_eq!(params, &vec!["x".to_string(), "y".to_string()]);
                    }
                    _ => panic!("Expected FunctionDecl"),
                }
            }
            _ => panic!("Expected Program node"),
        }
    }
    
    #[test]
    fn test_position_information() {
        let source = "fn dsp() { 42 }";
        let tokens = tokenize(source);
        
        // Check that tokens have correct positions
        assert_eq!(tokens[0].kind, TokenKind::Function);
        assert_eq!(tokens[0].text(source), "fn");
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].length, 2);
        
        // Find the "42" token
        let num_token = tokens.iter().find(|t| t.kind == TokenKind::Int).unwrap();
        assert_eq!(num_token.text(source), "42");
    }
}
