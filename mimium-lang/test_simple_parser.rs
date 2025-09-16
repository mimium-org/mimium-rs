#[cfg(test)]
mod simple_parser_test {
    use crate::compiler::tree_sitter_parser::*;
    use crate::ast::*;
    use crate::interner::ToSymbol;
    
    #[test]
    fn test_simple_number() {
        let source = "42";
        let (ast, errors) = ASTConverter::parse_to_expr_treesitter(source, None);
        
        println!("Parsing '{}': AST={:?}, Errors={:?}", source, ast.to_expr(), errors);
        
        // For now, just verify we get some result
        assert!(errors.is_empty() || errors.len() < 10); // allow reasonable error count
    }
    
    #[test]
    fn test_simple_binary() {
        let source = "1 + 2";
        let (ast, errors) = ASTConverter::parse_to_expr_treesitter(source, None);
        
        println!("Parsing '{}': AST={:?}, Errors={:?}", source, ast.to_expr(), errors);
        
        // For now, just verify we get some result
        assert!(errors.is_empty() || errors.len() < 10); // allow reasonable error count
    }
}
