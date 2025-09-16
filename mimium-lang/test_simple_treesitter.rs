#[cfg(test)]
mod test_treesitter_simple {
    use crate::compiler::tree_sitter_parser::ASTConverter;
    
    #[test]
    fn test_simple_42() {
        let source = "42";
        let (program, errors) = ASTConverter::parse_treesitter(source, None);
        println!("Program: {:#?}", program);
        println!("Errors: {:#?}", errors);
        
        // Test conversion to expression
        let (expr, errors2) = ASTConverter::parse_to_expr_treesitter(source, None);
        println!("Expression: {:#?}", expr.to_expr());
        println!("Errors2: {:#?}", errors2);
    }
}
