#[cfg(test)]
mod debug_test {
    use crate::compiler::tree_sitter_parser::*;
    use crate::interner::ToSymbol;
    
    #[test]
    fn test_debug_parsing() {
        let source = r#"
fn counter(){
    self + 1
}
fn dsp(input){
    let res = input + counter()
    (0,res)
}
"#;
        
        // Parse the source and see what we get
        let (ast, errors) = ASTConverter::parse_to_expr_treesitter(source, None);
        
        println!("Errors: {:?}", errors);
        println!("AST: {:?}", ast.to_expr());
        
        assert!(errors.is_empty());
    }
}
