/// CST Parser - parses token indices into Green Tree
/// This is a simple recursive descent parser that produces a lossless CST
use super::green::{GreenNode, GreenTreeBuilder, SyntaxKind};
use super::preparser::PreParsedTokens;
use super::token::{LosslessToken, TokenKind};
use std::sync::Arc;

/// Parser state
pub struct Parser<'a> {
    tokens: &'a [LosslessToken],
    preparsed: &'a PreParsedTokens,
    current: usize, // Index into preparsed.token_indices
    builder: GreenTreeBuilder,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [LosslessToken], preparsed: &'a PreParsedTokens) -> Self {
        Self {
            tokens,
            preparsed,
            current: 0,
            builder: GreenTreeBuilder::new(),
        }
    }
    
    /// Get the current token kind
    fn peek(&self) -> Option<TokenKind> {
        self.preparsed
            .get_token(self.current, self.tokens)
            .map(|t| t.kind)
    }
    
    /// Get the current token
    fn current_token(&self) -> Option<&LosslessToken> {
        self.preparsed.get_token(self.current, self.tokens)
    }
    
    /// Check if current token matches the expected kind
    fn check(&self, kind: TokenKind) -> bool {
        self.peek() == Some(kind)
    }
    
    /// Advance to the next token and add current to the tree
    fn bump(&mut self) {
        if let Some(&token_idx) = self.preparsed.token_indices.get(self.current) {
            if let Some(token) = self.tokens.get(token_idx) {
                self.builder.add_token(token_idx, token.length);
            }
        }
        self.current += 1;
    }
    
    /// Expect a specific token kind and consume it
    fn expect(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.bump();
            true
        } else {
            false
        }
    }
    
    /// Parse the entire program
    pub fn parse(mut self) -> Arc<GreenNode> {
        self.builder.start_node(SyntaxKind::Program);
        
        while !self.is_at_end() {
            self.parse_statement();
        }
        
        self.builder.finish_node().unwrap()
    }
    
    /// Check if we've reached the end
    fn is_at_end(&self) -> bool {
        self.peek().map_or(true, |k| k == TokenKind::Eof)
    }
    
    /// Parse a statement
    fn parse_statement(&mut self) {
        self.builder.start_node(SyntaxKind::Statement);
        
        match self.peek() {
            Some(TokenKind::Function) => self.parse_function_decl(),
            Some(TokenKind::Let) => self.parse_let_decl(),
            Some(TokenKind::LetRec) => self.parse_letrec_decl(),
            _ => {
                // Try parsing as expression
                self.parse_expr();
            }
        }
        
        self.builder.finish_node();
    }
    
    /// Parse function declaration: fn name(params) { body }
    fn parse_function_decl(&mut self) {
        self.builder.start_node(SyntaxKind::FunctionDecl);
        
        self.expect(TokenKind::Function);
        self.expect(TokenKind::Ident); // function name
        
        // Parameters
        if self.check(TokenKind::ParenBegin) {
            self.parse_param_list();
        }
        
        // Body
        if self.check(TokenKind::BlockBegin) {
            self.parse_block_expr();
        }
        
        self.builder.finish_node();
    }
    
    /// Parse let declaration: let name = expr
    fn parse_let_decl(&mut self) {
        self.builder.start_node(SyntaxKind::LetDecl);
        
        self.expect(TokenKind::Let);
        self.expect(TokenKind::Ident); // variable name
        
        if self.expect(TokenKind::Assign) {
            self.parse_expr();
        }
        
        self.builder.finish_node();
    }
    
    /// Parse letrec declaration
    fn parse_letrec_decl(&mut self) {
        self.builder.start_node(SyntaxKind::LetRecDecl);
        
        self.expect(TokenKind::LetRec);
        self.expect(TokenKind::Ident);
        
        if self.expect(TokenKind::Assign) {
            self.parse_expr();
        }
        
        self.builder.finish_node();
    }
    
    /// Parse parameter list: (param1, param2, ...)
    fn parse_param_list(&mut self) {
        self.builder.start_node(SyntaxKind::ParamList);
        
        self.expect(TokenKind::ParenBegin);
        
        while !self.check(TokenKind::ParenEnd) && !self.is_at_end() {
            self.expect(TokenKind::Ident);
            
            if self.check(TokenKind::Comma) {
                self.bump();
            } else {
                break;
            }
        }
        
        self.expect(TokenKind::ParenEnd);
        self.builder.finish_node();
    }
    
    /// Parse expression (simplified)
    fn parse_expr(&mut self) {
        self.parse_primary();
    }
    
    /// Parse primary expression
    fn parse_primary(&mut self) {
        match self.peek() {
            Some(TokenKind::Int) => {
                self.builder.start_node(SyntaxKind::IntLiteral);
                self.bump();
                self.builder.finish_node();
            }
            Some(TokenKind::Float) => {
                self.builder.start_node(SyntaxKind::FloatLiteral);
                self.bump();
                self.builder.finish_node();
            }
            Some(TokenKind::Str) => {
                self.builder.start_node(SyntaxKind::StringLiteral);
                self.bump();
                self.builder.finish_node();
            }
            Some(TokenKind::Ident) => {
                self.builder.start_node(SyntaxKind::Identifier);
                self.bump();
                self.builder.finish_node();
            }
            Some(TokenKind::ParenBegin) => {
                self.bump(); // (
                self.parse_expr();
                self.expect(TokenKind::ParenEnd);
            }
            Some(TokenKind::BlockBegin) => {
                self.parse_block_expr();
            }
            Some(TokenKind::If) => {
                self.parse_if_expr();
            }
            _ => {
                // Error: unexpected token
                // For now, just skip it
                if !self.is_at_end() {
                    self.bump();
                }
            }
        }
    }
    
    /// Parse block expression: { statements }
    fn parse_block_expr(&mut self) {
        self.builder.start_node(SyntaxKind::BlockExpr);
        
        self.expect(TokenKind::BlockBegin);
        
        while !self.check(TokenKind::BlockEnd) && !self.is_at_end() {
            self.parse_statement();
        }
        
        self.expect(TokenKind::BlockEnd);
        self.builder.finish_node();
    }
    
    /// Parse if expression: if cond { then } else { else }
    fn parse_if_expr(&mut self) {
        self.builder.start_node(SyntaxKind::IfExpr);
        
        self.expect(TokenKind::If);
        self.parse_expr(); // condition
        
        if self.check(TokenKind::BlockBegin) {
            self.parse_block_expr(); // then branch
        }
        
        if self.expect(TokenKind::Else) {
            if self.check(TokenKind::BlockBegin) {
                self.parse_block_expr(); // else branch
            }
        }
        
        self.builder.finish_node();
    }
}

/// Parse tokens into a Green Tree (CST)
pub fn parse_cst(tokens: &[LosslessToken], preparsed: &PreParsedTokens) -> Arc<GreenNode> {
    let parser = Parser::new(tokens, preparsed);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lossless_parser::preparser::preparse;
    use crate::lossless_parser::tokenizer::tokenize;
    
    #[test]
    fn test_parse_simple_program() {
        let source = "fn dsp() { 42 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let cst = parse_cst(&tokens, &preparsed);
        
        assert_eq!(cst.kind(), Some(SyntaxKind::Program));
        assert!(cst.children().is_some());
    }
    
    #[test]
    fn test_parse_let_statement() {
        let source = "let x = 42";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let cst = parse_cst(&tokens, &preparsed);
        
        let children = cst.children().unwrap();
        assert!(!children.is_empty());
    }
    
    #[test]
    fn test_parse_function() {
        let source = "fn add(x, y) { x }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let cst = parse_cst(&tokens, &preparsed);
        
        assert!(cst.width() > 0);
    }
}
