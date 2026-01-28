/// CST Parser - parses token indices into Green Tree
/// This is a simple recursive descent parser that produces a lossless CST
use super::green::{GreenNodeArena, GreenNodeId, GreenTreeBuilder, SyntaxKind};
use super::preparser::PreParsedTokens;
use super::token::{LosslessToken, TokenKind};

/// Parser state
pub struct Parser<'a> {
    tokens: Vec<LosslessToken>,
    preparsed: &'a PreParsedTokens,
    current: usize, // Index into preparsed.token_indices
    builder: GreenTreeBuilder,
}

/// Maximum lookahead distance for disambiguation
const MAX_LOOKAHEAD: usize = 20;

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<LosslessToken>, preparsed: &'a PreParsedTokens) -> Self {
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
            .get_token(self.current, &self.tokens)
            .map(|t| t.kind)
    }

    /// Peek ahead n tokens
    fn peek_ahead(&self, n: usize) -> Option<TokenKind> {
        self.preparsed
            .get_token(self.current + n, &self.tokens)
            .map(|t| t.kind)
    }

    /// Get the current token
    fn current_token(&self) -> Option<&LosslessToken> {
        self.preparsed.get_token(self.current, &self.tokens)
    }

    /// Check if current token matches the expected kind
    fn check(&self, kind: TokenKind) -> bool {
        self.peek() == Some(kind)
    }

    /// Advance to the next token and add current to the tree
    fn bump(&mut self) {
        if let Some(&token_idx) = self.preparsed.token_indices.get(self.current)
            && let Some(token) = self.tokens.get(token_idx)
        {
            self.builder.add_token(token_idx, token.length);
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
    pub fn parse(mut self) -> (GreenNodeId, GreenNodeArena, Vec<LosslessToken>) {
        self.builder.start_node(SyntaxKind::Program);

        while !self.is_at_end() {
            self.parse_statement();
        }

        let root_id = self.builder.finish_node().unwrap();
        let arena = self.builder.into_arena();
        (root_id, arena, self.tokens)
    }

    /// Check if we've reached the end
    fn is_at_end(&self) -> bool {
        self.peek().is_none_or(|k| k == TokenKind::Eof)
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

        // Mark function name with IdentFunction kind
        if self.check(TokenKind::Ident) {
            if let Some(&token_idx) = self.preparsed.token_indices.get(self.current) {
                self.tokens[token_idx].kind = TokenKind::IdentFunction;
            }
            self.bump();
        }

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
            // Mark parameter name with IdentParameter kind
            if self.check(TokenKind::Ident) {
                if let Some(&token_idx) = self.preparsed.token_indices.get(self.current) {
                    // Change token kind to IdentParameter
                    self.tokens[token_idx].kind = TokenKind::IdentParameter;
                }
                self.bump();
            }

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
            Some(TokenKind::LambdaArgBeginEnd) => {
                self.parse_lambda_expr();
            }
            Some(TokenKind::Ident) => {
                self.builder.start_node(SyntaxKind::Identifier);
                self.bump();
                self.builder.finish_node();
            }
            Some(TokenKind::ParenBegin) => {
                // Need lookahead to distinguish tuple from parenthesized expression
                // Tuple: (a, b) or (a,)
                // Paren: (a) or (expr)
                if self.is_tuple_expr() {
                    self.parse_tuple_expr();
                } else {
                    self.bump(); // (
                    self.parse_expr();
                    self.expect(TokenKind::ParenEnd);
                }
            }
            Some(TokenKind::BlockBegin) => {
                // Need lookahead to distinguish record from block
                // Record: {a = 1, b = 2}
                // Block: {stmt; stmt}
                if self.is_record_expr() {
                    self.parse_record_expr();
                } else {
                    self.parse_block_expr();
                }
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

    /// Parse lambda expression: |x, y| expr
    fn parse_lambda_expr(&mut self) {
        self.builder.start_node(SyntaxKind::LambdaExpr);

        // Opening '|'
        self.expect(TokenKind::LambdaArgBeginEnd);

        // Parameters
        while !self.check(TokenKind::LambdaArgBeginEnd) && !self.is_at_end() {
            if self.check(TokenKind::Ident) {
                if let Some(&token_idx) = self.preparsed.token_indices.get(self.current) {
                    self.tokens[token_idx].kind = TokenKind::IdentParameter;
                }
                self.bump();
            } else {
                // Skip unexpected tokens in parameter list to recover
                self.bump();
            }

            if self.check(TokenKind::Comma) {
                self.bump();
            } else if !self.check(TokenKind::LambdaArgBeginEnd) {
                // Invalid separator; try to recover by stopping params
                break;
            }
        }

        // Closing '|'
        self.expect(TokenKind::LambdaArgBeginEnd);

        // Optional return type annotation after '->'. We skip its tokens for now.
        if self.check(TokenKind::Arrow) {
            self.bump();
            while !self.is_at_end() {
                match self.peek() {
                    // Stop before the lambda body expression
                    Some(TokenKind::BlockBegin)
                    | Some(TokenKind::ParenBegin)
                    | Some(TokenKind::Ident)
                    | Some(TokenKind::Int)
                    | Some(TokenKind::Float)
                    | Some(TokenKind::Str)
                    | Some(TokenKind::If)
                    | Some(TokenKind::LambdaArgBeginEnd) => break,
                    _ => self.bump(),
                }
            }
        }

        // Body expression
        if !self.is_at_end() {
            self.parse_expr();
        }

        self.builder.finish_node();
    }

    /// Check if current position is a tuple expression
    /// Tuple: (a, b) or (a,) - has comma
    /// Paren: (a) - no comma
    fn is_tuple_expr(&self) -> bool {
        if !self.check(TokenKind::ParenBegin) {
            return false;
        }

        // Look ahead to find comma before ParenEnd
        let mut depth = 0;
        for i in 1..MAX_LOOKAHEAD {
            match self.peek_ahead(i) {
                Some(TokenKind::ParenBegin) => depth += 1,
                Some(TokenKind::ParenEnd) => {
                    if depth == 0 {
                        return false; // no comma found
                    }
                    depth -= 1;
                }
                Some(TokenKind::Comma) if depth == 0 => return true,
                None => return false,
                _ => {}
            }
        }
        false
    }

    /// Check if current position is a record expression
    /// Record: {a = 1, b = 2} - has assignment
    /// Block: {stmt; stmt} - has other tokens
    fn is_record_expr(&self) -> bool {
        if !self.check(TokenKind::BlockBegin) {
            return false;
        }

        // Look for pattern: Ident =
        if let Some(TokenKind::Ident) = self.peek_ahead(1)
            && let Some(TokenKind::Assign) = self.peek_ahead(2)
        {
            return true;
        }

        false
    }

    /// Parse tuple expression: (a, b, c)
    fn parse_tuple_expr(&mut self) {
        self.builder.start_node(SyntaxKind::TupleExpr);

        self.expect(TokenKind::ParenBegin);

        if !self.check(TokenKind::ParenEnd) {
            self.parse_expr();

            while self.check(TokenKind::Comma) {
                self.bump(); // ,
                if !self.check(TokenKind::ParenEnd) {
                    self.parse_expr();
                }
            }
        }

        self.expect(TokenKind::ParenEnd);
        self.builder.finish_node();
    }

    /// Parse record expression: {field1 = expr1, field2 = expr2}
    fn parse_record_expr(&mut self) {
        self.builder.start_node(SyntaxKind::RecordExpr);

        self.expect(TokenKind::BlockBegin);

        if !self.check(TokenKind::BlockEnd) {
            // Parse field: name = expr
            self.expect(TokenKind::Ident); // field name
            self.expect(TokenKind::Assign);
            self.parse_expr();

            while self.check(TokenKind::Comma) {
                self.bump(); // ,
                if !self.check(TokenKind::BlockEnd) {
                    self.expect(TokenKind::Ident); // field name
                    self.expect(TokenKind::Assign);
                    self.parse_expr();
                }
            }
        }

        self.expect(TokenKind::BlockEnd);
        self.builder.finish_node();
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

        if self.expect(TokenKind::Else) && self.check(TokenKind::BlockBegin) {
            self.parse_block_expr(); // else branch
        }

        self.builder.finish_node();
    }
}

/// Parse tokens into a Green Tree (CST)
pub fn parse_cst(
    tokens: Vec<LosslessToken>,
    preparsed: &PreParsedTokens,
) -> (GreenNodeId, GreenNodeArena, Vec<LosslessToken>) {
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
        let (root_id, arena, _tokens) = parse_cst(tokens, &preparsed);

        assert_eq!(arena.kind(root_id), Some(SyntaxKind::Program));
        assert!(arena.children(root_id).is_some());
    }

    #[test]
    fn test_parse_let_statement() {
        let source = "let x = 42";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens) = parse_cst(tokens, &preparsed);

        let children = arena.children(root_id).unwrap();
        assert!(!children.is_empty());
    }

    #[test]
    fn test_parse_function() {
        let source = "fn add(x, y) { x }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens) = parse_cst(tokens, &preparsed);

        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_tuple() {
        let source = "(1, 2, 3)";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens) = parse_cst(tokens, &preparsed);

        let children = arena.children(root_id).unwrap();
        assert!(!children.is_empty());
    }

    #[test]
    fn test_parse_record() {
        let source = "{x = 1, y = 2}";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens) = parse_cst(tokens, &preparsed);

        let children = arena.children(root_id).unwrap();
        assert!(!children.is_empty());
    }
}
