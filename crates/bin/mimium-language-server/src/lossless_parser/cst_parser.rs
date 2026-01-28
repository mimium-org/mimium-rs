/// CST Parser - parses token indices into Green Tree
/// This is a simple recursive descent parser that produces a lossless CST
use super::green::{GreenNodeArena, GreenNodeId, GreenTreeBuilder, SyntaxKind};
use super::preparser::PreParsedTokens;
use super::token::{LosslessToken, TokenKind};

/// Helper trait to reduce node boilerplate
trait NodeBuilder {
    fn emit_node<F>(&mut self, kind: SyntaxKind, f: F)
    where
        F: FnOnce(&mut Self);
}

impl NodeBuilder for Parser<'_> {
    fn emit_node<F>(&mut self, kind: SyntaxKind, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.builder.start_node(kind);
        f(self);
        self.builder.finish_node();
    }
}

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
        self.emit_node(SyntaxKind::Statement, |this| {
            match this.peek() {
                Some(TokenKind::Function) => this.parse_function_decl(),
                Some(TokenKind::Let) => this.parse_let_decl(),
                Some(TokenKind::LetRec) => this.parse_letrec_decl(),
                _ => {
                    // Try parsing as expression
                    this.parse_expr();
                }
            }
        });
    }

    /// Parse function declaration: fn name(params) { body }
    fn parse_function_decl(&mut self) {
        self.emit_node(SyntaxKind::FunctionDecl, |this| {
            this.expect(TokenKind::Function);

            // Mark function name with IdentFunction kind
            if this.check(TokenKind::Ident) {
                if let Some(&token_idx) = this.preparsed.token_indices.get(this.current) {
                    this.tokens[token_idx].kind = TokenKind::IdentFunction;
                }
                this.bump();
            }

            // Parameters
            if this.check(TokenKind::ParenBegin) {
                this.parse_param_list();
            }

            // Body
            if this.check(TokenKind::BlockBegin) {
                this.parse_block_expr();
            }
        });
    }

    /// Parse let declaration: let name = expr
    fn parse_let_decl(&mut self) {
        self.emit_node(SyntaxKind::LetDecl, |this| {
            this.expect(TokenKind::Let);
            this.expect(TokenKind::Ident); // variable name

            if this.expect(TokenKind::Assign) {
                this.parse_expr();
            }
        });
    }

    /// Parse letrec declaration
    fn parse_letrec_decl(&mut self) {
        self.emit_node(SyntaxKind::LetRecDecl, |this| {
            this.expect(TokenKind::LetRec);
            this.expect(TokenKind::Ident);

            if this.expect(TokenKind::Assign) {
                this.parse_expr();
            }
        });
    }

    /// Parse parameter list: (param1, param2, ...)
    fn parse_param_list(&mut self) {
        self.emit_node(SyntaxKind::ParamList, |this| {
            this.expect(TokenKind::ParenBegin);

            while !this.check(TokenKind::ParenEnd) && !this.is_at_end() {
                // Mark parameter name with IdentParameter kind
                if this.check(TokenKind::Ident) {
                    if let Some(&token_idx) = this.preparsed.token_indices.get(this.current) {
                        // Change token kind to IdentParameter
                        this.tokens[token_idx].kind = TokenKind::IdentParameter;
                    }
                    this.bump();
                }

                if this.check(TokenKind::Comma) {
                    this.bump();
                } else {
                    break;
                }
            }

            this.expect(TokenKind::ParenEnd);
        });
    }

    /// Parse expression (simplified)
    fn parse_expr(&mut self) {
        self.parse_primary();
    }

    /// Parse primary expression
    fn parse_primary(&mut self) {
        match self.peek() {
            Some(TokenKind::Int) => {
                self.emit_node(SyntaxKind::IntLiteral, |this| {
                    this.bump();
                });
            }
            Some(TokenKind::Float) => {
                self.emit_node(SyntaxKind::FloatLiteral, |this| {
                    this.bump();
                });
            }
            Some(TokenKind::Str) => {
                self.emit_node(SyntaxKind::StringLiteral, |this| {
                    this.bump();
                });
            }
            Some(TokenKind::LambdaArgBeginEnd) => {
                self.parse_lambda_expr();
            }
            Some(TokenKind::Ident) => {
                self.emit_node(SyntaxKind::Identifier, |this| {
                    this.bump();
                });
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
        self.emit_node(SyntaxKind::LambdaExpr, |this| {
            // Opening '|'
            this.expect(TokenKind::LambdaArgBeginEnd);

            // Parameters
            while !this.check(TokenKind::LambdaArgBeginEnd) && !this.is_at_end() {
                if this.check(TokenKind::Ident) {
                    if let Some(&token_idx) = this.preparsed.token_indices.get(this.current) {
                        this.tokens[token_idx].kind = TokenKind::IdentParameter;
                    }
                    this.bump();
                } else {
                    // Skip unexpected tokens in parameter list to recover
                    this.bump();
                }

                if this.check(TokenKind::Comma) {
                    this.bump();
                } else if !this.check(TokenKind::LambdaArgBeginEnd) {
                    // Invalid separator; try to recover by stopping params
                    break;
                }
            }

            // Closing '|'
            this.expect(TokenKind::LambdaArgBeginEnd);

            // Optional return type annotation after '->'. We skip its tokens for now.
            if this.check(TokenKind::Arrow) {
                this.bump();
                while !this.is_at_end() {
                    match this.peek() {
                        // Stop before the lambda body expression
                        Some(TokenKind::BlockBegin)
                        | Some(TokenKind::ParenBegin)
                        | Some(TokenKind::Ident)
                        | Some(TokenKind::Int)
                        | Some(TokenKind::Float)
                        | Some(TokenKind::Str)
                        | Some(TokenKind::If)
                        | Some(TokenKind::LambdaArgBeginEnd) => break,
                        _ => this.bump(),
                    }
                }
            }

            // Body expression
            if !this.is_at_end() {
                this.parse_expr();
            }
        });
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
        self.emit_node(SyntaxKind::TupleExpr, |this| {
            this.expect(TokenKind::ParenBegin);

            if !this.check(TokenKind::ParenEnd) {
                this.parse_expr();

                while this.check(TokenKind::Comma) {
                    this.bump(); // ,
                    if !this.check(TokenKind::ParenEnd) {
                        this.parse_expr();
                    }
                }
            }

            this.expect(TokenKind::ParenEnd);
        });
    }

    /// Parse record expression: {field1 = expr1, field2 = expr2}
    fn parse_record_expr(&mut self) {
        self.emit_node(SyntaxKind::RecordExpr, |this| {
            this.expect(TokenKind::BlockBegin);

            if !this.check(TokenKind::BlockEnd) {
                // Parse field: name = expr
                this.expect(TokenKind::Ident); // field name
                this.expect(TokenKind::Assign);
                this.parse_expr();

                while this.check(TokenKind::Comma) {
                    this.bump(); // ,
                    if !this.check(TokenKind::BlockEnd) {
                        this.expect(TokenKind::Ident); // field name
                        this.expect(TokenKind::Assign);
                        this.parse_expr();
                    }
                }
            }

            this.expect(TokenKind::BlockEnd);
        });
    }

    /// Parse block expression: { statements }
    fn parse_block_expr(&mut self) {
        self.emit_node(SyntaxKind::BlockExpr, |this| {
            this.expect(TokenKind::BlockBegin);

            while !this.check(TokenKind::BlockEnd) && !this.is_at_end() {
                this.parse_statement();
            }

            this.expect(TokenKind::BlockEnd);
        });
    }

    /// Parse if expression: if cond { then } else { else }
    fn parse_if_expr(&mut self) {
        self.emit_node(SyntaxKind::IfExpr, |this| {
            this.expect(TokenKind::If);
            this.parse_expr(); // condition

            if this.check(TokenKind::BlockBegin) {
                this.parse_block_expr(); // then branch
            }

            if this.expect(TokenKind::Else) && this.check(TokenKind::BlockBegin) {
                this.parse_block_expr(); // else branch
            }
        });
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
