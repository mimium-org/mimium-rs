/// CST Parser - parses token indices into Green Tree
/// This is a simple recursive descent parser that produces a lossless CST
use super::green::{GreenNodeArena, GreenNodeId, GreenTreeBuilder, SyntaxKind};
use super::preparser::PreParsedTokens;
use super::token::{LosslessToken, TokenKind};
use std::fmt;

/// Error detail - structured error information
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorDetail {
    /// Expected token not found (e.g., expected `)` but found `}`)
    UnexpectedToken { expected: String, found: String },
    /// Unexpected end of input
    UnexpectedEof { expected: String },
    /// Invalid syntax with reason
    InvalidSyntax { reason: String },
}

impl fmt::Display for ErrorDetail {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorDetail::UnexpectedToken { expected, found } => {
                write!(f, "Expected {expected}, found {found}")
            }
            ErrorDetail::UnexpectedEof { expected } => {
                write!(f, "Unexpected end of input, expected {expected}")
            }
            ErrorDetail::InvalidSyntax { reason } => {
                write!(f, "Invalid syntax: {reason}")
            }
        }
    }
}

/// Parser error with recovery information
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserError {
    /// Token index where the error occurred
    pub token_index: usize,
    /// Error detail
    pub detail: ErrorDetail,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.token_index, self.detail)
    }
}

impl ParserError {
    pub fn unexpected_token(token_index: usize, expected: &str, found: &str) -> Self {
        Self {
            token_index,
            detail: ErrorDetail::UnexpectedToken {
                expected: expected.to_string(),
                found: found.to_string(),
            },
        }
    }

    pub fn unexpected_eof(token_index: usize, expected: &str) -> Self {
        Self {
            token_index,
            detail: ErrorDetail::UnexpectedEof {
                expected: expected.to_string(),
            },
        }
    }

    pub fn invalid_syntax(token_index: usize, reason: &str) -> Self {
        Self {
            token_index,
            detail: ErrorDetail::InvalidSyntax {
                reason: reason.to_string(),
            },
        }
    }
}

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
    /// Collected parser errors for error recovery
    errors: Vec<ParserError>,
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
            errors: Vec::new(),
        }
    }

    /// Record a parser error
    fn add_error(&mut self, error: ParserError) {
        self.errors.push(error);
    }

    /// Get the current token index in the original token array
    fn current_token_index(&self) -> usize {
        self.preparsed
            .token_indices
            .get(self.current)
            .copied()
            .unwrap_or(0)
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
    #[allow(dead_code)]
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
    /// Returns true if the token matched, false otherwise (and records error)
    fn expect(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.bump();
            true
        } else if self.is_at_end() {
            self.add_error(ParserError::unexpected_eof(
                self.current_token_index(),
                &format!("{kind:?}"),
            ));
            false
        } else {
            let found = self.peek().map(|k| format!("{k:?}")).unwrap_or_default();
            self.add_error(ParserError::unexpected_token(
                self.current_token_index(),
                &format!("{kind:?}"),
                &found,
            ));
            false
        }
    }

    /// Parse the entire program
    pub fn parse(mut self) -> (GreenNodeId, GreenNodeArena, Vec<LosslessToken>, Vec<ParserError>) {
        self.builder.start_node(SyntaxKind::Program);

        while !self.is_at_end() {
            self.parse_statement();
        }

        let root_id = self.builder.finish_node().unwrap();
        let arena = self.builder.into_arena();
        let errors = self.errors;
        (root_id, arena, self.tokens, errors)
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

    /// Parse parameter list: (param1, param2, ...) or (param1: Type, param2: Type)
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
                    
                    // Optional type annotation
                    if this.check(TokenKind::Colon) {
                        this.parse_type_annotation();
                    }
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

    /// Parse type annotation: : Type
    fn parse_type_annotation(&mut self) {
        self.emit_node(SyntaxKind::TypeAnnotation, |this| {
            this.expect(TokenKind::Colon);
            this.parse_type();
        });
    }

    /// Parse type expression
    fn parse_type(&mut self) {
        // Check for function type with parentheses: (T1, T2) -> R
        if self.check(TokenKind::ParenBegin) {
            let ahead_arrow = (1..MAX_LOOKAHEAD)
                .find(|&i| matches!(self.peek_ahead(i), Some(TokenKind::Arrow)));
            
            if ahead_arrow.is_some() {
                // Function type
                self.emit_node(SyntaxKind::FunctionType, |inner| {
                    inner.parse_type_tuple_or_paren();
                    inner.expect(TokenKind::Arrow);
                    inner.parse_type();
                });
                return;
            }
        }
        
        self.parse_type_primary();
    }

    /// Parse primary type (primitive, tuple, etc.)
    fn parse_type_primary(&mut self) {
        match self.peek() {
            Some(TokenKind::FloatType) | Some(TokenKind::IntegerType) | Some(TokenKind::StringType) => {
                self.emit_node(SyntaxKind::PrimitiveType, |inner| {
                    inner.bump();
                });
            }
            Some(TokenKind::ParenBegin) => {
                // Tuple type: (T1, T2, ...)
                self.parse_type_tuple_or_paren();
            }
            Some(TokenKind::BlockBegin) => {
                // Record type: {field: Type, ...}
                self.parse_type_record();
            }
            Some(TokenKind::ArrayBegin) => {
                // Array type: [T]
                self.emit_node(SyntaxKind::ArrayType, |inner| {
                    inner.expect(TokenKind::ArrayBegin);
                    inner.parse_type();
                    inner.expect(TokenKind::ArrayEnd);
                });
            }
            Some(TokenKind::Ident) => {
                // Type variable or named type
                self.emit_node(SyntaxKind::TypeIdent, |inner| {
                    inner.bump();
                });
            }
            _ => {
                // Error: unexpected token for type
                if !self.is_at_end() {
                    self.add_error(ParserError::unexpected_token(
                        self.current_token_index(),
                        "type",
                        &format!("{:?}", self.peek().unwrap_or(TokenKind::Eof)),
                    ));
                    self.bump();
                }
            }
        }
    }

    /// Parse tuple type or parenthesized type: (T) or (T1, T2)
    fn parse_type_tuple_or_paren(&mut self) {
        // Look ahead to determine if it's a tuple
        let is_tuple = (1..MAX_LOOKAHEAD)
            .find_map(|i| {
                match self.peek_ahead(i) {
                    Some(TokenKind::Comma) => Some(true),
                    Some(TokenKind::ParenEnd) => Some(false),
                    None => Some(false),
                    _ => None,
                }
            })
            .unwrap_or(false);

        if is_tuple {
            self.emit_node(SyntaxKind::TupleType, |inner| {
                inner.expect(TokenKind::ParenBegin);
                if !inner.check(TokenKind::ParenEnd) {
                    inner.parse_type();
                    while inner.check(TokenKind::Comma) {
                        inner.bump();
                        if !inner.check(TokenKind::ParenEnd) {
                            inner.parse_type();
                        }
                    }
                }
                inner.expect(TokenKind::ParenEnd);
            });
        } else {
            // Just parenthesized type
            self.bump(); // (
            self.parse_type();
            self.expect(TokenKind::ParenEnd);
        }
    }

    /// Parse record type: {field1: Type1, field2: Type2}
    fn parse_type_record(&mut self) {
        self.emit_node(SyntaxKind::RecordType, |inner| {
            inner.expect(TokenKind::BlockBegin);
            
            if !inner.check(TokenKind::BlockEnd) {
                // Parse field: name : Type
                inner.expect(TokenKind::Ident);
                inner.expect(TokenKind::Colon);
                inner.parse_type();
                
                while inner.check(TokenKind::Comma) {
                    inner.bump();
                    if !inner.check(TokenKind::BlockEnd) {
                        inner.expect(TokenKind::Ident);
                        inner.expect(TokenKind::Colon);
                        inner.parse_type();
                    }
                }
            }
            
            inner.expect(TokenKind::BlockEnd);
        });
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
            None | Some(TokenKind::Eof) => {
                // EOF reached when expecting an expression
                self.add_error(ParserError::unexpected_eof(
                    self.current_token_index(),
                    "expression",
                ));
            }
            _ => {
                // Error: unexpected token
                // Record the error and continue
                if let Some(kind) = self.peek() {
                    self.add_error(ParserError::unexpected_token(
                        self.current_token_index(),
                        "expression",
                        &format!("{kind:?}"),
                    ));
                }
                // Skip the unexpected token to recover
                self.bump();
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
                    
                    // Optional type annotation
                    if this.check(TokenKind::Colon) {
                        this.parse_type_annotation();
                    }
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

            // Optional return type annotation after '->'
            if this.check(TokenKind::Arrow) {
                this.bump();
                this.parse_type();
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

/// Parse tokens into a Green Tree (CST) with error collection
pub fn parse_cst(
    tokens: Vec<LosslessToken>,
    preparsed: &PreParsedTokens,
) -> (GreenNodeId, GreenNodeArena, Vec<LosslessToken>, Vec<ParserError>) {
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
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert_eq!(arena.kind(root_id), Some(SyntaxKind::Program));
        assert!(arena.children(root_id).is_some());
        assert!(
            errors.is_empty(),
            "Expected no errors, got: {}",
            errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("; ")
        );
    }

    #[test]
    fn test_parse_let_statement() {
        let source = "let x = 42";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        let children = arena.children(root_id).unwrap();
        assert!(!children.is_empty());
        assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
    }

    #[test]
    fn test_parse_function() {
        let source = "fn add(x, y) { x }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(arena.width(root_id) > 0);
        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }

    #[test]
    fn test_parse_tuple() {
        let source = "(1, 2, 3)";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        let children = arena.children(root_id).unwrap();
        assert!(!children.is_empty());
        assert!(errors.is_empty(), "Expected no Ïerrors, got {errors:?}");
    }

    #[test]
    fn test_parse_record() {
        let source = "{x = 1, y = 2}";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        let children = arena.children(root_id).unwrap();
        assert!(!children.is_empty());
        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }

    #[test]
    fn test_parse_error_recovery_missing_paren() {
        // Missing closing parenthesis in function
        let source = "fn test(x { 42 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (_root_id, _arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        // Should have collected an error
        assert!(!errors.is_empty(), "Expected parse errors");
        assert!(matches!(errors[0].detail, ErrorDetail::UnexpectedToken { .. }));
    }

    #[test]
    fn test_parse_error_recovery_missing_block() {
        // Missing expression in let binding
        let source = "let x =";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (_root_id, _arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        // Should have collected an error when EOF is reached after '='
        assert!(!errors.is_empty(), "Expected parse errors for incomplete let binding");
        // Error should be about unexpected EOF expecting an expression
        assert!(matches!(&errors[0].detail, ErrorDetail::UnexpectedEof { expected } if expected == "expression"),
                "Expected UnexpectedEof for 'expression', got: {:?}", errors[0].detail);
        // Parser should continue without crashing
        assert!(_arena.kind(_root_id).is_some());
    }

    #[test]
    fn test_parse_type_annotation_simple() {
        let source = "fn add(x: float, y: float) { x }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_lambda_with_type() {
        let source = "|x: float| -> float x";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_function_type() {
        let source = "fn apply(f: (float) -> float, x: float) { f(x) }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        assert!(arena.width(root_id) > 0);
    }

    // Quick Wins: Phase 1 test conversions from original parser tests
    // These tests verify basic CST structure for literals, variables, and simple constructs

    #[test]
    fn test_parse_int_literal() {
        // Converted from: test_int() in parser/test.rs
        let source = "3466";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        // Verify structure: Program -> Statement -> IntLiteral
        use crate::lossless_parser::cst_test_helpers::*;
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_string_literal() {
        // Converted from: test_string() in parser/test.rs
        let source = r#""teststr""#;
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        use crate::lossless_parser::cst_test_helpers::*;
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::StringLiteral);
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_variable() {
        // Converted from: test_var() in parser/test.rs
        let source = "hoge";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        use crate::lossless_parser::cst_test_helpers::*;
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::Identifier);
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_float_literal() {
        let source = "3.14";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        use crate::lossless_parser::cst_test_helpers::*;
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::FloatLiteral);
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_type_annotation_function_param() {
        // Converted from: test_typed_param() - simple parameter with type
        let source = "fn test(x: float) { x }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        use crate::lossless_parser::cst_test_helpers::*;
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::FunctionDecl);
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::TypeAnnotation);
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_lambda_simple() {
        // Converted from: test_lambda() - simple lambda expression
        let source = "|x| x";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        use crate::lossless_parser::cst_test_helpers::*;
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::LambdaExpr);
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_tuple_simple() {
        // Converted from: test_tuple() - simple tuple expression
        let source = "(1, 2)";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        use crate::lossless_parser::cst_test_helpers::*;
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::TupleExpr);
        // Verify two IntLiterals in the tuple
        assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 2);
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_record_simple() {
        // Converted from: test_record() - simple record expression
        let source = "{x = 1}";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        use crate::lossless_parser::cst_test_helpers::*;
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::RecordExpr);
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_if_simple() {
        // Converted from: test_if() - simple if expression with condition and branches
        let source = "if (1) { 2 } else { 3 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        use crate::lossless_parser::cst_test_helpers::*;
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::IfExpr);
        // Verify three IntLiterals (condition 1, then 2, else 3)
        assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 3);
        assert!(arena.width(root_id) > 0);
    }

    #[test]
    fn test_parse_let_binding() {
        // Converted from: test_let() - let binding expression
        let source = "let x = 42";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
        use crate::lossless_parser::cst_test_helpers::*;
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::LetDecl);
        assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
        assert!(arena.width(root_id) > 0);
    }
}
