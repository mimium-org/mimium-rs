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
                Some(TokenKind::Include) => this.parse_include_stmt(),
                Some(TokenKind::Sharp) => this.parse_stage_decl(),
                _ => {
                    // Try parsing as expression
                    this.parse_expr();
                }
            }
        });
    }

    /// Parse include statement: include("filename.mmm")
    fn parse_include_stmt(&mut self) {
        self.emit_node(SyntaxKind::IncludeStmt, |this| {
            this.expect(TokenKind::Include);
            this.expect(TokenKind::ParenBegin);
            this.expect(TokenKind::Str);
            this.expect(TokenKind::ParenEnd);
        });
    }

    /// Parse stage declaration: #stage(main) or #stage(macro)
    fn parse_stage_decl(&mut self) {
        self.emit_node(SyntaxKind::StageDecl, |this| {
            this.expect(TokenKind::Sharp);
            this.expect(TokenKind::StageKwd);
            this.expect(TokenKind::ParenBegin);
            
            // Expect either 'main' or 'macro'
            if this.check(TokenKind::Main) {
                this.bump();
            } else if this.check(TokenKind::Macro) {
                this.bump();
            } else {
                this.expect(TokenKind::Main); // Will trigger error
            }
            
            this.expect(TokenKind::ParenEnd);
        });
    }

    /// Parse macro expansion: MacroName!(args)
    fn parse_macro_expansion(&mut self) {
        self.emit_node(SyntaxKind::MacroExpansion, |this| {
            // Macro name (identifier)
            this.expect(TokenKind::Ident);
            // Exclamation mark
            this.expect(TokenKind::MacroExpand);
            // Arguments in parentheses
            this.expect(TokenKind::ParenBegin);
            
            // Parse arguments as expression list
            if !this.check(TokenKind::ParenEnd) {
                this.parse_expr();
                while this.check(TokenKind::Comma) {
                    this.bump();
                    if !this.check(TokenKind::ParenEnd) {
                        this.parse_expr();
                    }
                }
            }
            
            this.expect(TokenKind::ParenEnd);
        });
    }

    /// Parse bracket (quote) expression: `expr or `{block}
    fn parse_bracket_expr(&mut self) {
        self.emit_node(SyntaxKind::BracketExpr, |this| {
            this.expect(TokenKind::BackQuote);
            
            // Check if it's a block or single expression
            if this.check(TokenKind::BlockBegin) {
                this.parse_block_expr();
            } else {
                this.parse_expr();
            }
        });
    }

    /// Parse escape (unquote) expression: $expr
    fn parse_escape_expr(&mut self) {
        self.emit_node(SyntaxKind::EscapeExpr, |this| {
            this.expect(TokenKind::Dollar);
            this.parse_expr();
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

    /// Parse let declaration: let pattern = expr
    fn parse_let_decl(&mut self) {
        self.emit_node(SyntaxKind::LetDecl, |this| {
            this.expect(TokenKind::Let);
            this.parse_pattern(); // Parse pattern instead of just identifier

            if this.expect(TokenKind::Assign) {
                this.parse_expr();
            }
        });
    }

    /// Parse letrec declaration
    fn parse_letrec_decl(&mut self) {
        self.emit_node(SyntaxKind::LetRecDecl, |this| {
            this.expect(TokenKind::LetRec);
            this.expect(TokenKind::Ident); // letrec only supports simple identifier

            if this.expect(TokenKind::Assign) {
                this.parse_expr();
            }
        });
    }

    /// Parse pattern: single identifier, tuple pattern, or record pattern
    fn parse_pattern(&mut self) {
        match self.peek() {
            Some(TokenKind::Ident) | Some(TokenKind::PlaceHolder) => {
                // Single pattern: x or _
                self.emit_node(SyntaxKind::SinglePattern, |this| {
                    this.bump();
                });
            }
            Some(TokenKind::ParenBegin) => {
                // Tuple pattern: (x, y, z)
                self.parse_tuple_pattern();
            }
            Some(TokenKind::BlockBegin) => {
                // Record pattern: {a = x, b = y}
                self.parse_record_pattern();
            }
            _ => {
                // Error: unexpected token
                if let Some(kind) = self.peek() {
                    self.add_error(ParserError::unexpected_token(
                        self.current_token_index(),
                        "pattern (identifier, tuple, or record)",
                        &format!("{kind:?}"),
                    ));
                }
                self.bump();
            }
        }
    }

    /// Parse tuple pattern: (x, y, z)
    fn parse_tuple_pattern(&mut self) {
        self.emit_node(SyntaxKind::TuplePattern, |this| {
            this.expect(TokenKind::ParenBegin);

            if !this.check(TokenKind::ParenEnd) {
                this.parse_pattern();

                while this.check(TokenKind::Comma) {
                    this.bump(); // ,
                    if !this.check(TokenKind::ParenEnd) {
                        this.parse_pattern();
                    }
                }
            }

            this.expect(TokenKind::ParenEnd);
        });
    }

    /// Parse record pattern: {a = x, b = y}
    fn parse_record_pattern(&mut self) {
        self.emit_node(SyntaxKind::RecordPattern, |this| {
            this.expect(TokenKind::BlockBegin);

            if !this.check(TokenKind::BlockEnd) {
                // Parse field: name = pattern
                this.expect(TokenKind::Ident); // field name
                this.expect(TokenKind::Assign);
                this.parse_pattern();

                while this.check(TokenKind::Comma) {
                    this.bump(); // ,
                    if !this.check(TokenKind::BlockEnd) {
                        this.expect(TokenKind::Ident); // field name
                        this.expect(TokenKind::Assign);
                        this.parse_pattern();
                    }
                }
            }

            this.expect(TokenKind::BlockEnd);
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

    /// Parse expression with operator precedence and assignment
    fn parse_expr(&mut self) {
        self.parse_assignment_expr();
    }

    /// Parse assignment expression: expr = expr
    /// Assignment has lower precedence than all binary operators
    fn parse_assignment_expr(&mut self) {
        let start_pos = self.current;
        self.parse_expr_with_precedence(0);

        // Check if this is an assignment
        if self.check(TokenKind::Assign) {
            // Need to wrap the already-parsed LHS in AssignExpr
            // This is a bit tricky since we've already added nodes to the tree
            // Solution: We'll handle this at the CST level by creating AssignExpr node
            // and the RHS after the operator
            self.emit_node(SyntaxKind::AssignExpr, |this| {
                this.expect(TokenKind::Assign);
                this.parse_expr_with_precedence(0);
            });
        }
    }

    /// Parse expression with given minimum precedence (Pratt parser)
    fn parse_expr_with_precedence(&mut self, min_prec: usize) {
        self.parse_prefix_expr();

        while let Some(token_kind) = self.peek() {
            if let Some(prec) = self.get_infix_precedence(token_kind) {
                if prec < min_prec {
                    break;
                }
                self.parse_infix_expr(prec);
            } else {
                break;
            }
        }
    }

    /// Get precedence of an infix operator (None means not an infix operator)
    fn get_infix_precedence(&self, token_kind: TokenKind) -> Option<usize> {
        match token_kind {
            TokenKind::OpOr => Some(1),
            TokenKind::OpAnd => Some(2),
            TokenKind::OpEqual | TokenKind::OpNotEqual => Some(3),
            TokenKind::OpLessThan
            | TokenKind::OpLessEqual
            | TokenKind::OpGreaterThan
            | TokenKind::OpGreaterEqual => Some(4),
            TokenKind::OpSum | TokenKind::OpMinus => Some(5),
            TokenKind::OpProduct | TokenKind::OpDivide | TokenKind::OpModulo => Some(6),
            TokenKind::OpExponent => Some(7),
            TokenKind::OpPipe => Some(8),
            TokenKind::OpAt => Some(9),
            _ => None,
        }
    }

    /// Get unary operator precedence
    fn get_prefix_precedence(&self, token_kind: Option<TokenKind>) -> bool {
        matches!(
            token_kind,
            Some(TokenKind::OpMinus)
                | Some(TokenKind::OpSum)
                | Some(TokenKind::Dollar)
                | Some(TokenKind::BackQuote)
        )
    }

    /// Parse prefix expression (unary operators and postfix operations)
    fn parse_prefix_expr(&mut self) {
        if self.get_prefix_precedence(self.peek()) {
            self.parse_unary_expr();
        } else {
            self.parse_postfix_expr();
        }
    }

    /// Parse unary expression
    fn parse_unary_expr(&mut self) {
        match self.peek() {
            Some(TokenKind::BackQuote) => {
                self.parse_bracket_expr();
            }
            Some(TokenKind::Dollar) => {
                self.parse_escape_expr();
            }
            _ => {
                // Standard unary operator (-, +)
                self.emit_node(SyntaxKind::UnaryExpr, |this| {
                    // Consume the unary operator
                    this.bump();
                    // Parse the operand with high precedence
                    this.parse_prefix_expr();
                });
            }
        }
    }

    /// Parse postfix expression (handles function calls, field access, etc.)
    fn parse_postfix_expr(&mut self) {
        self.parse_primary();

        loop {
            match self.peek() {
                Some(TokenKind::ParenBegin) => {
                    // Function call
                    self.emit_node(SyntaxKind::CallExpr, |this| {
                        this.parse_arg_list();
                    });
                }
                Some(TokenKind::Dot) => {
                    // Field access: expr.field or projection: expr.0, expr.1
                    self.emit_node(SyntaxKind::FieldAccess, |this| {
                        this.expect(TokenKind::Dot);
                        // Can be either Ident (field name) or Int (tuple projection)
                        if this.check(TokenKind::Ident) {
                            this.bump();
                        } else if this.check(TokenKind::Int) {
                            this.bump();
                        } else {
                            this.expect(TokenKind::Ident); // Will trigger error
                        }
                    });
                }
                Some(TokenKind::ArrayBegin) => {
                    // Array indexing: expr[index]
                    self.emit_node(SyntaxKind::IndexExpr, |this| {
                        this.expect(TokenKind::ArrayBegin);
                        this.parse_expr();
                        this.expect(TokenKind::ArrayEnd);
                    });
                }
                _ => break,
            }
        }
    }

    /// Parse infix expression
    fn parse_infix_expr(&mut self, prec: usize) {
        self.emit_node(SyntaxKind::BinaryExpr, |this| {
            // Consume the binary operator
            this.bump();
            // Parse the right-hand side with appropriate precedence
            // For left-associative operators, use prec + 1
            // For right-associative operators, use prec
            this.parse_expr_with_precedence(prec + 1);
        });
    }

    /// Parse argument list: (expr, expr, ...)
    fn parse_arg_list(&mut self) {
        self.emit_node(SyntaxKind::ArgList, |this| {
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
            Some(TokenKind::SelfLit) => {
                self.emit_node(SyntaxKind::SelfLiteral, |this| {
                    this.bump();
                });
            }
            Some(TokenKind::Now) => {
                self.emit_node(SyntaxKind::NowLiteral, |this| {
                    this.bump();
                });
            }
            Some(TokenKind::SampleRate) => {
                self.emit_node(SyntaxKind::SampleRateLiteral, |this| {
                    this.bump();
                });
            }
            Some(TokenKind::LambdaArgBeginEnd) => {
                self.parse_lambda_expr();
            }
            Some(TokenKind::Ident) => {
                // Check if next token is ! for macro expansion
                if self.peek_ahead(1) == Some(TokenKind::MacroExpand) {
                    self.parse_macro_expansion();
                } else {
                    self.emit_node(SyntaxKind::Identifier, |this| {
                        this.bump();
                    });
                }
            }
            Some(TokenKind::ArrayBegin) => {
                // Array literal: [1, 2, 3]
                self.parse_array_expr();
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
            Some(TokenKind::PlaceHolder) => {
                // Placeholder: _
                self.emit_node(SyntaxKind::Identifier, |this| {
                    this.bump();
                });
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

        // Get lookahead tokens once to avoid repeated peek_ahead calls
        let token1 = self.peek_ahead(1);
        let token2 = self.peek_ahead(2);
        
        match (token1, token2) {
            (Some(TokenKind::DoubleDot), _) => true,                          // .. (incomplete records)
            (Some(TokenKind::Ident), Some(TokenKind::Assign)) => true,        // field = value
            (Some(TokenKind::Ident), Some(TokenKind::LeftArrow)) => true,     // record <- field = value (update)
            _ => false,
        }
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
    /// Also handles record update: {base <- field = value}
    /// And incomplete records: {field1 = expr1, ..}
    fn parse_record_expr(&mut self) {
        self.emit_node(SyntaxKind::RecordExpr, |this| {
            this.expect(TokenKind::BlockBegin);

            if !this.check(TokenKind::BlockEnd) {
                // Check for record update: {base <- field = value}
                if this.check(TokenKind::Ident) && this.peek_ahead(1) == Some(TokenKind::LeftArrow) {
                    this.parse_expr(); // base record
                    this.expect(TokenKind::LeftArrow); // <-
                    
                    // Parse updated fields
                    this.expect(TokenKind::Ident);
                    this.expect(TokenKind::Assign);
                    this.parse_expr();
                    
                    while this.check(TokenKind::Comma) {
                        this.bump();
                        if !this.check(TokenKind::BlockEnd) {
                            this.expect(TokenKind::Ident);
                            this.expect(TokenKind::Assign);
                            this.parse_expr();
                        }
                    }
                } else {
                    // Regular record: {field = expr, ...} or {field = expr, ..}
                    // Parse field: name = expr
                    this.expect(TokenKind::Ident);
                    this.expect(TokenKind::Assign);
                    this.parse_expr();
                    
                    while this.check(TokenKind::Comma) {
                        this.bump();
                        if !this.check(TokenKind::BlockEnd) {
                            if this.check(TokenKind::DoubleDot) {
                                // Incomplete record: field = expr, ..
                                this.bump(); // ..
                                break;
                            } else {
                                this.expect(TokenKind::Ident);
                                this.expect(TokenKind::Assign);
                                this.parse_expr();
                            }
                        }
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

            // Else branch: can be either block or if expression (for else-if chains)
            if this.check(TokenKind::Else) {
                this.bump();
                if this.check(TokenKind::If) {
                    // Else-if: parse another if expression
                    this.parse_if_expr();
                } else if this.check(TokenKind::BlockBegin) {
                    // Regular else block
                    this.parse_block_expr();
                }
            }
        });
    }

    /// Parse array expression: [expr1, expr2, ...]
    fn parse_array_expr(&mut self) {
        self.emit_node(SyntaxKind::ArrayExpr, |this| {
            this.expect(TokenKind::ArrayBegin);

            if !this.check(TokenKind::ArrayEnd) {
                this.parse_expr();

                while this.check(TokenKind::Comma) {
                    this.bump(); // ,
                    if !this.check(TokenKind::ArrayEnd) {
                        this.parse_expr();
                    }
                }
            }

            this.expect(TokenKind::ArrayEnd);
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
