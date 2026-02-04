/// CST Parser - parses token indices into Green Tree
/// This is a simple recursive descent parser that produces a CST
use super::green::{GreenNodeArena, GreenNodeId, GreenTreeBuilder, Marker, SyntaxKind};
use super::preparser::PreParsedTokens;
use super::token::{Token, TokenKind};
use thiserror::Error;

/// Error detail - structured error information
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ErrorDetail {
    /// Expected token not found (e.g., expected `)` but found `}`)
    #[error("Expected {expected}, found {found}")]
    UnexpectedToken { expected: String, found: String },
    /// Unexpected end of input
    #[error("Unexpected end of input, expected {expected}")]
    UnexpectedEof { expected: String },
    /// Invalid syntax with reason
    #[error("Invalid syntax: {reason}")]
    InvalidSyntax { reason: String },
}

/// Parser error with recovery information
#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[error("{detail}")]
pub struct ParserError {
    /// Token index where the error occurred
    pub token_index: usize,
    /// Error detail
    pub detail: ErrorDetail,
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
    tokens: Vec<Token>,
    preparsed: &'a PreParsedTokens,
    current: usize, // Index into preparsed.token_indices
    builder: GreenTreeBuilder,
    /// Collected parser errors for error recovery
    errors: Vec<ParserError>,
}

/// Maximum lookahead distance for disambiguation
const MAX_LOOKAHEAD: usize = 20;

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, preparsed: &'a PreParsedTokens) -> Self {
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

    /// Check if current position starts a path (possibly qualified) followed by macro expansion (!)
    /// Returns the offset where MacroExpand is found, or None if not a macro expansion
    fn find_macro_expand_after_path(&self) -> Option<usize> {
        let mut offset = 0;

        // First must be Ident
        if self.peek_ahead(offset) != Some(TokenKind::Ident) {
            return None;
        }
        offset += 1;

        // Consume :: Ident pairs
        while self.peek_ahead(offset) == Some(TokenKind::DoubleColon) {
            offset += 1; // ::
            if self.peek_ahead(offset) != Some(TokenKind::Ident) {
                return None; // Malformed path
            }
            offset += 1; // Ident
        }

        // Check if followed by MacroExpand
        if self.peek_ahead(offset) == Some(TokenKind::MacroExpand) {
            Some(offset)
        } else {
            None
        }
    }

    /// Get the current token
    #[allow(dead_code)]
    fn current_token(&self) -> Option<&Token> {
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

    /// Expect any of the given token kinds and consume the first match.
    /// Returns true if matched; otherwise records an error and returns false.
    fn expects(&mut self, kinds: &[TokenKind]) -> bool {
        let expected = kinds
            .iter()
            .map(|k| format!("{k:?}"))
            .collect::<Vec<_>>()
            .join(" or ");

        match self.peek() {
            Some(kind) if kinds.contains(&kind) => {
                self.bump();
                true
            }
            Some(found) => {
                self.add_error(ParserError::unexpected_token(
                    self.current_token_index(),
                    &expected,
                    &format!("{found:?}"),
                ));
                false
            }
            None => {
                self.add_error(ParserError::unexpected_eof(
                    self.current_token_index(),
                    &expected,
                ));
                false
            }
        }
    }

    /// Expect all token kinds in order, consuming each. Returns true if all matched.
    fn expect_all(&mut self, kinds: &[TokenKind]) -> bool {
        kinds
            .iter()
            .fold(true, |acc, &kind| acc & self.expect(kind))
    }

    /// Parse the entire program
    pub fn parse(mut self) -> (GreenNodeId, GreenNodeArena, Vec<Token>, Vec<ParserError>) {
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

    /// Check if the previous token has a linebreak in its trailing trivia
    fn has_trailing_linebreak(&self) -> bool {
        if self.current == 0 {
            return false;
        }
        let prev_idx = self.current - 1;
        if let Some(trivia_indices) = self.preparsed.trailing_trivia_map.get(&prev_idx) {
            for &trivia_idx in trivia_indices {
                if let Some(token) = self.tokens.get(trivia_idx)
                    && token.kind == TokenKind::LineBreak
                {
                    return true;
                }
            }
        }
        if let Some(trivia_indices) = self.preparsed.leading_trivia_map.get(&self.current) {
            for &trivia_idx in trivia_indices {
                if let Some(token) = self.tokens.get(trivia_idx)
                    && token.kind == TokenKind::LineBreak
                {
                    return true;
                }
            }
        }
        false
    }

    /// Get previous token kind if there is no trivia between previous and current tokens.
    fn prev_kind_if_adjacent(&self) -> Option<TokenKind> {
        if self.current == 0 {
            return None;
        }
        let curr_raw = *self.preparsed.token_indices.get(self.current)?;
        let prev_raw = *self.preparsed.token_indices.get(self.current - 1)?;
        if curr_raw == prev_raw + 1 {
            self.tokens.get(prev_raw).map(|t| t.kind)
        } else {
            None
        }
    }

    /// Parse a statement
    fn parse_statement(&mut self) {
        self.emit_node(SyntaxKind::Statement, |this| {
            // Check for visibility modifier first
            let has_pub = this.check(TokenKind::Pub);
            if has_pub {
                this.emit_node(SyntaxKind::VisibilityPub, |this| {
                    this.bump(); // consume 'pub'
                });
            }

            match this.peek() {
                Some(TokenKind::Function) => this.parse_function_decl(),
                Some(TokenKind::Macro) => this.parse_macro_decl(),
                Some(TokenKind::Let) => this.parse_let_decl(),
                Some(TokenKind::LetRec) => this.parse_letrec_decl(),
                Some(TokenKind::Include) => this.parse_include_stmt(),
                Some(TokenKind::Sharp) => this.parse_stage_decl(),
                Some(TokenKind::Mod) => this.parse_module_decl(),
                Some(TokenKind::Use) => this.parse_use_stmt(),
                _ => {
                    // Try parsing as expression
                    this.parse_expr();
                }
            }
        });
    }

    /// Parse module declaration: mod name { ... } or mod name;
    fn parse_module_decl(&mut self) {
        self.emit_node(SyntaxKind::ModuleDecl, |this| {
            this.expect(TokenKind::Mod);
            this.expect(TokenKind::Ident);

            // Check if this is an external file module (mod foo;) or inline module (mod foo { ... })
            if this.check(TokenKind::LineBreak) {
                // External file module: mod foo;
                // Just consume the line break, no body
                this.bump();
            } else if this.check(TokenKind::BlockBegin) {
                // Inline module: mod foo { ... }
                this.expect(TokenKind::BlockBegin);

                // Parse module body (statements)
                while !this.check(TokenKind::BlockEnd) && !this.is_at_end() {
                    this.parse_statement();
                }

                this.expect(TokenKind::BlockEnd);
            }
            // If neither, error recovery will handle it
        });
    }

    /// Parse use statement:
    /// - `use path::to::module` (single import)
    /// - `use path::{a, b, c}` (multiple imports)
    /// - `use path::*` (wildcard import)
    fn parse_use_stmt(&mut self) {
        self.emit_node(SyntaxKind::UseStmt, |this| {
            this.expect(TokenKind::Use);
            this.parse_use_path();
        });
    }

    /// Parse use path with support for multiple/wildcard imports
    fn parse_use_path(&mut self) {
        self.emit_node(SyntaxKind::QualifiedPath, |this| {
            this.expect(TokenKind::Ident);
            while this.check(TokenKind::DoubleColon) {
                this.bump(); // consume '::'

                // Check for wildcard: use foo::*
                if this.check(TokenKind::OpProduct) {
                    this.emit_node(SyntaxKind::UseTargetWildcard, |this2| {
                        this2.bump(); // consume '*'
                    });
                    return;
                }

                // Check for multiple imports: use foo::{a, b}
                if this.check(TokenKind::BlockBegin) {
                    this.emit_node(SyntaxKind::UseTargetMultiple, |this2| {
                        this2.bump(); // consume '{'
                        // Parse comma-separated identifiers
                        if this2.check(TokenKind::Ident) {
                            this2.bump();
                            while this2.check(TokenKind::Comma) {
                                this2.bump(); // consume ','
                                this2.expect(TokenKind::Ident);
                            }
                        }
                        this2.expect(TokenKind::BlockEnd);
                    });
                    return;
                }

                // Regular identifier
                this.expect(TokenKind::Ident);
            }
        });
    }

    /// Parse qualified path: ident (:: ident)*
    fn parse_qualified_path(&mut self) {
        self.emit_node(SyntaxKind::QualifiedPath, |this| {
            this.expect(TokenKind::Ident);
            while this.check(TokenKind::DoubleColon) {
                this.bump(); // consume '::'
                this.expect(TokenKind::Ident);
            }
        });
    }

    /// Parse macro declaration: macro name(params) { body }
    fn parse_macro_decl(&mut self) {
        self.emit_node(SyntaxKind::FunctionDecl, |this| {
            this.expect(TokenKind::Macro);

            // Mark macro name with IdentFunction kind
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

            // Optional return type annotation after '->'
            if this.check(TokenKind::Arrow) {
                this.bump();
                this.parse_type();
            }

            // Body
            if this.check(TokenKind::BlockBegin) {
                this.parse_block_expr();
            }
        });
    }

    /// Parse include statement: include("filename.mmm")
    fn parse_include_stmt(&mut self) {
        self.emit_node(SyntaxKind::IncludeStmt, |this| {
            this.expect_all(&[
                TokenKind::Include,
                TokenKind::ParenBegin,
                TokenKind::Str,
                TokenKind::ParenEnd,
            ]);
        });
    }

    /// Parse stage declaration: #stage(main) or #stage(macro)
    fn parse_stage_decl(&mut self) {
        self.emit_node(SyntaxKind::StageDecl, |this| {
            this.expect_all(&[TokenKind::Sharp, TokenKind::StageKwd, TokenKind::ParenBegin]);

            // Expect either 'main' or 'macro'
            this.expects(&[TokenKind::Main, TokenKind::Macro]);

            this.expect(TokenKind::ParenEnd);
        });
    }

    /// Parse macro expansion: MacroName!(args) or Qualified::Path!(args)
    fn parse_macro_expansion(&mut self) {
        self.emit_node(SyntaxKind::MacroExpansion, |this| {
            // Parse the macro name which can be a simple identifier or qualified path
            // Check if it's a qualified path
            if this.peek_ahead(1) == Some(TokenKind::DoubleColon) {
                this.parse_qualified_path();
            } else {
                this.expect(TokenKind::Ident);
            }

            // '!('
            this.expect_all(&[TokenKind::MacroExpand, TokenKind::ParenBegin]);

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
            this.parse_prefix_expr();
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

            // Optional return type annotation after '->'
            if this.check(TokenKind::Arrow) {
                this.bump();
                this.parse_type();
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

            // Optional type annotation after ':'
            if this.check(TokenKind::Colon) {
                this.parse_type_annotation();
            }

            if this.expect(TokenKind::Assign) {
                this.parse_expr();
            }
        });
    }

    /// Parse letrec declaration
    fn parse_letrec_decl(&mut self) {
        self.emit_node(SyntaxKind::LetRecDecl, |this| {
            this.expect_all(&[TokenKind::LetRec, TokenKind::Ident]);

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
            Some(TokenKind::BackQuote) => {
                // Code type: `Type
                self.emit_node(SyntaxKind::CodeType, |inner| {
                    inner.expect(TokenKind::BackQuote);
                    inner.parse_type();
                });
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
                this.expect_all(&[TokenKind::Ident, TokenKind::Assign]);
                this.parse_pattern();

                while this.check(TokenKind::Comma) {
                    this.bump(); // ,
                    if !this.check(TokenKind::BlockEnd) {
                        this.expect_all(&[TokenKind::Ident, TokenKind::Assign]);
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

                    // Optional default value
                    if this.check(TokenKind::Assign) {
                        this.emit_node(SyntaxKind::ParamDefault, |inner| {
                            inner.expect(TokenKind::Assign);
                            // Use precedence 1 to avoid statement boundary detection within params
                            inner.parse_expr_with_precedence(1);
                        });
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
        // Save marker BEFORE parsing prefix (LHS)
        let mut lhs_marker = self.builder.marker();

        self.parse_prefix_expr();

        // In statement context (min_prec == 0), stop at linebreak after prefix
        // unless the next token is an infix operator (e.g. leading '|>').
        if min_prec == 0
            && self.has_trailing_linebreak()
            && self
                .peek()
                .and_then(|k| self.get_infix_precedence(k))
                .is_none()
        {
            return;
        }

        while let Some(token_kind) = self.peek() {
            if let Some(prec) = self.get_infix_precedence(token_kind) {
                if prec < min_prec {
                    break;
                }

                // Wrap LHS and infix into BinaryExpr using the marker
                self.builder
                    .start_node_at(lhs_marker, SyntaxKind::BinaryExpr);
                // Consume the binary operator
                self.bump();
                // Parse the right-hand side with appropriate precedence
                // For left-associative operators, use prec + 1
                self.parse_expr_with_precedence(prec + 1);
                self.builder.finish_node();

                // Update marker for next iteration (now points to the BinaryExpr we just created)
                lhs_marker = Marker {
                    pos: self.builder.marker().pos.saturating_sub(1),
                };

                // After parsing infix, check for linebreak in statement context
                if min_prec == 0
                    && self.has_trailing_linebreak()
                    && self
                        .peek()
                        .and_then(|k| self.get_infix_precedence(k))
                        .is_none()
                {
                    break;
                }
            } else {
                break;
            }
        }
    }

    /// Parse expression with given minimum precedence without linebreak stopping.
    fn parse_expr_with_precedence_no_linebreak(&mut self, min_prec: usize) {
        // Save marker BEFORE parsing prefix (LHS)
        let mut lhs_marker = self.builder.marker();

        self.parse_prefix_expr();

        while let Some(token_kind) = self.peek() {
            if let Some(prec) = self.get_infix_precedence(token_kind) {
                if prec < min_prec {
                    break;
                }

                // Wrap LHS and infix into BinaryExpr using the marker
                self.builder
                    .start_node_at(lhs_marker, SyntaxKind::BinaryExpr);
                // Consume the binary operator
                self.bump();
                // Parse the right-hand side with appropriate precedence
                self.parse_expr_with_precedence_no_linebreak(prec + 1);
                self.builder.finish_node();

                // Update marker for next iteration
                lhs_marker = Marker {
                    pos: self.builder.marker().pos.saturating_sub(1),
                };
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
            // Pipe should have the lowest precedence to allow chaining with other operators
            TokenKind::OpPipe => Some(0),
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
                    if matches!(
                        this.peek(),
                        Some(TokenKind::OpSum) | Some(TokenKind::OpMinus)
                    ) && let Some(prev) = this.prev_kind_if_adjacent()
                        && matches!(
                            prev,
                            TokenKind::OpSum
                                | TokenKind::OpMinus
                                | TokenKind::OpProduct
                                | TokenKind::OpDivide
                                | TokenKind::OpModulo
                                | TokenKind::OpExponent
                        )
                    {
                        this.add_error(ParserError::invalid_syntax(
                            this.current_token_index(),
                            "Consecutive operators without whitespace are not allowed",
                        ));
                    }
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
        // Save marker before parsing primary (the potential callee/lhs)
        let mut lhs_marker = self.builder.marker();

        self.parse_primary();

        loop {
            // Do not allow postfix operators across a line break
            if self.has_trailing_linebreak() {
                break;
            }
            match self.peek() {
                Some(TokenKind::ParenBegin) => {
                    // Function call - wrap callee and arglist into CallExpr
                    self.builder.start_node_at(lhs_marker, SyntaxKind::CallExpr);
                    self.parse_arg_list();
                    self.builder.finish_node();

                    // Update marker for chained calls (e.g., foo()())
                    lhs_marker = Marker {
                        pos: self.builder.marker().pos.saturating_sub(1),
                    };
                }
                Some(TokenKind::Dot) => {
                    // Field access: expr.field or projection: expr.0, expr.1
                    self.builder
                        .start_node_at(lhs_marker, SyntaxKind::FieldAccess);
                    self.bump(); // consume Dot
                    // Can be either Ident (field name) or Int (tuple projection)
                    self.expects(&[TokenKind::Ident, TokenKind::Int]);
                    self.builder.finish_node();

                    // Update marker for chained access
                    lhs_marker = Marker {
                        pos: self.builder.marker().pos.saturating_sub(1),
                    };
                }
                Some(TokenKind::ArrayBegin) => {
                    // Array indexing: expr[index]
                    self.builder
                        .start_node_at(lhs_marker, SyntaxKind::IndexExpr);
                    self.bump(); // consume [
                    self.parse_expr();
                    self.expect(TokenKind::ArrayEnd);
                    self.builder.finish_node();

                    // Update marker for chained indexing
                    lhs_marker = Marker {
                        pos: self.builder.marker().pos.saturating_sub(1),
                    };
                }
                _ => break,
            }
        }
    }

    /// Parse argument list: (expr, expr, ...)
    fn parse_arg_list(&mut self) {
        self.emit_node(SyntaxKind::ArgList, |this| {
            this.expect(TokenKind::ParenBegin);

            if !this.check(TokenKind::ParenEnd) {
                // Do not stop at linebreaks, and allow low-precedence operators in args
                this.parse_expr_with_precedence_no_linebreak(0);

                while this.check(TokenKind::Comma) {
                    this.bump(); // ,
                    if !this.check(TokenKind::ParenEnd) {
                        this.parse_expr_with_precedence_no_linebreak(0);
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
            let ahead_arrow =
                (1..MAX_LOOKAHEAD).find(|&i| matches!(self.peek_ahead(i), Some(TokenKind::Arrow)));

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
            Some(TokenKind::FloatType)
            | Some(TokenKind::IntegerType)
            | Some(TokenKind::StringType) => {
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
            Some(TokenKind::BackQuote) => {
                // Code type: `Type
                self.emit_node(SyntaxKind::CodeType, |inner| {
                    inner.expect(TokenKind::BackQuote);
                    inner.parse_type();
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
            .find_map(|i| match self.peek_ahead(i) {
                Some(TokenKind::Comma) => Some(true),
                Some(TokenKind::ParenEnd) => Some(false),
                None => Some(false),
                _ => None,
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
            // Parenthesized type or unit type
            if self.peek_ahead(1) == Some(TokenKind::ParenEnd) {
                self.emit_node(SyntaxKind::UnitType, |inner| {
                    inner.expect(TokenKind::ParenBegin);
                    inner.expect(TokenKind::ParenEnd);
                });
            } else {
                self.bump(); // (
                self.parse_type();
                self.expect(TokenKind::ParenEnd);
            }
        }
    }

    /// Parse record type: {field1: Type1, field2: Type2}
    fn parse_type_record(&mut self) {
        self.emit_node(SyntaxKind::RecordType, |inner| {
            inner.expect(TokenKind::BlockBegin);

            if !inner.check(TokenKind::BlockEnd) {
                // Parse field: name : Type
                inner.expect_all(&[TokenKind::Ident, TokenKind::Colon]);
                inner.parse_type();

                while inner.check(TokenKind::Comma) {
                    inner.bump();
                    if !inner.check(TokenKind::BlockEnd) {
                        inner.expect_all(&[TokenKind::Ident, TokenKind::Colon]);
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
                // Check if this is a macro expansion (including qualified paths)
                if self.find_macro_expand_after_path().is_some() {
                    self.parse_macro_expansion();
                } else if self.peek_ahead(1) == Some(TokenKind::DoubleColon) {
                    // Qualified path: mod::submod::name
                    self.parse_qualified_path();
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
                    self.emit_node(SyntaxKind::ParenExpr, |this| {
                        this.bump(); // (
                        this.parse_expr();
                        this.expect(TokenKind::ParenEnd);
                    });
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
                self.emit_node(SyntaxKind::PlaceHolderLiteral, |this| {
                    this.bump();
                });
            }
            Some(TokenKind::BlockEnd) | Some(TokenKind::ParenEnd) | Some(TokenKind::ArrayEnd) => {
                // Do not consume closing tokens here; let the enclosing parser handle them.
                if let Some(kind) = self.peek() {
                    self.add_error(ParserError::unexpected_token(
                        self.current_token_index(),
                        "expression",
                        &format!("{kind:?}"),
                    ));
                }
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
                if this.check(TokenKind::BlockBegin) {
                    this.parse_block_expr();
                } else {
                    this.parse_expr();
                }
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

        let is_record_key = matches!(
            token1,
            Some(TokenKind::Ident) | Some(TokenKind::IdentParameter)
        );

        (matches!(token1, Some(TokenKind::DoubleDot))
            || (is_record_key
                && matches!(token2, Some(TokenKind::Assign) | Some(TokenKind::LeftArrow))))
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
                if this.check(TokenKind::Ident) && this.peek_ahead(1) == Some(TokenKind::LeftArrow)
                {
                    this.parse_expr(); // base record
                    this.expect(TokenKind::LeftArrow); // <-

                    // Parse updated fields
                    this.expects(&[TokenKind::Ident, TokenKind::IdentParameter]);
                    this.expect(TokenKind::Assign);
                    this.parse_expr();

                    while this.check(TokenKind::Comma) {
                        this.bump();
                        if !this.check(TokenKind::BlockEnd) {
                            this.expects(&[TokenKind::Ident, TokenKind::IdentParameter]);
                            this.expect(TokenKind::Assign);
                            this.parse_expr();
                        }
                    }
                } else {
                    // Regular record: {field = expr, ...} or {field = expr, ..}
                    // Allow incomplete record: {..}
                    if this.check(TokenKind::DoubleDot) {
                        this.bump(); // ..
                    } else {
                        // Parse field: name = expr
                        this.expects(&[TokenKind::Ident, TokenKind::IdentParameter]);
                        this.expect(TokenKind::Assign);
                        this.parse_expr();
                    }

                    while this.check(TokenKind::Comma) {
                        this.bump();
                        if !this.check(TokenKind::BlockEnd) {
                            if this.check(TokenKind::DoubleDot) {
                                // Incomplete record: field = expr, ..
                                this.bump(); // ..
                                break;
                            } else {
                                this.expects(&[TokenKind::Ident, TokenKind::IdentParameter]);
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

                // After a statement, if there's trailing linebreak/semicolon,
                // prepare for next statement. Otherwise, assume statement continues
                // (but note that parse_statement itself stops at natural boundaries)
                if this.has_trailing_linebreak() {
                    // Linebreak found - ready for next statement
                    continue;
                }
                // If no linebreak and not at block end, something may be off
                // but let parse_statement handle it in next iteration
            }

            this.expect(TokenKind::BlockEnd);
        });
    }

    /// Parse if expression: if cond then-expr [else expr]
    fn parse_if_expr(&mut self) {
        self.emit_node(SyntaxKind::IfExpr, |this| {
            this.expect(TokenKind::If);
            this.parse_expr(); // condition

            if this.check(TokenKind::BlockBegin) {
                this.parse_block_expr();
            } else {
                this.parse_expr();
            }

            if this.check(TokenKind::Else) {
                this.bump();
                if this.check(TokenKind::If) {
                    this.parse_if_expr();
                } else if this.check(TokenKind::BlockBegin) {
                    this.parse_block_expr();
                } else {
                    this.parse_expr();
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
    tokens: Vec<Token>,
    preparsed: &PreParsedTokens,
) -> (GreenNodeId, GreenNodeArena, Vec<Token>, Vec<ParserError>) {
    let parser = Parser::new(tokens, preparsed);
    parser.parse()
}
