#![allow(dead_code)]

use crate::ast::operators::Op;
use crate::ast::program::{Program, ProgramStatement, QualifiedPath, Visibility};
use crate::ast::statement::{Statement, into_then_expr, stmt_from_expr_top};
use crate::ast::{Expr, Literal};
use crate::compiler::parser::cst_parser::ParserError;
use crate::compiler::parser::green::{GreenNode, GreenNodeArena, GreenNodeId, SyntaxKind};
use crate::compiler::parser::token::{Token, TokenKind};
use crate::interner::{ExprNodeId, Symbol, ToSymbol};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::types::Type;
use crate::utils::metadata::{Location, Span};
use std::ops::ControlFlow;
use std::path::PathBuf;

/// CST -> existing `Program`/`Expr` lowering.
/// This is an initial, incomplete bridge used to incrementally replace
/// the legacy chumsky parser. It intentionally handles a subset of
/// syntax; unsupported constructs currently lower to `Expr::Error` to
/// keep the compiler stable while we flesh out coverage.
pub struct Lowerer<'a> {
    source: &'a str,
    tokens: &'a [Token],
    arena: &'a GreenNodeArena,
    file_path: PathBuf,
}

impl<'a> Lowerer<'a> {
    pub fn new(
        source: &'a str,
        tokens: &'a [Token],
        arena: &'a GreenNodeArena,
        file_path: PathBuf,
    ) -> Self {
        Self {
            source,
            tokens,
            arena,
            file_path,
        }
    }

    /// Lower a full program node into the existing `Program` structure.
    pub fn lower_program(&self, root: GreenNodeId) -> Program {
        let (mut program_statements, pending_statements, pending_span) = self
            .arena
            .children(root)
            .map_or((Vec::new(), Vec::new(), 0..0), |children| {
                children
                    .iter()
                    .copied()
                    .filter(|child| self.arena.kind(*child) == Some(SyntaxKind::Statement))
                    .filter_map(|child| self.lower_statement(child))
                    .fold(
                        (Vec::new(), Vec::new(), 0..0),
                        |(mut program_statements, mut pending_statements, mut pending_span),
                         (stmt, span)| {
                            match &stmt {
                                ProgramStatement::GlobalStatement(Statement::Single(expr)) => {
                                    // Collect global single statements for potential Then-chaining
                                    let stmts = stmt_from_expr_top(*expr);
                                    let new_pending = stmts
                                        .into_iter()
                                        .map(|s| (s, self.location_from_span(span.clone())));
                                    pending_statements.extend(new_pending);
                                    pending_span = span;
                                }
                                _ => {
                                    // Non-global statement: flush pending, then add this
                                    if !pending_statements.is_empty() {
                                        if let Some(merged_expr) =
                                            into_then_expr(&pending_statements)
                                        {
                                            program_statements.push((
                                                ProgramStatement::GlobalStatement(
                                                    Statement::Single(merged_expr),
                                                ),
                                                pending_span.clone(),
                                            ));
                                        }
                                        pending_statements.clear();
                                    }
                                    program_statements.push((stmt, span));
                                }
                            }
                            (program_statements, pending_statements, pending_span)
                        },
                    )
            });

        // Flush any remaining pending statements
        if !pending_statements.is_empty()
            && let Some(merged_expr) = into_then_expr(&pending_statements)
        {
            program_statements.push((
                ProgramStatement::GlobalStatement(Statement::Single(merged_expr)),
                pending_span.clone(),
            ));
        }

        Program {
            statements: program_statements,
        }
    }

    /// Lower a statement node. Unknown forms become `Statement::Error`.
    fn lower_statement(&self, node: GreenNodeId) -> Option<(ProgramStatement, Span)> {
        let span = self.node_span(node)?;
        let stmt = match self.arena.kind(node) {
            Some(SyntaxKind::Statement) => {
                // Check for visibility modifier
                let visibility = self.extract_visibility(node);

                // Inspect children to find the first meaningful node.
                let (inner_kind, inner_id) = self
                    .arena
                    .children(node)
                    .and_then(|children| {
                        children
                            .iter()
                            .copied()
                            .find_map(|child| {
                                let kind = self.arena.kind(child)?;
                                // Skip VisibilityPub when looking for the main statement kind
                                if kind == SyntaxKind::VisibilityPub {
                                    None
                                } else {
                                    Some((kind, child))
                                }
                            })
                    })
                    .map(|(kind, id)| (Some(kind), Some(id)))
                    .unwrap_or((None, None));

                match (inner_kind, inner_id) {
                    (Some(SyntaxKind::FunctionDecl), Some(id)) => self
                        .lower_function_decl(id, visibility)
                        .unwrap_or(ProgramStatement::Error),
                    (Some(SyntaxKind::LetDecl), Some(id)) => {
                        self.lower_let_decl(id).unwrap_or(ProgramStatement::Error)
                    }
                    (Some(SyntaxKind::LetRecDecl), Some(id)) => self
                        .lower_letrec_decl(id)
                        .unwrap_or(ProgramStatement::Error),
                    (Some(SyntaxKind::IncludeStmt), Some(id)) => {
                        self.lower_include(id).unwrap_or(ProgramStatement::Error)
                    }
                    (Some(SyntaxKind::StageDecl), Some(id)) => {
                        self.lower_stage_decl(id).unwrap_or(ProgramStatement::Error)
                    }
                    (Some(SyntaxKind::ModuleDecl), Some(id)) => self
                        .lower_module_decl(id, visibility)
                        .unwrap_or(ProgramStatement::Error),
                    (Some(SyntaxKind::UseStmt), Some(id)) => {
                        self.lower_use_stmt(id).unwrap_or(ProgramStatement::Error)
                    }
                    (Some(_), _) => {
                        let expr_nodes = self.collect_expr_nodes(node);
                        let expr = self.lower_expr_sequence(&expr_nodes);
                        ProgramStatement::GlobalStatement(Statement::Single(expr))
                    }
                    _ => ProgramStatement::Error,
                }
            }
            _ => ProgramStatement::Error,
        };

        Some((stmt, span))
    }

    /// Extract visibility modifier from a statement node
    fn extract_visibility(&self, node: GreenNodeId) -> Visibility {
        self.arena
            .children(node)
            .and_then(|children| {
                children
                    .iter()
                    .find(|child| self.arena.kind(**child) == Some(SyntaxKind::VisibilityPub))
            })
            .map(|_| Visibility::Public)
            .unwrap_or(Visibility::Private)
    }

    /// Lower module declaration: mod name { ... }
    fn lower_module_decl(
        &self,
        node: GreenNodeId,
        visibility: Visibility,
    ) -> Option<ProgramStatement> {
        let name_idx = self.find_token(node, |kind| matches!(kind, TokenKind::Ident))?;
        let name = self.token_text(name_idx)?.to_symbol();

        // Lower the body statements
        let body: Vec<(ProgramStatement, Span)> = self
            .arena
            .children(node)
            .map(|children| {
                children
                    .iter()
                    .copied()
                    .filter(|child| self.arena.kind(*child) == Some(SyntaxKind::Statement))
                    .filter_map(|child| self.lower_statement(child))
                    .collect()
            })
            .unwrap_or_default();

        Some(ProgramStatement::ModuleDefinition {
            visibility,
            name,
            body,
        })
    }

    /// Lower use statement: use path::to::module
    fn lower_use_stmt(&self, node: GreenNodeId) -> Option<ProgramStatement> {
        let path_node = self.find_child(node, |kind| kind == SyntaxKind::QualifiedPath)?;
        let path = self.lower_qualified_path(path_node)?;
        Some(ProgramStatement::UseStatement { path })
    }

    /// Lower qualified path: ident (:: ident)*
    fn lower_qualified_path(&self, node: GreenNodeId) -> Option<QualifiedPath> {
        let segments: Vec<Symbol> = self
            .arena
            .children(node)
            .map(|children| {
                children
                    .iter()
                    .filter_map(|child| {
                        if let Some(token_idx) = self.get_token_index(*child)
                            && let Some(token) = self.tokens.get(token_idx)
                            && token.kind == TokenKind::Ident
                        {
                            self.token_text(token_idx).map(|s| s.to_symbol())
                        } else {
                            None
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();

        if segments.is_empty() {
            None
        } else {
            Some(QualifiedPath::new(segments))
        }
    }

    fn lower_let_decl(&self, node: GreenNodeId) -> Option<ProgramStatement> {
        let pattern_node = self.find_child(node, Self::is_pattern_kind)?;
        let (pat, pat_span) = self.lower_pattern(pattern_node)?;

        // Check for type annotation
        let type_annotation = if let Some(type_anno_node) =
            self.find_child(node, |kind| kind == SyntaxKind::TypeAnnotation)
        {
            // Find the actual type node within the TypeAnnotation
            if let Some(type_children) = self.arena.children(type_anno_node) {
                type_children
                    .iter()
                    .find(|c| {
                        self.arena
                            .kind(**c)
                            .map(Self::is_type_kind)
                            .unwrap_or(false)
                    })
                    .map(|type_node| self.lower_type(*type_node))
            } else {
                None
            }
        } else {
            None
        };

        let expr_nodes = self.collect_expr_nodes_after(node, pattern_node);
        let value = self.lower_expr_sequence(&expr_nodes);
        let loc = self.location_from_span(pat_span.clone());

        let ty =
            type_annotation.unwrap_or_else(|| Type::Unknown.into_id_with_location(loc.clone()));
        let typed = TypedPattern::new(pat, ty);

        Some(ProgramStatement::GlobalStatement(Statement::Let(
            typed, value,
        )))
    }

    fn lower_function_decl(&self, node: GreenNodeId, visibility: Visibility) -> Option<ProgramStatement> {
        let name_idx = self.find_token(node, |kind| {
            matches!(kind, TokenKind::IdentFunction | TokenKind::Ident)
        })?;
        let name = self.token_text(name_idx)?.to_symbol();

        let (params, params_span) = self
            .find_child(node, |kind| kind == SyntaxKind::ParamList)
            .map(|id| self.lower_param_list(id))
            .unwrap_or_else(|| (Vec::new(), self.node_span(node).unwrap_or(0..0)));

        // Check for return type annotation
        let return_type = self
            .find_child(node, Self::is_type_kind)
            .map(|type_node| self.lower_type(type_node));

        let body_node = self
            .find_child(node, |kind| kind == SyntaxKind::BlockExpr)
            .or_else(|| self.find_child(node, Self::is_expr_kind))?;
        let body = if self.arena.kind(body_node) == Some(SyntaxKind::BlockExpr) {
            // Flatten block body to then-expression for function definitions
            let stmts = self.lower_block_statements(body_node);
            into_then_expr(&stmts).unwrap_or_else(|| Expr::Error.into_id_without_span())
        } else {
            self.lower_expr(body_node)
        };

        let arg_loc = self.location_from_span(params_span.clone());
        Some(ProgramStatement::FnDefinition {
            visibility,
            name,
            args: (params, arg_loc),
            return_type,
            body,
        })
    }

    fn lower_letrec_decl(&self, node: GreenNodeId) -> Option<ProgramStatement> {
        let ident_token = self.find_token(node, |kind| matches!(kind, TokenKind::Ident))?;
        let expr_nodes = self.collect_expr_nodes(node);

        let name = self.token_text(ident_token)?.to_symbol();
        let span = self.node_span(node)?;
        let loc = self.location_from_span(span.clone());
        let ty = Type::Unknown.into_id_with_location(loc.clone());
        let id = TypedId::new(name, ty);
        let value = self.lower_expr_sequence(&expr_nodes);
        Some(ProgramStatement::GlobalStatement(Statement::LetRec(
            id, value,
        )))
    }

    fn lower_include(&self, node: GreenNodeId) -> Option<ProgramStatement> {
        let string_token = self.find_token(node, |kind| matches!(kind, TokenKind::Str))?;
        let raw = self.token_text(string_token)?;
        // Keep the original quoted content and strip quotes for the symbol.
        let content = raw.trim_matches('"').to_symbol();
        Some(ProgramStatement::Import(content))
    }

    fn lower_stage_decl(&self, node: GreenNodeId) -> Option<ProgramStatement> {
        use crate::ast::StageKind;
        let stage_token = self.find_token(node, |kind| {
            matches!(kind, TokenKind::Main | TokenKind::Macro)
        })?;
        let stage_kind = match self.tokens.get(stage_token)?.kind {
            TokenKind::Main => StageKind::Main,
            TokenKind::Macro => StageKind::Macro,
            _ => StageKind::Main,
        };
        Some(ProgramStatement::StageDeclaration { stage: stage_kind })
    }

    /// Lower expressions; unsupported constructs become `Expr::Error`.
    fn lower_expr(&self, node: GreenNodeId) -> ExprNodeId {
        let loc = self
            .node_span(node)
            .map(|span| self.location_from_span(span))
            .unwrap_or_default();

        match self.arena.kind(node) {
            Some(SyntaxKind::IntLiteral) => {
                let text = self.text_of_first_token(node).unwrap_or("0");
                Expr::Literal(Literal::Float(text.to_symbol())).into_id(loc)
            }
            Some(SyntaxKind::FloatLiteral) => {
                let text = self.text_of_first_token(node).unwrap_or("0.0");
                Expr::Literal(Literal::Float(text.to_symbol())).into_id(loc)
            }
            Some(SyntaxKind::StringLiteral) => {
                let text = self.text_of_first_token(node).unwrap_or("\"");
                Expr::Literal(Literal::String(text.trim_matches('"').to_symbol())).into_id(loc)
            }
            Some(SyntaxKind::SelfLiteral) => Expr::Literal(Literal::SelfLit).into_id(loc),
            Some(SyntaxKind::NowLiteral) => Expr::Literal(Literal::Now).into_id(loc),
            Some(SyntaxKind::SampleRateLiteral) => Expr::Literal(Literal::SampleRate).into_id(loc),
            Some(SyntaxKind::PlaceHolderLiteral) => {
                Expr::Literal(Literal::PlaceHolder).into_id(loc)
            }
            Some(SyntaxKind::Identifier) => {
                let text = self.text_of_first_token(node).unwrap_or("");
                Expr::Var(text.to_symbol()).into_id(loc)
            }
            Some(SyntaxKind::QualifiedPath) => {
                // Qualified path in expression context: modA::funcB
                if let Some(path) = self.lower_qualified_path(node) {
                    // If it's a single-segment path, treat it as a simple Var
                    if path.segments.len() == 1 {
                        Expr::Var(path.segments[0]).into_id(loc)
                    } else {
                        Expr::QualifiedVar(path).into_id(loc)
                    }
                } else {
                    Expr::Error.into_id(loc)
                }
            }
            Some(SyntaxKind::TupleExpr) => {
                let elems = self.lower_expr_list(node);
                Expr::Tuple(elems).into_id(loc)
            }
            Some(SyntaxKind::ArrayExpr) => {
                let elems = self.lower_expr_list(node);
                Expr::ArrayLiteral(elems).into_id(loc)
            }
            Some(SyntaxKind::RecordExpr) => {
                let fields = self.lower_record_fields(node);
                Expr::RecordLiteral(fields).into_id(loc)
            }
            Some(SyntaxKind::IfExpr) => {
                let expr_children = self.child_exprs(node);
                let raw_cond = expr_children
                    .first()
                    .map(|&id| self.lower_expr(id))
                    .unwrap_or_else(|| Expr::Error.into_id(loc.clone()));
                // Unwrap parens around condition to match legacy AST
                let cond = match raw_cond.to_expr() {
                    Expr::Paren(inner) => inner,
                    _ => raw_cond,
                };
                let then_expr = expr_children
                    .get(1)
                    .map(|&id| self.lower_expr(id))
                    .unwrap_or_else(|| Expr::Error.into_id(loc.clone()));
                let else_expr = expr_children.get(2).map(|&id| self.lower_expr(id));
                Expr::If(cond, then_expr, else_expr).into_id(loc)
            }
            Some(SyntaxKind::BlockExpr) => {
                let stmts = self.lower_block_statements(node);
                Expr::Block(into_then_expr(&stmts)).into_id(loc)
            }
            Some(SyntaxKind::LambdaExpr) => {
                let (params, body) = self.lower_lambda(node);
                Expr::Lambda(params, None, body).into_id(loc)
            }
            Some(SyntaxKind::UnaryExpr) => {
                let op = self.extract_unary_op(node).unwrap_or(Op::Minus);
                let rhs_nodes = self.child_exprs(node);
                let rhs = if rhs_nodes.is_empty() {
                    Expr::Error.into_id(loc.clone())
                } else {
                    self.lower_expr_sequence(&rhs_nodes)
                };
                Expr::UniOp((op, loc.span.clone()), rhs).into_id(loc)
            }
            Some(SyntaxKind::ParenExpr) => {
                let inner_nodes = self.child_exprs(node);
                let inner = if inner_nodes.is_empty() {
                    Expr::Error.into_id(loc.clone())
                } else {
                    self.lower_expr_sequence(&inner_nodes)
                };
                Expr::Paren(inner).into_id(loc)
            }
            Some(SyntaxKind::MacroExpansion) => {
                let (callee, args) = self.lower_macro_expand(node);
                Expr::MacroExpand(callee, args).into_id(loc)
            }
            Some(SyntaxKind::BinaryExpr) => self.lower_binary(node),
            Some(SyntaxKind::CallExpr) => self.lower_call(node),
            Some(SyntaxKind::FieldAccess) => self.lower_field_access(node),
            Some(SyntaxKind::IndexExpr) => self.lower_index(node),
            Some(SyntaxKind::AssignExpr) => {
                // AssignExpr still needs left-hand context from sequence
                Expr::Error.into_id(loc)
            }
            Some(SyntaxKind::BracketExpr) => {
                let body_nodes = self.child_exprs(node);
                let body = if body_nodes.is_empty() {
                    Expr::Error.into_id(loc.clone())
                } else {
                    self.lower_expr_sequence(&body_nodes)
                };
                Expr::Bracket(body).into_id(loc)
            }
            Some(SyntaxKind::EscapeExpr) => {
                let body_nodes = self.child_exprs(node);
                let body = if body_nodes.is_empty() {
                    Expr::Error.into_id(loc.clone())
                } else {
                    self.lower_expr_sequence(&body_nodes)
                };
                Expr::Escape(body).into_id(loc)
            }
            _ => Expr::Error.into_id(loc),
        }
    }

    fn lower_lambda(&self, node: GreenNodeId) -> (Vec<TypedId>, ExprNodeId) {
        let (params, body_nodes) = self
            .arena
            .children(node)
            .map(|children| {
                children.iter().copied().fold(
                    (Vec::new(), Vec::new()),
                    |(mut params, mut body_nodes), child| {
                        match self.arena.kind(child) {
                            None => {
                                // Check if it's a valid identifier token (not |, commas, etc.)
                                if let Some(token_index) = self.get_token_index(child)
                                    && let Some(token) = self.tokens.get(token_index)
                                    && matches!(
                                        token.kind,
                                        TokenKind::Ident | TokenKind::IdentParameter
                                    )
                                {
                                    let ty = Type::Unknown.into_id_with_location(
                                        self.location_from_span(
                                            self.node_span(node).unwrap_or(0..0),
                                        ),
                                    );
                                    params.push(TypedId::new(
                                        token.text(self.source).to_symbol(),
                                        ty,
                                    ));
                                }
                            }
                            Some(kind) if Self::is_expr_kind(kind) => {
                                body_nodes.push(child);
                            }
                            _ => {}
                        }
                        (params, body_nodes)
                    },
                )
            })
            .unwrap_or_else(|| (Vec::new(), Vec::new()));

        let body = self.lower_expr_sequence(&body_nodes);

        (params, body)
    }

    fn lower_record_fields(&self, node: GreenNodeId) -> Vec<crate::ast::RecordField> {
        let (mut fields, _) = self
            .arena
            .children(node)
            .map_or((Vec::new(), None), |children| {
                children.iter().copied().fold(
                    (Vec::new(), None),
                    |(mut fields, mut current), child| {
                        // Check if it's an identifier token (not =, comma, etc.)
                        if let Some(token_index) = self.get_token_index(child)
                            && let Some(token) = self.tokens.get(token_index)
                            && matches!(token.kind, TokenKind::Ident)
                        {
                            current = Some(token.text(self.source).to_symbol());
                            return (fields, current);
                        }

                        if self.arena.kind(child).map(Self::is_expr_kind) == Some(true)
                            && let Some(name) = current.take()
                        {
                            fields.push(crate::ast::RecordField {
                                name,
                                expr: self.lower_expr(child),
                            });
                        }
                        (fields, current)
                    },
                )
            });

        fields.sort_by(|a, b| a.name.as_ref().cmp(b.name.as_ref()));
        fields
    }

    fn lower_block_statements(&self, node: GreenNodeId) -> Vec<(Statement, Location)> {
        self.arena
            .children(node)
            .into_iter()
            .flatten()
            .copied()
            .filter(|child| self.arena.kind(*child) == Some(SyntaxKind::Statement))
            .filter_map(|child| self.lower_statement(child))
            .map(|(stmt, span)| {
                (
                    match stmt {
                        ProgramStatement::GlobalStatement(s) => s,
                        ProgramStatement::StageDeclaration { stage } => {
                            Statement::DeclareStage(stage)
                        }
                        _ => Statement::Error,
                    },
                    self.location_from_span(span),
                )
            })
            .collect()
    }

    fn lower_pattern(&self, node: GreenNodeId) -> Option<(Pattern, Span)> {
        let span = self.node_span(node)?;
        let pat = match self.arena.kind(node) {
            Some(SyntaxKind::Pattern) => {
                if let Some(child) = self.child_patterns(node).into_iter().next() {
                    // Forward to actual contained pattern node
                    return self.lower_pattern(child);
                } else {
                    Pattern::Error
                }
            }
            Some(SyntaxKind::SinglePattern) => {
                let name_text = self.text_of_first_token(node).unwrap_or("");
                if name_text == "_" {
                    Pattern::Placeholder
                } else {
                    Pattern::Single(name_text.to_symbol())
                }
            }
            Some(SyntaxKind::TuplePattern) => {
                let elems = self
                    .child_patterns(node)
                    .into_iter()
                    .filter_map(|id| self.lower_pattern(id))
                    .map(|(p, _)| p)
                    .collect();
                Pattern::Tuple(elems)
            }
            Some(SyntaxKind::RecordPattern) => {
                let (items, _) = self
                    .arena
                    .children(node)
                    .map_or((Vec::new(), None), |children| {
                        children.iter().copied().fold(
                            (Vec::new(), None),
                            |(mut items, mut current), child| {
                                if let Some(token_index) = self.get_token_index(child)
                                    && let Some(token) = self.tokens.get(token_index)
                                    && matches!(
                                        token.kind,
                                        TokenKind::Ident | TokenKind::IdentParameter
                                    )
                                {
                                    current = Some(token.text(self.source).to_symbol());
                                    return (items, current);
                                }

                                if self.arena.kind(child).map(Self::is_pattern_kind) == Some(true)
                                    && let Some((p, _)) = self.lower_pattern(child)
                                    && let Some(name) = current.take()
                                {
                                    items.push((name, p));
                                }
                                (items, current)
                            },
                        )
                    });
                Pattern::Record(items)
            }
            _ => Pattern::Error,
        };

        Some((pat, span))
    }

    fn lower_expr_sequence(&self, nodes: &[GreenNodeId]) -> ExprNodeId {
        let result = (0..nodes.len()).try_fold(
            (Option::<ExprNodeId>::None, false),
            |(acc, skip_next), i| {
                if skip_next {
                    return ControlFlow::Continue((acc, false));
                }

                let node = nodes[i];
                match self.arena.kind(node) {
                    Some(SyntaxKind::BinaryExpr) => {
                        ControlFlow::Continue((Some(self.lower_binary(node)), false))
                    }
                    Some(SyntaxKind::CallExpr) => {
                        ControlFlow::Continue((Some(self.lower_call(node)), false))
                    }
                    Some(SyntaxKind::FieldAccess) => {
                        ControlFlow::Continue((Some(self.lower_field_access(node)), false))
                    }
                    Some(SyntaxKind::IndexExpr) => {
                        ControlFlow::Continue((Some(self.lower_index(node)), false))
                    }
                    Some(SyntaxKind::AssignExpr) => {
                        if let Some(lhs) = acc {
                            let assign = self.lower_assign(lhs, node);
                            // Check if there's a continuation
                            if i + 1 < nodes.len() {
                                let cont = self.lower_expr_sequence(&nodes[(i + 1)..]);
                                let loc = self.location_from_span(merge_spans(
                                    assign.to_span(),
                                    cont.to_span(),
                                ));
                                return ControlFlow::Break(
                                    Expr::Then(assign, Some(cont)).into_id(loc),
                                );
                            }
                            // No continuation in this sequence; just return bare assign
                            // Multi-statement chaining will be handled by lower_program's into_then_expr
                            ControlFlow::Continue((Some(assign), false))
                        } else {
                            ControlFlow::Continue((Some(self.lower_expr(node)), false))
                        }
                    }
                    Some(_) => {
                        // If we previously built a Then(assign, None), attach the remainder as continuation
                        if let Some(prev) = acc
                            && let Expr::Then(first, None) = prev.to_expr()
                        {
                            let rhs = self.lower_expr_sequence(&nodes[i..]);
                            let loc =
                                self.location_from_span(merge_spans(prev.to_span(), rhs.to_span()));
                            return ControlFlow::Break(Expr::Then(first, Some(rhs)).into_id(loc));
                        }
                        ControlFlow::Continue((Some(self.lower_expr(node)), false))
                    }
                    None => ControlFlow::Continue((acc, false)),
                }
            },
        );

        match result {
            ControlFlow::Break(expr) => expr,
            ControlFlow::Continue((acc, _)) => {
                acc.unwrap_or_else(|| Expr::Error.into_id_without_span())
            }
        }
    }

    /// Lower a BinaryExpr where LHS is a child of the node
    fn lower_binary(&self, node: GreenNodeId) -> ExprNodeId {
        let (op, op_span) = self
            .extract_binary_op(node)
            .unwrap_or((Op::Unknown("".to_string()), 0..0));

        let expr_children = self.child_exprs(node);

        // With the new CST structure, BinaryExpr contains exactly:
        // - LHS expression (first child expression)
        // - operator token (handled by extract_binary_op)
        // - RHS expression (second child expression)
        // The LHS might itself be a BinaryExpr for chained operations like a + b + c

        let (lhs, rhs) = if expr_children.len() >= 2 {
            // Standard case: first expression is LHS, second is RHS
            let lhs_node = expr_children[0];
            let rhs_node = expr_children[expr_children.len() - 1];
            (self.lower_expr(lhs_node), self.lower_expr(rhs_node))
        } else if expr_children.len() == 1 {
            // Edge case: only one expression child (RHS), LHS might be missing
            (
                Expr::Error.into_id_without_span(),
                self.lower_expr(expr_children[0]),
            )
        } else {
            // No expression children
            (
                Expr::Error.into_id_without_span(),
                Expr::Error.into_id_without_span(),
            )
        };

        let loc = self.location_from_span(merge_spans(lhs.to_span(), rhs.to_span()));
        Expr::BinOp(lhs, (op, op_span), rhs).into_id(loc)
    }

    /// Lower a CallExpr where callee is a child of the node
    fn lower_call(&self, node: GreenNodeId) -> ExprNodeId {
        let expr_children = self.child_exprs(node);

        // CallExpr contains: callee expression, then ArgList
        let callee = if !expr_children.is_empty() {
            self.lower_expr(expr_children[0])
        } else {
            Expr::Error.into_id_without_span()
        };

        let args = self.lower_arg_list(node);
        let call_span = self.node_span(node).unwrap_or_else(|| callee.to_span());
        let loc = self.location_from_span(merge_spans(callee.to_span(), call_span));
        Expr::Apply(callee, args).into_id(loc)
    }

    /// Lower a FieldAccess where LHS is a child of the node
    fn lower_field_access(&self, node: GreenNodeId) -> ExprNodeId {
        let expr_children = self.child_exprs(node);

        let lhs = if !expr_children.is_empty() {
            self.lower_expr(expr_children[0])
        } else {
            Expr::Error.into_id_without_span()
        };

        let lhs_span = lhs.to_span();

        // Find the field name or index token (direct children only)
        let expr = self
            .arena
            .children(node)
            .into_iter()
            .flatten()
            .filter_map(|&child| self.get_token_index(child))
            .filter_map(|idx| self.tokens.get(idx))
            .find_map(|tok| match tok.kind {
                TokenKind::Ident => Some(Expr::FieldAccess(lhs, tok.text(self.source).to_symbol())),
                TokenKind::Int => tok
                    .text(self.source)
                    .parse::<i64>()
                    .ok()
                    .map(|n| Expr::Proj(lhs, n)),
                _ => None,
            })
            .unwrap_or(Expr::Error);

        let span = self
            .node_span(node)
            .map(|s| merge_spans(lhs_span.clone(), s))
            .unwrap_or(lhs_span);
        let loc = self.location_from_span(span);
        match expr {
            Expr::FieldAccess(_, _) | Expr::Proj(_, _) => expr.into_id(loc),
            _ => Expr::Error.into_id(loc),
        }
    }

    /// Lower an IndexExpr where LHS is a child of the node
    fn lower_index(&self, node: GreenNodeId) -> ExprNodeId {
        let expr_children = self.child_exprs(node);

        // IndexExpr contains: LHS expression, index expression (between [ and ])
        let (lhs, index) = if expr_children.len() >= 2 {
            (
                self.lower_expr(expr_children[0]),
                self.lower_expr(expr_children[1]),
            )
        } else if expr_children.len() == 1 {
            (
                self.lower_expr(expr_children[0]),
                Expr::Error.into_id_without_span(),
            )
        } else {
            (
                Expr::Error.into_id_without_span(),
                Expr::Error.into_id_without_span(),
            )
        };

        let lhs_span = lhs.to_span();
        let index_span = index.to_span();
        let loc = self.location_from_span(merge_spans(lhs_span, index_span));
        Expr::ArrayAccess(lhs, index).into_id(loc)
    }

    fn lower_assign(&self, lhs: ExprNodeId, node: GreenNodeId) -> ExprNodeId {
        let rhs_nodes = self.child_exprs(node);
        let rhs = self.lower_expr_sequence(&rhs_nodes);
        let loc = self.location_from_span(merge_spans(lhs.to_span(), rhs.to_span()));
        Expr::Assign(lhs, rhs).into_id(loc)
    }

    fn lower_macro_expand(&self, node: GreenNodeId) -> (ExprNodeId, Vec<ExprNodeId>) {
        let name_idx = self.find_token(node, |kind| matches!(kind, TokenKind::Ident));
        let name_text = name_idx.and_then(|idx| self.token_text(idx)).unwrap_or("");
        let name = name_text.to_symbol();
        // Span should include trailing '!' of macro invocation
        let ident_span = name_idx
            .and_then(|idx| self.tokens.get(idx).map(|t| t.start..t.end()))
            .unwrap_or(0..0);
        let bang_end = self
            .find_token(node, |kind| matches!(kind, TokenKind::MacroExpand))
            .and_then(|idx| self.tokens.get(idx).map(|t| t.end()))
            .unwrap_or(ident_span.end);
        let name_span = ident_span.start..bang_end;
        let args = self.lower_arg_list(node);
        let loc = self.location_from_span(name_span);
        (Expr::Var(name).into_id(loc), args)
    }

    fn lower_param_list(&self, node: GreenNodeId) -> (Vec<TypedId>, Span) {
        let params = if let Some(children) = self.arena.children(node) {
            let (new_params, _) =
                (0..children.len()).fold((Vec::new(), 0usize), |(mut acc, mut next_index), i| {
                    if i < next_index {
                        return (acc, next_index);
                    }

                    let child = children[i];
                    // Check for identifier token
                    if let GreenNode::Token { token_index, .. } = self.arena.get(child)
                        && let Some(token) = self.tokens.get(*token_index)
                        && matches!(token.kind, TokenKind::Ident | TokenKind::IdentParameter)
                    {
                        let name = token.text(self.source).to_symbol();
                        let loc = self.location_from_span(token.start..token.end());
                        let mut next = i + 1;
                        let mut ty = Type::Unknown.into_id_with_location(loc.clone());
                        let mut default_value = None;

                        // Optional type annotation
                        if next < children.len()
                            && self.arena.kind(children[next]) == Some(SyntaxKind::TypeAnnotation)
                        {
                            if let Some(type_children) = self.arena.children(children[next]) {
                                ty = type_children
                                    .iter()
                                    .find(|c| {
                                        self.arena
                                            .kind(**c)
                                            .map(Self::is_type_kind)
                                            .unwrap_or(false)
                                    })
                                    .map(|type_node| self.lower_type(*type_node))
                                    .unwrap_or_else(|| {
                                        Type::Unknown.into_id_with_location(loc.clone())
                                    });
                            }
                            next += 1;
                        }

                        // Optional default value
                        if next < children.len()
                            && self.arena.kind(children[next]) == Some(SyntaxKind::ParamDefault)
                        {
                            let expr_nodes = self.child_exprs(children[next]);
                            if !expr_nodes.is_empty() {
                                default_value = Some(self.lower_expr_sequence(&expr_nodes));
                            }
                            next += 1;
                        }

                        let tid = match default_value {
                            Some(default_value) => TypedId::with_default(name, ty, default_value),
                            None => TypedId::new(name, ty),
                        };

                        acc.push(tid);
                        next_index = next;
                    }

                    (acc, next_index.max(i + 1))
                });
            new_params
        } else {
            Vec::new()
        };

        let span = self.node_span(node).unwrap_or(0..0);
        (params, span)
    }

    fn child_exprs(&self, node: GreenNodeId) -> Vec<GreenNodeId> {
        self.arena
            .children(node)
            .into_iter()
            .flatten()
            .copied()
            .filter(|child| self.arena.kind(*child).map(Self::is_expr_kind) == Some(true))
            .collect()
    }

    fn child_patterns(&self, node: GreenNodeId) -> Vec<GreenNodeId> {
        self.arena
            .children(node)
            .into_iter()
            .flatten()
            .copied()
            .filter(|child| self.arena.kind(*child).map(Self::is_pattern_kind) == Some(true))
            .collect()
    }

    fn find_child(
        &self,
        node: GreenNodeId,
        predicate: impl Fn(SyntaxKind) -> bool,
    ) -> Option<GreenNodeId> {
        self.arena
            .children(node)?
            .iter()
            .copied()
            .find(|child| match self.arena.kind(*child) {
                Some(kind) => predicate(kind),
                None => false,
            })
    }

    fn collect_expr_nodes(&self, node: GreenNodeId) -> Vec<GreenNodeId> {
        self.arena
            .children(node)
            .into_iter()
            .flatten()
            .copied()
            .filter(|child| self.arena.kind(*child).map(Self::is_expr_kind) == Some(true))
            .collect()
    }

    fn collect_expr_nodes_after(&self, node: GreenNodeId, after: GreenNodeId) -> Vec<GreenNodeId> {
        self.arena
            .children(node)
            .into_iter()
            .flatten()
            .copied()
            .skip_while(|child| *child != after)
            .skip(1)
            .filter(|child| self.arena.kind(*child).map(Self::is_expr_kind) == Some(true))
            .collect()
    }

    fn find_token(
        &self,
        node: GreenNodeId,
        predicate: impl Fn(TokenKind) -> bool,
    ) -> Option<usize> {
        self.walk_tokens(node).into_iter().find(|&idx| {
            self.tokens
                .get(idx)
                .map(|t| predicate(t.kind))
                .unwrap_or(false)
        })
    }

    fn walk_tokens(&self, node: GreenNodeId) -> Vec<usize> {
        match self.arena.get(node) {
            GreenNode::Token { token_index, .. } => vec![*token_index],
            GreenNode::Internal { children, .. } => children
                .iter()
                .flat_map(|child| self.walk_tokens(*child))
                .collect(),
        }
    }

    fn text_of_first_token(&self, node: GreenNodeId) -> Option<&'a str> {
        self.walk_tokens(node)
            .into_iter()
            .next()
            .and_then(|idx| self.token_text(idx))
    }

    fn text_of_token_node(&self, node: GreenNodeId) -> Option<&'a str> {
        match self.arena.get(node) {
            GreenNode::Token { token_index, .. } => self.token_text(*token_index),
            _ => None,
        }
    }

    fn get_token_index(&self, node: GreenNodeId) -> Option<usize> {
        match self.arena.get(node) {
            GreenNode::Token { token_index, .. } => Some(*token_index),
            _ => None,
        }
    }

    fn token_text(&self, index: usize) -> Option<&'a str> {
        self.tokens.get(index).map(|t| t.text(self.source))
    }

    fn node_span(&self, node: GreenNodeId) -> Option<Span> {
        match self.arena.get(node) {
            GreenNode::Token { token_index, .. } => {
                self.tokens.get(*token_index).map(|t| t.start..t.end())
            }
            GreenNode::Internal { children, .. } => {
                let (start, end) = children
                    .iter()
                    .filter_map(|child| self.node_span(*child))
                    .fold(
                        (Option::<usize>::None, Option::<usize>::None),
                        |(start, end), span| {
                            let next_start = Some(start.map_or(span.start, |s| s.min(span.start)));
                            let next_end = Some(end.map_or(span.end, |e| e.max(span.end)));
                            (next_start, next_end)
                        },
                    );

                start.zip(end).map(|(s, e)| s..e)
            }
        }
    }

    fn location_from_span(&self, span: Span) -> Location {
        Location {
            span,
            path: self.file_path.clone(),
        }
    }

    fn lower_arg_list(&self, node: GreenNodeId) -> Vec<ExprNodeId> {
        let collect_args = |children: &[GreenNodeId]| {
            let (mut args, current) = children.iter().copied().fold(
                (Vec::new(), Vec::new()),
                |(mut args, mut current), child| {
                    match self.arena.get(child) {
                        GreenNode::Token { token_index, .. }
                            if self.tokens.get(*token_index).map(|t| t.kind)
                                == Some(TokenKind::Comma) =>
                        {
                            if !current.is_empty() {
                                args.push(self.lower_expr_sequence(&current));
                                current.clear();
                            }
                        }
                        _ => {
                            if self.arena.kind(child).map(Self::is_expr_kind) == Some(true) {
                                current.push(child);
                            }
                        }
                    }
                    (args, current)
                },
            );
            if !current.is_empty() {
                args.push(self.lower_expr_sequence(&current));
            }
            args
        };

        // Prefer extracting from an explicit ArgList child if present.
        if let Some(arg_node) = self.find_child(node, |kind| kind == SyntaxKind::ArgList) {
            if let Some(children) = self.arena.children(arg_node) {
                return collect_args(children);
            }
            return Vec::new();
        }

        // Fallback: scan direct children if no ArgList exists.
        if let Some(children) = self.arena.children(node) {
            return collect_args(children);
        }
        Vec::new()
    }

    fn lower_expr_list(&self, node: GreenNodeId) -> Vec<ExprNodeId> {
        let collect_elems = |children: &[GreenNodeId]| {
            let (mut elems, current) = children.iter().copied().fold(
                (Vec::new(), Vec::new()),
                |(mut elems, mut current), child| {
                    match self.arena.get(child) {
                        GreenNode::Token { token_index, .. }
                            if self.tokens.get(*token_index).map(|t| t.kind)
                                == Some(TokenKind::Comma) =>
                        {
                            if !current.is_empty() {
                                elems.push(self.lower_expr_sequence(&current));
                                current.clear();
                            }
                        }
                        _ => {
                            if self.arena.kind(child).map(Self::is_expr_kind) == Some(true) {
                                current.push(child);
                            }
                        }
                    }
                    (elems, current)
                },
            );
            if !current.is_empty() {
                elems.push(self.lower_expr_sequence(&current));
            }
            elems
        };

        self.arena
            .children(node)
            .map_or_else(Vec::new, collect_elems)
    }

    fn is_expr_kind(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::BinaryExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::ParenExpr
                | SyntaxKind::CallExpr
                | SyntaxKind::FieldAccess
                | SyntaxKind::IndexExpr
                | SyntaxKind::AssignExpr
                | SyntaxKind::ArrayExpr
                | SyntaxKind::MacroExpansion
                | SyntaxKind::BracketExpr
                | SyntaxKind::EscapeExpr
                | SyntaxKind::LambdaExpr
                | SyntaxKind::IfExpr
                | SyntaxKind::BlockExpr
                | SyntaxKind::TupleExpr
                | SyntaxKind::RecordExpr
                | SyntaxKind::IntLiteral
                | SyntaxKind::FloatLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::SelfLiteral
                | SyntaxKind::NowLiteral
                | SyntaxKind::SampleRateLiteral
                | SyntaxKind::PlaceHolderLiteral
                | SyntaxKind::Identifier
                | SyntaxKind::QualifiedPath
        )
    }

    fn is_pattern_kind(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::Pattern
                | SyntaxKind::SinglePattern
                | SyntaxKind::TuplePattern
                | SyntaxKind::RecordPattern
        )
    }

    fn extract_unary_op(&self, node: GreenNodeId) -> Option<Op> {
        self.walk_tokens(node)
            .into_iter()
            .find_map(|idx| match self.tokens.get(idx)?.kind {
                TokenKind::OpMinus => Some(Op::Minus),
                TokenKind::OpSum => Some(Op::Sum),
                _ => None,
            })
    }

    fn extract_binary_op(&self, node: GreenNodeId) -> Option<(Op, Span)> {
        // Only look at direct children of this node, not recursively
        // This is important because nested BinaryExpr would also contain operator tokens
        let children = self.arena.children(node)?;
        for &child in children {
            if let Some(idx) = self.get_token_index(child)
                && let Some(tok) = self.tokens.get(idx)
            {
                let op = match tok.kind {
                    TokenKind::OpSum => Some(Op::Sum),
                    TokenKind::OpMinus => Some(Op::Minus),
                    TokenKind::OpProduct => Some(Op::Product),
                    TokenKind::OpDivide => Some(Op::Divide),
                    TokenKind::OpEqual => Some(Op::Equal),
                    TokenKind::OpNotEqual => Some(Op::NotEqual),
                    TokenKind::OpLessThan => Some(Op::LessThan),
                    TokenKind::OpLessEqual => Some(Op::LessEqual),
                    TokenKind::OpGreaterThan => Some(Op::GreaterThan),
                    TokenKind::OpGreaterEqual => Some(Op::GreaterEqual),
                    TokenKind::OpModulo => Some(Op::Modulo),
                    TokenKind::OpExponent => Some(Op::Exponent),
                    TokenKind::OpAnd => Some(Op::And),
                    TokenKind::OpOr => Some(Op::Or),
                    TokenKind::OpAt => Some(Op::At),
                    TokenKind::OpPipe => Some(Op::Pipe),
                    _ => None,
                };
                if let Some(op) = op {
                    return Some((op, tok.start..tok.end()));
                }
            }
        }
        None
    }
}

/// Full pipeline helper: tokenize, parse CST, then lower into `Program`.
pub fn parse_program(source: &str, file_path: PathBuf) -> (Program, Vec<ParserError>) {
    let tokens = crate::compiler::parser::tokenize(source);
    let preparsed = crate::compiler::parser::preparse(&tokens);
    let (root, arena, tokens, errors) = crate::compiler::parser::parse_cst(tokens, &preparsed);
    let lowerer = Lowerer::new(source, &tokens, &arena, file_path);
    let program = lowerer.lower_program(root);
    (program, errors)
}

/// Parse source to ExprNodeId with error collection.
/// This is a compatibility function for the old parser API.
/// Also returns the ModuleEnv built from the parsed program.
pub fn parse_to_expr(
    source: &str,
    file_path: Option<PathBuf>,
) -> (
    ExprNodeId,
    crate::utils::module_env::ModuleEnv,
    crate::ast::program::ModuleInfo,
    Vec<Box<dyn crate::utils::error::ReportableError>>,
) {
    let path = file_path.unwrap_or_default();
    let (prog, parse_errs) = parse_program(source, path.clone());
    let errs =
        crate::compiler::parser::parser_errors_to_reportable(source, path.clone(), parse_errs);

    if prog.statements.is_empty() {
        return (
            Expr::Error.into_id_without_span(),
            crate::utils::module_env::ModuleEnv::new(),
            crate::ast::program::ModuleInfo::new(),
            errs,
        );
    }

    // Build module environment from the program before converting to expressions
    let module_env = crate::utils::module_env::ModuleEnv::from_program(&prog);

    let (expr, module_info, mut new_errs) = crate::ast::program::expr_from_program(prog, path);
    let mut all_errs = errs;
    all_errs.append(&mut new_errs);
    (expr, module_env, module_info, all_errs)
}

/// Add global context wrapper around AST.
/// This is a compatibility function for the old parser API.
pub fn add_global_context(ast: ExprNodeId, file_path: PathBuf) -> ExprNodeId {
    let span = ast.to_span();
    let loc = crate::utils::metadata::Location {
        span: span.clone(),
        path: file_path,
    };
    let res = Expr::Let(
        TypedPattern::new(
            Pattern::Single(crate::utils::metadata::GLOBAL_LABEL.to_symbol()),
            Type::Unknown.into_id_with_location(loc.clone()),
        ),
        Expr::Lambda(vec![], None, ast).into_id(loc.clone()),
        None,
    );
    res.into_id(loc)
}

/// Check if a SyntaxKind represents a type node
impl<'a> Lowerer<'a> {
    fn is_type_kind(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::PrimitiveType
                | SyntaxKind::UnitType
                | SyntaxKind::TupleType
                | SyntaxKind::RecordType
                | SyntaxKind::FunctionType
                | SyntaxKind::ArrayType
                | SyntaxKind::CodeType
                | SyntaxKind::TypeIdent
        )
    }

    /// Lower a type node to TypeNodeId
    fn lower_type(&self, node: GreenNodeId) -> crate::interner::TypeNodeId {
        use crate::types::{PType, RecordTypeField, Type};

        let span = self.node_span(node).unwrap_or(0..0);
        let loc = self.location_from_span(span);

        match self.arena.kind(node) {
            Some(SyntaxKind::PrimitiveType) => {
                let text = self.text_of_first_token(node).unwrap_or("float");
                let ptype = match text {
                    "float" => PType::Numeric,
                    "int" => PType::Int,
                    "string" => PType::String,
                    _ => PType::Numeric,
                };
                Type::Primitive(ptype).into_id_with_location(loc)
            }
            Some(SyntaxKind::UnitType) => Type::Primitive(PType::Unit).into_id_with_location(loc),
            Some(SyntaxKind::TupleType) => {
                let elem_types = self
                    .arena
                    .children(node)
                    .into_iter()
                    .flatten()
                    .filter(|child| {
                        self.arena
                            .kind(**child)
                            .map(Self::is_type_kind)
                            .unwrap_or(false)
                    })
                    .map(|child| self.lower_type(*child))
                    .collect::<Vec<_>>();
                Type::Tuple(elem_types).into_id_with_location(loc)
            }
            Some(SyntaxKind::FunctionType) => {
                // Function type: (T1, T2) -> R
                let children: Vec<_> = self
                    .arena
                    .children(node)
                    .into_iter()
                    .flatten()
                    .filter(|child| {
                        self.arena
                            .kind(**child)
                            .map(Self::is_type_kind)
                            .unwrap_or(false)
                    })
                    .collect();

                if children.len() >= 2 {
                    let param_type = self.lower_type(*children[0]);
                    let return_type = self.lower_type(*children[1]);
                    Type::Function {
                        arg: param_type,
                        ret: return_type,
                    }
                    .into_id_with_location(loc)
                } else {
                    Type::Unknown.into_id_with_location(loc)
                }
            }
            Some(SyntaxKind::RecordType) => {
                // Record type: {field: Type, ...}
                let mut fields = Vec::new();
                let mut current_field: Option<Symbol> = None;

                if let Some(children) = self.arena.children(node) {
                    for child in children {
                        // Check if it's an identifier token (not =, comma, etc.)
                        if let Some(token_index) = self.get_token_index(*child)
                            && let Some(token) = self.tokens.get(token_index)
                            && matches!(token.kind, TokenKind::Ident | TokenKind::IdentParameter)
                        {
                            current_field = Some(token.text(self.source).to_symbol());
                            continue;
                        }

                        if self.arena.kind(*child).map(Self::is_type_kind) == Some(true)
                            && let Some(name) = current_field.take()
                        {
                            fields.push(RecordTypeField::new(name, self.lower_type(*child), false));
                        }
                    }
                }
                Type::Record(fields).into_id_with_location(loc)
            }
            Some(SyntaxKind::CodeType) => {
                let inner = self
                    .arena
                    .children(node)
                    .into_iter()
                    .flatten()
                    .find(|child| self.arena.kind(**child).map(Self::is_type_kind) == Some(true))
                    .map(|child| self.lower_type(*child))
                    .unwrap_or_else(|| Type::Unknown.into_id_with_location(loc.clone()));
                Type::Code(inner).into_id_with_location(loc)
            }
            _ => Type::Unknown.into_id_with_location(loc),
        }
    }
}

fn merge_spans(a: Span, b: Span) -> Span {
    a.start.min(b.start)..a.end.max(b.end)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::{parse_cst, preparse, tokenize};

    fn parse_source(source: &str) -> Program {
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        let (green_id, arena, tokens, _errors) = parse_cst(tokens, &preparsed);
        let lowerer = Lowerer::new(source, &tokens, &arena, PathBuf::new());
        lowerer.lower_program(green_id)
    }

    #[test]
    fn test_parse_module_declaration() {
        let source = "mod mymod { fn foo() { 42 } }";
        let prog = parse_source(source);

        assert!(!prog.statements.is_empty());

        let stmt = &prog.statements[0].0;
        match stmt {
            ProgramStatement::ModuleDefinition {
                visibility,
                name,
                body,
            } => {
                assert_eq!(*visibility, Visibility::Private);
                assert_eq!(name.as_str(), "mymod");
                assert!(!body.is_empty());
            }
            _ => panic!("Expected ModuleDefinition, got {:?}", stmt),
        }
    }

    #[test]
    fn test_parse_pub_module_declaration() {
        let source = "pub mod mymod { fn bar() { 1 } }";
        let prog = parse_source(source);

        assert!(!prog.statements.is_empty());

        let stmt = &prog.statements[0].0;
        match stmt {
            ProgramStatement::ModuleDefinition {
                visibility,
                name,
                body,
            } => {
                assert_eq!(*visibility, Visibility::Public);
                assert_eq!(name.as_str(), "mymod");
                assert!(!body.is_empty());
            }
            _ => panic!("Expected ModuleDefinition, got {:?}", stmt),
        }
    }

    #[test]
    fn test_parse_use_statement() {
        let source = "use modA::funcB";
        let prog = parse_source(source);

        assert!(!prog.statements.is_empty());

        let stmt = &prog.statements[0].0;
        match stmt {
            ProgramStatement::UseStatement { path } => {
                assert_eq!(path.segments.len(), 2);
                assert_eq!(path.segments[0].as_str(), "modA");
                assert_eq!(path.segments[1].as_str(), "funcB");
            }
            _ => panic!("Expected UseStatement, got {:?}", stmt),
        }
    }

    #[test]
    fn test_parse_nested_module() {
        let source = "mod outer { mod inner { fn baz() { 0 } } }";
        let prog = parse_source(source);

        assert!(!prog.statements.is_empty());

        let stmt = &prog.statements[0].0;
        match stmt {
            ProgramStatement::ModuleDefinition {
                visibility,
                name,
                body,
            } => {
                assert_eq!(*visibility, Visibility::Private);
                assert_eq!(name.as_str(), "outer");
                assert!(!body.is_empty());

                // Check inner module
                let inner_stmt = &body[0].0;
                match inner_stmt {
                    ProgramStatement::ModuleDefinition {
                        name: inner_name, ..
                    } => {
                        assert_eq!(inner_name.as_str(), "inner");
                    }
                    _ => panic!("Expected inner ModuleDefinition"),
                }
            }
            _ => panic!("Expected ModuleDefinition, got {:?}", stmt),
        }
    }

    #[test]
    fn test_parse_use_with_long_path() {
        let source = "use a::b::c::d";
        let prog = parse_source(source);

        assert!(!prog.statements.is_empty());

        let stmt = &prog.statements[0].0;
        match stmt {
            ProgramStatement::UseStatement { path } => {
                assert_eq!(path.segments.len(), 4);
                assert_eq!(path.segments[0].as_str(), "a");
                assert_eq!(path.segments[1].as_str(), "b");
                assert_eq!(path.segments[2].as_str(), "c");
                assert_eq!(path.segments[3].as_str(), "d");
            }
            _ => panic!("Expected UseStatement, got {:?}", stmt),
        }
    }

    #[test]
    fn test_module_with_multiple_items() {
        let source = r#"
mod mymod {
    pub fn add(x, y) { x + y }
    fn private_func() { 42 }
    use other_mod
}
"#;
        let prog = parse_source(source);

        assert!(!prog.statements.is_empty());

        let stmt = &prog.statements[0].0;
        match stmt {
            ProgramStatement::ModuleDefinition { body, .. } => {
                // Should have at least 3 items
                assert!(body.len() >= 2);
            }
            _ => panic!("Expected ModuleDefinition"),
        }
    }
}
