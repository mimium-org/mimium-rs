#![allow(dead_code)]

use crate::ast::operators::Op;
use crate::ast::program::{Program, ProgramStatement};
use crate::ast::statement::{Statement, into_then_expr};
use crate::ast::{Expr, Literal};
use crate::interner::{ExprNodeId, Symbol, ToSymbol};
use crate::lossless_parser::cst_parser::ParserError;
use crate::lossless_parser::green::{GreenNode, GreenNodeArena, GreenNodeId, SyntaxKind};
use crate::lossless_parser::token::{LosslessToken, TokenKind};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::types::Type;
use crate::utils::metadata::{Location, Span};
use std::path::PathBuf;

/// Lossless CST -> existing `Program`/`Expr` lowering.
/// This is an initial, incomplete bridge used to incrementally replace
/// the legacy chumsky parser. It intentionally handles a subset of
/// syntax; unsupported constructs currently lower to `Expr::Error` to
/// keep the compiler stable while we flesh out coverage.
pub struct LosslessLowerer<'a> {
    source: &'a str,
    tokens: &'a [LosslessToken],
    arena: &'a GreenNodeArena,
    file_path: PathBuf,
}

impl<'a> LosslessLowerer<'a> {
    pub fn new(
        source: &'a str,
        tokens: &'a [LosslessToken],
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
        let mut statements = Vec::new();

        if let Some(children) = self.arena.children(root) {
            for child in children {
                if self.arena.kind(*child) == Some(SyntaxKind::Statement)
                    && let Some((stmt, span)) = self.lower_statement(*child)
                {
                    statements.push((stmt, span));
                }
            }
        }

        Program { statements }
    }

    /// Lower a statement node. Unknown forms become `Statement::Error`.
    fn lower_statement(&self, node: GreenNodeId) -> Option<(ProgramStatement, Span)> {
        let span = self.node_span(node)?;
        let stmt = match self.arena.kind(node) {
            Some(SyntaxKind::Statement) => {
                // Inspect children to find the first meaningful node.
                let mut inner_kind = None;
                let mut inner_id = None;
                if let Some(children) = self.arena.children(node) {
                    for child in children {
                        if let Some(kind) = self.arena.kind(*child) {
                            inner_kind = Some(kind);
                            inner_id = Some(*child);
                            break;
                        }
                    }
                }

                match (inner_kind, inner_id) {
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

    fn lower_let_decl(&self, node: GreenNodeId) -> Option<ProgramStatement> {
        let pattern_node = self.find_child(node, Self::is_pattern_kind)?;
        let (pat, pat_span) = self.lower_pattern(pattern_node)?;

        let expr_nodes = self.collect_expr_nodes_after(node, pattern_node);
        let value = self.lower_expr_sequence(&expr_nodes);
        let loc = self.location_from_span(pat_span.clone());
        let typed = TypedPattern::new(pat, Type::Unknown.into_id_with_location(loc.clone()));

        Some(ProgramStatement::GlobalStatement(Statement::Let(
            typed, value,
        )))
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
            Some(SyntaxKind::Identifier) => {
                let text = self.text_of_first_token(node).unwrap_or("");
                Expr::Var(text.to_symbol()).into_id(loc)
            }
            Some(SyntaxKind::TupleExpr) => {
                let elems = self
                    .child_exprs(node)
                    .into_iter()
                    .map(|id| self.lower_expr(id))
                    .collect();
                Expr::Tuple(elems).into_id(loc)
            }
            Some(SyntaxKind::ArrayExpr) => {
                let elems = self
                    .child_exprs(node)
                    .into_iter()
                    .map(|id| self.lower_expr(id))
                    .collect();
                Expr::ArrayLiteral(elems).into_id(loc)
            }
            Some(SyntaxKind::RecordExpr) => {
                let fields = self.lower_record_fields(node);
                Expr::RecordLiteral(fields).into_id(loc)
            }
            Some(SyntaxKind::IfExpr) => {
                let children = self
                    .arena
                    .children(node)
                    .map(|c| c.to_vec())
                    .unwrap_or_default();
                let mut iter = children.iter().copied();

                // condition: nodes before first BlockExpr/IfExpr
                let mut cond_nodes = Vec::new();
                let mut then_node = None;
                let mut else_node = None;
                for child in iter.by_ref() {
                    match self.arena.kind(child) {
                        Some(SyntaxKind::BlockExpr) | Some(SyntaxKind::IfExpr) => {
                            then_node = Some(child);
                            break;
                        }
                        Some(kind) if Self::is_expr_kind(kind) => {
                            cond_nodes.push(child);
                        }
                        _ => {}
                    }
                }

                // remaining after then_node may contain else branch
                if let Some(child) = iter.next().filter(|child| {
                    matches!(
                        self.arena.kind(*child),
                        Some(SyntaxKind::BlockExpr | SyntaxKind::IfExpr)
                    )
                }) {
                    else_node = Some(child);
                }

                let cond = self.lower_expr_sequence(&cond_nodes);
                let then_expr = then_node
                    .map(|id| self.lower_expr(id))
                    .unwrap_or_else(|| Expr::Error.into_id(loc.clone()));
                let else_expr = else_node.map(|id| self.lower_expr(id));
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
                let rhs = self
                    .child_exprs(node)
                    .first()
                    .map(|&id| self.lower_expr(id))
                    .unwrap_or_else(|| Expr::Error.into_id(loc.clone()));
                Expr::UniOp((op, loc.span.clone()), rhs).into_id(loc)
            }
            Some(SyntaxKind::MacroExpansion) => {
                let (callee, args) = self.lower_macro_expand(node);
                Expr::MacroExpand(callee, args).into_id(loc)
            }
            Some(SyntaxKind::BinaryExpr)
            | Some(SyntaxKind::AssignExpr)
            | Some(SyntaxKind::CallExpr)
            | Some(SyntaxKind::FieldAccess)
            | Some(SyntaxKind::IndexExpr) => {
                // These need left-hand context; they are handled in `lower_expr_sequence`.
                Expr::Error.into_id(loc)
            }
            Some(SyntaxKind::BracketExpr) => {
                let body = self
                    .child_exprs(node)
                    .first()
                    .map(|&id| self.lower_expr(id))
                    .unwrap_or_else(|| Expr::Error.into_id(loc.clone()));
                Expr::Bracket(body).into_id(loc)
            }
            Some(SyntaxKind::EscapeExpr) => {
                let body = self
                    .child_exprs(node)
                    .first()
                    .map(|&id| self.lower_expr(id))
                    .unwrap_or_else(|| Expr::Error.into_id(loc.clone()));
                Expr::Escape(body).into_id(loc)
            }
            _ => Expr::Error.into_id(loc),
        }
    }

    fn lower_lambda(&self, node: GreenNodeId) -> (Vec<TypedId>, ExprNodeId) {
        let mut params = Vec::new();
        let mut body_nodes = Vec::new();

        if let Some(children) = self.arena.children(node) {
            for child in children {
                match self.arena.kind(*child) {
                    None => {
                        if let Some(token_text) = self.text_of_token_node(*child) {
                            let ty = Type::Unknown.into_id_with_location(
                                self.location_from_span(self.node_span(node).unwrap_or(0..0)),
                            );
                            params.push(TypedId::new(token_text.to_symbol(), ty));
                        }
                    }
                    Some(kind) if Self::is_expr_kind(kind) => {
                        body_nodes.push(*child);
                    }
                    _ => {}
                }
            }
        }

        let body = self.lower_expr_sequence(&body_nodes);

        (params, body)
    }

    fn lower_record_fields(&self, node: GreenNodeId) -> Vec<crate::ast::RecordField> {
        let mut fields = Vec::new();
        let mut current: Option<Symbol> = None;

        if let Some(children) = self.arena.children(node) {
            for child in children {
                if let Some(text) = self.text_of_token_node(*child) {
                    // Ident before '='
                    current = Some(text.to_symbol());
                } else if self.arena.kind(*child).map(Self::is_expr_kind) == Some(true)
                    && let Some(name) = current.take()
                {
                    fields.push(crate::ast::RecordField {
                        name,
                        expr: self.lower_expr(*child),
                    });
                }
            }
        }

        fields
    }

    fn lower_block_statements(&self, node: GreenNodeId) -> Vec<(Statement, Location)> {
        let mut stmts = Vec::new();
        if let Some(children) = self.arena.children(node) {
            for child in children {
                if self.arena.kind(*child) == Some(SyntaxKind::Statement)
                    && let Some((stmt, span)) = self.lower_statement(*child)
                {
                    stmts.push((
                        match stmt {
                            ProgramStatement::GlobalStatement(s) => s,
                            ProgramStatement::StageDeclaration { stage } => {
                                Statement::DeclareStage(stage)
                            }
                            _ => Statement::Error,
                        },
                        self.location_from_span(span),
                    ));
                }
            }
        }
        stmts
    }

    fn lower_pattern(&self, node: GreenNodeId) -> Option<(Pattern, Span)> {
        let span = self.node_span(node)?;
        let pat = match self.arena.kind(node) {
            Some(SyntaxKind::SinglePattern) => {
                let name = self.text_of_first_token(node).unwrap_or("").to_symbol();
                Pattern::Single(name)
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
                let mut items = Vec::new();
                if let Some(children) = self.arena.children(node) {
                    let mut current = None;
                    for child in children {
                        if let Some(text) = self.text_of_token_node(*child) {
                            current = Some(text.to_symbol());
                        } else if let Some((p, _)) = self.lower_pattern(*child)
                            && let Some(name) = current.take()
                        {
                            items.push((name, p));
                        }
                    }
                }
                Pattern::Record(items)
            }
            _ => Pattern::Error,
        };

        Some((pat, span))
    }

    fn lower_expr_sequence(&self, nodes: &[GreenNodeId]) -> ExprNodeId {
        let mut acc: Option<ExprNodeId> = None;

        for &node in nodes {
            let kind = self.arena.kind(node);
            match kind {
                Some(SyntaxKind::BinaryExpr) => {
                    if let Some(lhs) = acc.take() {
                        acc = Some(self.lower_binary(lhs, node));
                    } else {
                        acc = Some(self.lower_expr(node));
                    }
                }
                Some(SyntaxKind::AssignExpr) => {
                    if let Some(lhs) = acc.take() {
                        acc = Some(self.lower_assign(lhs, node));
                    } else {
                        acc = Some(self.lower_expr(node));
                    }
                }
                Some(SyntaxKind::CallExpr) => {
                    if let Some(callee) = acc.take() {
                        acc = Some(self.lower_call(callee, node));
                    } else {
                        acc = Some(self.lower_expr(node));
                    }
                }
                Some(SyntaxKind::FieldAccess) => {
                    if let Some(lhs) = acc.take() {
                        acc = Some(self.lower_field_access(lhs, node));
                    } else {
                        acc = Some(self.lower_expr(node));
                    }
                }
                Some(SyntaxKind::IndexExpr) => {
                    if let Some(lhs) = acc.take() {
                        acc = Some(self.lower_index(lhs, node));
                    } else {
                        acc = Some(self.lower_expr(node));
                    }
                }
                Some(_) => {
                    acc = Some(self.lower_expr(node));
                }
                None => {}
            }
        }

        acc.unwrap_or_else(|| Expr::Error.into_id_without_span())
    }

    fn lower_binary(&self, lhs: ExprNodeId, node: GreenNodeId) -> ExprNodeId {
        let (op, op_span) = self
            .extract_binary_op(node)
            .unwrap_or((Op::Unknown("".to_string()), 0..0));

        let rhs_nodes = self.child_exprs(node);
        let rhs = self.lower_expr_sequence(&rhs_nodes);

        let lhs_span = lhs.to_span();
        let rhs_span = rhs.to_span();
        let loc = self.location_from_span(merge_spans(lhs_span, rhs_span));
        Expr::BinOp(lhs, (op, op_span), rhs).into_id(loc)
    }

    fn lower_assign(&self, lhs: ExprNodeId, node: GreenNodeId) -> ExprNodeId {
        let rhs_nodes = self.child_exprs(node);
        let rhs = self.lower_expr_sequence(&rhs_nodes);
        let loc = self.location_from_span(merge_spans(lhs.to_span(), rhs.to_span()));
        Expr::Assign(lhs, rhs).into_id(loc)
    }

    fn lower_call(&self, callee: ExprNodeId, node: GreenNodeId) -> ExprNodeId {
        let args = self.lower_arg_list(node);
        let call_span = self.node_span(node).unwrap_or_else(|| callee.to_span());
        let loc = self.location_from_span(merge_spans(callee.to_span(), call_span));
        Expr::Apply(callee, args).into_id(loc)
    }

    fn lower_field_access(&self, lhs: ExprNodeId, node: GreenNodeId) -> ExprNodeId {
        let lhs_span = lhs.to_span();
        let mut field = None;
        for idx in self.walk_tokens(node) {
            if let Some(tok) = self.tokens.get(idx) {
                match tok.kind {
                    TokenKind::Ident => {
                        field = Some(Expr::FieldAccess(lhs, tok.text(self.source).to_symbol()));
                        break;
                    }
                    TokenKind::Int => {
                        if let Ok(n) = tok.text(self.source).parse::<i64>() {
                            field = Some(Expr::Proj(lhs, n));
                            break;
                        }
                    }
                    _ => {}
                }
            }
        }
        let expr = field.unwrap_or(Expr::Error);
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

    fn lower_index(&self, lhs: ExprNodeId, node: GreenNodeId) -> ExprNodeId {
        let lhs_span = lhs.to_span();
        let rhs_nodes = self.child_exprs(node);
        let idx = self.lower_expr_sequence(&rhs_nodes);
        let loc = self.location_from_span(merge_spans(lhs_span, idx.to_span()));
        Expr::ArrayAccess(lhs, idx).into_id(loc)
    }

    fn lower_macro_expand(&self, node: GreenNodeId) -> (ExprNodeId, Vec<ExprNodeId>) {
        let name = self
            .find_token(node, |kind| matches!(kind, TokenKind::Ident))
            .and_then(|idx| self.token_text(idx))
            .unwrap_or("")
            .to_symbol();
        let args = self.lower_arg_list(node);
        (Expr::Var(name).into_id_without_span(), args)
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

    fn token_text(&self, index: usize) -> Option<&'a str> {
        self.tokens.get(index).map(|t| t.text(self.source))
    }

    fn node_span(&self, node: GreenNodeId) -> Option<Span> {
        match self.arena.get(node) {
            GreenNode::Token { token_index, .. } => {
                self.tokens.get(*token_index).map(|t| t.start..t.end())
            }
            GreenNode::Internal { children, .. } => {
                let mut start: Option<usize> = None;
                let mut end: Option<usize> = None;

                for child in children.iter() {
                    if let Some(span) = self.node_span(*child) {
                        start = Some(start.map_or(span.start, |s| s.min(span.start)));
                        end = Some(end.map_or(span.end, |e| e.max(span.end)));
                    }
                }

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
        let mut args = Vec::new();
        let mut current: Vec<GreenNodeId> = Vec::new();

        if let Some(children) = self.arena.children(node) {
            for child in children {
                match self.arena.get(*child) {
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
                        if self.arena.kind(*child).map(Self::is_expr_kind) == Some(true) {
                            current.push(*child);
                        }
                    }
                }
            }
        }

        if !current.is_empty() {
            args.push(self.lower_expr_sequence(&current));
        }

        args
    }

    fn is_expr_kind(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::BinaryExpr
                | SyntaxKind::UnaryExpr
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
                | SyntaxKind::Identifier
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
        self.walk_tokens(node).into_iter().find_map(|idx| {
            let tok = self.tokens.get(idx)?;
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
                _ => None,
            }?;
            Some((op, tok.start..tok.end()))
        })
    }
}

/// Full pipeline helper: tokenize, parse CST, then lower into `Program`.
pub fn parse_program_lossless(source: &str, file_path: PathBuf) -> (Program, Vec<ParserError>) {
    let tokens = crate::lossless_parser::tokenize(source);
    let preparsed = crate::lossless_parser::preparse(&tokens);
    let (root, arena, tokens, errors) = crate::lossless_parser::parse_cst(tokens, &preparsed);
    let lowerer = LosslessLowerer::new(source, &tokens, &arena, file_path);
    let program = lowerer.lower_program(root);
    (program, errors)
}

fn merge_spans(a: Span, b: Span) -> Span {
    a.start.min(b.start)..a.end.max(b.end)
}
