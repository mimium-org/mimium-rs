//! CST-based pretty printer with comment-aware width calculation
//!
//! This module provides pretty printing directly from the CST (Concrete Syntax Tree),
//! ensuring that comments are included in width calculations for proper line breaking.

use std::path::PathBuf;

use mimium_lang::compiler::parser::{
    GreenNodeArena, GreenNodeId, PreParsedTokens, SyntaxKind, Token, TokenKind, parse_cst,
    preparse, tokenize,
};
use mimium_lang::utils::error::ReportableError;
use pretty::{Arena, DocAllocator, DocBuilder, Pretty};

use crate::GLOBAL_DATA;

fn get_indent_size() -> usize {
    if let Ok(gdata) = GLOBAL_DATA.try_lock() {
        gdata.indent_size
    } else {
        4
    }
}

/// Pretty print source code using CST-based approach with comment-aware width calculation
pub fn pretty_print(
    src: &str,
    _file_path: &Option<PathBuf>,
    width: usize,
) -> Result<String, Vec<Box<dyn ReportableError>>> {
    let tokens = tokenize(src);
    let preparsed = preparse(&tokens);
    let leading_comments = extract_file_leading_comments(src, &tokens);

    let (cst_root, arena, _tokens, errors) = parse_cst(tokens.clone(), &preparsed);

    if !errors.is_empty() {
        // For now, return a simple error message
        // TODO: Convert CST parser errors to reportable errors properly
        return Err(vec![]);
    }

    let doc_allocator: Arena<'_, ()> = Arena::new();
    let ctx = PrintContext {
        arena: &arena,
        tokens: &tokens,
        preparsed: &preparsed,
        source: src,
    };

    let doc = cst_to_doc::<_, ()>(cst_root, &ctx, &doc_allocator);

    let mut output = Vec::new();
    doc.render(width, &mut output).unwrap();
    let mut formatted = String::from_utf8(output).unwrap();

    // Ensure output ends with a trailing newline
    if !formatted.ends_with('\n') {
        formatted.push('\n');
    }

    // Prepend file leading comments
    if leading_comments.is_empty() {
        Ok(formatted)
    } else {
        Ok(format!("{leading_comments}{formatted}"))
    }
}

/// Extract comments at the start of the file (before any non-trivia token)
fn extract_file_leading_comments(source: &str, tokens: &[Token]) -> String {
    let mut output = String::new();
    for token in tokens {
        if token.is_trivia() {
            if matches!(
                token.kind,
                TokenKind::SingleLineComment | TokenKind::MultiLineComment
            ) {
                output.push_str(token.text(source));
                output.push('\n');
            }
            continue;
        }
        break;
    }
    output
}

/// Context for CST printing
struct PrintContext<'a> {
    arena: &'a GreenNodeArena,
    tokens: &'a [Token],
    preparsed: &'a PreParsedTokens,
    source: &'a str,
}

/// Helper to create a breakable comma separator
fn breakable_comma<'a, D, A>(allocator: &'a D) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone,
{
    allocator.text(",").append(allocator.softline())
}

/// Convert a CST node to a pretty document
fn cst_to_doc<'a, D, A>(
    node_id: GreenNodeId,
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let node = ctx.arena.get(node_id);

    match node {
        mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } => {
            // Token node - emit the token text with its trivia
            emit_token_with_trivia(*token_index, ctx, allocator)
        }
        mimium_lang::compiler::parser::green::GreenNode::Internal { kind, children, .. } => {
            // Internal node - dispatch based on syntax kind
            match kind {
                SyntaxKind::Program => print_program(children, ctx, allocator),
                SyntaxKind::FunctionDecl => print_function_decl(children, ctx, allocator),
                SyntaxKind::LetDecl => print_let_decl(children, ctx, allocator),
                SyntaxKind::LetRecDecl => print_letrec_decl(children, ctx, allocator),
                SyntaxKind::BinaryExpr => print_binary_expr(children, ctx, allocator),
                SyntaxKind::UnaryExpr => print_unary_expr(children, ctx, allocator),
                SyntaxKind::CallExpr => print_call_expr(children, ctx, allocator),
                SyntaxKind::LambdaExpr => print_lambda_expr(children, ctx, allocator),
                SyntaxKind::IfExpr => print_if_expr(children, ctx, allocator),
                SyntaxKind::BlockExpr => print_block_expr(children, ctx, allocator),
                SyntaxKind::TupleExpr => print_tuple_expr(children, ctx, allocator),
                SyntaxKind::RecordExpr => print_record_expr(children, ctx, allocator),
                SyntaxKind::ArrayExpr => print_array_expr(children, ctx, allocator),
                SyntaxKind::ParenExpr => print_paren_expr(children, ctx, allocator),
                SyntaxKind::FieldAccess => print_field_access(children, ctx, allocator),
                SyntaxKind::IndexExpr => print_index_expr(children, ctx, allocator),
                SyntaxKind::MacroExpansion => print_macro_expansion(children, ctx, allocator),
                SyntaxKind::EscapeExpr => print_escape_expr(children, ctx, allocator),
                SyntaxKind::BracketExpr => print_bracket_expr(children, ctx, allocator),
                SyntaxKind::IncludeStmt => print_include_stmt(children, ctx, allocator),
                SyntaxKind::StageDecl => print_stage_decl(children, ctx, allocator),
                SyntaxKind::TypeAnnotation => print_type_annotation(children, ctx, allocator),
                SyntaxKind::ParamList => print_param_list(children, ctx, allocator),
                SyntaxKind::ArgList => print_arg_list(children, ctx, allocator),
                SyntaxKind::Pattern => print_pattern(children, ctx, allocator),
                SyntaxKind::SinglePattern => print_single_pattern(children, ctx, allocator),
                SyntaxKind::TuplePattern => print_tuple_pattern(children, ctx, allocator),
                SyntaxKind::RecordPattern => print_record_pattern(children, ctx, allocator),
                // Literals
                SyntaxKind::IntLiteral
                | SyntaxKind::FloatLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::SelfLiteral
                | SyntaxKind::NowLiteral
                | SyntaxKind::SampleRateLiteral
                | SyntaxKind::PlaceHolderLiteral
                | SyntaxKind::Identifier => print_leaf_children(children, ctx, allocator),
                // Types
                SyntaxKind::PrimitiveType | SyntaxKind::UnitType | SyntaxKind::TypeIdent => {
                    print_leaf_children(children, ctx, allocator)
                }
                SyntaxKind::FunctionType => print_function_type(children, ctx, allocator),
                SyntaxKind::TupleType => print_tuple_type(children, ctx, allocator),
                SyntaxKind::RecordType => print_record_type(children, ctx, allocator),
                SyntaxKind::ArrayType => print_array_type(children, ctx, allocator),
                SyntaxKind::CodeType => print_code_type(children, ctx, allocator),
                SyntaxKind::UnionType => print_union_type(children, ctx, allocator),
                // Statement wrapper
                SyntaxKind::Statement => print_statement(children, ctx, allocator),
                SyntaxKind::AssignExpr => print_assign_expr(children, ctx, allocator),
                SyntaxKind::ExprList => print_expr_list(children, ctx, allocator),
                SyntaxKind::ParamDefault => print_param_default(children, ctx, allocator),
                // Module system
                SyntaxKind::ModuleDecl => print_module_decl(children, ctx, allocator),
                SyntaxKind::UseStmt => print_use_stmt(children, ctx, allocator),
                SyntaxKind::QualifiedPath => print_qualified_path(children, ctx, allocator),
                SyntaxKind::UseTargetMultiple => {
                    print_use_target_multiple(children, ctx, allocator)
                }
                SyntaxKind::UseTargetWildcard => {
                    print_use_target_wildcard(children, ctx, allocator)
                }
                SyntaxKind::VisibilityPub => print_visibility_pub(children, ctx, allocator),
                SyntaxKind::MatchExpr
                | SyntaxKind::MatchArm
                | SyntaxKind::MatchArmList
                | SyntaxKind::MatchPattern
                | SyntaxKind::ConstructorPattern
                | SyntaxKind::TypeDecl
                | SyntaxKind::VariantDef => print_leaf_children(children, ctx, allocator),
                SyntaxKind::Error => allocator.text("/* error */"),
            }
        }
    }
}

/// Find the preparsed index for an original token index
fn find_preparsed_index(token_index: usize, preparsed: &PreParsedTokens) -> Option<usize> {
    preparsed
        .token_indices
        .iter()
        .position(|&idx| idx == token_index)
}

/// Emit a token with its leading and trailing trivia
fn emit_token_with_trivia<'a, D, A>(
    token_index: usize,
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone,
{
    let token = &ctx.tokens[token_index];
    let token_text = token.text(ctx.source);

    // Find preparsed index for this token
    let preparsed_idx = find_preparsed_index(token_index, ctx.preparsed);

    let mut doc = allocator.nil();

    // Emit leading trivia
    if let Some(idx) = preparsed_idx {
        let leading = ctx.preparsed.get_leading_trivia(idx, ctx.tokens);
        for trivia in leading {
            doc = doc.append(emit_trivia(trivia, ctx.source, allocator));
        }
    }

    // Emit the token itself
    doc = doc.append(allocator.text(token_text.to_string()));

    // Emit trailing trivia
    if let Some(idx) = preparsed_idx {
        let trailing = ctx.preparsed.get_trailing_trivia(idx, ctx.tokens);
        for trivia in trailing {
            doc = doc.append(emit_trivia(trivia, ctx.source, allocator));
        }
    }

    doc
}

/// Emit a trivia token (comment or whitespace)
fn emit_trivia<'a, D, A>(trivia: &Token, source: &str, allocator: &'a D) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone,
{
    match trivia.kind {
        TokenKind::SingleLineComment => {
            // Single-line comment - include the text and add hardline
            let text = trivia.text(source);
            allocator
                .text(" ")
                .append(allocator.text(text.to_string()))
                .append(allocator.hardline())
        }
        TokenKind::MultiLineComment => {
            // Block comment - include as-is with surrounding space
            let text = trivia.text(source);
            allocator
                .text(" ")
                .append(allocator.text(text.to_string()))
                .append(allocator.text(" "))
        }
        TokenKind::LineBreak => {
            // Skip linebreaks - we control line separation in block/program contexts
            allocator.nil()
        }
        TokenKind::Whitespace => {
            // Skip whitespace trivia - let the formatting logic handle spacing
            allocator.nil()
        }
        _ => allocator.nil(),
    }
}

/// Print all children as concatenated tokens (for leaf-like nodes)
fn print_leaf_children<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let docs: Vec<_> = children
        .iter()
        .map(|&child| cst_to_doc(child, ctx, allocator))
        .collect();
    allocator.concat(docs)
}

// ============================================================================
// Program and top-level statements
// ============================================================================

fn print_program<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let stmt_docs: Vec<_> = children
        .iter()
        .map(|&child| cst_to_doc(child, ctx, allocator))
        .collect();
    allocator.intersperse(stmt_docs, allocator.hardline())
}

fn print_statement<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // Statement is a wrapper - just print children
    print_leaf_children(children, ctx, allocator)
}

fn print_include_stmt<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // include("path")
    print_leaf_children(children, ctx, allocator)
}

fn print_stage_decl<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // #stage(main)
    print_leaf_children(children, ctx, allocator)
}

// ============================================================================
// Declarations
// ============================================================================

fn print_function_decl<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // fn name(params) { body } or fn name(params)->type { body }
    // Structure: fn, name_token, ParamList, optional -> and type, BlockExpr or expr
    let mut result = allocator.nil();
    let mut seen_fn = false;
    let mut seen_name = false;

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];

            match token.kind {
                TokenKind::Function => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_fn = true;
                    continue;
                }
                TokenKind::Ident | TokenKind::IdentFunction => {
                    if seen_fn && !seen_name {
                        result =
                            result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                        seen_name = true;
                        continue;
                    }
                }
                TokenKind::Arrow => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    continue;
                }
                _ => {}
            }
        }

        // For internal nodes (ParamList, BlockExpr, type nodes)
        if let mimium_lang::compiler::parser::green::GreenNode::Internal { kind, .. } = node {
            let child_doc = cst_to_doc(child, ctx, allocator);

            match kind {
                SyntaxKind::ParamList => {
                    // Param list comes right after function name (no space)
                    result = result.append(child_doc);
                }
                SyntaxKind::BlockExpr => {
                    // Block comes right after param list or return type (no space)
                    result = result.append(child_doc);
                }
                _ => {
                    // Type nodes, etc.
                    result = result.append(child_doc);
                }
            }
            continue;
        }

        // Default: just append
        result = result.append(cst_to_doc(child, ctx, allocator));
    }

    result
}

fn print_let_decl<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // let pattern = expr
    // Structure: let, pattern, =, expr(s)
    let mut result = allocator.nil();
    let mut seen_let = false;
    let mut seen_pattern = false;
    let mut seen_eq = false;
    let mut rhs_docs = Vec::new();

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];

            match token.kind {
                TokenKind::Let => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_let = true;
                    continue;
                }
                TokenKind::Assign => {
                    result = result.append(allocator.space());
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_eq = true;
                    continue;
                }
                _ => {}
            }
        }

        let child_doc = cst_to_doc(child, ctx, allocator);

        if seen_let && !seen_eq {
            // Pattern - just concatenate
            if !seen_pattern {
                result = result.append(child_doc);
                seen_pattern = true;
            } else {
                result = result.append(child_doc);
            }
        } else if seen_eq {
            // Collect RHS expressions
            rhs_docs.push(child_doc);
        }
    }

    // Build RHS - let the group control breaking
    if !rhs_docs.is_empty() {
        let rhs = allocator.concat(rhs_docs);
        result = result.append(rhs.group());
    }

    result
}

fn print_letrec_decl<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // letrec name = expr
    // Structure: letrec, identifier, =, expr(s)
    let mut result = allocator.nil();
    let mut seen_letrec = false;
    let mut seen_name = false;
    let mut seen_eq = false;
    let mut rhs_docs = Vec::new();

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];

            match token.kind {
                TokenKind::LetRec => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_letrec = true;
                    continue;
                }
                TokenKind::Assign => {
                    result = result.append(allocator.space());
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_eq = true;
                    continue;
                }
                _ => {}
            }
        }

        let child_doc = cst_to_doc(child, ctx, allocator);

        if seen_letrec && !seen_eq {
            // Name/pattern
            if !seen_name {
                result = result.append(child_doc);
                seen_name = true;
            } else {
                result = result.append(child_doc);
            }
        } else if seen_eq {
            // Collect RHS expressions
            rhs_docs.push(child_doc);
        }
    }

    // Build RHS - let the group control breaking
    if !rhs_docs.is_empty() {
        let rhs = allocator.concat(rhs_docs);
        result = result.append(rhs.group());
    }

    result
}

fn print_assign_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // lhs = rhs
    // Add spaces around =
    let mut result = allocator.nil();
    let mut seen_eq = false;

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];
            if token.kind == TokenKind::Assign {
                result = result
                    .append(allocator.space())
                    .append(emit_token_with_trivia(*token_index, ctx, allocator))
                    .append(allocator.space());
                seen_eq = true;
                continue;
            }
        }

        result = result.append(cst_to_doc(child, ctx, allocator));
    }

    result
}

// ============================================================================
// Expressions
// ============================================================================

fn print_binary_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // lhs op rhs - add spaces around operator with breakable newline before operator
    // For pipe operator, break before operator and indent the continuation
    let mut lhs_doc = allocator.nil();
    let mut op_doc = allocator.nil();
    let mut rhs_doc = allocator.nil();
    let mut is_pipe = false;
    let mut seen_op = false;

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        // Check if this is an operator token
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];
            let is_operator = matches!(
                token.kind,
                TokenKind::OpSum
                    | TokenKind::OpMinus
                    | TokenKind::OpProduct
                    | TokenKind::OpDivide
                    | TokenKind::OpModulo
                    | TokenKind::OpExponent
                    | TokenKind::OpAnd
                    | TokenKind::OpOr
                    | TokenKind::OpEqual
                    | TokenKind::OpNotEqual
                    | TokenKind::OpLessThan
                    | TokenKind::OpGreaterThan
                    | TokenKind::OpLessEqual
                    | TokenKind::OpGreaterEqual
                    | TokenKind::OpAt
                    | TokenKind::OpPipe
            );

            if is_operator {
                is_pipe = token.kind == TokenKind::OpPipe;
                op_doc = emit_token_with_trivia(*token_index, ctx, allocator);
                seen_op = true;
                continue;
            }
        }

        // For expressions
        let child_doc = cst_to_doc(child, ctx, allocator);
        if !seen_op {
            lhs_doc = lhs_doc.append(child_doc);
        } else {
            rhs_doc = rhs_doc.append(child_doc);
        }
    }

    if is_pipe {
        // Pipe operator: break before operator
        // Structure: lhs
        //     |> rhs
        lhs_doc
            .append(
                allocator
                    .line()
                    .append(op_doc)
                    .append(allocator.space())
                    .append(rhs_doc)
                    .nest(get_indent_size() as isize),
            )
            .group()
    } else {
        // Other operators: operator stays with lhs, break before rhs with indent
        // Structure: lhs +
        //     rhs
        lhs_doc
            .append(allocator.space())
            .append(op_doc)
            .append(
                allocator
                    .line()
                    .append(rhs_doc)
                    .nest(get_indent_size() as isize),
            )
            .group()
    }
}

fn print_unary_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // op expr (e.g., -x, !b)
    // Simply concatenate operator and expression without space
    let mut result = allocator.nil();

    for &child in children.iter() {
        result = result.append(cst_to_doc(child, ctx, allocator));
    }

    result
}

fn print_call_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // callee(args) or callee(args) @ time
    // Structure: callee, ArgList, (optional: @, time_expr)
    let mut result = allocator.nil();

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        // Check if this is an ArgList node - format specially
        if let mimium_lang::compiler::parser::green::GreenNode::Internal { kind, .. } = node
            && *kind == SyntaxKind::ArgList
        {
            result = result.append(cst_to_doc(child, ctx, allocator));
            continue;
        }

        // For other children (callee, @, time), concatenate directly
        result = result.append(cst_to_doc(child, ctx, allocator));
    }

    result.group()
}

fn print_lambda_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // |params| body or |params|->return_type body
    // Structure: |, param tokens (with optional :type), commas, |, optional -> and return_type, body_expr(s)
    //
    // Note: Due to CST parser structure, body expressions may be split into multiple children.
    // For example, `x+1` becomes Identifier(x) followed by BinaryExpr(+, 1) as siblings.
    // We need to concatenate all body children without adding extra spaces between them.

    let mut result = allocator.nil();
    let mut in_params = false;
    let mut params_docs = Vec::new();
    let mut current_param = allocator.nil();
    let mut has_param_content = false;
    let mut after_params = false;
    let mut after_arrow = false;
    let mut has_return_type = false;
    let mut body_started = false;

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];

            match token.kind {
                TokenKind::LambdaArgBeginEnd => {
                    if !in_params && !after_params {
                        // Opening |
                        result =
                            result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                        in_params = true;
                    } else if in_params {
                        // Closing | - flush the current param if any
                        if has_param_content {
                            params_docs.push(current_param.clone());
                        }

                        // Join params with comma and space
                        let params_combined = if params_docs.is_empty() {
                            allocator.nil()
                        } else {
                            allocator.intersperse(params_docs.clone(), allocator.text(", "))
                        };

                        result = result.append(params_combined);
                        result =
                            result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                        in_params = false;
                        after_params = true;
                        params_docs.clear();
                        current_param = allocator.nil();
                        has_param_content = false;
                    }
                    continue;
                }
                TokenKind::Comma if in_params => {
                    // Comma separates params
                    if has_param_content {
                        params_docs.push(current_param.clone());
                        current_param = allocator.nil();
                        has_param_content = false;
                    }
                    continue;
                }
                TokenKind::Arrow if after_params => {
                    // Return type annotation
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    after_arrow = true;
                    continue;
                }
                _ => {}
            }
        }

        // Check if this is a type node
        let is_type_node =
            if let mimium_lang::compiler::parser::green::GreenNode::Internal { kind, .. } = node {
                matches!(
                    kind,
                    SyntaxKind::PrimitiveType
                        | SyntaxKind::UnitType
                        | SyntaxKind::TypeIdent
                        | SyntaxKind::FunctionType
                        | SyntaxKind::TupleType
                        | SyntaxKind::RecordType
                        | SyntaxKind::ArrayType
                        | SyntaxKind::CodeType
                )
            } else {
                false
            };

        let child_doc = cst_to_doc(child, ctx, allocator);

        if in_params {
            // Accumulate parameter tokens
            current_param = current_param.append(child_doc);
            has_param_content = true;
        } else if after_params {
            if after_arrow && !has_return_type && is_type_node {
                // Return type after ->
                result = result.append(child_doc);
                has_return_type = true;
            } else {
                // Body expression(s) - add space only before the FIRST body child
                if !body_started {
                    result = result.append(allocator.space()).append(child_doc);
                    body_started = true;
                } else {
                    // Subsequent body children are concatenated without extra space
                    // (they're part of the same logical expression)
                    result = result.append(child_doc);
                }
            }
        }
    }

    result.group()
}

fn print_if_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // if cond then_expr [else alt_expr]
    // Structure: if, condition_expr, then_expr, (optional: else, alt_expr or nested IfExpr)
    let mut result = allocator.nil();
    let mut seen_if = false;
    let mut seen_cond = false;
    let mut seen_then = false;
    let mut seen_else = false;

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];

            match token.kind {
                TokenKind::If => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    seen_if = true;
                    continue;
                }
                TokenKind::Else => {
                    // Add softline before else to allow breaking there
                    result = result
                        .append(allocator.softline())
                        .append(emit_token_with_trivia(*token_index, ctx, allocator));
                    seen_else = true;
                    continue;
                }
                _ => {}
            }
        }

        // Process expression children
        let child_doc = cst_to_doc(child, ctx, allocator);

        if !seen_cond && seen_if {
            // This is the condition expression - keep it flat (don't break inside)
            // We use group() on the condition to try to keep it on one line
            result = result.append(child_doc.group());
            seen_cond = true;
        } else if !seen_then && seen_cond {
            // This is the then branch - use softline before to allow breaking
            result = result
                .append(allocator.softline())
                .append(child_doc.group());
            seen_then = true;
        } else if seen_else {
            // This is the else branch (could be nested IfExpr for else if)
            result = result.append(allocator.space()).append(child_doc.group());
        }
    }

    result.group()
}

fn print_block_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // { stmts }
    // Structure: {, statements..., }

    let mut result = allocator.nil();
    let mut in_body = false;
    let mut body_docs = Vec::new();
    let mut open_brace_trivia = allocator.nil();
    let mut has_open_trivia = false;

    for &child in children.iter() {
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } =
            ctx.arena.get(child)
        {
            let token = &ctx.tokens[*token_index];
            match token.kind {
                TokenKind::BlockBegin => {
                    // Emit { with trailing trivia (comments after {)
                    result = result.append(allocator.text("{"));
                    // Capture trailing trivia for { (like // comment after {)
                    if let Some(idx) = find_preparsed_index(*token_index, ctx.preparsed) {
                        let trailing = ctx.preparsed.get_trailing_trivia(idx, ctx.tokens);
                        for trivia in trailing {
                            // Only mark has_open_trivia for actual comments
                            let is_comment = matches!(
                                trivia.kind,
                                TokenKind::SingleLineComment | TokenKind::MultiLineComment
                            );
                            open_brace_trivia = open_brace_trivia
                                .append(emit_trivia(trivia, ctx.source, allocator));
                            if is_comment {
                                has_open_trivia = true;
                            }
                        }
                    }
                    in_body = true;
                    continue;
                }
                TokenKind::BlockEnd => {
                    // Build body with indentation
                    if !body_docs.is_empty() {
                        let body = allocator.intersperse(body_docs.clone(), allocator.hardline());
                        // Add trivia from { before the body (if any)
                        // Trivia like single-line comments already include hardline at the end
                        if has_open_trivia {
                            // When there's trivia (e.g., // comment), it already ends with hardline
                            // We nest everything including the trivia so indentation applies after the hardline
                            let content = open_brace_trivia.clone().append(body);
                            result = result.append(content.nest(get_indent_size() as isize));
                        } else {
                            result = result.append(
                                allocator
                                    .hardline()
                                    .append(body)
                                    .nest(get_indent_size() as isize),
                            );
                        }
                        result = result.append(allocator.hardline());
                    } else if has_open_trivia {
                        // Empty block but has trailing comment on {
                        result = result.append(open_brace_trivia.clone());
                    }
                    result = result.append(allocator.text("}"));
                    in_body = false;
                    continue;
                }
                _ => {}
            }
        }

        if in_body {
            let child_doc = cst_to_doc(child, ctx, allocator);
            body_docs.push(child_doc);
        }
    }

    result
}

fn print_tuple_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (e1, e2, ...)
    print_grouped_list(children, ctx, allocator, "(", ")")
}

fn print_record_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // {field = val, field2 = val2, ...}
    // Record children are: {, ident, =, expr, comma, ident, =, expr, ..., }
    // We need to group them as: {, [ident = expr], comma, [ident = expr], }

    let mut fields: Vec<DocBuilder<'a, D, A>> = Vec::new();
    let mut current_field = allocator.nil();
    let mut has_current_field = false;
    let mut open_doc = allocator.nil();
    let mut close_doc = allocator.nil();
    let mut in_body = false;

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];

            match token.kind {
                TokenKind::BlockBegin => {
                    open_doc = emit_token_with_trivia(*token_index, ctx, allocator);
                    in_body = true;
                    continue;
                }
                TokenKind::BlockEnd => {
                    // Push current field if any
                    if has_current_field {
                        fields.push(current_field.clone());
                    }
                    close_doc = emit_token_with_trivia(*token_index, ctx, allocator);
                    in_body = false;
                    continue;
                }
                TokenKind::Comma if in_body => {
                    // End of current field, push it
                    if has_current_field {
                        fields.push(current_field.clone());
                        current_field = allocator.nil();
                        has_current_field = false;
                    }
                    continue;
                }
                TokenKind::Assign if in_body => {
                    // Add = with spaces
                    current_field = current_field
                        .append(allocator.space())
                        .append(emit_token_with_trivia(*token_index, ctx, allocator))
                        .append(allocator.space());
                    has_current_field = true;
                    continue;
                }
                TokenKind::LeftArrow if in_body => {
                    // Record update: base <- field = expr
                    current_field = current_field
                        .append(allocator.space())
                        .append(emit_token_with_trivia(*token_index, ctx, allocator))
                        .append(allocator.space());
                    has_current_field = true;
                    continue;
                }
                _ => {}
            }
        }

        if in_body {
            current_field = current_field.append(cst_to_doc(child, ctx, allocator));
            has_current_field = true;
        }
    }

    // Build the result
    if fields.is_empty() {
        open_doc.append(close_doc)
    } else {
        let fields_doc = allocator.intersperse(fields, breakable_comma(allocator));
        open_doc
            .append(fields_doc.nest(get_indent_size() as isize).group())
            .append(close_doc)
    }
}

fn print_array_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // [e1, e2, ...]
    print_grouped_list(children, ctx, allocator, "[", "]")
}

fn print_paren_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (expr) - wrap in group to avoid breaking inside parentheses when possible
    print_leaf_children(children, ctx, allocator).group()
}

fn print_field_access<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // expr.field
    print_leaf_children(children, ctx, allocator)
}

fn print_index_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // expr.0
    print_leaf_children(children, ctx, allocator)
}

fn print_macro_expansion<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // macro!(args)
    // Structure: identifier, !, (, args..., )
    let mut result = allocator.nil();
    let mut args = Vec::new();
    let mut in_args = false;
    let mut open_doc = allocator.nil();
    let mut close_doc = allocator.nil();

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];

            match token.kind {
                TokenKind::Ident | TokenKind::IdentFunction => {
                    if !in_args {
                        // Macro name
                        result =
                            result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                        continue;
                    }
                }
                TokenKind::MacroExpand => {
                    // The ! token
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    continue;
                }
                TokenKind::ParenBegin => {
                    open_doc = emit_token_with_trivia(*token_index, ctx, allocator);
                    in_args = true;
                    continue;
                }
                TokenKind::ParenEnd => {
                    close_doc = emit_token_with_trivia(*token_index, ctx, allocator);
                    in_args = false;
                    continue;
                }
                TokenKind::Comma if in_args => {
                    // Skip comma - we'll add our own with proper spacing
                    continue;
                }
                _ => {}
            }
        }

        if in_args {
            args.push(cst_to_doc(child, ctx, allocator));
        } else {
            result = result.append(cst_to_doc(child, ctx, allocator));
        }
    }

    // Build args with comma + space
    if args.is_empty() {
        result.append(open_doc).append(close_doc)
    } else {
        let args_doc = allocator.intersperse(args, allocator.text(", "));
        result.append(open_doc).append(args_doc).append(close_doc)
    }
}

fn print_escape_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // $expr
    print_leaf_children(children, ctx, allocator)
}

fn print_bracket_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // `expr
    print_leaf_children(children, ctx, allocator)
}

// ============================================================================
// Types
// ============================================================================

fn print_type_annotation<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // : type
    print_leaf_children(children, ctx, allocator)
}

fn print_function_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (params) -> return
    print_leaf_children(children, ctx, allocator)
}

fn print_tuple_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (t1, t2)
    print_grouped_list(children, ctx, allocator, "(", ")")
}

fn print_record_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // {field: type, ...}
    print_grouped_list(children, ctx, allocator, "{", "}")
}

fn print_array_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // [type; size]
    print_leaf_children(children, ctx, allocator)
}

fn print_code_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // `type
    print_leaf_children(children, ctx, allocator)
}

fn print_union_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // A | B | C
    print_leaf_children(children, ctx, allocator)
}

// ============================================================================
// Patterns
// ============================================================================

fn print_pattern<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    print_leaf_children(children, ctx, allocator)
}

fn print_single_pattern<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // name: type
    print_leaf_children(children, ctx, allocator)
}

fn print_tuple_pattern<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (p1, p2)
    print_grouped_list(children, ctx, allocator, "(", ")")
}

fn print_record_pattern<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // {field, ...}
    print_grouped_list(children, ctx, allocator, "{", "}")
}

// ============================================================================
// Lists
// ============================================================================

fn print_param_list<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (p1, p2, ...)
    print_grouped_list(children, ctx, allocator, "(", ")")
}

fn print_arg_list<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (a1, a2, ...)
    print_grouped_list(children, ctx, allocator, "(", ")")
}

fn print_expr_list<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // e1, e2, ...
    let docs: Vec<_> = children
        .iter()
        .map(|&child| cst_to_doc(child, ctx, allocator))
        .collect();
    allocator.concat(docs)
}

fn print_param_default<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    print_leaf_children(children, ctx, allocator)
}

// ============================================================================
// Helper functions
// ============================================================================

/// Print a grouped list with delimiters (parens, braces, brackets)
/// Handles indentation for multi-line lists
fn print_grouped_list<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
    _open: &str,
    _close: &str,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // Collect items between delimiters, excluding commas
    let mut items = Vec::new();
    let mut open_doc = allocator.nil();
    let mut close_doc = allocator.nil();
    let mut found_open = false;

    for &child in children.iter() {
        let node = ctx.arena.get(child);
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];
            match token.kind {
                TokenKind::ParenBegin | TokenKind::BlockBegin | TokenKind::ArrayBegin => {
                    open_doc = emit_token_with_trivia(*token_index, ctx, allocator);
                    found_open = true;
                    continue;
                }
                TokenKind::ParenEnd | TokenKind::BlockEnd | TokenKind::ArrayEnd => {
                    close_doc = emit_token_with_trivia(*token_index, ctx, allocator);
                    continue;
                }
                TokenKind::Comma => {
                    // Skip commas - we'll add them with proper breaking
                    continue;
                }
                _ => {}
            }
        }

        if found_open {
            items.push(cst_to_doc(child, ctx, allocator));
        }
    }

    if items.is_empty() {
        open_doc.append(close_doc)
    } else {
        // Use softline between items (after comma), but not after opening delimiter
        // This prioritizes breaking at binary operators over breaking at function call boundaries
        let items_doc = allocator.intersperse(items, breakable_comma(allocator));
        // Wrap in group for proper line breaking
        open_doc
            .append(items_doc.nest(get_indent_size() as isize))
            .append(close_doc)
            .group()
    }
}

// ============================================================================
// Module system
// ============================================================================

/// Print module declaration: `mod name { body }` or `pub mod name { body }`
fn print_module_decl<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let mut result = allocator.nil();
    let mut seen_mod = false;
    let mut seen_name = false;

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];

            match token.kind {
                TokenKind::Mod => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_mod = true;
                    continue;
                }
                TokenKind::Ident | TokenKind::IdentFunction | TokenKind::IdentVariable => {
                    if seen_mod && !seen_name {
                        result =
                            result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                        result = result.append(allocator.space());
                        seen_name = true;
                        continue;
                    }
                }
                _ => {}
            }
        }

        // For internal nodes (VisibilityPub, BlockExpr)
        if let mimium_lang::compiler::parser::green::GreenNode::Internal { kind, .. } = node {
            let child_doc = cst_to_doc(child, ctx, allocator);

            match kind {
                SyntaxKind::VisibilityPub => {
                    // pub keyword (already includes trailing space)
                    result = result.append(child_doc);
                }
                SyntaxKind::BlockExpr => {
                    // Block body
                    result = result.append(child_doc);
                }
                _ => {
                    result = result.append(child_doc);
                }
            }
            continue;
        }

        result = result.append(cst_to_doc(child, ctx, allocator));
    }

    result
}

/// Print use statement: `use path::to::item` or `pub use path::to::item`
fn print_use_stmt<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let mut result = allocator.nil();
    let mut seen_use = false;

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];

            if token.kind == TokenKind::Use {
                result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                result = result.append(allocator.space());
                seen_use = true;
                continue;
            }
        }

        // For internal nodes (VisibilityPub, QualifiedPath, UseTargetMultiple, UseTargetWildcard)
        if let mimium_lang::compiler::parser::green::GreenNode::Internal { kind, .. } = node {
            let child_doc = cst_to_doc(child, ctx, allocator);

            match kind {
                SyntaxKind::VisibilityPub => {
                    // pub keyword (already includes trailing space)
                    result = result.append(child_doc);
                }
                SyntaxKind::QualifiedPath
                | SyntaxKind::UseTargetMultiple
                | SyntaxKind::UseTargetWildcard => {
                    if seen_use {
                        result = result.append(child_doc);
                    }
                }
                _ => {
                    result = result.append(child_doc);
                }
            }
            continue;
        }

        result = result.append(cst_to_doc(child, ctx, allocator));
    }

    result
}

/// Print qualified path: `foo::bar::baz`
fn print_qualified_path<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let mut result = allocator.nil();

    for &child in children.iter() {
        let node = ctx.arena.get(child);

        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];

            match token.kind {
                TokenKind::Ident
                | TokenKind::IdentFunction
                | TokenKind::IdentVariable
                | TokenKind::DoubleColon => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                }
                _ => {}
            }
        } else {
            result = result.append(cst_to_doc(child, ctx, allocator));
        }
    }

    result
}

/// Print use target multiple: `{a, b, c}`
fn print_use_target_multiple<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let mut items = Vec::new();
    let mut found_open = false;
    let mut open_doc = allocator.nil();
    let mut close_doc = allocator.nil();

    for &child in children.iter() {
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } =
            ctx.arena.get(child)
        {
            let token = &ctx.tokens[*token_index];
            match token.kind {
                TokenKind::BlockBegin => {
                    open_doc = allocator.text("{");
                    found_open = true;
                    continue;
                }
                TokenKind::BlockEnd => {
                    close_doc = allocator.text("}");
                    continue;
                }
                TokenKind::Comma => {
                    continue;
                }
                TokenKind::Ident | TokenKind::IdentFunction | TokenKind::IdentVariable => {
                    if found_open {
                        items.push(emit_token_with_trivia(*token_index, ctx, allocator));
                    }
                    continue;
                }
                _ => {}
            }
        }

        if found_open {
            items.push(cst_to_doc(child, ctx, allocator));
        }
    }

    if items.is_empty() {
        open_doc.append(close_doc)
    } else {
        let items_doc = allocator.intersperse(items, allocator.text(", "));
        open_doc.append(items_doc).append(close_doc)
    }
}

/// Print use target wildcard: `*`
fn print_use_target_wildcard<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // Structure: ::*
    let mut result = allocator.nil();

    for &child in children.iter() {
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } =
            ctx.arena.get(child)
        {
            let token = &ctx.tokens[*token_index];
            match token.kind {
                TokenKind::DoubleColon | TokenKind::OpProduct => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                }
                _ => {}
            }
        }
    }

    result
}

/// Print visibility pub keyword: `pub`
fn print_visibility_pub<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let mut result = allocator.nil();

    for &child in children.iter() {
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } =
            ctx.arena.get(child)
        {
            let token = &ctx.tokens[*token_index];
            if token.kind == TokenKind::Pub {
                result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                result = result.append(allocator.space());
            }
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper function to format and normalize output
    fn format(src: &str) -> String {
        pretty_print(src, &None, 80).expect("formatting failed")
    }

    /// Helper to format with custom width
    fn format_width(src: &str, width: usize) -> String {
        pretty_print(src, &None, width).expect("formatting failed")
    }

    // ========================================================================
    // Basic tests
    // ========================================================================

    #[test]
    fn test_simple_function() {
        let output = format("fn dsp() { 42 }");
        assert_eq!(output, "fn dsp(){\n    42\n}\n");
    }

    #[test]
    fn test_with_comment() {
        let output = format("let x = 1 // comment\nlet y = 2");
        // Single-line comment adds extra hardline, resulting in double newline
        assert_eq!(output, "let x = 1 // comment\n\nlet y = 2\n");
    }

    // ========================================================================
    // Literal tests
    // ========================================================================

    #[test]
    fn test_int_literal() {
        let output = format("let x = 42");
        assert_eq!(output, "let x = 42\n");
    }

    #[test]
    fn test_float_literal() {
        let output = format("let x = 3.14");
        assert_eq!(output, "let x = 3.14\n");
    }

    #[test]
    fn test_string_literal() {
        let output = format("let x = \"hello\"");
        assert_eq!(output, "let x = \"hello\"\n");
    }

    #[test]
    fn test_self_literal() {
        let output = format("let x = self");
        assert_eq!(output, "let x = self\n");
    }

    #[test]
    fn test_now_literal() {
        let output = format("let x = now");
        assert_eq!(output, "let x = now\n");
    }

    #[test]
    fn test_samplerate_literal() {
        let output = format("let x = samplerate");
        assert_eq!(output, "let x = samplerate\n");
    }

    // ========================================================================
    // Declaration tests
    // ========================================================================

    #[test]
    fn test_let_simple() {
        let output = format("let x = 1");
        assert_eq!(output, "let x = 1\n");
    }

    #[test]
    fn test_let_with_type() {
        let output = format("let x:float = 1.0");
        assert_eq!(output, "let x:float = 1.0\n");
    }

    #[test]
    fn test_letrec() {
        let output = format("letrec x = self + 1");
        assert_eq!(output, "letrec x = self + 1\n");
    }

    #[test]
    fn test_function_decl() {
        let output = format("fn add(a, b) { a + b }");
        assert_eq!(output, "fn add(a, b){\n    a + b\n}\n");
    }

    #[test]
    fn test_function_with_return_type() {
        let output = format("fn double(x)->float { x * 2.0 }");
        assert_eq!(output, "fn double(x)->float{\n    x * 2.0\n}\n");
    }

    // ========================================================================
    // Binary expression tests
    // ========================================================================

    #[test]
    fn test_binary_add() {
        let output = format("let x = 1 + 2");
        assert_eq!(output, "let x = 1 + 2\n");
    }

    #[test]
    fn test_binary_sub() {
        let output = format("let x = 5 - 3");
        assert_eq!(output, "let x = 5 - 3\n");
    }

    #[test]
    fn test_binary_mul() {
        let output = format("let x = 2 * 3");
        assert_eq!(output, "let x = 2 * 3\n");
    }

    #[test]
    fn test_binary_div() {
        let output = format("let x = 10 / 2");
        assert_eq!(output, "let x = 10 / 2\n");
    }

    #[test]
    fn test_binary_mod() {
        let output = format("let x = 10 % 3");
        assert_eq!(output, "let x = 10 % 3\n");
    }

    #[test]
    fn test_binary_exp() {
        let output = format("let x = 2 ^ 3");
        assert_eq!(output, "let x = 2 ^ 3\n");
    }

    #[test]
    fn test_binary_and() {
        let output = format("let x = true && false");
        assert_eq!(output, "let x = true && false\n");
    }

    #[test]
    fn test_binary_or() {
        let output = format("let x = true || false");
        assert_eq!(output, "let x = true || false\n");
    }

    #[test]
    fn test_comparison_ops() {
        assert_eq!(format("let a = x == y"), "let a = x == y\n");
        assert_eq!(format("let b = x != y"), "let b = x != y\n");
        assert_eq!(format("let c = x < y"), "let c = x < y\n");
        assert_eq!(format("let d = x > y"), "let d = x > y\n");
    }

    #[test]
    fn test_pipe_operator() {
        let output = format("let x = a |> b |> c");
        assert_eq!(output, "let x = a |> b |> c\n");
    }

    #[test]
    fn test_at_operator() {
        let output = format("let x = f(y) @ 1000");
        assert_eq!(output, "let x = f(y) @ 1000\n");
    }

    // ========================================================================
    // Unary expression tests
    // ========================================================================

    #[test]
    fn test_unary_minus() {
        let output = format("let x = -5");
        assert_eq!(output, "let x = -5\n");
    }

    #[test]
    fn test_unary_plus() {
        let output = format("let x = +5");
        assert_eq!(output, "let x = +5\n");
    }

    // ========================================================================
    // Lambda tests
    // ========================================================================

    #[test]
    fn test_lambda_simple() {
        let output = format("let f = |x| x + 1");
        assert_eq!(output, "let f = |x| x + 1\n");
    }

    #[test]
    fn test_lambda_multiple_params() {
        let output = format("let f = |x, y| x + y");
        assert_eq!(output, "let f = |x, y| x + y\n");
    }

    #[test]
    fn test_lambda_with_type() {
        let output = format("let f = |x:float| x * 2.0");
        assert_eq!(output, "let f = |x:float| x * 2.0\n");
    }

    #[test]
    fn test_lambda_with_return_type() {
        let output = format("let f = |x|->float x * 2.0");
        assert_eq!(output, "let f = |x|->float x * 2.0\n");
    }

    #[test]
    fn test_lambda_with_block() {
        let output = format("let f = |x| { x + 1 }");
        assert_eq!(output, "let f = |x| {\n    x + 1\n}\n");
    }

    // ========================================================================
    // If expression tests
    // ========================================================================

    #[test]
    fn test_if_simple() {
        let output = format("let x = if (a > 0) 1 else 0");
        assert_eq!(output, "let x = if(a > 0) 1 else 0\n");
    }

    #[test]
    fn test_if_with_block() {
        let output = format("let x = if (a > 0) { 1 } else { 0 }");
        assert_eq!(output, "let x = if(a > 0) {\n    1\n} else {\n    0\n}\n");
    }

    #[test]
    fn test_if_else_if() {
        let output = format("let x = if (a > 0) {1} else if (a < 0) {2} else {0}");
        assert_eq!(
            output,
            "let x = if(a > 0) {\n    1\n} else if(a < 0) {\n    2\n} else {\n    0\n}\n"
        );
    }

    // ========================================================================
    // Block expression tests
    // ========================================================================

    #[test]
    fn test_block_single_expr() {
        let output = format("fn f() { 42 }");
        assert_eq!(output, "fn f(){\n    42\n}\n");
    }

    #[test]
    fn test_block_multiple_statements() {
        let output = format("fn f() {\nlet x = 1\nlet y = 2\nx + y\n}");
        assert_eq!(
            output,
            "fn f(){\n    let x = 1\n    let y = 2\n    x + y\n}\n"
        );
    }

    // ========================================================================
    // Tuple tests
    // ========================================================================

    #[test]
    fn test_tuple_expr() {
        let output = format("let x = (1, 2, 3)");
        assert_eq!(output, "let x = (1, 2, 3)\n");
    }

    #[test]
    fn test_tuple_pattern() {
        let output = format("let (a, b) = x");
        assert_eq!(output, "let (a, b) = x\n");
    }

    // ========================================================================
    // Record tests
    // ========================================================================

    #[test]
    fn test_record_expr() {
        let output = format("let r = {a = 1, b = 2}");
        assert_eq!(output, "let r = {a = 1, b = 2}\n");
    }

    // ========================================================================
    // Array tests
    // ========================================================================

    #[test]
    fn test_array_expr() {
        let output = format("let arr = [1, 2, 3]");
        assert_eq!(output, "let arr = [1, 2, 3]\n");
    }

    // ========================================================================
    // Call expression tests
    // ========================================================================

    #[test]
    fn test_call_no_args() {
        let output = format("let x = f()");
        assert_eq!(output, "let x = f()\n");
    }

    #[test]
    fn test_call_with_args() {
        let output = format("let x = f(1, 2, 3)");
        assert_eq!(output, "let x = f(1, 2, 3)\n");
    }

    #[test]
    fn test_call_chained() {
        let output = format("let x = f(1)(2)");
        assert_eq!(output, "let x = f(1)(2)\n");
    }

    // ========================================================================
    // Field access tests
    // ========================================================================

    #[test]
    fn test_field_access() {
        let output = format("let x = obj.field");
        assert_eq!(output, "let x = obj.field\n");
    }

    #[test]
    fn test_tuple_projection() {
        let output = format("let x = t.0");
        assert_eq!(output, "let x = t.0\n");
    }

    // ========================================================================
    // Index expression tests
    // ========================================================================

    #[test]
    fn test_index_expr() {
        let output = format("let x = arr[0]");
        assert_eq!(output, "let x = arr[0]\n");
    }

    // ========================================================================
    // Include statement tests
    // ========================================================================

    #[test]
    fn test_include() {
        let output = format("include(\"file.mmm\")");
        assert_eq!(output, "include(\"file.mmm\")\n");
    }

    // ========================================================================
    // Quote/escape tests
    // ========================================================================

    #[test]
    fn test_quote_expr() {
        let output = format("let x = `y");
        assert_eq!(output, "let x = `y\n");
    }

    #[test]
    fn test_escape_expr() {
        let output = format("let x = $y");
        assert_eq!(output, "let x = $y\n");
    }

    // ========================================================================
    // Type annotation tests
    // ========================================================================

    #[test]
    fn test_type_float() {
        let output = format("let x:float = 1.0");
        assert_eq!(output, "let x:float = 1.0\n");
    }

    #[test]
    fn test_type_int() {
        let output = format("let x:int = 1");
        assert_eq!(output, "let x:int = 1\n");
    }

    #[test]
    fn test_type_string() {
        let output = format("let x:string = \"hello\"");
        assert_eq!(output, "let x:string = \"hello\"\n");
    }

    #[test]
    fn test_function_type() {
        let output = format("let f:(float)->float = |x| x");
        assert_eq!(output, "let f:(float)->float = |x| x\n");
    }

    // ========================================================================
    // Width tests
    // ========================================================================

    #[test]
    fn test_narrow_width_breaks_lines() {
        let src = "let x = very_long_function_call(argument1, argument2, argument3)";
        let narrow = format_width(src, 20);
        let wide = format_width(src, 200);
        // Narrow output should have more characters due to line breaks
        assert!(narrow.lines().count() >= wide.lines().count());
    }

    // ========================================================================
    // Comment preservation tests
    // ========================================================================

    #[test]
    fn test_single_line_comment_preserved() {
        let output = format("let x = 1 // this is a comment");
        assert_eq!(output, "let x = 1 // this is a comment\n");
    }

    #[test]
    fn test_block_comment_preserved() {
        let output = format("let x = /* comment */ 1");
        // Block comment adds extra space after
        assert_eq!(output, "let x = /* comment */  1\n");
    }

    // ========================================================================
    // Idempotency tests
    // ========================================================================

    #[test]
    fn test_format_idempotent() {
        let src = "fn dsp() {\n    let x = 1\n    x + 1\n}";
        let first = format(src);
        let second = format(&first);
        assert_eq!(first, second, "Formatting should be idempotent");
    }

    #[test]
    fn test_format_idempotent_complex() {
        let src = r#"fn adsr(a, b) {
    let s = self
    let x = a + b
    x
}"#;
        let first = format(src);
        let second = format(&first);
        assert_eq!(first, second, "Complex formatting should be idempotent");
    }

    #[test]
    fn test_format_idempotent_nested_blocks() {
        let src = r#"fn test(x) {
    if (x > 0) {
        let a = 1
        a
    } else {
        let b = 2
        b
    }
}"#;
        let first = format(src);
        let second = format(&first);
        assert_eq!(first, second, "Nested blocks should be idempotent");
    }

    #[test]
    fn test_format_idempotent_if_else_if() {
        let src = r#"fn test(x) {
    if (x > 0) {
        1
    } else if (x < 0) {
        2
    } else {
        0
    }
}"#;
        let first = format(src);
        let second = format(&first);
        assert_eq!(first, second, "if-else-if should be idempotent");
    }

    // ========================================================================
    // File leading comment tests
    // ========================================================================

    #[test]
    fn test_file_leading_comment() {
        let output = format("// file header comment\nlet x = 1");
        assert_eq!(output, "// file header comment\nlet x = 1\n");
    }

    #[test]
    fn test_file_leading_multiple_comments() {
        let output = format("// comment 1\n// comment 2\nlet x = 1");
        assert_eq!(output, "// comment 1\n// comment 2\nlet x = 1\n");
    }

    // ========================================================================
    // Block with comment tests
    // ========================================================================

    #[test]
    fn test_block_with_trailing_comment() {
        let output = format("fn f() { // comment after {\n    42\n}");
        assert_eq!(output, "fn f(){ // comment after {\n    42\n}\n");
    }

    // ========================================================================
    // Module system tests
    // ========================================================================

    #[test]
    fn test_module_decl_basic() {
        let output = format("mod foo { fn bar() { 1 } }");
        assert!(output.starts_with("mod foo {"), "output: {output}");
        assert!(output.contains("fn bar()"), "output: {output}");
    }

    #[test]
    fn test_module_decl_pub() {
        let output = format("pub mod foo { fn bar() { 1 } }");
        assert!(output.starts_with("pub mod foo {"), "output: {output}");
        assert!(output.contains("fn bar()"), "output: {output}");
    }

    #[test]
    fn test_use_stmt_simple() {
        let output = format("use foo::bar");
        assert_eq!(output, "use foo::bar\n");
    }

    #[test]
    fn test_use_stmt_multiple() {
        let output = format("use foo::{bar, baz}");
        assert_eq!(output, "use foo::{bar, baz}\n");
    }

    #[test]
    fn test_use_stmt_wildcard() {
        let output = format("use foo::*");
        assert_eq!(output, "use foo::*\n");
    }

    #[test]
    fn test_pub_use() {
        let output = format("pub use foo::bar");
        assert_eq!(output, "pub use foo::bar\n");
    }

    #[test]
    fn test_qualified_path_call() {
        let output = format("let x = foo::bar::baz()");
        assert_eq!(output, "let x = foo::bar::baz()\n");
    }
}
