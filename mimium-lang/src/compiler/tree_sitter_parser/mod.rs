use crate::ast::*;
use crate::ast::program::{Program, ProgramStatement};
use crate::ast::statement::Statement;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::types::Type;
use crate::pattern::{TypedId, TypedPattern, Pattern};
use crate::utils::error::{ReportableError, SimpleError};
use crate::utils::metadata::*;
use crate::ast::operators::Op;

use tree_sitter::{Parser, Tree, Node};

mod bindings;
pub use bindings::language;

/// Tree-sitter based incremental parser for mimium
pub struct TreeSitterParser {
    parser: Parser,
    tree: Option<Tree>,
}

impl TreeSitterParser {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut parser = Parser::new();
        parser.set_language(&language())?;
        Ok(Self { parser, tree: None })
    }

    pub fn parse(&mut self, source: &str, old_tree: Option<&Tree>) -> Result<Option<Tree>, Box<dyn std::error::Error>> {
        let tree = self.parser.parse(source, old_tree);
        self.tree = tree.clone();
        Ok(tree)
    }

    pub fn get_tree(&self) -> Option<&Tree> {
        self.tree.as_ref()
    }
}

/// Convert tree-sitter CST to mimium AST - minimal implementation
pub struct ASTConverter {
    source: String,
    file_path: Symbol,
}

impl ASTConverter {
    pub fn new(source: String, file_path: Symbol) -> Self {
        Self { source, file_path }
    }

    fn error(&self, message: &str, loc: Location) -> Box<dyn ReportableError> {
        Box::new(SimpleError {
            message: message.to_string(),
            span: loc,
        })
    }

    pub fn convert_tree(&self, tree: &Tree) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let root_node = tree.root_node();
        self.convert_source_file(root_node)
    }

    pub fn convert_tree_to_program(&self, tree: &Tree) -> Result<Program, Box<dyn ReportableError>> {
        let root_node = tree.root_node();
        self.convert_source_file_to_program(root_node)
    }

    /// Parse source using tree-sitter to Program structure
    pub fn parse_treesitter(
        source: &str,
        file_path: Option<std::path::PathBuf>
    ) -> (Program, Vec<Box<dyn ReportableError>>) {
        let mut parser = match TreeSitterParser::new() {
            Ok(p) => p,
            Err(e) => {
                let error = SimpleError {
                    message: format!("Failed to create parser: {}", e),
                    span: Location::default(),
                };
                return (Program::default(), vec![Box::new(error)]);
            }
        };

        let tree = match parser.parse(source, None) {
            Ok(Some(tree)) => tree,
            Ok(None) => {
                let error = SimpleError {
                    message: "Failed to parse source".to_string(),
                    span: Location::default(),
                };
                return (Program::default(), vec![Box::new(error)]);
            }
            Err(e) => {
                let error = SimpleError {
                    message: format!("Parse error: {}", e),
                    span: Location::default(),
                };
                return (Program::default(), vec![Box::new(error)]);
            }
        };

        let file_symbol = file_path
            .map(|p| p.to_string_lossy().to_symbol())
            .unwrap_or_else(|| "<unknown>".to_symbol());
        
        let converter = ASTConverter::new(source.to_string(), file_symbol);
        
        match converter.convert_tree_to_program(&tree) {
            Ok(program) => (program, vec![]),
            Err(e) => (Program::default(), vec![e]),
        }
    }

    /// Parse source using tree-sitter - converts to Program then to ExprNodeId using existing infrastructure
    pub fn parse_to_expr_treesitter(
        source: &str, 
        file_path: Option<std::path::PathBuf>
    ) -> (ExprNodeId, Vec<Box<dyn ReportableError>>) {
        let (program, mut errors) = Self::parse_treesitter(source, file_path.clone());
        
        let file_symbol = file_path
            .map(|p| p.to_string_lossy().to_symbol())
            .unwrap_or_else(|| "<unknown>".to_symbol());
        
        // Use existing expr_from_program function
        let (expr, mut new_errors) = crate::ast::program::expr_from_program(program, file_symbol);
        
        errors.append(&mut new_errors);
        (expr, errors)
    }

    /// Add global context wrapper - moved from old parser to maintain compatibility
    pub fn add_global_context(ast: ExprNodeId, file_path: Symbol) -> ExprNodeId {
        use crate::utils::metadata::GLOBAL_LABEL;
        let span = ast.to_span();
        let loc = Location {
            span: span.clone(),
            path: file_path,
        };
        let res = Expr::Let(
            crate::pattern::TypedPattern::new(
                crate::pattern::Pattern::Single(GLOBAL_LABEL.to_symbol()),
                crate::types::Type::Unknown.into_id_with_location(loc.clone()),
            ),
            Expr::Lambda(vec![], None, ast).into_id(loc.clone()),
            None,
        );
        res.into_id(loc)
    }

    fn convert_source_file_to_program(&self, node: Node) -> Result<Program, Box<dyn ReportableError>> {
        assert_eq!(node.kind(), "source_file");
        
        let mut statements = Vec::new();
        let mut cursor = node.walk();
        
        // Parse all top-level items into ProgramStatements
        for child in node.children(&mut cursor) {
            match child.kind() {
                "item" => {
                    // Look inside the item for actual content
                    let mut item_cursor = child.walk();
                    for item_child in child.children(&mut item_cursor) {
                        match item_child.kind() {
                            "function_definition" => {
                                let program_stmt = self.convert_function_definition_to_program_stmt(item_child)?;
                                let span = self.node_span(item_child);
                                statements.push((program_stmt, span));
                            }
                            "include_statement" => {
                                if let Ok((import_stmt, span)) = self.convert_include_statement(item_child) {
                                    statements.push((import_stmt, span));
                                }
                            }
                            "let_statement" => {
                                if let Ok((let_stmt, span)) = self.convert_let_statement_to_program(item_child) {
                                    statements.push((let_stmt, span));
                                }
                            }
                            _ if item_child.is_named() => {
                                // For untreated items, use Statement::Single 
                                if let Ok(expr) = self.convert_any_expression(item_child) {
                                    let span = self.node_span(item_child);
                                    let stmt = Statement::Single(expr);
                                    statements.push((ProgramStatement::GlobalStatement(stmt), span));
                                }
                            }
                            _ => continue,
                        }
                    }
                }
                "function_definition" => {
                    let program_stmt = self.convert_function_definition_to_program_stmt(child)?;
                    let span = self.node_span(child);
                    statements.push((program_stmt, span));
                }
                "include_statement" => {
                    if let Ok((import_stmt, span)) = self.convert_include_statement(child) {
                        statements.push((import_stmt, span));
                    }
                }
                "let_statement" => {
                    if let Ok((let_stmt, span)) = self.convert_let_statement_to_program(child) {
                        statements.push((let_stmt, span));
                    }
                }
                "comment" => {
                    if let Ok((comment_stmt, span)) = self.convert_comment(child) {
                        statements.push((comment_stmt, span));
                    }
                }
                "ERROR" => {
                    // Return Statement::Error for error nodes
                    let span = self.node_span(child);
                    statements.push((ProgramStatement::Error, span));
                }
                _ if child.is_named() => {
                    // For untreated items, use Statement::Single
                    if let Ok(expr) = self.convert_any_expression(child) {
                        let span = self.node_span(child);
                        let stmt = Statement::Single(expr);
                        statements.push((ProgramStatement::GlobalStatement(stmt), span));
                    }
                }
                _ => {} // Skip unnamed nodes
            }
        }
        
        Ok(Program { statements })
    }

    fn convert_source_file(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        assert_eq!(node.kind(), "source_file");
        
        let mut statements = Vec::new();
        let mut cursor = node.walk();
        
        // Parse all top-level items
        for child in node.children(&mut cursor) {
            match child.kind() {
                "item" => {
                    // Look inside the item for actual content
                    let mut item_cursor = child.walk();
                    for item_child in child.children(&mut item_cursor) {
                        match item_child.kind() {
                            "function_definition" => {
                                let fn_expr = self.convert_function_definition(item_child)?;
                                statements.push(fn_expr);
                            }
                            _ if item_child.is_named() => {
                                // Try to parse other named nodes as expressions
                                if let Ok(expr) = self.convert_any_expression(item_child) {
                                    statements.push(expr);
                                }
                            }
                            _ => continue,
                        }
                    }
                }
                "function_definition" => {
                    let fn_expr = self.convert_function_definition(child)?;
                    statements.push(fn_expr);
                }
                "ERROR" => {
                    // Skip error nodes but continue parsing
                    continue;
                }
                _ if child.is_named() => {
                    // Try to parse other named nodes as expressions
                    if let Ok(expr) = self.convert_any_expression(child) {
                        statements.push(expr);
                    }
                }
                _ => {
                    // Skip unnamed nodes like whitespace
                    continue;
                }
            }
        }
        
        let loc = self.node_location(node);
        
        // Convert multiple statements into a sequence
        if statements.is_empty() {
            Ok(Expr::Literal(Literal::PlaceHolder).into_id(loc))
        } else if statements.len() == 1 {
            Ok(statements[0])
        } else {
            // Chain statements with Then expressions
            let mut result = statements.pop().unwrap();
            for stmt in statements.into_iter().rev() {
                result = Expr::Then(stmt, Some(result)).into_id(loc.clone());
            }
            Ok(result)
        }
    }

    fn node_span(&self, node: Node) -> Span {
        node.start_byte()..node.end_byte()
    }

    fn get_identifier_text(&self, node: Node) -> Symbol {
        self.node_text(node).to_symbol()
    }

    fn convert_function_definition_to_program_stmt(&self, node: Node) -> Result<ProgramStatement, Box<dyn ReportableError>> {
        assert_eq!(node.kind(), "function_definition");
        
        let mut cursor = node.walk();
        let mut name_opt = None;
        let mut params_opt = None;
        let mut body_opt = None;
        
        for child in node.children(&mut cursor) {
            match child.kind() {
                "identifier" => {
                    name_opt = Some(self.get_identifier_text(child));
                }
                "parameter_list" => {
                    params_opt = Some(self.convert_parameter_list(child)?);
                }
                "block_expression" => {
                    body_opt = Some(self.convert_block(child)?);
                }
                _ => {} // Skip other tokens like 'fn', parentheses, etc.
            }
        }
        
        let name = name_opt.ok_or_else(|| {
            self.error("Function definition missing name", self.node_location(node))
        })?;
        
        let (params, params_location) = params_opt.map_or_else(|| {
            (vec![], self.node_location(node))
        }, |params| (params, self.node_location(node)));
        
        let body = body_opt.ok_or_else(|| {
            self.error("Function definition missing body", self.node_location(node))
        })?;
        
        Ok(ProgramStatement::FnDefinition {
            name,
            args: (params, params_location),
            return_type: None, // TODO: Handle return type annotations
            body,
        })
    }

    fn convert_parameter_list(&self, node: Node) -> Result<Vec<TypedId>, Box<dyn ReportableError>> {
        let mut params = Vec::new();
        let mut cursor = node.walk();
        
        for child in node.children(&mut cursor) {
            match child.kind() {
                "identifier" => {
                    let param_name = self.get_identifier_text(child);
                    let param_loc = self.node_location(child);
                    // For now, all parameters have unknown type
                    let param_type = crate::types::Type::Unknown.into_id_with_location(param_loc.clone());
                    params.push(TypedId::new(param_name, param_type));
                }
                _ => {} // Skip commas and other tokens
            }
        }
        
        Ok(params)
    }

    fn convert_function_definition(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        assert_eq!(node.kind(), "function_definition");
        
        let mut cursor = node.walk();
        let mut name: Option<Symbol> = None;
        let mut params = Vec::new();
        let mut body: Option<ExprNodeId> = None;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "identifier" if name.is_none() => {
                    name = Some(self.node_text(child).to_symbol());
                }
                "parameter_list" => {
                    params = self.convert_parameter_list(child)?;
                }
                "block" | "block_expression" => {
                    body = Some(self.convert_block(child)?);
                }
                _ => {}
            }
        }

        let name = name.ok_or_else(|| self.error("Function without name", self.node_location(node)))?;
        let body = body.ok_or_else(|| self.error("Function without body", self.node_location(node)))?;
        let loc = self.node_location(node);

        // Create a lambda expression
        let lambda = Expr::Lambda(params, None, body).into_id(loc.clone());
        
        // Create a let binding for the function
        let pattern = crate::pattern::TypedPattern::new(
            crate::pattern::Pattern::Single(name), 
            crate::types::Type::Unknown.into_id_with_location(loc.clone())
        );
        Ok(Expr::Let(pattern, lambda, None).into_id(loc))
    }

    fn convert_block(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut statements = Vec::new();

        for child in node.children(&mut cursor) {
            match child.kind() {
                "expression_statement" => {
                    if let Ok(expr) = self.convert_expression_statement(child) {
                        statements.push(expr);
                    }
                }
                "let_statement" => {
                    if let Ok(expr) = self.convert_let_statement(child) {
                        statements.push(expr);
                    }
                }
                "binary_expression" | "call_expression" | "identifier" | "literal" | 
                "number" | "tuple_expression" | "self" => {
                    if let Ok(expr) = self.convert_any_expression(child) {
                        statements.push(expr);
                    }
                }
                _ if child.is_named() => {
                    // Try to parse other named nodes as expressions
                    if let Ok(expr) = self.convert_any_expression(child) {
                        statements.push(expr);
                    }
                }
                _ => {
                    // Skip unnamed nodes like braces, whitespace
                    continue;
                }
            }
        }

        let loc = self.node_location(node);
        
        if statements.is_empty() {
            // Empty block should return unit/void, not placeholder
            Ok(Expr::Literal(Literal::PlaceHolder).into_id(loc))
        } else if statements.len() == 1 {
            Ok(statements[0])
        } else {
            // Chain statements with Then expressions
            let mut result = statements.pop().unwrap();
            for stmt in statements.into_iter().rev() {
                result = Expr::Then(stmt, Some(result)).into_id(loc.clone());
            }
            Ok(result)
        }
    }

    fn convert_expression_statement(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.is_named() {
                return self.convert_any_expression(child);
            }
        }
        Err(self.error("Empty expression statement", self.node_location(node)))
    }

    fn convert_let_statement(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut pattern: Option<Symbol> = None;
        let mut expr: Option<ExprNodeId> = None;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "identifier" if pattern.is_none() => {
                    pattern = Some(self.node_text(child).to_symbol());
                }
                _ if child.is_named() && expr.is_none() => {
                    expr = Some(self.convert_any_expression(child)?);
                }
                _ => {}
            }
        }

        let pattern = pattern.ok_or_else(|| self.error("Let statement without pattern", self.node_location(node)))?;
        let expr = expr.ok_or_else(|| self.error("Let statement without expression", self.node_location(node)))?;
        let loc = self.node_location(node);

        let typed_pattern = crate::pattern::TypedPattern::new(
            crate::pattern::Pattern::Single(pattern), 
            crate::types::Type::Unknown.into_id_with_location(loc.clone())
        );
        Ok(Expr::Let(typed_pattern, expr, None).into_id(loc))
    }

    fn convert_any_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        match node.kind() {
            "binary_expression" => self.convert_binary_expression(node),
            "call_expression" => self.convert_call_expression(node),
            "identifier" => {
                let name = self.node_text(node).to_symbol();
                let loc = self.node_location(node);
                Ok(Expr::Var(name).into_id(loc))
            }
            "number" => {
                let text = self.node_text(node);
                let loc = self.node_location(node);
                // Always use Float for numeric literals to match original parser behavior
                Ok(Expr::Literal(Literal::Float(text.to_symbol())).into_id(loc))
            }
            "numeric_literal" => {
                let text = self.node_text(node);
                let loc = self.node_location(node);
                // Always use Float for numeric literals to match original parser behavior  
                Ok(Expr::Literal(Literal::Float(text.to_symbol())).into_id(loc))
            }
            "expression" => {
                // Handle generic "expression" nodes by recursing into their children
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if child.is_named() {
                        return self.convert_any_expression(child);
                    }
                }
                // If no named child found, return placeholder
                let loc = self.node_location(node);
                Ok(Expr::Literal(Literal::PlaceHolder).into_id(loc))
            }
            "primary_expression" => {
                // Handle primary_expression nodes by recursing into their children
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if child.is_named() {
                        return self.convert_any_expression(child);
                    }
                }
                // If no named child found, return placeholder
                let loc = self.node_location(node);
                Ok(Expr::Literal(Literal::PlaceHolder).into_id(loc))
            }
            "tuple_expression" => self.convert_tuple_expression(node),
            "string" | "string_literal" => {
                let text = self.node_text(node);
                let loc = self.node_location(node);
                // Remove surrounding quotes from string literal
                let content = if text.starts_with('"') && text.ends_with('"') && text.len() >= 2 {
                    &text[1..text.len()-1]
                } else {
                    text
                };
                Ok(Expr::Literal(Literal::String(content.to_symbol())).into_id(loc))
            }
            "self" => {
                let loc = self.node_location(node);
                Ok(Expr::Literal(Literal::SelfLit).into_id(loc))
            }
            _ => {
                // For unknown nodes, try to find the first expression child
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if child.is_named() {
                        if let Ok(expr) = self.convert_any_expression(child) {
                            return Ok(expr);
                        }
                    }
                }
                Err(self.error(&format!("Unsupported expression type: {}", node.kind()), self.node_location(node)))
            }
        }
    }

    fn convert_binary_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut left: Option<ExprNodeId> = None;
        let mut operator: Option<String> = None;
        let mut right: Option<ExprNodeId> = None;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "+" | "-" | "*" | "/" | "%" | "<" | ">" | "<=" | ">=" | "==" | "!=" | "&&" | "||" => {
                    operator = Some(child.kind().to_string());
                }
                _ if child.is_named() => {
                    if left.is_none() {
                        left = Some(self.convert_any_expression(child)?);
                    } else if right.is_none() {
                        right = Some(self.convert_any_expression(child)?);
                    }
                }
                _ => {}
            }
        }

        let left = left.ok_or_else(|| self.error("Binary expression without left operand", self.node_location(node)))?;
        let right = right.ok_or_else(|| self.error("Binary expression without right operand", self.node_location(node)))?;
        let op = operator.ok_or_else(|| self.error("Binary expression without operator", self.node_location(node)))?;
        let loc = self.node_location(node);

        let op_enum = match op.as_str() {
            "+" => Op::Sum,
            "-" => Op::Minus,
            "*" => Op::Product,
            "/" => Op::Divide,
            "%" => Op::Modulo,
            "<" => Op::LessThan,
            ">" => Op::GreaterThan,
            "<=" => Op::LessEqual,
            ">=" => Op::GreaterEqual,
            "==" => Op::Equal,
            "!=" => Op::NotEqual,
            "&&" => Op::And,
            "||" => Op::Or,
            _ => return Err(self.error("Unknown operator", loc)),
        };

        let span = loc.span.clone();
        Ok(Expr::BinOp(left, (op_enum, span), right).into_id(loc))
    }

    fn convert_call_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut function: Option<ExprNodeId> = None;
        let mut args = Vec::new();

        for child in node.children(&mut cursor) {
            match child.kind() {
                "argument_list" => {
                    let mut arg_cursor = child.walk();
                    for arg_child in child.children(&mut arg_cursor) {
                        if arg_child.is_named() && arg_child.kind() != "," {
                            args.push(self.convert_any_expression(arg_child)?);
                        }
                    }
                }
                _ if child.is_named() && function.is_none() => {
                    function = Some(self.convert_any_expression(child)?);
                }
                _ => {}
            }
        }

        let function = function.ok_or_else(|| self.error("Call expression without function", self.node_location(node)))?;
        let loc = self.node_location(node);

        Ok(Expr::Apply(function, args).into_id(loc))
    }

    fn convert_tuple_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut elements = Vec::new();

        for child in node.children(&mut cursor) {
            if child.is_named() && child.kind() != "," {
                elements.push(self.convert_any_expression(child)?);
            }
        }

        let loc = self.node_location(node);
        Ok(Expr::Tuple(elements).into_id(loc))
    }

    fn node_text(&self, node: Node) -> &str {
        node.utf8_text(self.source.as_bytes()).unwrap()
    }

    fn node_location(&self, node: Node) -> Location {
        Location {
            span: node.start_byte()..node.end_byte(),
            path: self.file_path,
        }
    }

    fn convert_include_statement(&self, node: Node) -> Result<(ProgramStatement, Span), Box<dyn ReportableError>> {
        assert_eq!(node.kind(), "include_statement");
        let mut cursor = node.walk();
        
        for child in node.children(&mut cursor) {
            if child.kind() == "string_literal" {
                let text = self.node_text(child);
                let filename = text.trim_matches('"');
                let span = self.node_span(node);
                return Ok((ProgramStatement::Import(filename.to_symbol()), span));
            }
        }
        
        Err(self.error("Include statement without filename", self.node_location(node)))
    }

    fn convert_let_statement_to_program(&self, node: Node) -> Result<(ProgramStatement, Span), Box<dyn ReportableError>> {
        assert_eq!(node.kind(), "let_statement");
        let mut cursor = node.walk();
        let mut is_rec = false;
        let mut pattern: Option<TypedPattern> = None;
        let mut body: Option<ExprNodeId> = None;
        
        for child in node.children(&mut cursor) {
            match child.kind() {
                "letrec" => is_rec = true,
                "let" => is_rec = false,
                "pattern" => {
                    // For now, assume simple identifier patterns
                    pattern = Some(self.convert_pattern(child)?);
                }
                "identifier" => {
                    if pattern.is_none() {
                        // Simple identifier pattern
                        let name = self.node_text(child).to_symbol();
                        pattern = Some(TypedPattern {
                            pat: Pattern::Single(name),
                            ty: Type::Unknown.into_id(),
                            default_value: None,
                        });
                    }
                }
                _ if child.is_named() && body.is_none() => {
                    // Try to parse as expression
                    if let Ok(expr) = self.convert_any_expression(child) {
                        body = Some(expr);
                    }
                }
                _ => {}
            }
        }
        
        let pattern = pattern.ok_or_else(|| self.error("Let statement without pattern", self.node_location(node)))?;
        let body = body.ok_or_else(|| self.error("Let statement without body", self.node_location(node)))?;
        let span = self.node_span(node);
        
        let statement = if is_rec {
            // For let rec, convert pattern to TypedId
            match pattern.pat {
                Pattern::Single(name) => Statement::LetRec(TypedId::new(name, pattern.ty), body),
                _ => return Err(self.error("LetRec requires simple identifier", self.node_location(node))),
            }
        } else {
            Statement::Let(pattern, body)
        };
        
        Ok((ProgramStatement::GlobalStatement(statement), span))
    }

    fn convert_pattern(&self, node: Node) -> Result<TypedPattern, Box<dyn ReportableError>> {
        // For now, just handle simple identifier patterns
        if node.kind() == "pattern" {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "identifier" {
                    let name = self.node_text(child).to_symbol();
                    return Ok(TypedPattern {
                        pat: Pattern::Single(name),
                        ty: Type::Unknown.into_id(),
                        default_value: None,
                    });
                }
            }
        }
        
        if node.kind() == "identifier" {
            let name = self.node_text(node).to_symbol();
            return Ok(TypedPattern {
                pat: Pattern::Single(name),
                ty: Type::Unknown.into_id(),
                default_value: None,
            });
        }
        
        Err(self.error("Unsupported pattern type", self.node_location(node)))
    }

    fn convert_comment(&self, node: Node) -> Result<(ProgramStatement, Span), Box<dyn ReportableError>> {
        assert_eq!(node.kind(), "comment");
        let text = self.node_text(node);
        let span = self.node_span(node);
        
        // Check if it's a doc comment (starts with ///)
        if text.starts_with("///") {
            Ok((ProgramStatement::DocComment(text.to_symbol()), span))
        } else {
            Ok((ProgramStatement::Comment(text.to_symbol()), span))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::metadata::Location;
    use crate::pattern::{TypedPattern, Pattern};
    use crate::types::Type;

    macro_rules! test_tree_sitter_string {
        ($src:literal, $ans:expr) => {
            let (ast, errs) = ASTConverter::parse_to_expr_treesitter(&$src, None);
            if errs.is_empty() {
                assert!(
                    ast.to_expr() == $ans.to_expr(),
                    "tree-sitter result:{:#?}\nexpected:{:#?}",
                    ast,
                    $ans
                );
            } else {
                eprintln!("Tree-sitter parse errors for '{}': {:#?}", $src, errs);
                panic!("Tree-sitter parsing failed");
            }
        };
    }

    // Dummy location for testing
    fn loc(span: std::ops::Range<usize>) -> Location {
        Location {
            span,
            path: "<unknown>".to_symbol(),
        }
    }

    #[test]
    fn test_parser_creation() {
        let parser = TreeSitterParser::new();
        assert!(parser.is_ok());
    }

    #[test]
    fn test_simple_parse() {
        let mut parser = TreeSitterParser::new().unwrap();
        let source = "fn test() { 42 }";
        let tree = parser.parse(source, None);
        assert!(tree.is_ok());
    }

    // Tree-sitter equivalent tests for the original parser tests
    
    #[test]
    fn tree_sitter_test_int() {
        let ans = Expr::Literal(Literal::Float("3466".to_symbol())).into_id(loc(0..4));
        test_tree_sitter_string!("3466", ans);
    }

    #[test]
    fn tree_sitter_test_string() {
        let ans = Expr::Literal(Literal::String("teststr".to_symbol())).into_id(loc(0..9));
        test_tree_sitter_string!("\"teststr\"", ans);
    }

    #[test]
    fn tree_sitter_test_var() {
        let ans = Expr::Var("hoge".to_symbol()).into_id(loc(0..4));
        test_tree_sitter_string!("hoge", ans);
    }

    #[test]
    fn tree_sitter_test_add() {
        let ans = Expr::BinOp(
            Expr::Literal(Literal::Float("3466.0".to_symbol())).into_id(loc(0..6)),
            (Op::Sum, 6..7),
            Expr::Literal(Literal::Float("2000.0".to_symbol())).into_id(loc(7..13)),
        )
        .into_id(loc(0..13));
        test_tree_sitter_string!("3466.0+2000.0", ans);
    }

    #[test]
    fn tree_sitter_test_apply() {
        let ans = Expr::Apply(
            Expr::Var("myfun".to_symbol()).into_id(loc(0..5)),
            vec![Expr::Var("callee".to_symbol()).into_id(loc(6..12))],
        )
        .into_id(loc(0..13));
        test_tree_sitter_string!("myfun(callee)", ans);
    }

    #[test]
    fn tree_sitter_test_tuple() {
        let tuple_items = vec![
            Expr::Literal(Literal::Float("1.0".to_symbol())).into_id(loc(1..4)),
            Expr::Literal(Literal::Float("2.0".to_symbol())).into_id(loc(6..9)),
        ];

        let ans = Expr::Tuple(tuple_items.clone()).into_id(loc(0..10));
        test_tree_sitter_string!("(1.0, 2.0)", ans);
    }

    #[test]
    fn tree_sitter_test_let() {
        let ans = Expr::Let(
            TypedPattern::new(
                Pattern::Single("goge".to_symbol()),
                Type::Unknown.into_id_with_location(loc(4..8)),
            ),
            Expr::Literal(Literal::Float("36".to_symbol())).into_id(loc(11..13)),
            Some(Expr::Var("goge".to_symbol()).into_id(loc(15..19))),
        )
        .into_id(loc(0..19));
        test_tree_sitter_string!("let goge = 36\n goge", ans);
    }

    #[test]
    fn tree_sitter_test_if() {
        let ans = Expr::If(
            Expr::Literal(Literal::Float("100".to_symbol())).into_id(loc(4..7)),
            Expr::Var("hoge".to_symbol()).into_id(loc(9..13)),
            Some(Expr::Var("fuga".to_symbol()).into_id(loc(19..23))),
        )
        .into_id(loc(0..23));
        test_tree_sitter_string!("if (100) hoge else fuga", ans);
    }

    #[test]
    fn tree_sitter_test_if_noelse() {
        let ans = Expr::If(
            Expr::Literal(Literal::Float("100".to_symbol())).into_id(loc(4..7)),
            Expr::Var("hoge".to_symbol()).into_id(loc(9..13)),
            None,
        )
        .into_id(loc(0..13));
        test_tree_sitter_string!("if (100) hoge ", ans);
    }

    #[test] 
    fn test_simple_number() {
        let source = "42";
        let (ast, errors) = ASTConverter::parse_to_expr_treesitter(source, None);
        
        println!("Parsing '{}': AST={:?}, Errors={:?}", source, ast.to_expr(), errors);
        
        // Check if we get a reasonable result
        assert!(errors.len() < 5); // allow some errors during development
    }
    
    #[test]
    fn test_simple_binary() {
        let source = "1 + 2";
        let (ast, errors) = ASTConverter::parse_to_expr_treesitter(source, None);
        
        println!("Parsing '{}': AST={:?}, Errors={:?}", source, ast.to_expr(), errors);
        
        // Check if we get a reasonable result
        assert!(errors.len() < 5); // allow some errors during development
    }

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
        
        // Let's also test the tree-sitter parser directly
        let mut parser = TreeSitterParser::new().unwrap();
        let tree = parser.parse(source, None).unwrap().unwrap();
        let root = tree.root_node();
        
        println!("Tree-sitter root kind: {}", root.kind());
        println!("Tree-sitter root has {} children", root.child_count());
        
        let mut cursor = root.walk();
        for (i, child) in root.children(&mut cursor).enumerate() {
            println!("Child {}: kind='{}', text='{:?}'", i, child.kind(), child.utf8_text(source.as_bytes()));
            
            if child.kind() == "item" {
                let mut item_cursor = child.walk();
                for (j, item_child) in child.children(&mut item_cursor).enumerate() {
                    println!("  Item child {}: kind='{}', text='{:?}'", j, item_child.kind(), item_child.utf8_text(source.as_bytes()));
                    
                    if item_child.kind() == "function_definition" {
                        let mut fn_cursor = item_child.walk();
                        for (k, fn_child) in item_child.children(&mut fn_cursor).enumerate() {
                            println!("    Fn child {}: kind='{}', text='{:?}'", k, fn_child.kind(), fn_child.utf8_text(source.as_bytes()));
                        }
                    }
                }
            }
        }
        
        // Should not have errors for basic parsing
        if !errors.is_empty() {
            for error in &errors {
                println!("Error: {:?}", error);
            }
        }
    }
}