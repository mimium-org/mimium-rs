use crate::ast::*;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::types::{PType, RecordTypeField, Type};
use crate::utils::error::ReportableError;
use crate::utils::metadata::*;
use std::path::PathBuf;

use tree_sitter::{Language, Parser, Tree, Node};

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
        parser.set_language(language())?;
        Ok(Self { parser, tree: None })
    }

    pub fn parse(&mut self, source: &str, old_tree: Option<&Tree>) -> Result<Tree, Box<dyn std::error::Error>> {
        let tree = self.parser.parse(source, old_tree)?;
        self.tree = tree.as_ref().cloned();
        tree.ok_or_else(|| "Failed to parse".into())
    }

    pub fn get_tree(&self) -> Option<&Tree> {
        self.tree.as_ref()
    }
}

/// Convert tree-sitter CST to mimium AST
pub struct ASTConverter {
    source: String,
    file_path: Symbol,
}

impl ASTConverter {
    pub fn new(source: String, file_path: Symbol) -> Self {
        Self { source, file_path }
    }

    pub fn convert_tree(&self, tree: &Tree) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let root_node = tree.root_node();
        self.convert_source_file(root_node)
    }

    fn convert_source_file(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        assert_eq!(node.kind(), "source_file");
        
        let mut statements = Vec::new();
        let mut cursor = node.walk();
        
        for child in node.children(&mut cursor) {
            if child.kind() == "item" {
                let expr = self.convert_item(child)?;
                statements.push(expr);
            }
        }
        
        // Convert to a single expression (block-like)
        if statements.is_empty() {
            // Empty program - return unit literal
            Ok(Literal::Unit.into_id_with_location(self.node_location(node)))
        } else if statements.len() == 1 {
            Ok(statements[0])
        } else {
            // Create a sequence/block from the statements
            let loc = self.node_location(node);
            Ok(Expr::Block(statements).into_id_with_location(loc))
        }
    }

    fn convert_item(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        assert_eq!(node.kind(), "item");
        
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "include_statement" => return self.convert_include_statement(child),
                "function_definition" => return self.convert_function_definition(child),
                "macro_definition" => return self.convert_macro_definition(child),
                "let_statement" => return self.convert_let_statement(child),
                "expression" => return self.convert_expression(child),
                _ => continue,
            }
        }
        
        Err(format!("Unknown item type: {}", node.kind()).into())
    }

    fn convert_include_statement(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        // Find the string literal child
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "string_literal" {
                let path_str = self.node_text(child);
                let path_str = path_str.trim_matches('"'); // Remove quotes
                let path_sym = path_str.to_symbol();
                let loc = self.node_location(node);
                return Ok(Expr::Include(path_sym).into_id_with_location(loc));
            }
        }
        
        Err("Include statement without string literal".into())
    }

    fn convert_function_definition(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut name: Option<Symbol> = None;
        let mut params = Vec::new();
        let mut return_type = None;
        let mut body: Option<ExprNodeId> = None;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "identifier" if name.is_none() => {
                    name = Some(self.node_text(child).to_symbol());
                }
                "parameter_list" => {
                    params = self.convert_parameter_list(child)?;
                }
                "return_type" => {
                    return_type = Some(self.convert_return_type(child)?);
                }
                "block_expression" => {
                    body = Some(self.convert_block_expression(child)?);
                }
                _ => {}
            }
        }

        let name = name.ok_or("Function without name")?;
        let body = body.ok_or("Function without body")?;
        let loc = self.node_location(node);

        Ok(Expr::Var {
            name,
            expr: Expr::Lambda {
                args: params,
                body,
                captured_vars: vec![],
                ret_type: return_type,
            }.into_id_with_location(loc),
        }.into_id_with_location(loc))
    }

    fn convert_macro_definition(&self, _node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        // For now, treat macros similarly to functions
        // TODO: Implement proper macro handling
        Err("Macro definitions not yet supported in tree-sitter parser".into())
    }

    fn convert_let_statement(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut is_rec = false;
        let mut pattern: Option<TypedPattern> = None;
        let mut expr: Option<ExprNodeId> = None;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "letrec" => is_rec = true,
                "pattern" => {
                    pattern = Some(self.convert_pattern(child)?);
                }
                "expression" => {
                    expr = Some(self.convert_expression(child)?);
                }
                _ => {}
            }
        }

        let pattern = pattern.ok_or("Let statement without pattern")?;
        let expr = expr.ok_or("Let statement without expression")?;
        let loc = self.node_location(node);

        Ok(Expr::Let {
            is_rec,
            pattern,
            expr,
        }.into_id_with_location(loc))
    }

    fn convert_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "assignment" => return self.convert_assignment(child),
                "binary_expression" => return self.convert_binary_expression(child),
                "unary_expression" => return self.convert_unary_expression(child),
                "primary_expression" => return self.convert_primary_expression(child),
                "application_expression" => return self.convert_application_expression(child),
                "lambda_expression" => return self.convert_lambda_expression(child),
                "if_expression" => return self.convert_if_expression(child),
                "block_expression" => return self.convert_block_expression(child),
                "pipe_expression" => return self.convert_pipe_expression(child),
                _ => continue,
            }
        }
        
        Err(format!("Unknown expression type: {}", node.kind()).into())
    }

    fn convert_assignment(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut assignee: Option<ExprNodeId> = None;
        let mut expr: Option<ExprNodeId> = None;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "assignee" => {
                    assignee = Some(self.convert_assignee(child)?);
                }
                "expression" => {
                    expr = Some(self.convert_expression(child)?);
                }
                _ => {}
            }
        }

        let assignee = assignee.ok_or("Assignment without assignee")?;
        let expr = expr.ok_or("Assignment without expression")?;
        let loc = self.node_location(node);

        Ok(Expr::Assign {
            assignee,
            expr,
        }.into_id_with_location(loc))
    }

    fn convert_assignee(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "identifier" => {
                    let name = self.node_text(child).to_symbol();
                    let loc = self.node_location(child);
                    return Ok(Expr::Var { name, expr: Literal::Unit.into_id() }.into_id_with_location(loc));
                }
                "field_access" => return self.convert_field_access(child),
                "array_access" => return self.convert_array_access(child),
                _ => continue,
            }
        }
        
        Err("Unknown assignee type".into())
    }

    fn convert_binary_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut left: Option<ExprNodeId> = None;
        let mut operator: Option<String> = None;
        let mut right: Option<ExprNodeId> = None;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "expression" => {
                    if left.is_none() {
                        left = Some(self.convert_expression(child)?);
                    } else {
                        right = Some(self.convert_expression(child)?);
                    }
                }
                "+" | "-" | "*" | "/" | "%" | "<" | ">" | "<=" | ">=" | "==" | "!=" | "&&" | "||" => {
                    operator = Some(child.kind().to_string());
                }
                _ => {}
            }
        }

        let left = left.ok_or("Binary expression without left operand")?;
        let right = right.ok_or("Binary expression without right operand")?;
        let op = operator.ok_or("Binary expression without operator")?;
        let loc = self.node_location(node);

        // Convert string operator to Op enum
        let op = match op.as_str() {
            "+" => crate::ast::operators::Op::Add,
            "-" => crate::ast::operators::Op::Sub,
            "*" => crate::ast::operators::Op::Mult,
            "/" => crate::ast::operators::Op::Div,
            "%" => crate::ast::operators::Op::Mod,
            "<" => crate::ast::operators::Op::Less,
            ">" => crate::ast::operators::Op::Greater,
            "<=" => crate::ast::operators::Op::LessEq,
            ">=" => crate::ast::operators::Op::GreaterEq,
            "==" => crate::ast::operators::Op::Equal,
            "!=" => crate::ast::operators::Op::NotEq,
            "&&" => crate::ast::operators::Op::And,
            "||" => crate::ast::operators::Op::Or,
            _ => return Err(format!("Unknown operator: {}", op).into()),
        };

        Ok(Expr::BinOp {
            op,
            left,
            right,
        }.into_id_with_location(loc))
    }

    fn convert_unary_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut operator: Option<String> = None;
        let mut expr: Option<ExprNodeId> = None;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "-" | "!" => {
                    operator = Some(child.kind().to_string());
                }
                "expression" => {
                    expr = Some(self.convert_expression(child)?);
                }
                _ => {}
            }
        }

        let expr = expr.ok_or("Unary expression without operand")?;
        let op = operator.ok_or("Unary expression without operator")?;
        let loc = self.node_location(node);

        let op = match op.as_str() {
            "-" => crate::ast::operators::Op::Neg,
            "!" => crate::ast::operators::Op::Not,
            _ => return Err(format!("Unknown unary operator: {}", op).into()),
        };

        Ok(Expr::UnaryOp {
            op,
            expr,
        }.into_id_with_location(loc))
    }

    fn convert_primary_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "identifier" => {
                    let name = self.node_text(child).to_symbol();
                    let loc = self.node_location(child);
                    return Ok(Expr::Var { name, expr: Literal::Unit.into_id() }.into_id_with_location(loc));
                }
                "numeric_literal" => return self.convert_numeric_literal(child),
                "string_literal" => return self.convert_string_literal(child),
                "self_literal" => {
                    let loc = self.node_location(child);
                    return Ok(Expr::SelfLit.into_id_with_location(loc));
                }
                "now_literal" => {
                    let loc = self.node_location(child);
                    return Ok(Expr::Now.into_id_with_location(loc));
                }
                "samplerate_literal" => {
                    let loc = self.node_location(child);
                    return Ok(Expr::ExtFunCall {
                        fname: "samplerate".to_symbol(),
                        args: vec![],
                        ret_type: None,
                    }.into_id_with_location(loc));
                }
                "array_literal" => return self.convert_array_literal(child),
                "record_literal" => return self.convert_record_literal(child),
                "tuple_expression" => return self.convert_tuple_expression(child),
                "parenthesized_expression" => return self.convert_parenthesized_expression(child),
                "field_access" => return self.convert_field_access(child),
                "array_access" => return self.convert_array_access(child),
                "macro_call" => return self.convert_macro_call(child),
                _ => continue,
            }
        }
        
        Err(format!("Unknown primary expression type: {}", node.kind()).into())
    }

    fn convert_application_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut function: Option<ExprNodeId> = None;
        let mut args = Vec::new();

        for child in node.children(&mut cursor) {
            match child.kind() {
                "expression" => {
                    function = Some(self.convert_expression(child)?);
                }
                "argument_list" => {
                    args = self.convert_argument_list(child)?;
                }
                _ => {}
            }
        }

        let function = function.ok_or("Application without function")?;
        let loc = self.node_location(node);

        Ok(Expr::Apply {
            function,
            args,
            ret_type: None,
        }.into_id_with_location(loc))
    }

    fn convert_lambda_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut params = Vec::new();
        let mut body: Option<ExprNodeId> = None;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "parameter_list" => {
                    params = self.convert_parameter_list(child)?;
                }
                "expression" => {
                    body = Some(self.convert_expression(child)?);
                }
                _ => {}
            }
        }

        let body = body.ok_or("Lambda without body")?;
        let loc = self.node_location(node);

        Ok(Expr::Lambda {
            args: params,
            body,
            captured_vars: vec![],
            ret_type: None,
        }.into_id_with_location(loc))
    }

    fn convert_if_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut condition: Option<ExprNodeId> = None;
        let mut then_expr: Option<ExprNodeId> = None;
        let mut else_expr: Option<ExprNodeId> = None;
        let mut block_count = 0;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "expression" => {
                    condition = Some(self.convert_expression(child)?);
                }
                "block_expression" => {
                    if block_count == 0 {
                        then_expr = Some(self.convert_block_expression(child)?);
                        block_count += 1;
                    } else {
                        else_expr = Some(self.convert_block_expression(child)?);
                    }
                }
                _ => {}
            }
        }

        let condition = condition.ok_or("If expression without condition")?;
        let then_expr = then_expr.ok_or("If expression without then clause")?;
        let loc = self.node_location(node);

        Ok(Expr::IfElse {
            condition,
            then_expr,
            else_expr,
        }.into_id_with_location(loc))
    }

    fn convert_block_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut statements = Vec::new();

        for child in node.children(&mut cursor) {
            if child.kind() == "item" {
                let expr = self.convert_item(child)?;
                statements.push(expr);
            }
        }

        let loc = self.node_location(node);

        if statements.is_empty() {
            Ok(Literal::Unit.into_id_with_location(loc))
        } else if statements.len() == 1 {
            Ok(statements[0])
        } else {
            Ok(Expr::Block(statements).into_id_with_location(loc))
        }
    }

    fn convert_pipe_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut left: Option<ExprNodeId> = None;
        let mut right: Option<ExprNodeId> = None;

        for child in node.children(&mut cursor) {
            if child.kind() == "expression" {
                if left.is_none() {
                    left = Some(self.convert_expression(child)?);
                } else {
                    right = Some(self.convert_expression(child)?);
                }
            }
        }

        let left = left.ok_or("Pipe expression without left operand")?;
        let right = right.ok_or("Pipe expression without right operand")?;
        let loc = self.node_location(node);

        // Transform pipe expression into function application: right(left)
        Ok(Expr::Apply {
            function: right,
            args: vec![left],
            ret_type: None,
        }.into_id_with_location(loc))
    }

    // Helper methods

    fn convert_parameter_list(&self, node: Node) -> Result<Vec<TypedId>, Box<dyn ReportableError>> {
        let mut params = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() == "parameter" {
                params.push(self.convert_parameter(child)?);
            }
        }

        Ok(params)
    }

    fn convert_parameter(&self, node: Node) -> Result<TypedId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut pattern: Option<TypedPattern> = None;
        let mut type_annotation: Option<TypeNodeId> = None;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "pattern" => {
                    pattern = Some(self.convert_pattern(child)?);
                }
                "type_annotation" => {
                    type_annotation = Some(self.convert_type_annotation(child)?);
                }
                _ => {}
            }
        }

        let pattern = pattern.ok_or("Parameter without pattern")?;
        
        match pattern {
            TypedPattern::Variant { id, .. } => Ok(TypedId {
                id,
                ty: type_annotation,
            }),
            _ => Err("Only identifier patterns supported in parameters".into()),
        }
    }

    fn convert_return_type(&self, node: Node) -> Result<TypeNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "type" {
                return self.convert_type(child);
            }
        }
        
        Err("Return type without type".into())
    }

    fn convert_pattern(&self, node: Node) -> Result<TypedPattern, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "identifier_pattern" => {
                    let id = self.convert_identifier_pattern(child)?;
                    return Ok(TypedPattern::Variant { id, inner: None });
                }
                "wildcard_pattern" => {
                    let id = "_".to_symbol();
                    return Ok(TypedPattern::Variant { id, inner: None });
                }
                // TODO: Add support for other pattern types
                _ => continue,
            }
        }
        
        Err("Unknown pattern type".into())
    }

    fn convert_identifier_pattern(&self, node: Node) -> Result<Symbol, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "identifier" {
                return Ok(self.node_text(child).to_symbol());
            }
        }
        
        Err("Identifier pattern without identifier".into())
    }

    fn convert_argument_list(&self, node: Node) -> Result<Vec<ExprNodeId>, Box<dyn ReportableError>> {
        let mut args = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() == "expression" {
                args.push(self.convert_expression(child)?);
            }
        }

        Ok(args)
    }

    fn convert_numeric_literal(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "integer_literal" => {
                    let text = self.node_text(child);
                    let value: i64 = text.parse().map_err(|_| format!("Invalid integer: {}", text))?;
                    let loc = self.node_location(child);
                    return Ok(Literal::Int(value).into_id_with_location(loc));
                }
                "float_literal" => {
                    let text = self.node_text(child);
                    let value: f64 = text.parse().map_err(|_| format!("Invalid float: {}", text))?;
                    let loc = self.node_location(child);
                    return Ok(Literal::Float(value).into_id_with_location(loc));
                }
                _ => continue,
            }
        }
        
        Err("Numeric literal without value".into())
    }

    fn convert_string_literal(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let text = self.node_text(node);
        let value = text.trim_matches('"').to_string(); // Remove quotes
        let loc = self.node_location(node);
        Ok(Literal::String(value.to_symbol()).into_id_with_location(loc))
    }

    fn convert_array_literal(&self, _node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        // TODO: Implement array literal conversion
        Err("Array literals not yet supported".into())
    }

    fn convert_record_literal(&self, _node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        // TODO: Implement record literal conversion
        Err("Record literals not yet supported".into())
    }

    fn convert_tuple_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        let mut elements = Vec::new();

        for child in node.children(&mut cursor) {
            if child.kind() == "expression" {
                elements.push(self.convert_expression(child)?);
            }
        }

        let loc = self.node_location(node);
        Ok(Expr::Tuple(elements).into_id_with_location(loc))
    }

    fn convert_parenthesized_expression(&self, node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "expression" {
                return self.convert_expression(child);
            }
        }
        
        Err("Parenthesized expression without expression".into())
    }

    fn convert_field_access(&self, _node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        // TODO: Implement field access conversion
        Err("Field access not yet supported".into())
    }

    fn convert_array_access(&self, _node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        // TODO: Implement array access conversion
        Err("Array access not yet supported".into())
    }

    fn convert_macro_call(&self, _node: Node) -> Result<ExprNodeId, Box<dyn ReportableError>> {
        // TODO: Implement macro call conversion
        Err("Macro calls not yet supported".into())
    }

    fn convert_type(&self, _node: Node) -> Result<TypeNodeId, Box<dyn ReportableError>> {
        // TODO: Implement type conversion
        Err("Type conversion not yet implemented".into())
    }

    fn convert_type_annotation(&self, node: Node) -> Result<TypeNodeId, Box<dyn ReportableError>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "type" {
                return self.convert_type(child);
            }
        }
        
        Err("Type annotation without type".into())
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
}

#[cfg(test)]
mod tests {
    use super::*;

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
}