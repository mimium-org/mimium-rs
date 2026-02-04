use std::path::PathBuf;

use mimium_lang::{
    ast::{
        Expr, Literal,
        program::{Program, ProgramStatement},
    },
    interner::ExprNodeId,
    types::Type,
    utils::error::ReportableError,
};
use pretty::{Arena, DocAllocator, DocBuilder, Pretty};

use crate::{GLOBAL_DATA, GlobalConfig};

fn get_indent_size() -> usize {
    if let Ok(gdata) = GLOBAL_DATA.try_lock() {
        gdata.indent_size
    } else {
        GlobalConfig::default().indent_size
    }
}
fn breakable_comma<'a, D, A>(allocator: &'a D) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    allocator.text(",").append(allocator.softline())
}

mod types {
    use mimium_lang::{
        interner::TypeNodeId,
        types::{PType, RecordTypeField},
    };

    use super::*;
    pub(super) fn pretty<'a, D, A>(ty: TypeNodeId, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone + Pretty<'a, D, A>,
        A: Clone,
    {
        match ty.to_type() {
            Type::Unknown => allocator.nil(),
            Type::Primitive(PType::Int) => allocator.text("int"),
            Type::Primitive(PType::Numeric) => allocator.text("float"),
            Type::Primitive(PType::String) => allocator.text("string"),
            Type::Primitive(PType::Unit) => allocator.text("()"),
            Type::Array(item) => types::pretty(item, allocator).brackets(),
            Type::Tuple(items) => {
                let docs = items
                    .into_iter()
                    .map(|item| types::pretty(item, allocator))
                    .collect::<Vec<_>>();
                allocator
                    .intersperse(docs, breakable_comma(allocator))
                    .parens()
            }
            Type::Record(items) => {
                let docs = items
                    .into_iter()
                    .map(|RecordTypeField { key, ty, .. }| {
                        allocator
                            .text(key)
                            .append(allocator.text(": "))
                            .append(types::pretty(ty, allocator))
                    })
                    .collect::<Vec<_>>();
                allocator
                    .intersperse(docs, breakable_comma(allocator))
                    .braces()
            }
            Type::Function { arg, ret } => {
                let param_docs = types::pretty(arg, allocator);
                let ret_doc = types::pretty(ret, allocator);
                param_docs
                    .parens()
                    .append(allocator.text(" -> "))
                    .append(ret_doc)
            }
            Type::Code(ty) => allocator.text("`").append(types::pretty(ty, allocator)),
            Type::TypeScheme(_) => unreachable!(),
            Type::Intermediate(_) => unreachable!(),
            Type::Ref(_) => unreachable!(),
            Type::Failure => unreachable!(),
            Type::Any => allocator.text("any"),
        }
    }
}
mod patterns {
    use mimium_lang::pattern::Pattern;

    use super::*;
    pub(super) fn pretty<'a, D, A>(pat: Pattern, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone + Pretty<'a, D, A>,
        A: Clone,
    {
        match pat {
            Pattern::Single(name) => allocator.text(name),
            Pattern::Placeholder => allocator.text("_"),
            Pattern::Tuple(pats) => {
                let docs = pats
                    .into_iter()
                    .map(|p| pretty(p, allocator))
                    .collect::<Vec<_>>();
                allocator
                    .intersperse(docs, breakable_comma(allocator))
                    .parens()
            }
            Pattern::Record(items) => {
                let docs = items.into_iter().map(|(name, pattern)| {
                    allocator
                        .text(name)
                        .append(allocator.text(" = "))
                        .append(pretty(pattern, allocator))
                });
                allocator
                    .intersperse(docs, breakable_comma(allocator))
                    .braces()
            }
            Pattern::Error => todo!(),
        }
    }
}
mod typedpattern {
    use mimium_lang::pattern::TypedPattern;

    use super::*;
    pub(super) fn pretty<'a, D, A>(pat: TypedPattern, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone + Pretty<'a, D, A>,
        A: Clone,
    {
        patterns::pretty(pat.pat, allocator).append(types::pretty(pat.ty, allocator))
    }
}

mod expr {
    use std::ops::Add;

    use mimium_lang::ast::operators::Op;

    use super::*;
    pub(super) fn pretty<'a, D, A>(expr: ExprNodeId, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone + Pretty<'a, D, A>,
        A: Clone,
    {
        match expr.to_expr() {
            Expr::Literal(Literal::String(s)) => allocator.text(s).double_quotes(),
            Expr::Literal(Literal::Int(i)) => allocator.text(i.to_string()),
            Expr::Literal(Literal::Float(s)) => allocator.text(s),
            Expr::Literal(Literal::SelfLit) => allocator.text("self"),
            Expr::Literal(Literal::Now) => allocator.text("now"),
            Expr::Literal(Literal::SampleRate) => allocator.text("samplerate"),
            Expr::Literal(Literal::PlaceHolder) => allocator.text("_"),
            Expr::Var(name) => allocator.text(name),
            Expr::QualifiedVar(path) => {
                let path_str = path.segments.iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                allocator.text(path_str)
            }
            Expr::Block(Some(e)) => allocator
                .text("{")
                .append(allocator.line())
                .append(pretty(e, allocator))
                .group()
                .nest(get_indent_size() as isize)
                .append(allocator.line())
                .append(allocator.text("}")),
            Expr::Block(None) => allocator.nil().braces(),
            Expr::Tuple(es) => {
                let docs = es
                    .into_iter()
                    .map(|e| pretty(e, allocator))
                    .collect::<Vec<_>>();
                allocator
                    .intersperse(docs, breakable_comma(allocator))
                    .nest(get_indent_size() as isize)
                    .group()
                    .parens()
            }
            Expr::Proj(e, idx) => pretty(e, allocator)
                .append(allocator.text(".").add(allocator.text(idx.to_string())))
                .group(),
            Expr::Apply(e1, e2) => {
                let doc1 = pretty(e1, allocator);
                let docs2 = e2
                    .into_iter()
                    .map(|e| pretty(e, allocator).group())
                    .collect::<Vec<_>>();
                doc1.append(
                    allocator
                        .intersperse(docs2, breakable_comma(allocator))
                        .nest(get_indent_size() as isize)
                        .group()
                        .parens(),
                )
            }
            Expr::RecordLiteral(fields) => {
                let docs = fields
                    .into_iter()
                    .map(|f| {
                        allocator
                            .text(f.name)
                            .append(allocator.text(" = "))
                            .append(pretty(f.expr, allocator))
                            .group()
                    })
                    .collect::<Vec<_>>();
                allocator
                    .intersperse(docs, breakable_comma(allocator))
                    .nest(get_indent_size() as isize)
                    .group()
                    .braces()
            }
            Expr::ImcompleteRecord(fields) => {
                let docs = fields
                    .into_iter()
                    .map(|f| {
                        allocator
                            .text(f.name)
                            .append(allocator.text(" = "))
                            .append(pretty(f.expr, allocator))
                            .group()
                    })
                    .collect::<Vec<_>>();
                allocator
                    .intersperse(docs, breakable_comma(allocator))
                    .append(allocator.text(",")) //trailing comma
                    .append(allocator.softline())
                    .append(allocator.text(".."))
                    .group()
                    .braces()
            }
            Expr::RecordUpdate(record, fields) => {
                let record_doc = pretty(record, allocator);
                let field_docs = fields
                    .into_iter()
                    .map(|f| {
                        allocator
                            .text(f.name)
                            .append(allocator.text(" = "))
                            .append(pretty(f.expr, allocator))
                            .group()
                    })
                    .collect::<Vec<_>>();

                record_doc
                    .append(allocator.text(" <- "))
                    .append(allocator.intersperse(field_docs, breakable_comma(allocator)))
                    .group()
                    .braces()
            }

            Expr::FieldAccess(expr_node_id, symbol) => {
                let expr_doc = pretty(expr_node_id, allocator);
                //concat without space
                expr_doc
                    // .append(allocator.softline_())
                    .add(allocator.text("."))
                    .add(allocator.text(symbol))
                    .group()
            }
            Expr::ArrayAccess(e, i) => pretty(e, allocator).append(pretty(i, allocator).brackets()),
            Expr::ArrayLiteral(items) => {
                let docs = items
                    .into_iter()
                    .map(|e| pretty(e, allocator))
                    .collect::<Vec<_>>();
                allocator
                    .intersperse(docs, breakable_comma(allocator))
                    .brackets()
            }
            Expr::Feed(s, e) => {
                //will not be used actually
                allocator
                    .text("feed")
                    .append(allocator.text(s).parens())
                    .append(pretty(e, allocator))
            }
            Expr::Let(pat, expr, then) => {
                let pat_doc = typedpattern::pretty(pat, allocator);
                let expr_doc = pretty(expr, allocator);
                let then_doc = then.map_or(allocator.nil(), |e| {
                    allocator.hardline().append(pretty(e, allocator))
                });

                allocator
                    .text("let ")
                    .append(pat_doc)
                    .append(allocator.text(" ="))
                    .append(allocator.softline())
                    .append(expr_doc.group())
                    .nest(get_indent_size() as isize)
                    .group()
                    .append(then_doc)
            }
            Expr::LetRec(id, body, then) => {
                let body_doc = pretty(body, allocator);
                let then_doc = then.map_or(allocator.nil(), |e| {
                    allocator.hardline().append(pretty(e, allocator))
                });
                allocator
                    .text("letrec ")
                    .append(allocator.text(id.id))
                    .append(allocator.text(" ="))
                    .append(allocator.softline())
                    .append(body_doc.group())
                    .nest(get_indent_size() as isize)
                    .group()
                    .append(then_doc)
            }
            Expr::If(cond, then, optelse) => {
                let then_doc = pretty(then, allocator);
                let else_doc = optelse.map_or(allocator.nil(), |e| pretty(e, allocator));
                allocator
                    .text("if")
                    .append(pretty(cond, allocator).group().enclose("(", ")"))
                    .append(allocator.softline())
                    .append(then_doc.group())
                    .append(allocator.softline())
                    .append(allocator.text("else"))
                    .append(allocator.softline())
                    .append(else_doc.group())
                    .group()
            }
            Expr::Lambda(params, ret_type, body) => {
                let params_doc = if params.is_empty() {
                    vec![allocator.space()]
                } else {
                    params
                        .iter()
                        .map(|p| {
                            let name = allocator.text(p.id);
                            let t = match p.ty.to_type() {
                                Type::Unknown => allocator.nil(),
                                _ => allocator.text(":").append(types::pretty(p.ty, allocator)),
                            };
                            name.append(t)
                        })
                        .collect::<Vec<_>>()
                };
                let ret_type_doc = ret_type.as_ref().map_or(allocator.nil(), |t| {
                    allocator.text("->").append(types::pretty(*t, allocator))
                });
                allocator
                    .intersperse(params_doc, ", ")
                    .enclose("|", "|")
                    .append(ret_type_doc)
                    .append(allocator.space())
                    .append(pretty(body, allocator).group())
            }
            Expr::Assign(lid, rhs) => pretty(lid, allocator)
                .append(allocator.text(" = "))
                .append(pretty(rhs, allocator)),
            Expr::Then(first, second) => {
                let first_doc = pretty(first, allocator);
                let second_doc = second.map_or(allocator.nil(), |e| pretty(e, allocator));

                first_doc.append(allocator.hardline()).append(second_doc)
            }
            Expr::Bracket(e) => allocator.text("`").append(pretty(e, allocator)).group(),
            Expr::Escape(e) => allocator.text("$").append(pretty(e, allocator)).group(),
            Expr::Error => allocator.text("error"),
            Expr::BinOp(lhs, (op, _opspan), rhs) => {
                let lhs_doc = pretty(lhs, allocator);
                let rhs_doc = pretty(rhs, allocator);
                if op == Op::Pipe {
                    //only pipe operator is prefer to be in the head of the line
                    lhs_doc
                        .append(allocator.line())
                        .append(
                            allocator
                                .text(op.to_string())
                                .append(allocator.space())
                                .append(rhs_doc)
                                .group(),
                        )
                        .group()
                } else {
                    // the other operators can not be in the head of the line preventing
                    // from confusing with Then Expression w/ Unary operator
                    lhs_doc
                        .append(
                            allocator
                                .space()
                                .append(allocator.text(op.to_string()))
                                .group(),
                        )
                        .append(allocator.line())
                        .append(rhs_doc.nest(get_indent_size() as isize))
                        .group()
                }
            }
            Expr::UniOp((op, _span), expr) => {
                let expr_doc = pretty(expr, allocator);
                allocator
                    .text(op.to_string())
                    .append(allocator.softline())
                    .append(expr_doc)
            }
            Expr::Paren(expr_node_id) => pretty(expr_node_id, allocator).parens().group(),
            Expr::MacroExpand(callee, args_e) => {
                let expr_doc = pretty(callee, allocator);
                let args = args_e
                    .into_iter()
                    .map(|e| pretty(e, allocator))
                    .collect::<Vec<_>>();
                expr_doc
                    .append(allocator.text("!"))
                    .append(allocator.intersperse(args, ", ").parens())
            }
        }
    }
}
mod statement {
    use super::*;
    use mimium_lang::ast::statement::Statement;
    pub(super) fn pretty<'a, D, A>(stmt: Statement, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone + Pretty<'a, D, A>,
        A: Clone,
    {
        match stmt {
            Statement::Let(pat, body) => {
                let pat_doc = typedpattern::pretty(pat, allocator);
                let body_doc = expr::pretty(body, allocator);
                allocator
                    .text("let ")
                    .append(pat_doc)
                    .append(allocator.text(" = "))
                    .append(body_doc.group())
            }
            Statement::LetRec(id, body) => {
                let body_doc = expr::pretty(body, allocator);
                allocator
                    .text("letrec ")
                    .append(allocator.text(id.id))
                    .append(allocator.text(" ="))
                    .append(allocator.softline())
                    .append(body_doc.group())
                    .nest(get_indent_size() as isize)
                    .group()
            }
            Statement::Assign(name, body) => {
                let name_doc = expr::pretty(name, allocator);
                let body_doc = expr::pretty(body, allocator);
                name_doc.append(allocator.text(" = ")).append(body_doc)
            }
            Statement::Single(expr) => expr::pretty(expr, allocator),
            Statement::Error => allocator.text("error"),
            Statement::DeclareStage(stage_kind) => allocator
                .text(format!("#stage({stage_kind})"))
                .append(allocator.softline()),
        }
    }
}
pub mod program {

    use super::*;
    pub fn pretty<'a, D, A>(prog: Program, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone + Pretty<'a, D, A>,
        A: Clone,
    {
        let stmt_docs = prog.statements.into_iter().map(|(stmt, _span)| match stmt {
            ProgramStatement::FnDefinition {
                visibility: _,
                name,
                args,
                return_type,
                body,
            } => {
                let args = args.0.iter().map(|a| {
                    let name = allocator.text(a.id);
                    let t = match a.ty.to_type() {
                        Type::Unknown => allocator.nil(),
                        _ => allocator.text(":").append(types::pretty(a.ty, allocator)),
                    };
                    name.append(t).align()
                });
                let args = allocator
                    .intersperse(args, breakable_comma(allocator))
                    .group()
                    .nest(get_indent_size() as isize)
                    .parens();

                allocator
                    .text("fn ")
                    .append(allocator.text(name))
                    .append(args)
                    .append(return_type.map_or(allocator.nil(), |rtype| {
                        allocator.text("->").append(types::pretty(rtype, allocator))
                    }))
                    .append(
                        allocator
                            .text("{")
                            .append(allocator.hardline())
                            .append(expr::pretty(body, allocator))
                            .nest(get_indent_size() as isize)
                            .append(allocator.hardline())
                            .append(allocator.text("}")),
                    )
            }
            ProgramStatement::GlobalStatement(stmt) => statement::pretty(stmt, allocator),
            ProgramStatement::Import(symbol) => allocator
                .text("include")
                .append(allocator.text(symbol).double_quotes().parens()),
            ProgramStatement::Comment(symbol) => {
                allocator.text("//").append(allocator.text(symbol))
            }
            ProgramStatement::DocComment(symbol) => {
                allocator.text("///").append(allocator.text(symbol))
            }
            ProgramStatement::Error => allocator.text("error"),
            ProgramStatement::StageDeclaration { stage } => allocator
                .text(format!("#stage({stage})"))
                .append(allocator.softline()),
            ProgramStatement::ModuleDefinition { visibility, name, body: _ } => {
                let vis_doc = if visibility == mimium_lang::ast::program::Visibility::Public {
                    allocator.text("pub ")
                } else {
                    allocator.nil()
                };
                vis_doc.append(allocator.text("mod ")).append(allocator.text(name.to_string())).append(allocator.text(" { /* ... */ }"))
            }
            ProgramStatement::UseStatement { visibility, path, target } => {
                use mimium_lang::ast::program::UseTarget;
                let vis_doc = if visibility == mimium_lang::ast::program::Visibility::Public {
                    allocator.text("pub ")
                } else {
                    allocator.nil()
                };
                let path_str = path.segments.iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                let target_str = match target {
                    UseTarget::Single => String::new(),
                    UseTarget::Multiple(names) => {
                        let names_str = names.iter()
                            .map(|s| s.to_string())
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!("::{{{}}}", names_str)
                    }
                    UseTarget::Wildcard => "::*".to_string(),
                };
                vis_doc.append(allocator.text("use ")).append(allocator.text(path_str)).append(allocator.text(target_str))
            }
        });
        allocator.intersperse(stmt_docs, "\n")
    }
}

pub fn pretty_print(
    src: &str,
    file_path: &Option<PathBuf>,
    width: usize,
) -> Result<String, Vec<Box<dyn ReportableError>>> {
    use mimium_lang::compiler::parser::parse_program;
    use mimium_lang::compiler::parser::parser_errors_to_reportable;
    let (prog, parse_errs) = parse_program(src, file_path.clone().unwrap_or_default());
    let errs = parser_errors_to_reportable(src, file_path.clone().unwrap_or_default(), parse_errs);
    if !errs.is_empty() {
        return Err(errs);
    }
    let allocator = Arena::new();
    let doc = program::pretty::<_, ()>(prog, &allocator);
    let mut w = Vec::new();
    doc.render(width, &mut w).unwrap();
    Ok(String::from_utf8(w).unwrap())
}
