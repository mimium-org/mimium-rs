use mimium_lang::{
    ast::{
        program::{Program, ProgramStatement},
        Expr, Literal,
    },
    interner::ExprNodeId,
    types::Type,
};
use pretty::{DocAllocator, DocBuilder, Pretty};

use crate::{GlobalConfig, GLOBAL_DATA};

fn get_indent_size() -> usize {
    if let Ok(gdata) = GLOBAL_DATA.try_lock() {
        gdata.indent_size
    } else {
        GlobalConfig::default().indent_size
    }
}
mod types {
    use mimium_lang::{
        interner::TypeNodeId,
        types::{LabeledParam, PType},
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
                allocator.intersperse(docs, ", ").parens()
            }
            Type::Record(items) => {
                let docs = items
                    .into_iter()
                    .map(|(name, ty)| {
                        allocator
                            .text(name)
                            .append(allocator.text(": "))
                            .append(types::pretty(ty, allocator))
                    })
                    .collect::<Vec<_>>();
                allocator.intersperse(docs, ", ").braces()
            }
            Type::Function(params, ret, _state) => {
                let param_docs = params
                    .get_as_slice()
                    .iter()
                    .map(|LabeledParam { label: _, ty }| types::pretty(*ty, allocator))
                    .collect::<Vec<_>>();
                let ret_doc = types::pretty(ret, allocator);
                allocator
                    .intersperse(param_docs, ", ")
                    .parens()
                    .append(allocator.text(" -> "))
                    .append(ret_doc)
            }
            Type::Code(ty) => allocator.text("`").append(types::pretty(ty, allocator)),
            Type::TypeScheme(_) => unreachable!(),
            Type::Intermediate(_) => unreachable!(),
            Type::Ref(_) => unreachable!(),
            Type::Failure => unreachable!(),
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
            Pattern::Tuple(pats) => {
                let docs = pats
                    .into_iter()
                    .map(|p| pretty(p, allocator))
                    .collect::<Vec<_>>();
                allocator.intersperse(docs, ", ").parens()
            }
            Pattern::Record(items) => {
                let docs = items.into_iter().map(|(name, pattern)| {
                    allocator
                        .text(name)
                        .append(allocator.text(" = "))
                        .append(pretty(pattern, allocator))
                });
                allocator.intersperse(docs, ", ").braces()
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
            Expr::Block(Some(e)) => allocator
                .text("{")
                .append(allocator.line())
                .append(pretty(e, allocator))
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
                    .intersperse(
                        docs,
                        allocator.text(",").append(allocator.softline()).into_doc(),
                    )
                    .parens()
            }
            Expr::Proj(e, idx) => pretty(e, allocator)
                .append(allocator.text(".").append(allocator.text(idx.to_string()))),
            Expr::Apply(e1, e2) => {
                let doc1 = pretty(e1, allocator);
                let docs2 = e2
                    .into_iter()
                    .map(|e| pretty(e, allocator).group())
                    .collect::<Vec<_>>();
                doc1.append(allocator.intersperse(docs2, ", ").group().parens())
            }
            Expr::RecordLiteral(fields) => {
                let docs = fields
                    .into_iter()
                    .map(|f| {
                        allocator
                            .text(f.name)
                            .append(allocator.text(" = "))
                            .append(pretty(f.expr, allocator))
                    })
                    .collect::<Vec<_>>();
                allocator.intersperse(docs, ", ").braces()
            }
            Expr::ArrayAccess(e, i) => pretty(e, allocator).append(pretty(i, allocator).brackets()),
            Expr::ArrayLiteral(items) => {
                let docs = items
                    .into_iter()
                    .map(|e| pretty(e, allocator))
                    .collect::<Vec<_>>();
                allocator.intersperse(docs, ", ").brackets()
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
                    .append(body_doc)
                    .append(then_doc)
            }
            Expr::If(cond, then, optelse) => {
                let then_doc = pretty(then, allocator);
                let else_doc = optelse.map_or(allocator.nil(), |e| pretty(e, allocator));
                allocator
                    .text("if")
                    .append(pretty(cond, allocator).enclose(" (", ")").group())
                    .append(allocator.softline())
                    .append(then_doc.group())
                    .append(allocator.softline())
                    .append(allocator.text(" else "))
                    .append(allocator.softline())
                    .append(else_doc.group())
            }
            Expr::Lambda(params, ret_type, body) => {
                let params_doc = params
                    .iter()
                    .map(|p| {
                        let name = allocator.text(p.id);
                        let t = match p.ty.to_type() {
                            Type::Unknown => allocator.nil(),
                            _ => allocator.text(":").append(types::pretty(p.ty, allocator)),
                        };
                        name.append(t)
                    })
                    .collect::<Vec<_>>();
                let ret_type_doc = ret_type.as_ref().map_or(allocator.nil(), |t| {
                    allocator.text("->").append(types::pretty(*t, allocator))
                });
                allocator
                    .intersperse(params_doc, ", ")
                    .enclose("|", "|")
                    .append(ret_type_doc)
                    .append(pretty(body, allocator))
            }
            Expr::Assign(lid, rhs) => pretty(lid, allocator)
                .append(allocator.text(" = "))
                .append(pretty(rhs, allocator)),
            Expr::Then(first, second) => {
                let first_doc = pretty(first, allocator);
                let second_doc = second.map_or(allocator.nil(), |e| pretty(e, allocator));

                first_doc.append(allocator.hardline()).append(second_doc)
            }
            Expr::Bracket(e) => allocator.text("`").append(pretty(e, allocator)),
            Expr::Escape(e) => allocator.text("$").append(pretty(e, allocator)),
            Expr::Error => allocator.text("error"),
            Expr::BinOp(lhs, (op, _opspan), rhs) => {
                let lhs_doc = pretty(lhs, allocator);
                let rhs_doc = pretty(rhs, allocator);
                lhs_doc
                    .append(allocator.line())
                    .append(allocator.text(op.to_string()))
                    .append(allocator.space())
                    .append(rhs_doc)
                    .group()
            }
            Expr::UniOp((op, _span), expr) => {
                let expr_doc = pretty(expr, allocator);
                allocator
                    .text(op.to_string())
                    .append(allocator.softline())
                    .append(expr_doc)
            }
            Expr::Paren(expr_node_id) => pretty(expr_node_id, allocator).parens(),

            Expr::FieldAccess(expr_node_id, symbol) => {
                let expr_doc = pretty(expr_node_id, allocator);
                expr_doc
                    .append(allocator.softline())
                    .append(allocator.text("."))
                    .append(allocator.text(symbol))
            }
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
                    .append(allocator.text(" ="))
                    .append(allocator.softline())
                    .append(body_doc.group())
                    .nest(get_indent_size() as isize)
                    .group()
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
                name,
                args,
                return_type,
                body,
            } => {
                let args = args.iter().map(|a| {
                    let name = allocator.text(a.id);
                    let t = match a.ty.to_type() {
                        Type::Unknown => allocator.nil(),
                        _ => allocator.text(":").append(types::pretty(a.ty, allocator)),
                    };
                    name.append(t)
                });
                let args = allocator.intersperse(args, ", ").parens();

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
                .text("import")
                .append(allocator.text(symbol).double_quotes().parens()),
            ProgramStatement::Comment(symbol) => {
                allocator.text("//").append(allocator.text(symbol))
            }
            ProgramStatement::DocComment(symbol) => {
                allocator.text("///").append(allocator.text(symbol))
            }
            ProgramStatement::Error => allocator.text("error"),
        });
        allocator.intersperse(stmt_docs, "\n")
    }
}
