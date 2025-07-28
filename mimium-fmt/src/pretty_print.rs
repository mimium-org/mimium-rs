
use mimium_lang::{
    ast::{Expr, ExprNodeId, Literal, RecordField},
    interner::{with_session_globals, Symbol},
    pattern::{Pattern, TypedId, TypedPattern},
};
use pretty::{DocAllocator, DocBuilder};

pub trait ToDoc {
    fn to_doc<'a, A>(&self, allocator: &'a A) -> DocBuilder<'a, A>
    where
        A: DocAllocator<'a, Annotation = ()>;
}

fn intersperse<'a, A>(
    docs: impl IntoIterator<Item = DocBuilder<'a, A>>,
    separator: DocBuilder<'a, A>,
    allocator: &'a A,
) -> DocBuilder<'a, A>
where
    A: DocAllocator<'a, Annotation = ()>,
{
    allocator.intersperse(docs, separator)
}

impl ToDoc for Symbol {
    fn to_doc<'a, A>(&self, allocator: &'a A) -> DocBuilder<'a, A>
    where
        A: DocAllocator<'a, Annotation = ()>,
    {
        allocator.text(self.as_str())
    }
}

impl ToDoc for TypedId {
    fn to_doc<'a, A>(&self, allocator: &'a A) -> DocBuilder<'a, A>
    where
        A: DocAllocator<'a, Annotation = ()>,
    {
        // For now, we don't print types.
        self.id.to_doc(allocator)
    }
}

impl ToDoc for Pattern {
    fn to_doc<'a, A>(&self, allocator: &'a A) -> DocBuilder<'a, A>
    where
        A: DocAllocator<'a, Annotation = ()>,
    {
        match self {
            Pattern::Single(s) => s.to_doc(allocator),
            Pattern::Tuple(pats) => allocator
                .intersperse(
                    pats.iter().map(|p| p.to_doc(allocator)),
                    allocator.text(",").append(allocator.space()),
                )
                .parens(),
            Pattern::Record(fields) => allocator
                .intersperse(
                    fields.iter().map(|(name, pat)| {
                        name.to_doc(allocator)
                            .append(allocator.text(": "))
                            .append(pat.to_doc(allocator))
                    }),
                    allocator.text(",").append(allocator.space()),
                )
                .braces()
                .nest(2),
            Pattern::Error => allocator.text("<error>"),
        }
    }
}

impl ToDoc for TypedPattern {
    fn to_doc<'a, A>(&self, allocator: &'a A) -> DocBuilder<'a, A>
    where
        A: DocAllocator<'a, Annotation = ()>,
    {
        // For now, we don't print types.
        self.pat.to_doc(allocator)
    }
}

impl ToDoc for Literal {
    fn to_doc<'a, A>(&self, allocator: &'a A) -> DocBuilder<'a, A>
    where
        A: DocAllocator<'a, Annotation = ()>,
    {
        match self {
            Literal::String(s) => allocator.text(format!(r#""{}""#, s)),
            Literal::Int(i) => allocator.text(i.to_string()),
            Literal::Float(f) => allocator.text(f.to_string()),
            Literal::SelfLit => allocator.text("self"),
            Literal::Now => allocator.text("now"),
            Literal::SampleRate => allocator.text("samplerate"),
            Literal::PlaceHolder => allocator.text("_"),
        }
    }
}

impl ToDoc for RecordField {
    fn to_doc<'a, A>(&self, allocator: &'a A) -> DocBuilder<'a, A>
    where
        A: DocAllocator<'a, Annotation = ()>,
    {
        self.name
            .to_doc(allocator)
            .append(allocator.text(": "))
            .append(self.expr.to_doc(allocator))
    }
}

impl ToDoc for ExprNodeId {
    fn to_doc<'a, A>(&self, allocator: &'a A) -> DocBuilder<'a, A>
    where
        A: DocAllocator<'a, Annotation = ()>,
    {
        with_session_globals(|g| g.get_expr(*self).to_doc(allocator))
    }
}

impl ToDoc for Expr {
    fn to_doc<'a, A>(&self, allocator: &'a A) -> DocBuilder<'a, A>
    where
        A: DocAllocator<'a, Annotation = ()>,
    {
        match self {
            Expr::Literal(l) => l.to_doc(allocator),
            Expr::Var(s) => s.to_doc(allocator),
            Expr::Block(Some(expr)) => allocator
                .line()
                .append(expr.to_doc(allocator))
                .nest(2)
                .braces(),
            Expr::Block(None) => allocator.text("{}"),
            Expr::Tuple(exprs) => intersperse(
                exprs.iter().map(|e| e.to_doc(allocator)),
                allocator.text(",").append(allocator.space()),
                allocator,
            )
            .parens(),
            Expr::Proj(expr, idx) => expr.to_doc(allocator).append(allocator.text(format!(".{}", idx))),
            Expr::ArrayAccess(array, index) => array
                .to_doc(allocator)
                .append(index.to_doc(allocator).brackets()),
            Expr::ArrayLiteral(exprs) => intersperse(
                exprs.iter().map(|e| e.to_doc(allocator)),
                allocator.text(",").append(allocator.space()),
                allocator,
            )
            .brackets(),
            Expr::RecordLiteral(fields) => intersperse(
                fields.iter().map(|f| f.to_doc(allocator)),
                allocator.text(",").append(allocator.space()),
                allocator,
            )
            .braces()
            .nest(2),
            Expr::FieldAccess(expr, field) => expr.to_doc(allocator).append(allocator.text(".")).append(field.to_doc(allocator)),
            Expr::Apply(func, args) => func.to_doc(allocator).append(
                intersperse(
                    args.iter().map(|a| a.to_doc(allocator)),
                    allocator.text(",").append(allocator.space()),
                    allocator,
                )
                .parens(),
            ),
            Expr::PipeApply(lhs, rhs) => lhs
                .to_doc(allocator)
                .append(allocator.space())
                .append(allocator.text("|>"))
                .append(allocator.space())
                .append(rhs.to_doc(allocator)),
            Expr::Lambda(args, _ret_type, body) => allocator
                .text("|")
                .append(intersperse(
                    args.iter().map(|a| a.to_doc(allocator)),
                    allocator.text(", "),
                    allocator,
                ))
                .append(allocator.text("|"))
                .append(allocator.space())
                .append(body.to_doc(allocator)),
            Expr::Assign(lhs, rhs) => lhs
                .to_doc(allocator)
                .append(allocator.space())
                .append(allocator.text("="))
                .append(allocator.space())
                .append(rhs.to_doc(allocator)),
            Expr::Then(first, Some(second)) => first
                .to_doc(allocator)
                .append(allocator.text(";"))
                .append(allocator.line())
                .append(second.to_doc(allocator)),
            Expr::Then(first, None) => first.to_doc(allocator),
            Expr::Feed(s, expr) => s
                .to_doc(allocator)
                .append(allocator.space())
                .append(allocator.text("<-"))
                .append(allocator.space())
                .append(expr.to_doc(allocator)),
            Expr::Let(pat, expr, Some(then)) => allocator
                .text("let ")
                .append(pat.to_doc(allocator))
                .append(allocator.space())
                .append(allocator.text("="))
                .append(allocator.space())
                .append(expr.to_doc(allocator))
                .append(allocator.text(";"))
                .append(allocator.line())
                .append(then.to_doc(allocator)),
            Expr::Let(pat, expr, None) => allocator
                .text("let ")
                .append(pat.to_doc(allocator))
                .append(allocator.space())
                .append(allocator.text("="))
                .append(allocator.space())
                .append(expr.to_doc(allocator)),
            Expr::LetRec(id, expr, Some(then)) => allocator
                .text("let rec ")
                .append(id.to_doc(allocator))
                .append(allocator.space())
                .append(allocator.text("="))
                .append(allocator.space())
                .append(expr.to_doc(allocator))
                .append(allocator.text(";"))
                .append(allocator.line())
                .append(then.to_doc(allocator)),
            Expr::LetRec(id, expr, None) => allocator
                .text("let rec ")
                .append(id.to_doc(allocator))
                .append(allocator.space())
                .append(allocator.text("="))
                .append(allocator.space())
                .append(expr.to_doc(allocator)),
            Expr::FnDef(name, args, _ret_type, body, Some(then)) => allocator
                .text("fn ")
                .append(name.to_doc(allocator))
                .append(
                    intersperse(
                        args.iter().map(|a| a.to_doc(allocator)),
                        allocator.text(", "),
                        allocator,
                    )
                    .parens(),
                )
                .append(allocator.space())
                .append(body.to_doc(allocator))
                .append(allocator.text(";"))
                .append(allocator.line())
                .append(then.to_doc(allocator)),
            Expr::FnDef(name, args, _ret_type, body, None) => allocator
                .text("fn ")
                .append(name.to_doc(allocator))
                .append(
                    intersperse(
                        args.iter().map(|a| a.to_doc(allocator)),
                        allocator.text(", "),
                        allocator,
                    )
                    .parens(),
                )
                .append(allocator.space())
                .append(body.to_doc(allocator)),
            Expr::If(cond, then_branch, Some(else_branch)) => allocator
                .text("if ")
                .append(cond.to_doc(allocator))
                .append(allocator.space())
                .append(then_branch.to_doc(allocator))
                .append(allocator.space())
                .append(allocator.text("else "))
                .append(else_branch.to_doc(allocator)),
            Expr::If(cond, then_branch, None) => allocator
                .text("if ")
                .append(cond.to_doc(allocator))
                .append(allocator.space())
                .append(then_branch.to_doc(allocator)),
            Expr::Bracket(expr) => allocator.text("`").append(expr.to_doc(allocator)),
            Expr::Escape(expr) => allocator.text("$").append(expr.to_doc(allocator)),
            Expr::Error => allocator.text("<error>"),
        }
    }
}
