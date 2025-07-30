use super::resolve_include::resolve_include;
use super::statement::Statement;
use crate::ast::Expr;
use crate::ast::statement::into_then_expr;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::TypedId;
use crate::utils::error::ReportableError;
use crate::utils::metadata::{Location, Span};

#[derive(Clone, Debug, PartialEq)]
pub enum ProgramStatement {
    FnDefinition {
        name: Symbol,
        args: Vec<TypedId>,
        return_type: Option<TypeNodeId>,
        body: ExprNodeId,
    },
    GlobalStatement(Statement),
    Import(Symbol),
    Comment(Symbol),
    DocComment(Symbol),
    //ModuleDefinition(Symbol),
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Program {
    pub statements: Vec<(ProgramStatement, Span)>,
}
fn stmts_from_program(
    program: Program,
    file_path: Symbol,
    errs: &mut Vec<Box<dyn ReportableError>>,
) -> Vec<(Statement, Location)> {
    let res = program
        .statements
        .into_iter()
        .filter_map(|(stmt, span)| match stmt {
            ProgramStatement::FnDefinition {
                name,
                args,
                return_type,
                body,
            } => Some(vec![(
                Statement::LetRec(
                    TypedId::new(name),
                    Expr::Lambda(args, return_type, body).into_id_without_span(),
                ),
                Location::new(span, "".to_symbol()),
            )]),
            ProgramStatement::GlobalStatement(statement) => {
                Some(vec![(statement, Location::new(span, "".to_symbol()))])
            }
            ProgramStatement::Comment(_) | ProgramStatement::DocComment(_) => None,
            ProgramStatement::Import(filename) => {
                let (imported_program, mut new_errs) =
                    resolve_include(file_path.as_str(), filename.as_str(), span.clone());
                errs.append(&mut new_errs);
                let res = stmts_from_program(imported_program, file_path.clone(), errs);
                Some(res)
            }
        })
        .flatten()
        .collect();
    res
}
pub(crate) fn expr_from_program(
    program: Program,
    file_path: Symbol,
) -> (ExprNodeId, Vec<Box<dyn ReportableError>>) {
    let mut errs = vec![];
    let stmts = stmts_from_program(program, file_path, &mut errs);

    let res = into_then_expr(stmts.as_slice()).unwrap_or(Expr::Error.into_id_without_span());

    (res, errs)
}
