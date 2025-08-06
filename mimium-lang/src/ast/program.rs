use super::resolve_include::resolve_include;
use super::statement::Statement;
use crate::ast::Expr;
use crate::ast::statement::into_then_expr;
use crate::interner::{ExprNodeId, Symbol, TypeNodeId};
use crate::pattern::TypedId;
use crate::types::{LabeledParam, LabeledParams, Type};
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
    Error, //ModuleDefinition(Symbol),
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
    program
        .statements
        .into_iter()
        .filter_map(|(stmt, span)| match stmt {
            ProgramStatement::FnDefinition {
                name,
                args,
                return_type,
                body,
            } => {
                let loc = Location::new(span, file_path);
                let argsty = args
                    .clone()
                    .iter()
                    .map(|typedid| {
                        LabeledParam::new(typedid.id, typedid.ty, typedid.default_value.is_some())
                    })
                    .collect();
                let fnty = Type::Function(
                    LabeledParams::new(argsty),
                    return_type.unwrap_or(Type::Unknown.into_id_with_location(loc.clone())),
                    None,
                )
                .into_id_with_location(loc.clone());
                Some(vec![(
                    Statement::LetRec(
                        TypedId::new(name, fnty),
                        Expr::Lambda(args, return_type, body).into_id(loc.clone()),
                    ),
                    loc,
                )])
            }
            ProgramStatement::GlobalStatement(statement) => {
                Some(vec![(statement, Location::new(span, file_path))])
            }
            ProgramStatement::Comment(_) | ProgramStatement::DocComment(_) => None,
            ProgramStatement::Import(filename) => {
                let (imported_program, mut new_errs) =
                    resolve_include(file_path.as_str(), filename.as_str(), span.clone());
                errs.append(&mut new_errs);
                let res = stmts_from_program(imported_program, file_path, errs);
                Some(res)
            }
            ProgramStatement::Error => {
                Some(vec![(Statement::Error, Location::new(span, file_path))])
            }
        })
        .flatten()
        .collect()
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
