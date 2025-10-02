use std::path::PathBuf;

use super::resolve_include::resolve_include;
use super::statement::Statement;
use crate::ast::Expr;
use crate::ast::statement::into_then_expr;
use crate::interner::{ExprNodeId, Symbol, TypeNodeId};
use crate::pattern::TypedId;
use crate::types::{RecordTypeField, Type};
use crate::utils::error::ReportableError;
use crate::utils::metadata::{Location, Span};

use super::StageKind;

#[derive(Clone, Debug, PartialEq)]
pub enum ProgramStatement {
    FnDefinition {
        name: Symbol,
        args: (Vec<TypedId>, Location),
        return_type: Option<TypeNodeId>,
        body: ExprNodeId,
    },
    StageDeclaration {
        stage: StageKind,
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
    file_path: PathBuf,
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
                let loc = Location::new(span, file_path.clone());
                let argloc = args.1.clone();
                let argsty = args
                    .clone()
                    .0
                    .into_iter()
                    .map(RecordTypeField::from)
                    .collect::<Vec<_>>();
                let fnty = Type::Function {
                    arg: Type::Record(argsty).into_id_with_location(argloc),
                    ret: return_type.unwrap_or(Type::Unknown.into_id_with_location(loc.clone())),
                }
                .into_id_with_location(loc.clone());
                Some(vec![(
                    Statement::LetRec(
                        TypedId::new(name, fnty),
                        Expr::Lambda(args.0, return_type, body).into_id(loc.clone()),
                    ),
                    loc,
                )])
            }
            ProgramStatement::GlobalStatement(statement) => {
                Some(vec![(statement, Location::new(span, file_path.clone()))])
            }
            ProgramStatement::Comment(_) | ProgramStatement::DocComment(_) => None,
            ProgramStatement::Import(filename) => {
                let (imported_program, mut new_errs) =
                    resolve_include(file_path.to_str().unwrap(), filename.as_str(), span.clone());
                errs.append(&mut new_errs);
                let res = stmts_from_program(imported_program, file_path.clone(), errs);
                Some(res)
            }
            ProgramStatement::StageDeclaration { stage } => Some(vec![(
                Statement::DeclareStage(stage),
                Location::new(span, file_path.clone()),
            )]),
            ProgramStatement::Error => Some(vec![(
                Statement::Error,
                Location::new(span, file_path.clone()),
            )]),
        })
        .flatten()
        .collect()
}
pub(crate) fn expr_from_program(
    program: Program,
    file_path: PathBuf,
) -> (ExprNodeId, Vec<Box<dyn ReportableError>>) {
    let mut errs = vec![];
    let stmts = stmts_from_program(program, file_path.clone(), &mut errs);

    let res = into_then_expr(stmts.as_slice()).unwrap_or(Expr::Error.into_id_without_span());

    (res, errs)
}
