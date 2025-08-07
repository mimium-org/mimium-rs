use crate::ast::{Expr, Literal, RecordField};
use crate::compiler::intrinsics;
use crate::interner::{ExprKey, ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedPattern};
use crate::types::{
    IntermediateId, LabeledParam, LabeledParams, PType, RecordTypeField, Type, TypeSchemeId,
    TypeVar,
};
use crate::utils::metadata::Location;
use crate::utils::{environment::Environment, error::ReportableError};
use crate::{function, integer, numeric, unit};
use itertools::Itertools;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

mod unification;
use unification::{Error as UnificationError, Relation, unify_types};

#[derive(Clone, Debug)]
pub enum Error {
    TypeMismatch {
        left: (TypeNodeId, Location),
        right: (TypeNodeId, Location),
    },
    LengthMismatch {
        left: (usize, Location),
        right: (usize, Location),
    },
    PatternMismatch((TypeNodeId, Location), (Pattern, Location)),
    NonFunctionForLetRec(TypeNodeId, Location),
    NonFunctionForApply(TypeNodeId, Location),
    CircularType(Location, Location),
    IndexOutOfRange {
        len: u16,
        idx: u16,
        loc: Location,
    },
    IndexForNonTuple(Location, TypeNodeId),
    FieldForNonRecord(Location, TypeNodeId),
    FieldNotExist {
        field: Symbol,
        loc: Location,
        et: TypeNodeId,
    },
    FieldNotExistInParams {
        field: Symbol,
        loc: Location,
        params: LabeledParams,
    },
    DuplicateKeyInRecord {
        key: Vec<Symbol>,
        loc: Location,
    },
    DuplicateKeyInParams(Vec<(Symbol, Location)>),
    // The error of records, which contains both subtypes and supertypes.
    IncompatibleKeyInRecord {
        left: (Vec<(Symbol, TypeNodeId)>, Location),
        right: (Vec<(Symbol, TypeNodeId)>, Location),
    },
    VariableNotFound(Symbol, Location),
    NonPrimitiveInFeed(Location),
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Type Inference Error")
    }
}

impl std::error::Error for Error {}
impl ReportableError for Error {
    fn get_message(&self) -> String {
        match self {
            Error::TypeMismatch { .. } => format!("Type mismatch"),
            Error::PatternMismatch(..) => format!("Pattern mismatch"),
            Error::LengthMismatch { .. } => format!("Length of the elements are different"),
            Error::NonFunctionForLetRec(_, _) => format!("`letrec` can take only function type."),
            Error::NonFunctionForApply(_, _) => {
                format!("This is not applicable because it is not a function type.")
            }
            Error::CircularType(_, _) => format!("Circular loop of type definition detected."),
            Error::IndexOutOfRange { len, idx, .. } => {
                format!("Length of tuple elements is {len} but index was {idx}")
            }
            Error::IndexForNonTuple(_, _) => {
                format!("Index access for non-tuple variable.")
            }
            Error::VariableNotFound(symbol, _) => {
                format!("Variable {symbol} not found in this scope")
            }
            Error::NonPrimitiveInFeed(_) => {
                format!("Function that uses `self` cannot return function type.")
            }
            Error::DuplicateKeyInParams { .. } => {
                format!("Duplicate keys found in parameter list")
            }
            Error::DuplicateKeyInRecord { .. } => {
                format!("Duplicate keys found in record type")
            }
            Error::FieldForNonRecord { .. } => {
                format!("Field access for non-record variable.")
            }
            Error::FieldNotExist { field, .. } => {
                format!("Field `{field}` does not exist in the record type")
            }
            Error::IncompatibleKeyInRecord { .. } => {
                format!("Record type has incompatible keys.",)
            }
            Error::FieldNotExistInParams { .. } => {
                format!("Field contains non-existing keys in the parameter list")
            }
        }
    }
    fn get_labels(&self) -> Vec<(Location, String)> {
        match self {
            Error::TypeMismatch {
                left: (lty, locl),
                right: (rty, locr),
            } => vec![
                (locl.clone(), lty.to_type().to_string_for_error()),
                (locr.clone(), rty.to_type().to_string_for_error()),
            ],
            Error::PatternMismatch((ty, loct), (pat, locp)) => vec![
                (loct.clone(), ty.to_type().to_string_for_error()),
                (locp.clone(), pat.to_string()),
            ],
            Error::LengthMismatch {
                left: (l, locl),
                right: (r, locr),
            } => vec![
                (locl.clone(), format!("The length is {l}")),
                (locr.clone(), format!("but the length for here is {r}")),
            ],
            Error::NonFunctionForLetRec(ty, loc) => {
                vec![(loc.clone(), ty.to_type().to_string_for_error())]
            }
            Error::NonFunctionForApply(ty, loc) => {
                vec![(loc.clone(), ty.to_type().to_string_for_error())]
            }
            Error::CircularType(loc1, loc2) => vec![
                (loc1.clone(), format!("Circular type happens here")),
                (loc2.clone(), format!("and here")),
            ],
            Error::IndexOutOfRange { loc, len, .. } => {
                vec![(loc.clone(), format!("Length for this tuple is {len}"))]
            }
            Error::IndexForNonTuple(loc, ty) => {
                vec![(
                    loc.clone(),
                    format!(
                        "This is not tuple type but {}",
                        ty.to_type().to_string_for_error()
                    ),
                )]
            }
            Error::VariableNotFound(symbol, loc) => {
                vec![(loc.clone(), format!("{symbol} is not defined"))]
            }
            Error::NonPrimitiveInFeed(loc) => {
                vec![(loc.clone(), format!("This cannot be function type."))]
            }
            Error::DuplicateKeyInRecord { key, loc } => {
                vec![(
                    loc.clone(),
                    format!(
                        "Duplicate keys `{}` found in record type",
                        key.iter()
                            .map(|s| s.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                )]
            }
            Error::DuplicateKeyInParams(keys) => keys
                .iter()
                .map(|(key, loc)| {
                    (
                        loc.clone(),
                        format!("Duplicate key \"{key}\" found in parameter list"),
                    )
                })
                .collect(),
            Error::FieldForNonRecord(location, ty) => {
                vec![(
                    location.clone(),
                    format!(
                        "Field access for non-record type {}.",
                        ty.to_type().to_string_for_error()
                    ),
                )]
            }
            Error::FieldNotExist { field, loc, et } => vec![(
                loc.clone(),
                format!(
                    "Field `{}` does not exist in the type {}",
                    field,
                    et.to_type().to_string_for_error()
                ),
            )],
            Error::IncompatibleKeyInRecord {
                left: (left, lloc),
                right: (right, rloc),
            } => {
                vec![
                    (
                        lloc.clone(),
                        format!(
                            "the record here contains{}",
                            left.iter()
                                .map(|(key, ty)| format!(
                                    " \"{key}\":{}",
                                    ty.to_type().to_string_for_error()
                                ))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                    ),
                    (
                        rloc.clone(),
                        format!(
                            "but the record here contains{}",
                            right
                                .iter()
                                .map(|(key, ty)| format!(
                                    " \"{key}\":{}",
                                    ty.to_type().to_string_for_error()
                                ))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                    ),
                ]
            }
            Error::FieldNotExistInParams { field, loc, params } => vec![(
                loc.clone(),
                format!(
                    "Field \"{}\" does not exist in the parameter list{}",
                    field,
                    params
                        .get_as_slice()
                        .iter()
                        .map(|p| {
                            format!(
                                " \"{}\":{}",
                                p.label.map_or_else(|| "_".to_string(), |s| s.to_string()),
                                p.ty.to_type().to_string_for_error()
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            )],
        }
    }
}

#[derive(Clone, Debug)]
pub struct InferContext {
    interm_idx: IntermediateId,
    typescheme_idx: TypeSchemeId,
    level: u64,
    instantiated_map: BTreeMap<TypeSchemeId, TypeNodeId>, //from type scheme to typevar
    generalize_map: BTreeMap<IntermediateId, TypeSchemeId>,
    result_memo: BTreeMap<ExprKey, TypeNodeId>,
    file_path: Symbol,
    pub env: Environment<TypeNodeId>,
    pub errors: Vec<Error>,
}
impl InferContext {
    fn new(builtins: &[(Symbol, TypeNodeId)], file_path: Symbol) -> Self {
        let mut res = Self {
            interm_idx: Default::default(),
            typescheme_idx: Default::default(),
            level: Default::default(),
            instantiated_map: Default::default(),
            generalize_map: Default::default(),
            result_memo: Default::default(),
            file_path,
            env: Environment::<TypeNodeId>::default(),
            errors: Default::default(),
        };
        res.env.extend();
        res.env.add_bind(&Self::intrinsic_types());
        res.env.add_bind(builtins);
        res
    }
}
impl InferContext {
    fn intrinsic_types() -> Vec<(Symbol, TypeNodeId)> {
        let binop_ty = function!(vec![numeric!(), numeric!()], numeric!());
        let binop_names = [
            intrinsics::ADD,
            intrinsics::SUB,
            intrinsics::MULT,
            intrinsics::DIV,
            intrinsics::MODULO,
            intrinsics::POW,
            intrinsics::GT,
            intrinsics::LT,
            intrinsics::GE,
            intrinsics::LE,
            intrinsics::EQ,
            intrinsics::NE,
            intrinsics::AND,
            intrinsics::OR,
        ];
        let uniop_ty = function!(vec![numeric!()], numeric!());
        let uniop_names = [
            intrinsics::NEG,
            intrinsics::MEM,
            intrinsics::SIN,
            intrinsics::COS,
            intrinsics::ABS,
            intrinsics::LOG,
            intrinsics::SQRT,
        ];

        let binds = binop_names.map(|n| (n.to_symbol(), binop_ty));
        let unibinds = uniop_names.map(|n| (n.to_symbol(), uniop_ty));
        [
            (
                intrinsics::DELAY.to_symbol(),
                function!(vec![numeric!(), numeric!(), numeric!()], numeric!()),
            ),
            (
                intrinsics::TOFLOAT.to_symbol(),
                function!(vec![integer!()], numeric!()),
            ),
        ]
        .into_iter()
        .chain(binds)
        .chain(unibinds)
        .collect()
    }
    fn unwrap_result(&mut self, res: Result<TypeNodeId, Vec<Error>>) -> TypeNodeId {
        match res {
            Ok(t) => t,
            Err(mut e) => {
                let loc = &e[0].get_labels()[0].0; //todo
                self.errors.append(&mut e);
                Type::Failure.into_id_with_location(loc.clone())
            }
        }
    }
    fn gen_intermediate_type(&mut self) -> TypeNodeId {
        let res = Type::Intermediate(Rc::new(RefCell::new(TypeVar::new(
            self.interm_idx,
            self.level,
        ))))
        .into_id();
        self.interm_idx.0 += 1;
        res
    }
    fn get_typescheme(&mut self, tvid: IntermediateId) -> TypeNodeId {
        self.generalize_map.get(&tvid).cloned().map_or_else(
            || self.gen_typescheme(),
            |id| Type::TypeScheme(id).into_id(),
        )
    }
    fn gen_typescheme(&mut self) -> TypeNodeId {
        let res = Type::TypeScheme(self.typescheme_idx).into_id();
        self.typescheme_idx.0 += 1;
        res
    }

    fn gen_intermediate_type_with_location(&mut self, loc: Location) -> TypeNodeId {
        let res = Type::Intermediate(Rc::new(RefCell::new(TypeVar::new(
            self.interm_idx,
            self.level,
        ))))
        .into_id_with_location(loc);
        self.interm_idx.0 += 1;
        res
    }
    fn convert_unknown_to_intermediate(&mut self, t: TypeNodeId) -> TypeNodeId {
        match t.to_type() {
            Type::Unknown => self.gen_intermediate_type(),
            _ => t.apply_fn(|t| self.convert_unknown_to_intermediate(t)),
        }
    }
    fn convert_unify_error(&self, e: UnificationError) -> Error {
        let gen_loc = |span| Location::new(span, self.file_path);
        match e {
            UnificationError::TypeMismatch { left, right } => Error::TypeMismatch {
                left: (left, gen_loc(left.to_span())),
                right: (right, gen_loc(right.to_span())),
            },
            UnificationError::LengthMismatch {
                left: (left, lspan),
                right: (right, rspan),
            } => Error::LengthMismatch {
                left: (left.len(), gen_loc(lspan)),
                right: (right.len(), gen_loc(rspan)),
            },
            UnificationError::CircularType { left, right } => {
                Error::CircularType(gen_loc(left), gen_loc(right))
            }
            UnificationError::ImcompatibleRecords {
                left: (left, lspan),
                right: (right, rspan),
            } => Error::IncompatibleKeyInRecord {
                left: (left, gen_loc(lspan)),
                right: (right, gen_loc(rspan)),
            },
        }
    }
    fn unify_types(&self, t1: TypeNodeId, t2: TypeNodeId) -> Result<Relation, Vec<Error>> {
        unify_types(t1, t2)
            .map_err(|e| e.into_iter().map(|e| self.convert_unify_error(e)).collect())
    }
    // helper function
    fn merge_rel_result(
        &self,
        rel1: Result<Relation, Vec<Error>>,
        rel2: Result<Relation, Vec<Error>>,
        t1: TypeNodeId,
        t2: TypeNodeId,
    ) -> Result<(), Vec<Error>> {
        match (rel1, rel2) {
            (Ok(Relation::Subtype), Ok(Relation::Subtype)) => Ok(()),
            (Ok(_), Ok(_)) => Err(vec![Error::TypeMismatch {
                left: (t1, Location::new(t1.to_span(), self.file_path)),
                right: (t2, Location::new(t2.to_span(), self.file_path)),
            }]),
            (Err(e1), Err(e2)) => Err(e1.into_iter().chain(e2).collect()),
            (Err(e), _) | (_, Err(e)) => Err(e),
        }
    }
    pub fn substitute_type(t: TypeNodeId) -> TypeNodeId {
        match t.to_type() {
            Type::Intermediate(cell) => {
                let TypeVar { parent, .. } = &cell.borrow() as &TypeVar;
                match parent {
                    Some(p) => Self::substitute_type(*p),
                    None => t,
                }
            }
            _ => t.apply_fn(Self::substitute_type),
        }
    }
    fn substitute_all_intermediates(&mut self) {
        let mut e_list = self
            .result_memo
            .iter()
            .map(|(e, t)| (*e, Self::substitute_type(*t)))
            .collect::<Vec<_>>();

        e_list.iter_mut().for_each(|(e, t)| {
            log::trace!("e: {:?} t: {}", e, t.to_type());
            let _old = self.result_memo.insert(*e, *t);
        })
    }

    fn generalize(&mut self, t: TypeNodeId) -> TypeNodeId {
        match t.to_type() {
            Type::Intermediate(tvar) => {
                let &TypeVar { level, var, .. } = &tvar.borrow() as _;
                if level > self.level {
                    self.get_typescheme(var)
                } else {
                    t
                }
            }
            _ => t.apply_fn(|t| self.generalize(t)),
        }
    }
    fn instantiate(&mut self, t: TypeNodeId) -> TypeNodeId {
        match t.to_type() {
            Type::TypeScheme(id) => {
                if let Some(tvar) = self.instantiated_map.get(&id) {
                    *tvar
                } else {
                    let res = self.gen_intermediate_type();
                    self.instantiated_map.insert(id, res);
                    res
                }
            }
            _ => t.apply_fn(|t| self.instantiate(t)),
        }
    }

    // Note: the third argument `span` is used for the error location in case of
    // type mismatch. This is needed because `t`'s span refers to the location
    // where it originally defined (e.g. the explicit return type of the
    // function) and is not necessarily the same as where the error happens.
    fn bind_pattern(
        &mut self,
        pat: (TypedPattern, Location),
        body: (TypeNodeId, Location),
    ) -> Result<TypeNodeId, Vec<Error>> {
        let (TypedPattern { pat, ty }, loc_p) = pat;
        let (body_t, loc_b) = body.clone();
        let mut bind_item = |pat| {
            let newloc = Location::new(
                ty.to_span(), // todo: add span to untyped pattern
                loc_p.path,
            );
            let ity = self.gen_intermediate_type_with_location(newloc.clone());
            let p = TypedPattern { pat, ty: ity };
            self.bind_pattern((p, newloc.clone()), (ity, newloc))
        };
        let pat_t = match pat {
            Pattern::Single(id) => {
                let pat_t = self.convert_unknown_to_intermediate(ty);
                log::trace!("bind {} : {}", id, pat_t.to_type().to_string());
                self.env.add_bind(&[(id, pat_t)]);
                Ok::<TypeNodeId, Vec<Error>>(pat_t)
            }
            Pattern::Tuple(pats) => {
                let elems = pats.iter().map(|p| bind_item(p.clone())).try_collect()?; //todo multiple errors
                let res = Type::Tuple(elems).into_id();
                let target = self.convert_unknown_to_intermediate(ty);
                let rel = self.unify_types(res, target)?;
                Ok(res)
            }
            Pattern::Record(items) => {
                let res = items
                    .iter()
                    .map(|(key, v)| {
                        bind_item(v.clone()).map(|ty| RecordTypeField {
                            key: *key,
                            ty,
                            has_default: false,
                        })
                    })
                    .try_collect()?; //todo multiple errors
                let res = Type::Record(res).into_id();
                let target = self.convert_unknown_to_intermediate(ty);
                let rel = self.unify_types(res, target)?;
                Ok(res)
            }
            Pattern::Error => Err(vec![Error::PatternMismatch(
                (Type::Failure.into_id(), loc_b.clone()),
                (pat, loc_p.clone()),
            )]),
        }?;
        let rel = self.unify_types(pat_t, body_t)?;
        Ok(self.generalize(pat_t))
    }

    pub fn lookup(&self, name: Symbol, loc: Location) -> Result<TypeNodeId, Error> {
        self.env.lookup(&name).map_or_else(
            || Err(Error::VariableNotFound(name, loc)), //todo:Span
            |v| Ok(*v),
        )
    }
    pub(crate) fn infer_type_literal(e: &Literal) -> Result<TypeNodeId, Error> {
        let pt = match e {
            Literal::Float(_) | Literal::Now | Literal::SampleRate => PType::Numeric,
            Literal::Int(_s) => PType::Int,
            Literal::String(_s) => PType::String,
            Literal::SelfLit => panic!("\"self\" should not be shown at type inference stage"),
            Literal::PlaceHolder => panic!("\"_\" should not be shown at type inference stage"),
        };
        Ok(Type::Primitive(pt).into_id())
    }
    fn infer_vec(&mut self, e: &[ExprNodeId]) -> Result<Vec<TypeNodeId>, Vec<Error>> {
        e.iter().map(|e| self.infer_type(*e)).try_collect()
    }
    fn infer_type_levelup(&mut self, e: ExprNodeId) -> TypeNodeId {
        self.level += 1;
        let res = self.infer_type(e);
        let r = self.unwrap_result(res);
        self.level -= 1;
        r
    }
    pub fn infer_type(&mut self, e: ExprNodeId) -> Result<TypeNodeId, Vec<Error>> {
        if let Some(r) = self.result_memo.get(&e.0) {
            //use cached result
            return Ok(*r);
        }
        let loc = Location::new(e.to_span(), self.file_path); //todo file
        let res: Result<TypeNodeId, Vec<Error>> = match &e.to_expr() {
            Expr::Literal(l) => Self::infer_type_literal(l).map_err(|e| vec![e]),
            Expr::Tuple(e) => Ok(Type::Tuple(self.infer_vec(e.as_slice())?).into_id()),
            Expr::ArrayLiteral(e) => {
                let elem_types = self.infer_vec(e.as_slice())?;
                let first = elem_types
                    .first()
                    .copied()
                    .unwrap_or(Type::Unknown.into_id());
                //todo:collect multiple errors
                let elem_t = elem_types
                    .iter()
                    .try_fold(first, |acc, t| self.unify_types(acc, *t).map(|rel| *t))?;

                Ok(Type::Array(elem_t).into_id_with_location(loc.clone()))
            }
            Expr::ArrayAccess(e, idx) => {
                let arr_t = self.infer_type_unwrapping(*e);
                let idx_t = self.infer_type_unwrapping(*idx);

                let loc_e = Location::new(e.to_span(), loc.path);
                let elem_t = self.gen_intermediate_type_with_location(loc_e.clone());

                let loc_i = Location::new(idx.to_span(), loc.path);
                let rel1 = self.unify_types(
                    idx_t,
                    Type::Primitive(PType::Numeric).into_id_with_location(loc_i),
                );
                let rel2 = self.unify_types(
                    Type::Array(elem_t).into_id_with_location(loc_e.clone()),
                    arr_t,
                );
                let _ = self.merge_rel_result(rel1, rel2, arr_t, idx_t)?;
                Ok(elem_t)
            }
            Expr::Proj(e, idx) => {
                let tup = self.infer_type_unwrapping(*e);
                // we directly inspect if the intermediate type is a tuple or not.
                // this is because we can not infer the number of fields in the tuple from the fields access expression.
                // This rule will be loosened when structural subtyping is implemented.
                let vec_to_ans = |vec: &[_]| {
                    if vec.len() < *idx as usize {
                        Err(vec![Error::IndexOutOfRange {
                            len: vec.len() as u16,
                            idx: *idx as u16,
                            loc: loc.clone(),
                        }])
                    } else {
                        Ok(vec[*idx as usize])
                    }
                };
                match tup.to_type() {
                    Type::Tuple(vec) => vec_to_ans(&vec),
                    Type::Intermediate(tv) => {
                        let tv = tv.borrow();
                        if let Some(parent) = tv.parent {
                            match parent.to_type() {
                                Type::Tuple(vec) => vec_to_ans(&vec),
                                _ => Err(vec![Error::IndexForNonTuple(loc, tup)]),
                            }
                        } else {
                            Err(vec![Error::IndexForNonTuple(loc, tup)])
                        }
                    }
                    _ => Err(vec![Error::IndexForNonTuple(loc, tup)]),
                }
            }
            Expr::RecordLiteral(kvs) => {
                let duplicate_keys = kvs
                    .iter()
                    .map(|RecordField { name, .. }| *name)
                    .duplicates();
                if duplicate_keys.clone().count() > 0 {
                    Err(vec![Error::DuplicateKeyInRecord {
                        key: duplicate_keys.collect(),
                        loc,
                    }])
                } else {
                    let kts: Vec<_> = kvs
                        .iter()
                        .map(|RecordField { name, expr }| {
                            let ty = self.infer_type_unwrapping(*expr);
                            RecordTypeField {
                                key: *name,
                                ty,
                                has_default: true,
                            }
                        })
                        .collect();
                    Ok(Type::Record(kts).into_id())
                }
            }
            Expr::FieldAccess(expr, field) => {
                let et = self.infer_type_unwrapping(*expr);
                log::trace!("field access {} : {}", field, et.to_type());
                let fields_to_ans = |fields: &[RecordTypeField]| {
                    fields
                        .iter()
                        .find_map(
                            |RecordTypeField { key, ty, .. }| {
                                if *key == *field { Some(*ty) } else { None }
                            },
                        )
                        .ok_or_else(|| {
                            vec![Error::FieldNotExist {
                                field: *field,
                                loc: loc.clone(),
                                et,
                            }]
                        })
                };
                // we directly inspect if the intermediate type is a record or not.
                // this is because we can not infer the number of fields in the record from the fields access expression.
                // This rule will be loosened when structural subtyping is implemented.
                match et.to_type() {
                    Type::Record(fields) => fields_to_ans(&fields),
                    Type::Intermediate(tv) => {
                        let tv = tv.borrow();
                        if let Some(parent) = tv.parent {
                            match parent.to_type() {
                                Type::Record(fields) => fields_to_ans(&fields),
                                _ => Err(vec![Error::FieldForNonRecord(loc, et)]),
                            }
                        } else {
                            Err(vec![Error::FieldForNonRecord(loc, et)])
                        }
                    }
                    _ => Err(vec![Error::FieldForNonRecord(loc, et)]),
                }
            }
            Expr::Feed(id, body) => {
                //todo: add span to Feed expr for keeping the location of `self`.
                let feedv = self.gen_intermediate_type();

                self.env.add_bind(&[(*id, feedv)]);
                let bty = self.infer_type_unwrapping(*body);
                let _rel = self.unify_types(bty, feedv)?;
                if bty.to_type().contains_function() {
                    Err(vec![Error::NonPrimitiveInFeed(Location::new(
                        body.to_span().clone(),
                        loc.path,
                    ))])
                } else {
                    Ok(bty)
                }
            }
            Expr::Lambda(p, rtype, body) => {
                self.env.extend();
                let dup = p.iter().duplicates_by(|id| id.id).map(|id| {
                    let loc = Location::new(id.to_span(), self.file_path);
                    (id.id, loc)
                });
                if dup.clone().count() > 0 {
                    return Err(vec![Error::DuplicateKeyInParams(dup.collect())]);
                }
                let ptype = Type::Record(
                    p.iter()
                        .map(|id| {
                            let ity = self.convert_unknown_to_intermediate(id.ty);
                            self.env.add_bind(&[(id.id, ity)]);
                            RecordTypeField {
                                key: id.id,
                                ty: ity,
                                has_default: false,
                            }
                        })
                        .collect(),
                )
                .into_id(); //todo:span
                let bty = if let Some(r) = rtype {
                    let bty = self.infer_type_unwrapping(*body);
                    let _rel = self.unify_types(*r, bty)?;
                    bty
                } else {
                    self.infer_type_unwrapping(*body)
                };
                self.env.to_outer();
                Ok(Type::Function {
                    arg: ptype,
                    ret: bty,
                }
                .into_id_with_location(e.to_location()))
            }
            Expr::Let(tpat, body, then) => {
                let bodyt = self.infer_type_levelup(*body);
                let loc_p = Location::new(tpat.to_span(), self.file_path);
                let loc_b = Location::new(body.to_span(), self.file_path);
                let pat_t = self.bind_pattern((tpat.clone(), loc_p), (bodyt, loc_b));
                let _pat_t = self.unwrap_result(pat_t);
                match then {
                    Some(e) => self.infer_type(*e),
                    None => Ok(Type::Primitive(PType::Unit).into_id()),
                }
            }
            Expr::LetRec(id, body, then) => {
                let idt = self.convert_unknown_to_intermediate(id.ty);
                self.env.add_bind(&[(id.id, idt)]);
                //polymorphic inference is not allowed in recursive function.
                let bodyt = self.infer_type_levelup(*body);
                let _res = self.unify_types(idt, bodyt);
                match then {
                    Some(e) => self.infer_type(*e),
                    None => Ok(Type::Primitive(PType::Unit).into_id()),
                }
            }
            Expr::Assign(assignee, expr) => {
                let name = match assignee.to_expr() {
                    Expr::Var(v) => v,
                    Expr::ArrayAccess(_, _) => {
                        unimplemented!("Assignment to array is not implemented yet.")
                    }
                    _ => unreachable!(),
                };
                let assignee_t = self.unwrap_result(self.lookup(name, loc).map_err(|e| vec![e]));
                let e_t = self.infer_type_unwrapping(*expr);
                let rel = self.unify_types(assignee_t, e_t)?;
                Ok(unit!())
            }
            Expr::Then(e, then) => {
                let _ = self.infer_type(*e)?;
                then.map_or(Ok(unit!()), |t| self.infer_type(t))
            }
            Expr::Var(name) => {
                let res = self.unwrap_result(self.lookup(*name, loc).map_err(|e| vec![e]));
                log::trace!("{} {} /level{}", name.as_str(), res.to_type(), self.level);
                Ok(self.instantiate(res))
            }
            Expr::Apply(fun, callee) => {
                let loc_f = Location::new(fun.to_span(), self.file_path);
                let fnl = self.infer_type_unwrapping(*fun);
                let callee_t = self.infer_type_unwrapping(callee[0]);
                let res_t = self.gen_intermediate_type();
                let fntype = Type::Function {
                    arg: callee_t,
                    ret: res_t,
                }
                .into_id_with_location(loc_f.clone());
                let rel = self.unify_types(fnl, fntype)?;
                Ok(res_t)
            }
            Expr::If(cond, then, opt_else) => {
                let condt = self.infer_type_unwrapping(*cond);
                let cond_loc = Location::new(cond.to_span(), loc.path);
                let bt = self.unify_types(
                    Type::Primitive(PType::Numeric).into_id_with_location(cond_loc),
                    condt,
                )?; //todo:boolean type
                //todo: introduce row polymophism so that not narrowing the type of `then` and `else` too much.
                let thent = self.infer_type_unwrapping(*then);
                let elset = opt_else.map_or(Type::Primitive(PType::Unit).into_id(), |e| {
                    self.infer_type_unwrapping(e)
                });
                let rel = self.unify_types(thent, elset)?;
                Ok(thent)
            }
            Expr::Block(expr) => expr.map_or(Ok(Type::Primitive(PType::Unit).into_id()), |e| {
                self.env.extend(); //block creates local scope.
                let res = self.infer_type(e);
                self.env.to_outer();
                res
            }),
            Expr::Escape(e) => {
                let loc_e = Location::new(e.to_span(), self.file_path);
                let res = self.infer_type_unwrapping(*e);
                let intermediate = self.gen_intermediate_type_with_location(loc_e.clone());
                let rel = self.unify_types(
                    res,
                    Type::Code(intermediate).into_id_with_location(loc_e.clone()),
                )?;
                Ok(intermediate)
            }
            Expr::Bracket(e) => {
                let loc_e = Location::new(e.to_span(), self.file_path);
                let res = self.infer_type_unwrapping(*e);
                Ok(Type::Code(res).into_id_with_location(loc_e))
            }
            _ => Ok(Type::Failure.into_id_with_location(loc)),
        };
        res.inspect(|ty| {
            self.result_memo.insert(e.0, *ty);
        })
    }
    fn infer_type_unwrapping(&mut self, e: ExprNodeId) -> TypeNodeId {
        match self.infer_type(e) {
            Ok(t) => t,
            Err(err) => {
                self.errors.extend(err);
                Type::Failure.into_id_with_location(Location::new(e.to_span(), self.file_path))
            }
        }
    }
}

pub fn infer_root(
    e: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    file_path: Symbol,
) -> InferContext {
    let mut ctx = InferContext::new(builtin_types, file_path);
    let _t = ctx.infer_type(e).unwrap_or(Type::Failure.into_id());
    ctx.substitute_all_intermediates();
    ctx
}
