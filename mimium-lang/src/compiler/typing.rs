use crate::ast::{Expr, Literal, RecordField};
use crate::compiler::intrinsics;
use crate::interner::{ExprKey, ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedPattern};
use crate::types::{LabeledParam, LabeledParams, PType, Type, TypeVar};
use crate::utils::metadata::Location;
use crate::utils::{environment::Environment, error::ReportableError};
use crate::{function, integer, numeric, unit};
use itertools::{EitherOrBoth, Itertools};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::rc::Rc;

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

    /// This is temporary error which is used when the type of 2 records contain different keys.
    /// If structural subtyping is implemented, this error should be removed.
    IncompatibleKeyInRecord {
        key: Vec<Symbol>,
        loc: Location,
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
            Error::IndexForNonTuple(_, ty) => {
                format!("Index access for non-tuple variable.")
            }
            Error::VariableNotFound(symbol, _) => {
                format!("Variable {} not found in this scope", symbol)
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
            Error::IncompatibleKeyInRecord { key, loc } => {
                vec![(
                    loc.clone(),
                    format!(
                        "Incompatible key \"{}\" found in record type",
                        key.iter()
                            .map(|s| s.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                )]
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
    interm_idx: u64,
    typescheme_idx: u64,
    level: u64,
    instantiated_map: BTreeMap<u64, TypeNodeId>, //from type scheme to typevar
    generalize_map: BTreeMap<u64, u64>,
    result_map: BTreeMap<ExprKey, TypeNodeId>,
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
            result_map: Default::default(),
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
        self.interm_idx += 1;
        res
    }
    fn get_typescheme(&mut self, tvid: u64) -> TypeNodeId {
        self.generalize_map.get(&tvid).cloned().map_or_else(
            || self.gen_typescheme(),
            |id| Type::TypeScheme(id).into_id(),
        )
    }
    fn gen_typescheme(&mut self) -> TypeNodeId {
        let res = Type::TypeScheme(self.typescheme_idx).into_id();
        self.typescheme_idx += 1;
        res
    }

    fn gen_intermediate_type_with_location(&mut self, loc: Location) -> TypeNodeId {
        let res = Type::Intermediate(Rc::new(RefCell::new(TypeVar::new(
            self.interm_idx,
            self.level,
        ))))
        .into_id_with_location(loc);
        self.interm_idx += 1;
        res
    }
    fn convert_unknown_to_intermediate(&mut self, t: TypeNodeId) -> TypeNodeId {
        match t.to_type() {
            Type::Unknown => self.gen_intermediate_type(),
            _ => t.apply_fn(|t| self.convert_unknown_to_intermediate(t)),
        }
    }
    // return true when the circular loop of intermediate variable exists.
    fn occur_check(id1: u64, t2: TypeNodeId) -> bool {
        let cls = |t2dash: TypeNodeId| -> bool { Self::occur_check(id1, t2dash) };

        let vec_cls = |t: &[_]| -> bool { t.iter().any(|a| cls(*a)) };

        match &t2.to_type() {
            Type::Intermediate(cell) => cell
                .try_borrow()
                .map(|tv2| match tv2.parent {
                    Some(tid2) => id1 == tv2.var || Self::occur_check(id1, tid2),
                    None => id1 == tv2.var,
                })
                .unwrap_or(true),
            Type::Array(a) => cls(*a),
            Type::Tuple(t) => vec_cls(t),
            Type::Function(p, r, s) => {
                let vec = p.ty_iter().collect::<Vec<_>>();
                vec_cls(vec.as_slice())
                    && cls(*r)
                    && cls(s.map(|x| x).unwrap_or_else(|| Type::Unknown.into_id()))
            }
            Type::Record(s) => vec_cls(s.iter().map(|(_k, v)| *v).collect::<Vec<_>>().as_slice()),
            _ => false,
        }
    }

    fn substitute_type(t: TypeNodeId) -> TypeNodeId {
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
            .result_map
            .iter()
            .map(|(e, t)| (*e, Self::substitute_type(*t)))
            .collect::<Vec<_>>();

        e_list.iter_mut().for_each(|(e, t)| {
            log::trace!("e: {:?} t: {}", e, t.to_type());
            let _old = self.result_map.insert(*e, *t);
        })
    }
    fn unify_vec(
        a1: &[TypeNodeId],
        loc1: Location,
        a2: &[TypeNodeId],
        loc2: Location,
    ) -> (Vec<TypeNodeId>, Vec<Error>) {
        let (res, errs): (Vec<_>, Vec<_>) = a1
            .iter()
            .zip_longest(a2)
            .map(|pair| match pair {
                EitherOrBoth::Both(a1, a2) => {
                    Self::unify_types((*a1, loc1.clone()), (*a2, loc2.clone()))
                }
                EitherOrBoth::Left(t) | EitherOrBoth::Right(t) => Ok(*t),
            })
            .partition_result();
        let mut errs: Vec<_> = errs.into_iter().flatten().collect();
        if a1.len() != a2.len() {
            errs.push(Error::LengthMismatch {
                left: (a1.len(), loc1.clone()),
                right: (a2.len(), loc2.clone()),
            });
        }
        (res, errs)
    }
    // if used in let expression, tl1 means lefthand value
    fn unify_types(
        tl1: (TypeNodeId, Location),
        tl2: (TypeNodeId, Location),
    ) -> Result<TypeNodeId, Vec<Error>> {
        let (t1, loc1) = tl1; //todo file
        let (t2, loc2) = tl2;

        log::trace!("unify {} and {}", t1.to_type(), t2.to_type());
        let t1r = t1.get_root();
        let t2r = t2.get_root();
        match &(t1r.to_type(), t2r.to_type()) {
            (Type::Intermediate(i1), Type::Intermediate(i2)) if i1 == i2 => Ok(t1),

            (Type::Intermediate(i1), Type::Intermediate(i2)) => {
                let tv1 = &mut i1.borrow_mut() as &mut TypeVar;
                if Self::occur_check(tv1.var, t2) {
                    return Err(vec![Error::CircularType(loc1, loc2)]);
                }
                let tv2 = &mut i2.borrow_mut() as &mut TypeVar;
                if tv2.level > tv1.level {
                    tv2.level = tv1.level
                }
                match (tv1.parent, tv2.parent) {
                    (None, None) => {
                        if tv1.var > tv2.var {
                            tv2.parent = Some(t1r);
                            Ok(t1r)
                        } else {
                            tv1.parent = Some(t2r);
                            Ok(t2r)
                        }
                    }
                    (_, Some(p2)) => {
                        tv1.parent = Some(p2);
                        Ok(p2)
                    }
                    (Some(p1), _) => {
                        tv2.parent = Some(p1);
                        Ok(p1)
                    }
                }
            }
            (Type::Intermediate(i1), _) => {
                let tv1 = &mut i1.borrow_mut() as &mut TypeVar;
                tv1.parent = Some(t2r);
                Ok(t2r)
            }
            (_, Type::Intermediate(i2)) => {
                let tv2 = &mut i2.borrow_mut() as &mut TypeVar;
                tv2.parent = Some(t1r);
                Ok(t1r)
            }
            (Type::Array(a1), Type::Array(a2)) => {
                Ok(Type::Array(Self::unify_types((*a1, loc1), (*a2, loc2))?).into_id())
            }
            (Type::Ref(x1), Type::Ref(x2)) => {
                Ok(Type::Ref(Self::unify_types((*x1, loc1), (*x2, loc2))?).into_id())
            }
            (Type::Tuple(a1), Type::Tuple(a2)) => {
                let (vec, err) = Self::unify_vec(a1, loc1, a2, loc2);
                if err.is_empty() {
                    Ok(Type::Tuple(vec).into_id())
                } else {
                    Err(err) //todo:return both partial result and err
                }
            }
            (Type::Record(a1), Type::Record(a2)) => {
                {
                    let m1 = HashSet::<Symbol>::from_iter(a1.iter().map(|(k, _)| *k));
                    let m2 = HashSet::<Symbol>::from_iter(a2.iter().map(|(k, _)| *k));
                    if m1 != m2 {
                        let key_diffs = m1.difference(&m2).cloned().collect();
                        return Err(vec![Error::IncompatibleKeyInRecord {
                            key: key_diffs,
                            loc: loc1.clone(),
                        }]);
                    }
                }
                let get_key_val = |v: &Vec<(Symbol, TypeNodeId)>| {
                    let mut v_c = v.clone();
                    v_c.sort_by(|a, b| a.0.to_string().cmp(&b.0.to_string()));
                    let (ks, ts): (Vec<_>, Vec<_>) = v_c.into_iter().unzip();
                    (ks, ts)
                };
                let (ks1, ts1) = get_key_val(a1);
                let (ks2, ts2) = get_key_val(a2);
                debug_assert_eq!(ks1, ks2);
                let (vec, err) = Self::unify_vec(&ts1, loc1, &ts2, loc2);
                let rvec = ks1.into_iter().zip(vec).collect::<Vec<_>>();
                if err.is_empty() {
                    Ok(Type::Record(rvec).into_id())
                } else {
                    Err(err)
                }
            }
            (Type::Function(p1, r1, _s1), Type::Function(p2, r2, _s2)) => {
                let (param, errs) = Self::unify_named_params(p1, &loc1, p2, &loc2);
                let ret = Self::unify_types((*r1, loc1), (*r2, loc2));
                match (ret, errs) {
                    (Ok(ret), errs) if errs.is_empty() => {
                        Ok(Type::Function(param, ret, None).into_id())
                    }
                    (Ok(_ret), errs) => Err(errs),
                    (Err(mut e), mut errs) => {
                        errs.append(&mut e);
                        Err(errs)
                    }
                }
            }
            (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => {
                Ok(Type::Primitive(p1.clone()).into_id())
            }
            (Type::Failure, t) => Ok(t.clone().into_id_with_location(loc1.clone())),
            (t, Type::Failure) => Ok(t.clone().into_id_with_location(loc2.clone())),
            (Type::Code(_p1), Type::Code(_p2)) => {
                todo!("type system for multi-stage computation has not implemented yet")
            }
            (_p1, _p2) => Err(vec![Error::TypeMismatch {
                left: (t1, loc1),
                right: (t2, loc2),
            }]),
        }
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
                let res = pats.iter().map(|p| bind_item(p.clone())).try_collect()?; //todo multiple errors
                let res = Self::unify_types(
                    (Type::Tuple(res).into_id(), loc_p.clone()),
                    (self.convert_unknown_to_intermediate(ty), loc_p.clone()),
                )?;
                Ok(res)
            }
            Pattern::Record(items) => {
                let res = items
                    .iter()
                    .map(|(k, v)| bind_item(v.clone()).map(|t| (*k, t)))
                    .try_collect()?; //todo multiple errors
                let res = Self::unify_types(
                    (Type::Record(res).into_id(), loc_p.clone()),
                    (self.convert_unknown_to_intermediate(ty), loc_p.clone()),
                )?;
                Ok(res)
            }
            Pattern::Error => Err(vec![Error::PatternMismatch(
                (Type::Failure.into_id(), loc_b.clone()),
                (pat, loc_p.clone()),
            )]),
        }?;
        let t2 = Self::unify_types((pat_t, loc_p.clone()), (body_t, loc_b.clone()))?;
        Ok(self.generalize(t2))
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
    fn infer_type(&mut self, e: ExprNodeId) -> Result<TypeNodeId, Vec<Error>> {
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
                let arr_t = elem_types.iter().try_fold(first, |acc, t| {
                    Self::unify_types((acc, loc.clone()), (*t, loc.clone()))
                })?;
                Ok(Type::Array(arr_t).into_id())
            }
            Expr::ArrayAccess(e, idx) => {
                let arr = self.infer_type(*e);
                let idx_i = self.infer_type(*idx);
                let loc_e = Location::new(e.to_span(), loc.path);
                let ntype = Type::Primitive(PType::Numeric);
                match (arr, idx_i) {
                    (Ok(arr_t), Ok(idx_t)) => {
                        let elem_t = self.gen_intermediate_type_with_location(loc_e.clone());
                        let _ = Self::unify_types(
                            (idx_t, loc.clone()),
                            (ntype.into_id(), loc.clone()),
                        )?;
                        let _ = Self::unify_types(
                            (
                                Type::Array(elem_t).into_id_with_location(loc_e.clone()),
                                loc_e.clone(),
                            ),
                            (arr_t, loc_e.clone()),
                        );
                        Ok(elem_t)
                    }
                    (Err(e1), Err(e2)) => Err(e1.into_iter().chain(e2).collect()),
                    (Err(e), _) => Err(e),
                    (_, Err(e)) => Err(e),
                }
            }
            Expr::Proj(e, idx) => {
                let tup = self.infer_type(*e)?;
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
                            let t = self.infer_type(*expr);
                            t.map(|t| (*name, t))
                        })
                        .try_collect()?;
                    Ok(Type::Record(kts).into_id())
                }
            }
            Expr::FieldAccess(expr, field) => {
                let et = self.infer_type(*expr)?;
                log::trace!("field access {} : {}", field, et.to_type());
                let fields_to_ans = |fields: &[(Symbol, TypeNodeId)]| {
                    fields
                        .iter()
                        .find_map(|(name, t)| if *name == *field { Some(*t) } else { None })
                        .ok_or_else(|| vec![Error::VariableNotFound(*field, loc.clone())])
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
                let loc_b = Location::new(body.to_span(), loc.path);

                self.env.add_bind(&[(*id, feedv)]);
                let bty = self.infer_type(*body)?;
                let res = Self::unify_types((bty, loc.clone()), (feedv, loc_b));
                match res {
                    Ok(res) if res.to_type().contains_function() => {
                        Err(vec![Error::NonPrimitiveInFeed(Location::new(
                            body.to_span().clone(),
                            loc.path,
                        ))])
                    }
                    Ok(r) => Ok(r),
                    Err(e) => Err(e),
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

                let ptypes = LabeledParams::new(
                    p.iter()
                        .map(|id| {
                            let pt = if !id.is_unknown() {
                                id.ty
                            } else {
                                self.gen_intermediate_type()
                            };
                            self.env.add_bind(&[(id.id, pt)]);
                            LabeledParam::new(id.id, pt)
                        })
                        .collect(),
                );
                let bty = if let Some(r) = rtype {
                    let loc_r = Location::new(r.to_span(), self.file_path);
                    let bty = self.infer_type(*body)?;
                    let loc_b = Location::new(body.to_span(), self.file_path);
                    Self::unify_types((*r, loc_r), (bty, loc_b))?
                } else {
                    self.infer_type(*body)?
                };
                self.env.to_outer();
                Ok(Type::Function(ptypes, bty, None).into_id())
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
                let loc_id = Location::new(id.to_span(), self.file_path);
                let idt = self.convert_unknown_to_intermediate(id.ty);
                self.env.add_bind(&[(id.id, idt)]);
                //polymorphic inference is not allowed in recursive function.
                let bodyt = self.infer_type_levelup(*body);
                let loc_b = Location::new(body.to_span(), self.file_path);
                let _res = Self::unify_types((idt, loc_id), (bodyt, loc_b));
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
                let loc_a = Location::new(assignee.to_span(), self.file_path);
                let loc_e = Location::new(expr.to_span(), self.file_path);
                let assignee_t = self.unwrap_result(self.lookup(name, loc).map_err(|e| vec![e]));
                let t = self.infer_type(*expr);
                let e_t = self.unwrap_result(t);
                Self::unify_types((assignee_t, loc_a), (e_t, loc_e))?;
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
                let fnl = self.infer_type(*fun);
                let fnl = self.unwrap_result(fnl);
                let loc_f = Location::new(fun.to_span(), self.file_path);
                let callee_t = if callee.len() == 1 {
                    let callee_t = self.infer_type(callee[0])?;
                    match callee_t.to_type() {
                        //parameter-pack handling
                        Type::Tuple(t) => t.into_iter().map(LabeledParam::from).collect(),
                        Type::Record(kvs) => kvs
                            .into_iter()
                            .map(|(k, t)| LabeledParam::new(k, t))
                            .collect(),
                        _ => vec![LabeledParam::from(callee_t)],
                    }
                } else {
                    self.infer_vec(callee.as_slice())?
                        .into_iter()
                        .map(LabeledParam::from)
                        .collect::<Vec<_>>()
                };
                let res_t = self.gen_intermediate_type();
                let fntype = Type::Function(LabeledParams::new(callee_t), res_t, None).into_id();
                let restype = Self::unify_types((fnl, loc_f.clone()), (fntype, loc_f));
                match restype {
                    Ok(t) => match t.to_type() {
                        Type::Function(_, r, _) => Ok(r),
                        _ => unreachable!("non functional code in apply"),
                    },
                    Err(ref _e) => restype,
                }
            }
            Expr::If(cond, then, opt_else) => {
                let condt = self.infer_type(*cond)?;
                let cond_loc = Location::new(cond.to_span(), loc.path);
                let _bt = Self::unify_types(
                    (Type::Primitive(PType::Numeric).into_id(), cond_loc.clone()),
                    (condt, cond_loc),
                ); //todo:boolean type
                let thent = self.infer_type(*then);
                let thent = self.unwrap_result(thent);
                let then_loc = Location::new(then.to_span(), self.file_path);
                let elset = opt_else.map_or(Ok(Type::Primitive(PType::Unit).into_id()), |e| {
                    self.infer_type(e)
                });
                let elset = self.unwrap_result(elset);
                let else_loc =
                    Location::new(opt_else.map_or(loc.span, |e| e.to_span()), self.file_path);
                log::trace!("then: {}, else: {}", thent.to_type(), elset.to_type());
                Self::unify_types((thent, then_loc), (elset, else_loc))
            }
            Expr::Block(expr) => expr.map_or(Ok(Type::Primitive(PType::Unit).into_id()), |e| {
                self.env.extend(); //block creates local scope.
                let res = self.infer_type(e);
                self.env.to_outer();
                res
            }),
            Expr::Escape(e) => {
                let loc_e = Location::new(e.to_span(), self.file_path);
                let res = self.infer_type(*e)?;
                let intermediate =
                    Type::Code(self.gen_intermediate_type_with_location(loc_e.clone()))
                        .into_id_with_location(loc_e.clone());
                Self::unify_types((res, loc_e.clone()), (intermediate, loc_e))
            }
            Expr::Bracket(e) => {
                let loc_e = Location::new(e.to_span(), self.file_path);
                let res = self.infer_type(*e)?;
                Ok(Type::Code(res).into_id_with_location(loc_e))
            }
            _ => Ok(Type::Failure.into_id_with_location(loc)),
        };
        res.inspect(|ty| {
            self.result_map.insert(e.0, *ty);
        })
    }
    pub fn lookup_res(&self, e: ExprNodeId) -> TypeNodeId {
        *self.result_map.get(&e.0).expect("type inference failed")
    }
    // Helper function to unify function parameters with names
    fn unify_named_params(
        ps1: &LabeledParams, //function
        loc1: &Location,
        ps2: &LabeledParams, //arguments
        loc2: &Location,
    ) -> (LabeledParams, Vec<Error>) {
        let len1 = ps1.get_as_slice().len();
        let len2 = ps2.get_as_slice().len();
        let has_label_1 = ps1.has_label();
        let has_label_2 = ps2.has_label();
        if len1 != len2 {
            let err = Error::LengthMismatch {
                left: (len1, loc1.clone()),
                right: (len2, loc2.clone()),
            };
            return (LabeledParams::new(vec![]), vec![err]);
        }
        if has_label_1 && has_label_2 {
            //labels may be in different order
            let unified_types = ps2.get_as_slice().iter().map(|p2| {
                ps1.get_as_slice()
                    .iter()
                    .find(|p1| p1.label == p2.label)
                    .map_or(
                        Err(vec![Error::FieldNotExistInParams {
                            field: p2.label.unwrap(),
                            loc: loc1.clone(),
                            params: ps1.clone(),
                        }]),
                        |p1| Self::unify_types((p1.ty, loc1.clone()), (p2.ty, loc2.clone())),
                    )
            });
            if unified_types.clone().any(|x| x.is_err()) {
                let errs = unified_types.filter_map(|x| x.err()).flatten().collect();
                return (LabeledParams::new(vec![]), errs);
            }
            let res = unified_types
                .map(|x| x.unwrap())
                .zip(ps2.get_as_slice().iter().map(|p| p.label))
                .map(|(ty, label)| LabeledParam { label, ty })
                .collect::<Vec<_>>();
            return (LabeledParams::new(res), vec![]);
        }
        let label_vec = if has_label_1 && !has_label_2 {
            ps1.get_as_slice().iter().map(|p| p.label).collect()
        } else if !has_label_1 && has_label_2 {
            ps2.get_as_slice().iter().map(|p| p.label).collect()
        } else {
            vec![None; len1]
        };
        let ts = ps1
            .ty_iter()
            .zip(ps2.ty_iter())
            .map(|(t1, t2)| Self::unify_types((t1, loc1.clone()), (t2, loc2.clone())))
            .collect::<Vec<_>>();
        if ts.clone().into_iter().any(|x| x.is_err()) {
            let errs = ts.into_iter().filter_map(|x| x.err()).flatten().collect();
            return (LabeledParams::new(vec![]), errs);
        } else {
            let res = ts
                .into_iter()
                .zip(label_vec)
                .map(|(ty, label)| LabeledParam {
                    label,
                    ty: ty.unwrap(),
                })
                .collect::<Vec<_>>();
            return (LabeledParams::new(res), vec![]);
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
