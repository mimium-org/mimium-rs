use crate::utils::metadata::Span;

use super::*;

pub(super) enum Relation {
    Identical,
    Subtype,
    Supertype,
}
#[derive(PartialEq, Eq)]
pub(super) enum Error {
    TypeMismatch {
        left: TypeNodeId,
        right: TypeNodeId,
    },
    LengthMismatch {
        left: Vec<TypeNodeId>,
        right: Vec<TypeNodeId>,
    },
    CircularType {
        left: Span,
        right: Span,
    },
    ImcompatibleRecords {
        left: Vec<Symbol>,
        right: Vec<Symbol>,
    },
}

// return true when the circular loop of intermediate variable exists.
fn occur_check(id1: IntermediateId, t2: TypeNodeId) -> bool {
    let cls = |t2dash: TypeNodeId| -> bool { occur_check(id1, t2dash) };

    let vec_cls = |t: &[_]| -> bool { t.iter().any(|a| cls(*a)) };

    match &t2.to_type() {
        Type::Intermediate(cell) => cell
            .try_borrow()
            .map(|tv2| match tv2.parent {
                Some(tid2) => id1 == tv2.var || occur_check(id1, tid2),
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
        Type::Record(s) => vec_cls(
            s.iter()
                .map(|RecordTypeField { ty, .. }| *ty)
                .collect::<Vec<_>>()
                .as_slice(),
        ),
        _ => false,
    }
}

fn covariant(rel: Relation) -> Relation {
    match rel {
        Relation::Identical => Relation::Identical,
        Relation::Subtype => Relation::Subtype,
        Relation::Supertype => Relation::Supertype,
    }
}
fn contravariant(rel: Relation) -> Relation {
    match rel {
        Relation::Identical => Relation::Identical,
        Relation::Subtype => Relation::Supertype,
        Relation::Supertype => Relation::Subtype,
    }
}

//todo
fn unify_vec(a1: &[TypeNodeId], a2: &[TypeNodeId]) -> Result<(), Vec<Error>> {
    let (res, errs): (Vec<_>, Vec<_>) = a1
        .iter()
        .zip_longest(a2)
        .map(|pair| match pair {
            EitherOrBoth::Both(a1, a2) => unify_types((*a1, loc1.clone()), (*a2, loc2.clone())),
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
//todo
fn unify_named_params(
    ps1: &LabeledParams, //function
    ps2: &LabeledParams, //arguments
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
                    |p1| unify_types((p1.ty, loc1.clone()), (p2.ty, loc2.clone())),
                )
        });
        if unified_types.clone().any(|x| x.is_err()) {
            let errs = unified_types.filter_map(|x| x.err()).flatten().collect();
            return (LabeledParams::new(vec![]), errs);
        }
        let res = unified_types
            .map(|x| x.unwrap())
            .zip(ps2.get_as_slice().iter().map(|p| p.label))
            .map(|(ty, label)| LabeledParam {
                label,
                ty,
                has_default: true,
            })
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
        .map(|(t1, t2)| unify_types((t1, loc1.clone()), (t2, loc2.clone())))
        .collect::<Vec<_>>();
    if ts.clone().into_iter().any(|x| x.is_err()) {
        let errs = ts.into_iter().filter_map(|x| x.err()).flatten().collect();
        (LabeledParams::new(vec![]), errs)
    } else {
        let res = ts
            .into_iter()
            .zip(label_vec)
            .map(|(ty, label)| LabeledParam {
                label,
                ty: ty.unwrap(),
                has_default: true,
            })
            .collect::<Vec<_>>();
        (LabeledParams::new(res), vec![])
    }
}

/// Solve type constraints. Though the function arguments are immutable, it modified the content of Intermediate Type.
/// If the result is `Relation::Subtype`, it means "t1 is subtype of t2".
pub(super) fn unify_types(t1: TypeNodeId, t2: TypeNodeId) -> Result<Relation, Vec<Error>> {
    let loc1 = t1.to_span(); //todo file
    let loc2 = t1.to_span();

    log::trace!("unify {} and {}", t1.to_type(), t2.to_type());
    let t1r = t1.get_root();
    let t2r = t2.get_root();
    let res = match &(t1r.to_type(), t2r.to_type()) {
        (Type::Intermediate(i1), Type::Intermediate(i2)) if i1 == i2 => Relation::Identical,
        (Type::Intermediate(i1), Type::Intermediate(i2)) => {
            let tv1 = &mut i1.borrow_mut() as &mut TypeVar;
            if occur_check(tv1.var, t2) {
                return Err(vec![Error::CircularType {
                    left: loc1,
                    right: loc2,
                }]);
            }
            let tv2 = &mut i2.borrow_mut() as &mut TypeVar;
            if tv2.level > tv1.level {
                tv2.level = tv1.level
            }
            let _ = match (tv1.parent, tv2.parent) {
                (None, None) => {
                    if tv1.var > tv2.var {
                        tv2.parent = Some(t1r);
                    } else {
                        tv1.parent = Some(t2r);
                    };
                }
                (_, Some(p2)) => {
                    tv1.parent = Some(p2);
                }
                (Some(p1), _) => {
                    tv2.parent = Some(p1);
                }
            };
            Relation::Identical
        }
        (Type::Intermediate(i1), _) => {
            let tv1 = &mut i1.borrow_mut() as &mut TypeVar;
            tv1.parent = Some(t2r);
            Relation::Identical
        }
        (_, Type::Intermediate(i2)) => {
            let tv2 = &mut i2.borrow_mut() as &mut TypeVar;
            tv2.parent = Some(t1r);
            Relation::Identical
        }
        (Type::Array(a1), Type::Array(a2)) => {
            //theoriticaly, the array type can be covariant but it makes implementation complex and might be not intuitive for beginners.
            let _rel = unify_types(*a1, *a2)?;
            Relation::Identical
        }
        (Type::Ref(x1), Type::Ref(x2)) => {
            let _ = unify_types(*x1, *x2)?;
            Relation::Identical
        }
        (Type::Tuple(a1), Type::Tuple(a2)) => {
            // if a1 have nth elements, a2 must have at least n, or more elements.
            use std::cmp::Ordering;
            match a1.len().cmp(&a2.len()) {
                Ordering::Equal => {
                    let _ = unify_vec(a1, a2)?;
                    Relation::Identical
                }
                Ordering::Less => {
                    let _ = unify_vec(a1, &a2[0..a1.len()])?;
                    Relation::Subtype
                }
                Ordering::Greater => {
                    let _ = unify_vec(&a1[0..a2.len()], a2)?;
                    Relation::Supertype
                }
            }
        }
        // a and b are identical:
        // a: {key1:A, key2:_, key3:C}
        // b: {key1:_, key2:B, key3:C}
        // - a is subtype of b
        // a: {key1:A,       , key3:C,       , key5:E}
        // b: {key1:A, key2:_, key3:C, key4:D, key5:E}
        // - a is supertype of b
        // a: {key1:A, key2:_, key3:C, key4:D, key5:E}
        // b: {key1:A,       , key3:C,       , key5:E}
        // - a and b are imcompatible
        // a: {key1:A, key2:_,       , key4:D, key5:E}
        // b: {key1:A,       , key3:C,       , key5:E}
        (Type::Record(a1), Type::Record(a2)) => {
            // the algotithm used here may not be efficient but we prioritize the clarity of logic.
            // it will not matter because the code rarely contains huge entries of record

            //list up all keys. expect that the records are sorted by the alphabetical order.
            let extract_key_and_sort = |iter: std::slice::Iter<'_, RecordTypeField>| {
                iter.map(|RecordTypeField { key, .. }| *key)
                    .sorted_by(|keya, keyb| keya.as_str().cmp(keyb.as_str()))
            };
            let keys_a = extract_key_and_sort(a1.iter());
            let keys_b = extract_key_and_sort(a2.iter());
            let allkeys = keys_a.chain(keys_b).unique();
            let sparse_fields1 = allkeys
                .map(|skey: Symbol| a1.iter().find(|RecordTypeField { key, .. }| skey == *key));
            let sparse_fields2 = allkeys
                .map(|skey: Symbol| a2.iter().find(|RecordTypeField { key, .. }| skey == *key));
            #[derive(PartialEq, Eq)]
            enum SearchRes {
                Both,
                A,
                B,
            }
            let searchresults = sparse_fields1.zip(sparse_fields2).map(|pair| match pair {
                (Some(s1), Some(s2)) => unify_types(s1.ty, s2.ty).map(|_| SearchRes::Both),
                (Some(_), None) => Ok(SearchRes::A),
                (None, Some(_)) => Ok(SearchRes::B),
                (None, None) => unreachable!(),
            });
            let collected_errs = searchresults
                .filter_map(|r| r.err())
                .flatten()
                .collect::<Vec<_>>();
            let mut all_errs = vec![];
            let contains_err = !collected_errs.is_empty();
            if contains_err {
                all_errs = collected_errs;
            }
            let contains_a = searchresults.any(|r| r.is_ok_and(|r| r == SearchRes::A));
            let contains_b = searchresults.any(|r| r.is_ok_and(|r| r == SearchRes::B));

            if searchresults.all(|r| r == Ok(SearchRes::Both)) {
                Relation::Identical
            } else if !contains_err && contains_a && !contains_b {
                //a has more fields than b, that means A is
                Relation::Supertype
            } else if !contains_err && contains_b && !contains_a {
                Relation::Subtype
            } else if contains_b && contains_a {
                all_errs.push(Error::ImcompatibleRecords {
                    left: keys_a.collect(),
                    right: keys_b.collect(),
                });
                return Err(all_errs);
            } else {
                return Err(all_errs);
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
        (Type::Code(p1), Type::Code(p2)) => {
            let ret = Self::unify_types((*p1, loc1.clone()), (*p2, loc2))?;
            Ok(Type::Code(ret).into_id_with_location(loc1))
        }
        (_p1, _p2) => Err(vec![Error::TypeMismatch {
            left: (t1, loc1),
            right: (t2, loc2),
        }]),
    };
    Ok(res)
}
