use crate::utils::metadata::Span;

use super::*;

/// Represents the relationship between two types after unification.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub(crate) enum Relation {
    /// t1 is subtype of t2 (e.g., int is subtype of float | int)
    Subtype,
    /// t1 and t2 are identical types
    Identical,
    /// t1 is supertype of t2
    Supertype,
}
#[derive(PartialEq, Eq, Debug)]
pub(crate) enum Error {
    TypeMismatch {
        left: TypeNodeId,
        right: TypeNodeId,
    },
    LengthMismatch {
        left: (Vec<TypeNodeId>, Span),
        right: (Vec<TypeNodeId>, Span),
    },
    CircularType {
        left: Span,
        right: Span,
    },
    ImcompatibleRecords {
        left: (Vec<(Symbol, TypeNodeId)>, Span),
        right: (Vec<(Symbol, TypeNodeId)>, Span),
    },
}

// return true when the circular loop of intermediate variable exists.
fn occur_check(id1: IntermediateId, t2: TypeNodeId) -> bool {
    let cls = |t2dash: TypeNodeId| -> bool { occur_check(id1, t2dash) };

    let vec_cls = |t: &[_]| -> bool { t.iter().any(|a| cls(*a)) };

    match &t2.to_type() {
        Type::Intermediate(cell) => cell
            .read()
            .map(|tv2| match tv2.parent {
                Some(tid2) => id1 == tv2.var || occur_check(id1, tid2),
                None => id1 == tv2.var,
            })
            .unwrap_or(true),
        Type::Array(a) => cls(*a),
        Type::Tuple(t) => vec_cls(t),
        Type::Function { arg, ret } => cls(*arg) && cls(*ret),
        Type::Record(s) => vec_cls(
            s.iter()
                .map(|RecordTypeField { ty, .. }| *ty)
                .collect::<Vec<_>>()
                .as_slice(),
        ),
        Type::Union(types) => vec_cls(types),
        _ => false,
    }
}

// fn covariant(rel: Relation) -> Relation {
//     match rel {
//         Relation::Subtype => Relation::Subtype,
//         Relation::Identical => Relation::Subtype,
//         Relation::Supertype => Relation::Supertype,
//     }
// }
// fn contravariant(rel: Relation) -> Relation {
//     match rel {
//         Relation::Subtype => Relation::Supertype,
//         Relation::Supertype => Relation::Subtype,
//         Relation::Identical => Relation::Sub,
//     }
// }

//todo
fn unify_vec(a1: &[TypeNodeId], a2: &[TypeNodeId]) -> Result<Relation, Vec<Error>> {
    assert_eq!(a1.len(), a2.len());
    let (res, errs): (Vec<_>, Vec<_>) = a1
        .iter()
        .zip(a2)
        .map(|(a1, a2)| unify_types(*a1, *a2))
        .partition_result();
    let errs: Vec<_> = errs.into_iter().flatten().collect();

    let res_relation = if res.iter().all(|r| *r != Relation::Subtype) {
        Relation::Supertype
    } else if res.iter().all(|r| *r != Relation::Supertype) {
        Relation::Subtype
    } else {
        //TODO more specific error report, if the tuple contains both subtype and supertype
        return Err(errs);
    };
    Ok(res_relation)
}
fn unify_types_args(t1: TypeNodeId, t2: TypeNodeId) -> Result<Relation, Vec<Error>> {
    log::trace!("unify_args {} and {}", t1.to_type(), t2.to_type());
    let loc1 = t1.to_span(); //todo file
    let loc2 = t1.to_span();
    let t1r = t1.get_root();
    let t2r = t2.get_root();
    let res = match &(t1r.to_type(), t2r.to_type()) {
        (Type::Record(_), Type::Record(_)) | (Type::Tuple(_), Type::Tuple(_)) => {
            unify_types(t1, t2)?
        }
        (_t, Type::Tuple(v)) if v.len() == 1 => unify_types_args(t1, *v.first().unwrap())?,
        (Type::Tuple(v), _t) if v.len() == 1 => unify_types_args(*v.first().unwrap(), t2)?,

        (_t, Type::Record(v)) if v.len() == 1 => unify_types_args(t1, v.first().unwrap().ty)?,
        (Type::Record(v), _t) if v.len() == 1 => unify_types_args(v.first().unwrap().ty, t2)?,

        (Type::Intermediate(i1), Type::Intermediate(i2)) => {
            if *i1.read().unwrap() == *i2.read().unwrap() {
                return Ok(Relation::Identical);
            }
            if occur_check(i1.read().unwrap().var, t2) {
                return Err(vec![Error::CircularType {
                    left: loc1,
                    right: loc2,
                }]);
            }
            if i2.read().unwrap().level > i1.read().unwrap().level {
                i2.write().unwrap().level = i1.read().unwrap().level;
            }
            match (i1.read().unwrap().parent, i2.read().unwrap().parent) {
                (None, None) => {
                    if i1.read().unwrap().var > i2.read().unwrap().var {
                        i2.write().unwrap().parent = Some(t1r);
                    } else {
                        i1.write().unwrap().parent = Some(t2r);
                    };
                }
                (_, Some(p2)) => {
                    i1.write().unwrap().parent = Some(p2);
                }
                (Some(p1), _) => {
                    i2.write().unwrap().parent = Some(p1);
                }
            };
            Relation::Identical
        }
        (Type::Intermediate(i1), _) => {
            let mut tv1 = i1.write().unwrap();
            tv1.parent = Some(t2r);
            tv1.bound.upper = t2r;

            Relation::Identical
        }
        (_, Type::Intermediate(i2)) => {
            let mut tv2 = i2.write().unwrap();
            tv2.parent = Some(t1r);
            tv2.bound.upper = t1r;
            Relation::Identical
        }
        (Type::Record(kvs), Type::Tuple(_)) => {
            let recordvec = kvs
                .iter()
                .map(|RecordTypeField { ty, .. }| *ty)
                .collect::<Vec<_>>();
            let loc_record = t1.to_loc();
            let new_tup = Type::Tuple(recordvec).into_id_with_location(loc_record);
            unify_types_args(new_tup, t2)?
        }
        (Type::Tuple(_), Type::Record(_)) => unify_types_args(t2, t1)?,
        // Union type in argument position: check if actual type can be part of the union
        (Type::Union(union_types), _t) => {
            // Expected type is a union, actual type should match one of the union members
            for union_member in union_types {
                if unify_types_args(*union_member, t2r).is_ok() {
                    // Treat as identical when the actual type matches a union member
                    return Ok(Relation::Identical);
                }
            }
            return Err(vec![Error::TypeMismatch {
                left: t1,
                right: t2,
            }]);
        }
        (_, _) => unify_types(t1, t2)?,
    };
    Ok(res)
}

/// Solve type constraints. Though the function arguments are immutable, it modified the content of Intermediate Type.
/// If the result is `Relation::Subtype`, it means "t1 is subtype of t2".
pub(crate) fn unify_types(t1: TypeNodeId, t2: TypeNodeId) -> Result<Relation, Vec<Error>> {
    let loc1 = t1.to_span(); //todo file
    let loc2 = t1.to_span();

    let t1r = t1.get_root();
    let t2r = t2.get_root();
    let res = match &(t1r.to_type(), t2r.to_type()) {
        (Type::Intermediate(i1), Type::Intermediate(i2)) => {
            if *i1.read().unwrap() == *i2.read().unwrap() {
                return Ok(Relation::Identical);
            }
            if occur_check(i1.read().unwrap().var, t2) {
                return Err(vec![Error::CircularType {
                    left: loc1,
                    right: loc2,
                }]);
            }
            let (level1, level2) = (i1.read().unwrap().level, i2.read().unwrap().level);
            if level1 < level2 {
                i1.write().unwrap().level = level2;
            }
            let parent1 = i1.read().unwrap().parent;
            let parent2 = i2.read().unwrap().parent;
            match (parent1, parent2) {
                (None, None) => {
                    let (var1, var2) = (i1.read().unwrap().var, i2.read().unwrap().var);
                    if var1 > var2 {
                        i2.write().unwrap().parent = Some(t1r);
                    } else {
                        i1.write().unwrap().parent = Some(t2r);
                    };
                }
                (_, Some(p2)) => {
                    i1.write().unwrap().parent = Some(p2);
                }
                (Some(p1), _) => {
                    i2.write().unwrap().parent = Some(p1);
                }
            };
            Relation::Identical
        }
        (Type::Intermediate(i1), _) => {
            let mut tv1 = i1.write().unwrap();
            tv1.parent = Some(t2r);
            tv1.bound.lower = t2r;

            Relation::Identical
        }
        (_, Type::Intermediate(i2)) => {
            let mut tv2 = i2.write().unwrap();
            tv2.parent = Some(t1r);
            tv2.bound.lower = t1r;
            Relation::Identical
        }
        (Type::Array(a1), Type::Array(a2)) => {
            //theoriticaly, the array type can be covariant but it makes implementation complex and might be not intuitive for beginners.
            let res = unify_types(*a1, *a2)?;
            match res {
                Relation::Identical => Relation::Identical,
                _ => {
                    return Err(vec![Error::TypeMismatch {
                        left: *a1,
                        right: *a2,
                    }]);
                }
            }
        }
        (Type::Ref(x1), Type::Ref(x2)) => unify_types(*x1, *x2)?,
        (Type::Tuple(a1), Type::Tuple(a2)) => {
            // if a1 have nth elements, a2 must have at least n, or more elements.
            // but for simplicity, currently the tuple length must be identical.
            use std::cmp::Ordering;
            match a1.len().cmp(&a2.len()) {
                Ordering::Equal => {
                    let _ = unify_vec(a1, a2)?;
                    //todo covariance
                    Relation::Identical
                }
                // Ordering::Less => {
                //     let _ = unify_vec(a1, &a2[0..a1.len()])?;
                //     Relation::Subtype
                // }
                // Ordering::Greater => {
                //     let _ = unify_vec(&a1[0..a2.len()], a2)?;
                //     Relation::Supertype
                // }
                _ => {
                    return Err(vec![Error::LengthMismatch {
                        left: (a1.to_vec(), loc1),
                        right: (a2.to_vec(), loc2),
                    }]);
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

            let keys_a = a1.iter().sorted_by(move |a, b| {
                let keya = a.key;
                let keyb = b.key;
                keya.as_str().cmp(keyb.as_str())
            });
            let keys_b = a2.iter().sorted_by(move |a, b| {
                let keya = a.key;
                let keyb = b.key;
                keya.as_str().cmp(keyb.as_str())
            });
            let allkeys = keys_a
                .clone()
                .chain(keys_b.clone())
                .unique_by(|RecordTypeField { key, .. }| key.as_str());
            let sparse_fields1 = allkeys.clone().map(|parent| {
                a1.iter()
                    .find(|RecordTypeField { key, .. }| parent.key == *key)
                    .or(parent.has_default.then_some(parent))
            });
            let sparse_fields2 = allkeys.map(|parent| {
                a2.iter()
                    .find(|RecordTypeField { key, .. }| parent.key == *key)
                    .or(parent.has_default.then_some(parent))
            });
            #[derive(PartialEq, Eq, Debug)]
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
            log::trace!(
                "unify_records {} and {}: {:?}",
                t1,
                t2,
                searchresults.clone().collect_vec()
            );
            let all_both = searchresults
                .clone()
                .all(|r| r.is_ok_and(|r| r == SearchRes::Both));
            let collected_errs = searchresults
                .clone()
                .filter_map(|r| r.err())
                .flatten()
                .collect::<Vec<_>>();
            let mut all_errs = vec![];
            let contains_err = !collected_errs.is_empty();
            if contains_err {
                all_errs = collected_errs;
            }
            let contains_a = searchresults
                .clone()
                .any(|r| r.is_ok_and(|r| r == SearchRes::A));
            let contains_b = searchresults
                .clone()
                .any(|r| r.is_ok_and(|r| r == SearchRes::B));
            if all_both {
                Relation::Identical
            } else if !contains_err && contains_a && !contains_b {
                //a has more fields than b, that means A is
                Relation::Supertype
            } else if !contains_err && contains_b && !contains_a {
                Relation::Subtype
            } else if contains_b && contains_a {
                let keys_a = a1
                    .iter()
                    .map(|RecordTypeField { key, ty, .. }| (*key, *ty))
                    .collect::<Vec<_>>();
                let keys_b = a2
                    .iter()
                    .map(|RecordTypeField { key, ty, .. }| (*key, *ty))
                    .collect::<Vec<_>>();
                all_errs.push(Error::ImcompatibleRecords {
                    left: (keys_a, t1.to_span()),
                    right: (keys_b, t2.to_span()),
                });
                return Err(all_errs);
            } else {
                return Err(all_errs);
            }
        }
        (
            Type::Function {
                arg: arg1,
                ret: ret1,
            },
            Type::Function {
                arg: arg2,
                ret: ret2,
            },
        ) => {
            let arg_res = unify_types_args(*arg1, *arg2);
            let ret_res = unify_types(*ret1, *ret2);
            match (arg_res, ret_res) {
                (Ok(Relation::Subtype), Ok(_)) | (Ok(_), Ok(Relation::Supertype)) => {
                    return Err(vec![Error::TypeMismatch {
                        left: t1,
                        right: t2,
                    }]);
                }
                (Ok(Relation::Identical), Ok(Relation::Identical)) => Relation::Identical,
                (Ok(_), Err(errs)) | (Err(errs), Ok(_)) => {
                    return Err(errs);
                }
                (Err(mut e1), Err(mut e2)) => {
                    e1.append(&mut e2);
                    return Err(e1);
                }
                _ => Relation::Subtype,
            }
        }
        (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => Relation::Identical,
        (Type::Primitive(PType::Unit), Type::Tuple(v))
        | (Type::Tuple(v), Type::Primitive(PType::Unit))
            if v.is_empty() =>
        {
            Relation::Identical
        }
        (_t, Type::Tuple(v)) if v.len() == 1 => unify_types(t1, *v.first().unwrap())?,
        (Type::Tuple(v), _t) if v.len() == 1 => unify_types(*v.first().unwrap(), t2)?,
        (Type::Primitive(PType::Unit), Type::Record(v))
        | (Type::Record(v), Type::Primitive(PType::Unit))
            if v.is_empty() =>
        {
            Relation::Identical
        }
        (_t, Type::Record(v)) if v.len() == 1 => unify_types(t1, v.first().unwrap().ty)?,
        (Type::Record(v), _t) if v.len() == 1 => unify_types(v.first().unwrap().ty, t2)?,

        (Type::Failure, _t) | (_t, Type::Any) => Relation::Identical,
        (Type::Any, _t) | (_t, Type::Failure) => Relation::Identical,

        (Type::Code(p1), Type::Code(p2)) => unify_types(*p1, *p2)?,
        // Union type support: check if two union types are identical
        (Type::Union(union_types1), Type::Union(union_types2)) => {
            // Two unions are identical if they have the same members (in any order)
            if union_types1.len() != union_types2.len() {
                return Err(vec![Error::TypeMismatch {
                    left: t1,
                    right: t2,
                }]);
            }
            // Check that each member of union1 matches exactly one member of union2
            let all_match = union_types1.iter().all(|m1| {
                union_types2
                    .iter()
                    .any(|m2| unify_types(*m1, *m2).is_ok_and(|r| r == Relation::Identical))
            });
            if all_match {
                Relation::Identical
            } else {
                return Err(vec![Error::TypeMismatch {
                    left: t1,
                    right: t2,
                }]);
            }
        }
        // Union type support: T is a subtype of T | U if T can unify with one of the union members
        (_t, Type::Union(union_types)) => {
            // Check if t can unify with any type in the union
            for union_member in union_types {
                if unify_types(t1r, *union_member).is_ok() {
                    return Ok(Relation::Subtype);
                }
            }
            return Err(vec![Error::TypeMismatch {
                left: t1,
                right: t2,
            }]);
        }
        (Type::Union(union_types), _t) => {
            // Check if all types in the union can unify with t
            // Union is subtype of t only if all members are subtypes
            let all_match = union_types
                .iter()
                .all(|union_member| unify_types(*union_member, t2r).is_ok());
            if all_match {
                return Ok(Relation::Supertype);
            }
            return Err(vec![Error::TypeMismatch {
                left: t1,
                right: t2,
            }]);
        }
        // UserSum type support: same UserSum types are identical
        (
            Type::UserSum {
                name: n1,
                variants: v1,
            },
            Type::UserSum {
                name: n2,
                variants: v2,
            },
        ) => {
            if n1 == n2 && v1 == v2 {
                Relation::Identical
            } else {
                return Err(vec![Error::TypeMismatch {
                    left: t1,
                    right: t2,
                }]);
            }
        }
        (_p1, _p2) => {
            return Err(vec![Error::TypeMismatch {
                left: t1,
                right: t2,
            }]);
        }
    };
    log::trace!("unified {} and {}:{:?}", t1.to_type(), t2.to_type(), res);

    Ok(res)
}
