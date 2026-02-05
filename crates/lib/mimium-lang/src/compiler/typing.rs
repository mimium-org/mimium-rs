use crate::ast::{Expr, Literal, RecordField};
use crate::compiler::{EvalStage, intrinsics};
use crate::interner::{ExprKey, ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedPattern};
use crate::types::{IntermediateId, PType, RecordTypeField, Type, TypeSchemeId, TypeVar};
use crate::utils::metadata::Location;
use crate::utils::{environment::Environment, error::ReportableError};
use crate::{function, integer, numeric, unit};
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

mod unification;
pub(crate) use unification::Relation;
use unification::{Error as UnificationError, unify_types};

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
    NonSupertypeArgument {
        location: Location,
        expected: TypeNodeId,
        found: TypeNodeId,
    },
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
    /// Module not found in the current scope
    ModuleNotFound {
        module_path: Vec<Symbol>,
        location: Location,
    },
    /// Member not found in a module
    MemberNotFound {
        module_path: Vec<Symbol>,
        member: Symbol,
        location: Location,
    },
    /// Attempted to access a private module member
    PrivateMemberAccess {
        module_path: Vec<Symbol>,
        member: Symbol,
        location: Location,
    },
    StageMismatch {
        variable: Symbol,
        expected_stage: EvalStage,
        found_stage: EvalStage,
        location: Location,
    },
    NonPrimitiveInFeed(Location),
    /// Constructor pattern doesn't match any variant of the union type
    ConstructorNotInUnion {
        constructor: Symbol,
        union_type: TypeNodeId,
        location: Location,
    },
    /// Expected a union type for constructor pattern matching
    ExpectedUnionType {
        found: TypeNodeId,
        location: Location,
    },
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
                format!("Variable \"{symbol}\" not found in this scope")
            }
            Error::ModuleNotFound { module_path, .. } => {
                let path_str = module_path
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                format!("Module \"{path_str}\" not found")
            }
            Error::MemberNotFound {
                module_path,
                member,
                ..
            } => {
                let path_str = module_path
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                format!("Member \"{member}\" not found in module \"{path_str}\"")
            }
            Error::PrivateMemberAccess {
                module_path,
                member,
                ..
            } => {
                let path_str = module_path
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                format!("Member \"{member}\" in module \"{path_str}\" is private")
            }
            Error::StageMismatch {
                variable,
                expected_stage,
                found_stage,
                ..
            } => {
                format!(
                    "Variable {variable} is defined in stage {} but accessed from stage {}",
                    found_stage.format_for_error(),
                    expected_stage.format_for_error()
                )
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
                format!("Field \"{field}\" does not exist in the record type")
            }
            Error::IncompatibleKeyInRecord { .. } => {
                format!("Record type has incompatible keys.",)
            }

            Error::NonSupertypeArgument { .. } => {
                format!("Arguments for functions are less than required.")
            }
            Error::ConstructorNotInUnion { constructor, .. } => {
                format!("Constructor \"{constructor}\" is not a variant of the union type")
            }
            Error::ExpectedUnionType { found, .. } => {
                format!(
                    "Expected a union type but found {}",
                    found.to_type().to_string_for_error()
                )
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
            Error::ModuleNotFound {
                module_path,
                location,
            } => {
                let path_str = module_path
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                vec![(location.clone(), format!("Module \"{path_str}\" not found"))]
            }
            Error::MemberNotFound {
                module_path,
                member,
                location,
            } => {
                let path_str = module_path
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                vec![(
                    location.clone(),
                    format!("\"{member}\" is not a member of \"{path_str}\""),
                )]
            }
            Error::PrivateMemberAccess {
                module_path,
                member,
                location,
            } => {
                let path_str = module_path
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                vec![(
                    location.clone(),
                    format!("\"{member}\" in \"{path_str}\" is private and cannot be accessed"),
                )]
            }
            Error::StageMismatch {
                variable,
                expected_stage,
                found_stage,
                location,
            } => {
                vec![(
                    location.clone(),
                    format!(
                        "Variable \"{variable}\" defined in stage {} cannot be accessed from stage {}",
                        found_stage.format_for_error(),
                        expected_stage.format_for_error()
                    ),
                )]
            }
            Error::NonPrimitiveInFeed(loc) => {
                vec![(loc.clone(), format!("This cannot be function type."))]
            }
            Error::DuplicateKeyInRecord { key, loc } => {
                vec![(
                    loc.clone(),
                    format!(
                        "Duplicate keys \"{}\" found in record type",
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
                    "Field \"{}\" does not exist in the type {}",
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
                            "but the record here contains {}",
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

            Error::NonSupertypeArgument {
                location,
                expected,
                found,
            } => {
                vec![(
                    location.clone(),
                    format!(
                        "Type {} is not a supertype of the expected type {}",
                        found.to_type().to_string_for_error(),
                        expected.to_type().to_string_for_error()
                    ),
                )]
            }
            Error::ConstructorNotInUnion {
                constructor,
                union_type,
                location,
            } => {
                vec![(
                    location.clone(),
                    format!(
                        "Constructor \"{constructor}\" is not a variant of {}",
                        union_type.to_type().to_string_for_error()
                    ),
                )]
            }
            Error::ExpectedUnionType { found, location } => {
                vec![(
                    location.clone(),
                    format!(
                        "Expected a union type but found {}",
                        found.to_type().to_string_for_error()
                    ),
                )]
            }
        }
    }
}

/// Information about a constructor in a user-defined sum type
#[derive(Clone, Debug)]
pub struct ConstructorInfo {
    /// The type of the sum type this constructor belongs to
    pub sum_type: TypeNodeId,
    /// The index (tag) of this constructor in the sum type
    pub tag_index: usize,
    /// Optional payload type for this constructor
    pub payload_type: Option<TypeNodeId>,
}

/// Map from constructor name to its info
pub type ConstructorEnv = HashMap<Symbol, ConstructorInfo>;

#[derive(Clone, Debug)]
pub struct InferContext {
    interm_idx: IntermediateId,
    typescheme_idx: TypeSchemeId,
    level: u64,
    stage: EvalStage,
    instantiated_map: BTreeMap<TypeSchemeId, TypeNodeId>, //from type scheme to typevar
    generalize_map: BTreeMap<IntermediateId, TypeSchemeId>,
    result_memo: BTreeMap<ExprKey, TypeNodeId>,
    file_path: PathBuf,
    pub env: Environment<(TypeNodeId, EvalStage)>,
    /// Constructor environment for user-defined sum types
    pub constructor_env: ConstructorEnv,
    pub errors: Vec<Error>,
}
impl InferContext {
    pub fn new(
        builtins: &[(Symbol, TypeNodeId)],
        file_path: PathBuf,
        type_declarations: Option<&crate::ast::program::TypeDeclarationMap>,
    ) -> Self {
        let mut res = Self {
            interm_idx: Default::default(),
            typescheme_idx: Default::default(),
            level: Default::default(),
            stage: EvalStage::Stage(0), // Start at stage 0
            instantiated_map: Default::default(),
            generalize_map: Default::default(),
            result_memo: Default::default(),
            file_path,
            env: Environment::<(TypeNodeId, EvalStage)>::default(),
            constructor_env: Default::default(),
            errors: Default::default(),
        };
        res.env.extend();
        // Intrinsic types are persistent (available at all stages)
        let intrinsics = Self::intrinsic_types()
            .into_iter()
            .map(|(name, ty)| (name, (ty, EvalStage::Persistent)))
            .collect::<Vec<_>>();
        res.env.add_bind(&intrinsics);
        // Builtins are also persistent
        let builtins = builtins
            .iter()
            .map(|(name, ty)| (*name, (*ty, EvalStage::Persistent)))
            .collect::<Vec<_>>();
        res.env.add_bind(&builtins);
        // Register user-defined type constructors
        if let Some(type_decls) = type_declarations {
            res.register_type_declarations(type_decls);
        }
        res
    }

    /// Register type declarations from ModuleInfo into the constructor environment
    fn register_type_declarations(
        &mut self,
        type_declarations: &crate::ast::program::TypeDeclarationMap,
    ) {
        for (type_name, variants) in type_declarations {
            // Create the UserSum type
            let variant_names: Vec<Symbol> = variants.iter().map(|v| v.name).collect();
            let sum_type = Type::UserSum {
                name: *type_name,
                variants: variant_names.clone(),
            }
            .into_id();

            // Register each constructor
            for (tag_index, variant) in variants.iter().enumerate() {
                self.constructor_env.insert(
                    variant.name,
                    ConstructorInfo {
                        sum_type,
                        tag_index,
                        payload_type: variant.payload,
                    },
                );
            }
        }
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

    /// Get the type associated with a constructor name from a union or user-defined sum type
    /// For primitive types in unions like `float | string`, the constructor names are "float" and "string"
    /// For user-defined sum types, returns Unit for payloadless constructors
    fn get_constructor_type_from_union(
        &self,
        union_ty: TypeNodeId,
        constructor_name: Symbol,
    ) -> TypeNodeId {
        let resolved = Self::substitute_type(union_ty);
        match resolved.to_type() {
            Type::Union(variants) => {
                // Find a variant that matches the constructor name
                for variant_ty in variants.iter() {
                    let variant_resolved = Self::substitute_type(*variant_ty);
                    let variant_name = Self::type_constructor_name(&variant_resolved.to_type());
                    if variant_name == Some(constructor_name) {
                        return *variant_ty;
                    }
                }
                // Constructor not found in union - return Unknown as placeholder
                Type::Unknown.into_id_with_location(union_ty.to_loc())
            }
            Type::UserSum { name: _, variants } => {
                // Check if constructor_name is one of the variants
                if variants.contains(&constructor_name) {
                    // Return the payload type if available, otherwise Unit
                    if let Some(constructor_info) = self.constructor_env.get(&constructor_name) {
                        constructor_info.payload_type.unwrap_or_else(|| unit!())
                    } else {
                        unit!()
                    }
                } else {
                    Type::Unknown.into_id_with_location(union_ty.to_loc())
                }
            }
            // If not a union type, check if it matches the constructor directly
            other => {
                let type_name = Self::type_constructor_name(&other);
                if type_name == Some(constructor_name) {
                    resolved
                } else {
                    Type::Unknown.into_id_with_location(union_ty.to_loc())
                }
            }
        }
    }

    /// Get the constructor name for a type (used for matching in union types)
    /// Primitive types use their type name as constructor (e.g., "float", "string")
    fn type_constructor_name(ty: &Type) -> Option<Symbol> {
        match ty {
            Type::Primitive(PType::Numeric) => Some("float".to_symbol()),
            Type::Primitive(PType::String) => Some("string".to_symbol()),
            Type::Primitive(PType::Int) => Some("int".to_symbol()),
            Type::Primitive(PType::Unit) => Some("unit".to_symbol()),
            // For other types, we don't have built-in constructor names yet
            _ => None,
        }
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
    fn get_typescheme(&mut self, tvid: IntermediateId, loc: Location) -> TypeNodeId {
        self.generalize_map.get(&tvid).cloned().map_or_else(
            || self.gen_typescheme(loc),
            |id| Type::TypeScheme(id).into_id(),
        )
    }
    fn gen_typescheme(&mut self, loc: Location) -> TypeNodeId {
        let res = Type::TypeScheme(self.typescheme_idx).into_id_with_location(loc);
        self.typescheme_idx.0 += 1;
        res
    }

    fn gen_intermediate_type_with_location(&mut self, loc: Location) -> TypeNodeId {
        let res = Type::Intermediate(Arc::new(RwLock::new(TypeVar::new(
            self.interm_idx,
            self.level,
        ))))
        .into_id_with_location(loc);
        self.interm_idx.0 += 1;
        res
    }
    fn convert_unknown_to_intermediate(&mut self, t: TypeNodeId, loc: Location) -> TypeNodeId {
        match t.to_type() {
            Type::Unknown => self.gen_intermediate_type_with_location(loc.clone()),
            _ => t.apply_fn(|t| self.convert_unknown_to_intermediate(t, loc.clone())),
        }
    }
    fn convert_unify_error(&self, e: UnificationError) -> Error {
        let gen_loc = |span| Location::new(span, self.file_path.clone());
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
            (Ok(Relation::Identical), Ok(Relation::Identical)) => Ok(()),
            (Ok(_), Ok(_)) => Err(vec![Error::TypeMismatch {
                left: (t1, Location::new(t1.to_span(), self.file_path.clone())),
                right: (t2, Location::new(t2.to_span(), self.file_path.clone())),
            }]),
            (Err(e1), Err(e2)) => Err(e1.into_iter().chain(e2).collect()),
            (Err(e), _) | (_, Err(e)) => Err(e),
        }
    }
    pub fn substitute_type(t: TypeNodeId) -> TypeNodeId {
        match t.to_type() {
            Type::Intermediate(cell) => {
                let TypeVar { parent, .. } = &*cell.read().unwrap() as &TypeVar;
                match parent {
                    Some(p) => Self::substitute_type(*p),
                    None => Type::Unknown.into_id_with_location(t.to_loc()),
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
                let &TypeVar { level, var, .. } = &*tvar.read().unwrap() as &TypeVar;
                if level > self.level {
                    self.get_typescheme(var, t.to_loc())
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
                log::debug!("instantiate typescheme id: {id:?}");
                if let Some(tvar) = self.instantiated_map.get(&id) {
                    *tvar
                } else {
                    let res = self.gen_intermediate_type_with_location(t.to_loc());
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
        let (TypedPattern { pat, ty, .. }, loc_p) = pat;
        let (body_t, loc_b) = body.clone();
        let mut bind_item = |pat| {
            let newloc = ty.to_loc();
            let ity = self.gen_intermediate_type_with_location(newloc.clone());
            let p = TypedPattern::new(pat, ity);
            self.bind_pattern((p, newloc.clone()), (ity, newloc))
        };
        let pat_t = match pat {
            Pattern::Single(id) => {
                let pat_t = self.convert_unknown_to_intermediate(ty, loc_p);
                log::trace!("bind {} : {}", id, pat_t.to_type());
                self.env.add_bind(&[(id, (pat_t, self.stage))]);
                Ok::<TypeNodeId, Vec<Error>>(pat_t)
            }
            Pattern::Placeholder => {
                // Placeholder doesn't bind anything, just check the type
                let pat_t = self.convert_unknown_to_intermediate(ty, loc_p);
                log::trace!("bind _ (placeholder) : {}", pat_t.to_type());
                Ok::<TypeNodeId, Vec<Error>>(pat_t)
            }
            Pattern::Tuple(pats) => {
                let elems = pats.iter().map(|p| bind_item(p.clone())).try_collect()?; //todo multiple errors
                let res = Type::Tuple(elems).into_id_with_location(loc_p);
                let target = self.convert_unknown_to_intermediate(ty, loc_b);
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
                let res = Type::Record(res).into_id_with_location(loc_p);
                let target = self.convert_unknown_to_intermediate(ty, loc_b);
                let rel = self.unify_types(res, target)?;
                Ok(res)
            }
            Pattern::Error => Err(vec![Error::PatternMismatch(
                (
                    Type::Failure.into_id_with_location(loc_p.clone()),
                    loc_b.clone(),
                ),
                (pat, loc_p.clone()),
            )]),
        }?;
        let rel = self.unify_types(pat_t, body_t)?;
        Ok(self.generalize(pat_t))
    }

    pub fn lookup(&self, name: Symbol, loc: Location) -> Result<TypeNodeId, Error> {
        use crate::utils::environment::LookupRes;
        let lookup_res = self.env.lookup_cls(&name);
        match lookup_res {
            LookupRes::Local((ty, bound_stage)) if self.stage == *bound_stage => Ok(*ty),
            LookupRes::UpValue(_, (ty, bound_stage)) if self.stage == *bound_stage => Ok(*ty),
            LookupRes::Global((ty, bound_stage))
                if self.stage == *bound_stage || *bound_stage == EvalStage::Persistent =>
            {
                Ok(*ty)
            }
            LookupRes::None => Err(Error::VariableNotFound(name, loc)),
            LookupRes::Local((_, bound_stage))
            | LookupRes::UpValue(_, (_, bound_stage))
            | LookupRes::Global((_, bound_stage)) => Err(Error::StageMismatch {
                variable: name,
                expected_stage: self.stage,
                found_stage: *bound_stage,
                location: loc,
            }),
        }
    }
    pub(crate) fn infer_type_literal(e: &Literal, loc: Location) -> Result<TypeNodeId, Error> {
        let pt = match e {
            Literal::Float(_) | Literal::Now | Literal::SampleRate => PType::Numeric,
            Literal::Int(_s) => PType::Int,
            Literal::String(_s) => PType::String,
            Literal::SelfLit => panic!("\"self\" should not be shown at type inference stage"),
            Literal::PlaceHolder => panic!("\"_\" should not be shown at type inference stage"),
        };
        Ok(Type::Primitive(pt).into_id_with_location(loc))
    }
    fn infer_vec(&mut self, e: &[ExprNodeId]) -> Result<Vec<TypeNodeId>, Vec<Error>> {
        e.iter().map(|e| self.infer_type(*e)).try_collect()
    }
    fn infer_type_levelup(&mut self, e: ExprNodeId) -> TypeNodeId {
        self.level += 1;
        let res = self.infer_type_unwrapping(e);
        self.level -= 1;
        res
    }
    pub fn infer_type(&mut self, e: ExprNodeId) -> Result<TypeNodeId, Vec<Error>> {
        if let Some(r) = self.result_memo.get(&e.0) {
            //use cached result
            return Ok(*r);
        }
        let loc = e.to_location();
        let res: Result<TypeNodeId, Vec<Error>> = match &e.to_expr() {
            Expr::Literal(l) => Self::infer_type_literal(l, loc).map_err(|e| vec![e]),
            Expr::Tuple(e) => {
                Ok(Type::Tuple(self.infer_vec(e.as_slice())?).into_id_with_location(loc))
            }
            Expr::ArrayLiteral(e) => {
                let elem_types = self.infer_vec(e.as_slice())?;
                let first = elem_types
                    .first()
                    .copied()
                    .unwrap_or(Type::Unknown.into_id_with_location(loc.clone()));
                //todo:collect multiple errors
                let elem_t = elem_types
                    .iter()
                    .try_fold(first, |acc, t| self.unify_types(acc, *t).map(|rel| *t))?;

                Ok(Type::Array(elem_t).into_id_with_location(loc.clone()))
            }
            Expr::ArrayAccess(e, idx) => {
                let arr_t = self.infer_type_unwrapping(*e);
                let loc_e = e.to_location();
                let idx_t = self.infer_type_unwrapping(*idx);
                let loc_i = idx.to_location();

                let elem_t = self.gen_intermediate_type_with_location(loc_e.clone());

                let rel1 = self.unify_types(
                    idx_t,
                    Type::Primitive(PType::Numeric).into_id_with_location(loc_i),
                );
                let rel2 = self.unify_types(
                    Type::Array(elem_t).into_id_with_location(loc_e.clone()),
                    arr_t,
                );
                self.merge_rel_result(rel1, rel2, arr_t, idx_t)?;
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
                        let tv = tv.read().unwrap();
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
                    Ok(Type::Record(kts).into_id_with_location(loc))
                }
            }
            Expr::RecordUpdate(_, _) => {
                // RecordUpdate should never reach type inference as it gets expanded
                // to Block/Let/Assign expressions during syntax sugar conversion in convert_pronoun.rs
                unreachable!("RecordUpdate should be expanded before type inference")
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
                        let tv = tv.read().unwrap();
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
                let feedv = self.gen_intermediate_type_with_location(loc);

                self.env.add_bind(&[(*id, (feedv, self.stage))]);
                let bty = self.infer_type_unwrapping(*body);
                let _rel = self.unify_types(bty, feedv)?;
                if bty.to_type().contains_function() {
                    Err(vec![Error::NonPrimitiveInFeed(body.to_location())])
                } else {
                    Ok(bty)
                }
            }
            Expr::Lambda(p, rtype, body) => {
                self.env.extend();
                let dup = p.iter().duplicates_by(|id| id.id).map(|id| {
                    let loc = Location::new(id.to_span(), self.file_path.clone());
                    (id.id, loc)
                });
                if dup.clone().count() > 0 {
                    return Err(vec![Error::DuplicateKeyInParams(dup.collect())]);
                }
                let pvec = p
                    .iter()
                    .map(|id| {
                        let ity = self.convert_unknown_to_intermediate(id.ty, id.ty.to_loc());
                        self.env.add_bind(&[(id.id, (ity, self.stage))]);
                        RecordTypeField {
                            key: id.id,
                            ty: ity,
                            has_default: false,
                        }
                    })
                    .collect::<Vec<_>>();
                let ptype = if pvec.is_empty() {
                    Type::Primitive(PType::Unit).into_id_with_location(loc.clone())
                } else {
                    Type::Record(pvec).into_id_with_location(loc.clone())
                };
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

                let loc_p = tpat.to_loc();
                let loc_b = body.to_location();
                let pat_t = self.bind_pattern((tpat.clone(), loc_p), (bodyt, loc_b));
                let _pat_t = self.unwrap_result(pat_t);
                match then {
                    Some(e) => self.infer_type(*e),
                    None => Ok(Type::Primitive(PType::Unit).into_id_with_location(loc)),
                }
            }
            Expr::LetRec(id, body, then) => {
                let idt = self.convert_unknown_to_intermediate(id.ty, id.ty.to_loc());
                self.env.add_bind(&[(id.id, (idt, self.stage))]);
                //polymorphic inference is not allowed in recursive function.

                let bodyt = self.infer_type_levelup(*body);

                let _res = self.unify_types(idt, bodyt);
                match then {
                    Some(e) => self.infer_type(*e),
                    None => Ok(Type::Primitive(PType::Unit).into_id_with_location(loc)),
                }
            }
            Expr::Assign(assignee, expr) => {
                match assignee.to_expr() {
                    Expr::Var(name) => {
                        let assignee_t =
                            self.unwrap_result(self.lookup(name, loc).map_err(|e| vec![e]));
                        let e_t = self.infer_type_unwrapping(*expr);
                        let _rel = self.unify_types(assignee_t, e_t)?;
                        Ok(unit!())
                    }
                    Expr::FieldAccess(record, field_name) => {
                        // Handle field assignment: record.field = value
                        let record_type = self.infer_type_unwrapping(record);
                        let value_type = self.infer_type_unwrapping(*expr);
                        let tmptype = Type::Record(vec![RecordTypeField {
                            key: field_name,
                            ty: value_type,
                            has_default: false,
                        }])
                        .into_id();
                        if self.unify_types(record_type, tmptype)? == Relation::Supertype {
                            unreachable!(
                                "record field access for an empty record will not likely to happen."
                            )
                        };
                        Ok(value_type)
                    }
                    Expr::ArrayAccess(_, _) => {
                        unimplemented!("Assignment to array is not implemented yet.")
                    }
                    _ => {
                        // This should be caught by parser, but add a generic error just in case
                        Err(vec![Error::VariableNotFound(
                            "invalid_assignment_target".to_symbol(),
                            loc.clone(),
                        )])
                    }
                }
            }
            Expr::Then(e, then) => {
                let _ = self.infer_type(*e)?;
                then.map_or(Ok(unit!()), |t| self.infer_type(t))
            }
            Expr::Var(name) => {
                // First check if this is a constructor from a user-defined sum type
                if let Some(constructor_info) = self.constructor_env.get(name) {
                    // Return the sum type for this constructor
                    return Ok(constructor_info.sum_type);
                }
                // Aliases and wildcards are already resolved by convert_qualified_names
                let res = self.unwrap_result(self.lookup(*name, loc).map_err(|e| vec![e]));
                Ok(self.instantiate(res))
            }
            Expr::QualifiedVar(path) => {
                unreachable!("Qualified Var should be removed in the previous step.")
            }
            Expr::Apply(fun, callee) => {
                let loc_f = fun.to_location();
                let fnl = self.infer_type_unwrapping(*fun);
                let callee_t = match callee.len() {
                    0 => Type::Primitive(PType::Unit).into_id_with_location(loc.clone()),
                    1 => self.infer_type_unwrapping(callee[0]),
                    _ => {
                        let at_vec = self.infer_vec(callee.as_slice())?;
                        let span = callee[0].to_span().start..callee.last().unwrap().to_span().end;
                        let loc = Location::new(span, self.file_path.clone());
                        Type::Tuple(at_vec).into_id_with_location(loc)
                    }
                };
                let res_t = self.gen_intermediate_type_with_location(loc);
                let fntype = Type::Function {
                    arg: callee_t,
                    ret: res_t,
                }
                .into_id_with_location(loc_f.clone());
                match self.unify_types(fnl, fntype)? {
                    Relation::Subtype => Err(vec![Error::NonSupertypeArgument {
                        location: loc_f.clone(),
                        expected: fnl,
                        found: fntype,
                    }]),
                    _ => Ok(res_t),
                }
            }
            Expr::If(cond, then, opt_else) => {
                let condt = self.infer_type_unwrapping(*cond);
                let cond_loc = cond.to_location();
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
            Expr::Block(expr) => expr.map_or(
                Ok(Type::Primitive(PType::Unit).into_id_with_location(loc)),
                |e| {
                    self.env.extend(); //block creates local scope.
                    let res = self.infer_type(e);
                    self.env.to_outer();
                    res
                },
            ),
            Expr::Escape(e) => {
                let loc_e = Location::new(e.to_span(), self.file_path.clone());
                // Decrease stage for escape expression
                self.stage = self.stage.decrement();
                log::trace!("Unstaging escape expression, stage => {:?}", self.stage);
                let res = self.infer_type_unwrapping(*e);
                // Increase stage back
                self.stage = self.stage.increment();
                let intermediate = self.gen_intermediate_type_with_location(loc_e.clone());
                let rel = self.unify_types(
                    res,
                    Type::Code(intermediate).into_id_with_location(loc_e.clone()),
                )?;
                Ok(intermediate)
            }
            Expr::Bracket(e) => {
                let loc_e = Location::new(e.to_span(), self.file_path.clone());
                // Increase stage for bracket expression
                self.stage = self.stage.increment();
                log::trace!("Staging bracket expression, stage => {:?}", self.stage);
                let res = self.infer_type_unwrapping(*e);
                // Decrease stage back
                self.stage = self.stage.decrement();
                Ok(Type::Code(res).into_id_with_location(loc_e))
            }
            Expr::Match(scrutinee, arms) => {
                // Infer type of scrutinee
                let scrut_ty = self.infer_type_unwrapping(*scrutinee);

                // Infer types of all arm bodies, handling patterns with variable bindings
                let arm_tys: Vec<TypeNodeId> = arms
                    .iter()
                    .map(|arm| {
                        match &arm.pattern {
                            crate::ast::MatchPattern::Literal(lit) => {
                                // For numeric patterns, check scrutinee is numeric
                                let pat_ty = match lit {
                                    crate::ast::Literal::Int(_) | crate::ast::Literal::Float(_) => {
                                        Type::Primitive(PType::Numeric)
                                            .into_id_with_location(loc.clone())
                                    }
                                    _ => Type::Failure.into_id_with_location(loc.clone()),
                                };
                                let _ = self.unify_types(scrut_ty, pat_ty);
                                self.infer_type_unwrapping(arm.body)
                            }
                            crate::ast::MatchPattern::Wildcard => {
                                // Wildcard matches anything
                                self.infer_type_unwrapping(arm.body)
                            }
                            crate::ast::MatchPattern::Constructor(constructor_name, binding) => {
                                // Handle constructor patterns for union types
                                // Find the type associated with this constructor in the union
                                let binding_ty = self
                                    .get_constructor_type_from_union(scrut_ty, *constructor_name);

                                if let Some(bound_var) = binding {
                                    // Create a new scope with the binding variable
                                    self.env.extend();
                                    self.env.add_bind(&[(
                                        *bound_var,
                                        (binding_ty, self.stage.clone()),
                                    )]);
                                    let body_ty = self.infer_type_unwrapping(arm.body);
                                    self.env.to_outer();
                                    body_ty
                                } else {
                                    self.infer_type_unwrapping(arm.body)
                                }
                            }
                        }
                    })
                    .collect();

                if arm_tys.is_empty() {
                    Ok(Type::Primitive(PType::Unit).into_id_with_location(loc))
                } else {
                    let first = arm_tys[0];
                    for ty in arm_tys.iter().skip(1) {
                        let _ = self.unify_types(first, *ty);
                    }
                    Ok(first)
                }
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
                Type::Failure
                    .into_id_with_location(Location::new(e.to_span(), self.file_path.clone()))
            }
        }
    }
}

pub fn infer_root(
    e: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    file_path: PathBuf,
    type_declarations: Option<&crate::ast::program::TypeDeclarationMap>,
) -> InferContext {
    let mut ctx = InferContext::new(builtin_types, file_path.clone(), type_declarations);
    let _t = ctx
        .infer_type(e)
        .unwrap_or(Type::Failure.into_id_with_location(e.to_location()));
    ctx.substitute_all_intermediates();
    ctx
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interner::ToSymbol;
    use crate::types::Type;
    use crate::utils::metadata::{Location, Span};

    fn create_test_context() -> InferContext {
        InferContext::new(&[], PathBuf::from("test"), None)
    }

    fn create_test_location() -> Location {
        Location::new(Span { start: 0, end: 0 }, PathBuf::from("test"))
    }

    #[test]
    fn test_stage_mismatch_detection() {
        let mut ctx = create_test_context();
        let loc = create_test_location();

        // Define a variable 'x' at stage 0
        let var_name = "x".to_symbol();
        let var_type =
            Type::Primitive(crate::types::PType::Numeric).into_id_with_location(loc.clone());
        ctx.env
            .add_bind(&[(var_name, (var_type, EvalStage::Stage(0)))]);

        // Try to look it up from stage 0 - should succeed
        ctx.stage = EvalStage::Stage(0);
        let result = ctx.lookup(var_name, loc.clone());
        assert!(
            result.is_ok(),
            "Looking up variable from same stage should succeed"
        );

        // Try to look it up from stage 1 - should fail with stage mismatch
        ctx.stage = EvalStage::Stage(1);
        let result = ctx.lookup(var_name, loc.clone());
        assert!(
            result.is_err(),
            "Looking up variable from different stage should fail"
        );

        if let Err(Error::StageMismatch {
            variable,
            expected_stage,
            found_stage,
            ..
        }) = result
        {
            assert_eq!(variable, var_name);
            assert_eq!(expected_stage, EvalStage::Stage(1));
            assert_eq!(found_stage, EvalStage::Stage(0));
        } else {
            panic!("Expected StageMismatch error, got: {:?}", result);
        }
    }

    #[test]
    fn test_persistent_stage_access() {
        let mut ctx = create_test_context();
        let loc = create_test_location();

        // Define a variable at Persistent stage
        let var_name = "persistent_var".to_symbol();
        let var_type =
            Type::Primitive(crate::types::PType::Numeric).into_id_with_location(loc.clone());
        ctx.env
            .add_bind(&[(var_name, (var_type, EvalStage::Persistent))]);

        // Try to access from different stages - should all succeed
        for stage in [0, 1, 2] {
            ctx.stage = EvalStage::Stage(stage);
            let result = ctx.lookup(var_name, loc.clone());
            assert!(
                result.is_ok(),
                "Persistent stage variables should be accessible from stage {}",
                stage
            );
        }
    }

    #[test]
    fn test_same_stage_access() {
        let mut ctx = create_test_context();
        let loc = create_test_location();

        // Define variables at different stages
        for stage in [0, 1, 2] {
            let var_name = format!("var_stage_{}", stage).to_symbol();
            let var_type =
                Type::Primitive(crate::types::PType::Numeric).into_id_with_location(loc.clone());
            ctx.env
                .add_bind(&[(var_name, (var_type, EvalStage::Stage(stage)))]);
        }

        // Each variable should only be accessible from its own stage
        for stage in [0, 1, 2] {
            ctx.stage = EvalStage::Stage(stage);
            let var_name = format!("var_stage_{}", stage).to_symbol();
            let result = ctx.lookup(var_name, loc.clone());
            assert!(
                result.is_ok(),
                "Variable should be accessible from its own stage {}",
                stage
            );

            // Should not be accessible from other stages
            for other_stage in [0, 1, 2] {
                if other_stage != stage {
                    ctx.stage = EvalStage::Stage(other_stage);
                    let result = ctx.lookup(var_name, loc.clone());
                    assert!(
                        result.is_err(),
                        "Variable from stage {} should not be accessible from stage {}",
                        stage,
                        other_stage
                    );
                }
            }
        }
    }

    #[test]
    fn test_stage_transitions_bracket_escape() {
        let mut ctx = create_test_context();

        // Test that stage transitions work correctly
        assert_eq!(ctx.stage, EvalStage::Stage(0), "Initial stage should be 0");

        // Simulate bracket behavior - stage increment
        ctx.stage = ctx.stage.increment();
        assert_eq!(
            ctx.stage,
            EvalStage::Stage(1),
            "Stage should increment to 1 in bracket"
        );

        // Simulate escape behavior - stage decrement
        ctx.stage = ctx.stage.decrement();
        assert_eq!(
            ctx.stage,
            EvalStage::Stage(0),
            "Stage should decrement back to 0 after escape"
        );
    }

    #[test]
    fn test_multi_stage_environment() {
        let mut ctx = create_test_context();
        let loc = create_test_location();

        // Create nested scope with different stages
        ctx.env.extend(); // Create new scope

        // Add variable at stage 0
        let var_stage0 = "x".to_symbol();
        let var_type =
            Type::Primitive(crate::types::PType::Numeric).into_id_with_location(loc.clone());
        ctx.stage = EvalStage::Stage(0);
        ctx.env
            .add_bind(&[(var_stage0, (var_type, EvalStage::Stage(0)))]);

        ctx.env.extend(); // Create another scope

        // Add variable with same name at stage 1
        let var_stage1 = "x".to_symbol(); // Same name, different stage
        ctx.stage = EvalStage::Stage(1);
        ctx.env
            .add_bind(&[(var_stage1, (var_type, EvalStage::Stage(1)))]);

        // Test lookups from different stages
        ctx.stage = EvalStage::Stage(0);
        let result = ctx.lookup(var_stage0, loc.clone());
        assert!(
            result.is_err(),
            "Stage 0 variable should not be accessible from nested stage 0 context due to shadowing"
        );

        ctx.stage = EvalStage::Stage(1);
        let result = ctx.lookup(var_stage1, loc.clone());
        assert!(
            result.is_ok(),
            "Stage 1 variable should be accessible from stage 1"
        );

        ctx.stage = EvalStage::Stage(0);
        let result = ctx.lookup(var_stage1, loc.clone());
        assert!(
            result.is_err(),
            "Stage 1 variable should not be accessible from stage 0"
        );

        // Clean up scopes
        ctx.env.to_outer();
        ctx.env.to_outer();
    }

    #[test]
    fn test_qualified_var_mangling() {
        use crate::compiler;

        let src = r#"
mod mymath {
    pub fn add(x, y) {
        x + y
    }
}

fn dsp() {
    mymath::add(1.0, 2.0)
}
"#;
        // Use the compiler context to process the code through the full pipeline
        // (which includes convert_qualified_names before type checking)
        let empty_ext_fns: Vec<compiler::ExtFunTypeInfo> = vec![];
        let empty_macros: Vec<Box<dyn crate::plugin::MacroFunction>> = vec![];
        let ctx = compiler::Context::new(
            empty_ext_fns,
            empty_macros,
            Some(std::path::PathBuf::from("test")),
            compiler::Config::default(),
        );
        let result = ctx.emit_mir(src);

        // Check for compilation errors
        assert!(result.is_ok(), "Compilation failed: {:?}", result.err());
    }

    #[test]
    fn test_qualified_var_mir_generation() {
        use crate::compiler;

        let src = r#"
mod mymath {
    pub fn add(x, y) {
        x + y
    }
}

fn dsp() {
    mymath::add(1.0, 2.0)
}
"#;
        // Use the compiler context to generate MIR
        let empty_ext_fns: Vec<compiler::ExtFunTypeInfo> = vec![];
        let empty_macros: Vec<Box<dyn crate::plugin::MacroFunction>> = vec![];
        let ctx = compiler::Context::new(
            empty_ext_fns,
            empty_macros,
            Some(std::path::PathBuf::from("test")),
            compiler::Config::default(),
        );
        let result = ctx.emit_mir(src);

        // Check for compilation errors
        assert!(result.is_ok(), "MIR generation failed: {:?}", result.err());
    }
}
