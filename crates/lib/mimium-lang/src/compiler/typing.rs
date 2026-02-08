use crate::ast::program::TypeAliasMap;
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
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use thiserror::Error;

mod unification;
pub(crate) use unification::Relation;
use unification::{Error as UnificationError, unify_types};

#[derive(Clone, Debug, Error)]
#[error("Type Inference Error")]
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
    /// The error of records, which contains both subtypes and supertypes.
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
    /// Match expression is not exhaustive (missing patterns)
    NonExhaustiveMatch {
        missing_constructors: Vec<Symbol>,
        location: Location,
    },
    /// Recursive type alias detected (infinite expansion)
    RecursiveTypeAlias {
        type_name: Symbol,
        cycle: Vec<Symbol>,
        location: Location,
    },
    /// Private type accessed from outside its module
    PrivateTypeAccess {
        module_path: Vec<Symbol>,
        type_name: Symbol,
        location: Location,
    },
    /// Public function leaking private type in its signature
    PrivateTypeLeak {
        function_name: Symbol,
        private_type: Symbol,
        location: Location,
    },
}

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
            Error::NonExhaustiveMatch {
                missing_constructors,
                ..
            } => {
                let missing = missing_constructors
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Match expression is not exhaustive. Missing patterns: {missing}")
            }
            Error::RecursiveTypeAlias {
                type_name, cycle, ..
            } => {
                let cycle_str = cycle
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(" -> ");
                format!(
                    "Recursive type alias '{type_name}' detected. Cycle: {cycle_str} -> {type_name}. Use 'type rec' to declare recursive types."
                )
            }
            Error::PrivateTypeAccess {
                module_path,
                type_name,
                ..
            } => {
                let path_str = module_path
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                format!(
                    "Type '{type_name}' in module '{path_str}' is private and cannot be accessed from outside"
                )
            }
            Error::PrivateTypeLeak {
                function_name,
                private_type,
                ..
            } => {
                format!(
                    "Public function '{function_name}' cannot expose private type '{private_type}' in its signature"
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
            Error::NonExhaustiveMatch {
                missing_constructors,
                location,
            } => {
                let missing = missing_constructors
                    .iter()
                    .map(|s| format!("\"{s}\""))
                    .collect::<Vec<_>>()
                    .join(", ");
                vec![(location.clone(), format!("Missing patterns: {missing}"))]
            }
            Error::RecursiveTypeAlias {
                type_name,
                cycle,
                location,
            } => {
                let cycle_str = cycle
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(" -> ");
                vec![(
                    location.clone(),
                    format!(
                        "Type alias '{type_name}' creates a cycle: {cycle_str} -> {type_name}. Consider using 'type rec' instead of 'type alias'."
                    ),
                )]
            }
            Error::PrivateTypeAccess {
                module_path,
                type_name,
                location,
            } => {
                let path_str = module_path
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                vec![(
                    location.clone(),
                    format!("Type '{type_name}' in module '{path_str}' is private"),
                )]
            }
            Error::PrivateTypeLeak { location, .. } => {
                vec![(
                    location.clone(),
                    "private type leaked in public function signature".to_string(),
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
    /// Type alias resolution map
    pub type_aliases: HashMap<Symbol, TypeNodeId>,
    /// Module information for visibility checking
    module_info: Option<crate::ast::program::ModuleInfo>,
    /// Match expressions to check for exhaustiveness after type resolution
    match_expressions: Vec<(ExprNodeId, TypeNodeId)>,
    pub errors: Vec<Error>,
}
struct TypeCycle(pub Vec<Symbol>);

impl InferContext {
    pub fn new(
        builtins: &[(Symbol, TypeNodeId)],
        file_path: PathBuf,
        type_declarations: Option<&crate::ast::program::TypeDeclarationMap>,
        type_aliases: Option<&crate::ast::program::TypeAliasMap>,
        module_info: Option<crate::ast::program::ModuleInfo>,
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
            type_aliases: Default::default(),
            module_info,
            match_expressions: Default::default(),
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
        // Register type aliases
        if let Some(type_aliases) = type_aliases {
            res.register_type_aliases(type_aliases);
        }
        res
    }

    /// Register type declarations from ModuleInfo into the constructor environment
    /// Register type declarations from ModuleInfo into the constructor environment
    fn register_type_declarations(
        &mut self,
        type_declarations: &crate::ast::program::TypeDeclarationMap,
    ) {
        // First pass: Create all UserSum types without recursive wrapping
        // and register type names so that TypeAlias can be resolved
        let mut sum_types: std::collections::HashMap<Symbol, TypeNodeId> =
            std::collections::HashMap::new();

        for (type_name, decl_info) in type_declarations {
            let variants = &decl_info.variants;
            let variant_data: Vec<(Symbol, Option<TypeNodeId>)> =
                variants.iter().map(|v| (v.name, v.payload)).collect();

            let sum_type = Type::UserSum {
                name: *type_name,
                variants: variant_data.clone(),
            }
            .into_id();

            sum_types.insert(*type_name, sum_type);
            // Register the type name itself as Persistent so it's accessible from all stages
            self.env
                .add_bind(&[(*type_name, (sum_type, EvalStage::Persistent))]);
        }

        // Second pass: For recursive types, wrap self-references in Boxed
        for (type_name, decl_info) in type_declarations {
            if !decl_info.is_recursive {
                continue;
            }

            let variants = &decl_info.variants;
            let sum_type_id = sum_types[type_name];

            // Transform recursive references to Boxed
            let variant_data: Vec<(Symbol, Option<TypeNodeId>)> = variants
                .iter()
                .map(|v| {
                    let wrapped_payload = v.payload.map(|payload_type| {
                        Self::wrap_recursive_refs_static(payload_type, *type_name, sum_type_id)
                    });
                    (v.name, wrapped_payload)
                })
                .collect();

            // Update the UserSum type with wrapped variants
            let new_sum_type = Type::UserSum {
                name: *type_name,
                variants: variant_data.clone(),
            }
            .into_id();

            // Update the binding as Persistent
            self.env
                .add_bind(&[(*type_name, (new_sum_type, EvalStage::Persistent))]);

            // Register each constructor
            for (tag_index, (variant_name, payload_type)) in variant_data.iter().enumerate() {
                self.constructor_env.insert(
                    *variant_name,
                    ConstructorInfo {
                        sum_type: new_sum_type,
                        tag_index,
                        payload_type: *payload_type,
                    },
                );
            }
        }

        // Register constructors for non-recursive types
        for (type_name, decl_info) in type_declarations {
            if decl_info.is_recursive {
                continue;
            }

            let sum_type = sum_types[type_name];
            let variants = &decl_info.variants;

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

        // Check for recursive type declarations (not allowed without 'rec' keyword)
        self.check_type_declaration_recursion(type_declarations);
    }

    /// Wrap direct self-references in Boxed type for recursive type declarations
    /// This is a static function that transforms TypeAlias(self_name) -> Boxed(sum_type_id)
    /// in Tuple/Record positions. Does NOT recurse into Function/Array which already provide indirection.
    fn wrap_recursive_refs_static(
        ty: TypeNodeId,
        self_name: Symbol,
        sum_type_id: TypeNodeId,
    ) -> TypeNodeId {
        match ty.to_type() {
            Type::TypeAlias(name) if name == self_name => {
                // Direct self-reference: wrap the sum type in Boxed
                Type::Boxed(sum_type_id).into_id()
            }
            Type::Tuple(elements) => {
                // Recursively wrap in tuple elements
                let wrapped_elements: Vec<TypeNodeId> = elements
                    .iter()
                    .map(|&elem| Self::wrap_recursive_refs_static(elem, self_name, sum_type_id))
                    .collect();
                Type::Tuple(wrapped_elements).into_id()
            }
            Type::Record(fields) => {
                // Recursively wrap in record fields
                let wrapped_fields: Vec<RecordTypeField> = fields
                    .iter()
                    .map(|field| RecordTypeField {
                        key: field.key,
                        ty: Self::wrap_recursive_refs_static(field.ty, self_name, sum_type_id),
                        has_default: field.has_default,
                    })
                    .collect();
                Type::Record(wrapped_fields).into_id()
            }
            Type::Union(elements) => {
                // Recursively wrap in union elements
                let wrapped_elements: Vec<TypeNodeId> = elements
                    .iter()
                    .map(|&elem| Self::wrap_recursive_refs_static(elem, self_name, sum_type_id))
                    .collect();
                Type::Union(wrapped_elements).into_id()
            }
            // Do NOT recurse into Function, Array, Code, or Boxed - they already provide indirection
            _ => ty,
        }
    }

    /// Check for recursive references in type declarations
    /// Recursion is only allowed when the `rec` keyword is used
    fn check_type_declaration_recursion(
        &mut self,
        type_declarations: &crate::ast::program::TypeDeclarationMap,
    ) {
        for (type_name, decl_info) in type_declarations {
            // Skip the recursion check for types declared with `type rec`
            if decl_info.is_recursive {
                continue;
            }
            if let Some(location) =
                self.is_type_declaration_recursive(*type_name, &decl_info.variants)
            {
                self.errors.push(Error::RecursiveTypeAlias {
                    type_name: *type_name,
                    cycle: vec![*type_name],
                    location,
                });
            }
        }
    }

    /// Check if a type declaration contains recursive references
    /// Returns Some(location) if recursion is found, None otherwise
    fn is_type_declaration_recursive(
        &self,
        type_name: Symbol,
        variants: &[crate::ast::program::VariantDef],
    ) -> Option<Location> {
        variants.iter().find_map(|variant| {
            variant
                .payload
                .filter(|&payload_type| self.type_references_name(payload_type, type_name))
                .map(|payload_type| payload_type.to_loc())
        })
    }

    /// Check if a type references a specific type name (for recursion detection)
    fn type_references_name(&self, type_id: TypeNodeId, target_name: Symbol) -> bool {
        match type_id.to_type() {
            Type::TypeAlias(name) if name == target_name => true,
            Type::TypeAlias(name) => {
                // Follow type alias to see if it eventually references target
                if let Some(resolved_type) = self.type_aliases.get(&name) {
                    self.type_references_name(*resolved_type, target_name)
                } else {
                    false
                }
            }
            Type::Function { arg, ret } => {
                self.type_references_name(arg, target_name)
                    || self.type_references_name(ret, target_name)
            }
            Type::Tuple(elements) | Type::Union(elements) => elements
                .iter()
                .any(|t| self.type_references_name(*t, target_name)),
            Type::Array(elem) | Type::Code(elem) => self.type_references_name(elem, target_name),
            Type::Boxed(inner) => self.type_references_name(inner, target_name),
            Type::Record(fields) => fields
                .iter()
                .any(|f| self.type_references_name(f.ty, target_name)),
            Type::UserSum { name, .. } if name == target_name => true,
            Type::UserSum { variants, .. } => variants
                .iter()
                .filter_map(|(_, payload)| *payload)
                .any(|p| self.type_references_name(p, target_name)),
            _ => false,
        }
    }

    /// Register type aliases from ModuleInfo into the type environment
    fn register_type_aliases(&mut self, type_aliases: &crate::ast::program::TypeAliasMap) {
        // Store type aliases for resolution during unification
        for (alias_name, target_type) in type_aliases {
            self.type_aliases.insert(*alias_name, *target_type);
            // Also add to environment for name resolution
            self.env
                .add_bind(&[(*alias_name, (*target_type, EvalStage::Persistent))]);
        }

        // Check for circular type aliases
        self.check_type_alias_cycles(type_aliases);
    }

    /// Check for circular references in type aliases
    fn check_type_alias_cycles(&mut self, type_aliases: &TypeAliasMap) {
        let errors: Vec<_> = type_aliases
            .iter()
            .filter_map(|(alias_name, target_type)| {
                Self::detect_type_alias_cycle(*alias_name, type_aliases).map(|cycle| {
                    Error::RecursiveTypeAlias {
                        type_name: *alias_name,
                        cycle,
                        location: target_type.to_loc(),
                    }
                })
            })
            .collect();

        self.errors.extend(errors);
    }

    /// Detect a cycle starting from a given type alias name
    /// Returns Some(cycle) if a cycle is found, None otherwise
    fn detect_type_alias_cycle(start: Symbol, type_aliases: &TypeAliasMap) -> Option<Vec<Symbol>> {
        Self::detect_cycle_helper(start, vec![], type_aliases).map(|t| t.0)
    }

    /// Helper function for cycle detection
    fn detect_cycle_helper(
        current: Symbol,
        path: Vec<Symbol>,
        type_aliases: &TypeAliasMap,
    ) -> Option<TypeCycle> {
        // If we've seen this type before in the current path, we have a cycle
        if let Some(cycle_start) = path.iter().position(|&s| s == current) {
            return Some(TypeCycle(path[cycle_start..].to_vec()));
        }

        let new_path = [path, vec![current]].concat();

        type_aliases.get(&current).and_then(|target_type| {
            Self::find_type_aliases_in_type(*target_type)
                .into_iter()
                .find_map(|ref_alias| {
                    Self::detect_cycle_helper(ref_alias, new_path.clone(), type_aliases)
                })
        })
    }

    /// Find all type alias names referenced in a type
    fn find_type_aliases_in_type(type_id: TypeNodeId) -> Vec<Symbol> {
        match type_id.to_type() {
            Type::TypeAlias(name) => vec![name],
            Type::Function { arg, ret } => {
                let mut aliases = Self::find_type_aliases_in_type(arg);
                aliases.extend(Self::find_type_aliases_in_type(ret));
                aliases
            }
            Type::Tuple(elements) | Type::Union(elements) => elements
                .iter()
                .flat_map(|t| Self::find_type_aliases_in_type(*t))
                .collect(),
            Type::Array(elem) | Type::Code(elem) => Self::find_type_aliases_in_type(elem),
            Type::Record(fields) => fields
                .iter()
                .flat_map(|f| Self::find_type_aliases_in_type(f.ty))
                .collect(),
            Type::UserSum { variants, .. } => variants
                .iter()
                .filter_map(|(_, payload)| *payload)
                .flat_map(Self::find_type_aliases_in_type)
                .collect(),
            _ => vec![],
        }
    }

    /// Resolve type aliases recursively
    pub fn resolve_type_alias(&self, type_id: TypeNodeId) -> TypeNodeId {
        match type_id.to_type() {
            Type::TypeAlias(alias_name) => {
                if let Some(resolved_type) = self.type_aliases.get(&alias_name) {
                    // Recursively resolve in case the alias points to another alias
                    self.resolve_type_alias(*resolved_type)
                } else {
                    type_id // Return original if not found (shouldn't happen)
                }
            }
            _ => type_id, // Not an alias, return as-is
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
        // First, try to look up the constructor directly in constructor_env.
        // This handles cases where the union type is still an unresolved intermediate type.
        if let Some(constructor_info) = self.constructor_env.get(&constructor_name) {
            return constructor_info.payload_type.unwrap_or_else(|| unit!());
        }

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
                if let Some((_, payload_ty)) =
                    variants.iter().find(|(name, _)| *name == constructor_name)
                {
                    // Return the payload type if available, otherwise Unit
                    payload_ty.unwrap_or_else(|| unit!())
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

    /// Add bindings for a match pattern to the current environment
    /// Handles variable bindings, tuple patterns, and nested patterns
    fn add_pattern_bindings(&mut self, pattern: &crate::ast::MatchPattern, ty: TypeNodeId) {
        use crate::ast::MatchPattern;
        // Resolve the type to its concrete form (unwrap intermediate types)
        let resolved_ty = ty.get_root().to_type();
        match pattern {
            MatchPattern::Variable(var) => {
                self.env.add_bind(&[(*var, (ty, self.stage))]);
            }
            MatchPattern::Wildcard => {
                // No bindings for wildcard
            }
            MatchPattern::Literal(_) => {
                // No bindings for literal patterns
            }
            MatchPattern::Tuple(patterns) => {
                // For tuple patterns, we need to bind each element
                // The type should be a tuple type with matching elements
                if let Type::Tuple(elem_types) = resolved_ty {
                    for (pat, elem_ty) in patterns.iter().zip(elem_types.iter()) {
                        self.add_pattern_bindings(pat, *elem_ty);
                    }
                } else {
                    // If we have a single-element tuple pattern, try to unwrap and bind
                    // This handles the case of Tuple([inner_pattern]) where we should
                    // pass the type directly to the inner pattern
                    if patterns.len() == 1 {
                        self.add_pattern_bindings(&patterns[0], ty);
                    }
                }
            }
            MatchPattern::Constructor(_, inner) => {
                // For constructor patterns, recursively handle the inner pattern
                if let Some(inner_pat) = inner {
                    self.add_pattern_bindings(inner_pat, ty);
                }
            }
        }
    }

    /// Check a pattern against a type and add variable bindings
    /// This is used for tuple patterns in multi-scrutinee matching
    fn check_pattern_against_type(
        &mut self,
        pattern: &crate::ast::MatchPattern,
        ty: TypeNodeId,
        loc: &Location,
    ) {
        use crate::ast::MatchPattern;
        match pattern {
            MatchPattern::Literal(lit) => {
                // For literal patterns, unify with expected type
                let pat_ty = match lit {
                    crate::ast::Literal::Int(_) | crate::ast::Literal::Float(_) => {
                        Type::Primitive(PType::Numeric).into_id_with_location(loc.clone())
                    }
                    _ => Type::Failure.into_id_with_location(loc.clone()),
                };
                let _ = self.unify_types(ty, pat_ty);
            }
            MatchPattern::Wildcard => {
                // Wildcard matches anything, no binding
            }
            MatchPattern::Variable(var) => {
                // Bind variable to the expected type
                self.env.add_bind(&[(*var, (ty, self.stage))]);
            }
            MatchPattern::Constructor(constructor_name, inner) => {
                // Get the payload type for this constructor from the union/enum type
                let binding_ty = self.get_constructor_type_from_union(ty, *constructor_name);
                if let Some(inner_pat) = inner {
                    self.add_pattern_bindings(inner_pat, binding_ty);
                }
            }
            MatchPattern::Tuple(patterns) => {
                // Recursively check nested tuple pattern
                let resolved_ty = ty.get_root().to_type();
                if let Type::Tuple(elem_types) = resolved_ty {
                    for (pat, elem_ty) in patterns.iter().zip(elem_types.iter()) {
                        self.check_pattern_against_type(pat, *elem_ty, loc);
                    }
                }
            }
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
            Type::TypeAlias(name) => {
                // Determine if this is a qualified path (contains '$') or a simple name
                let resolved_name = if name.as_str().contains('$') {
                    // Already a mangled name from qualified path (e.g., mymath$PrivateNum)
                    // Use it directly
                    name
                } else if let Some(ref module_info) = self.module_info {
                    // Simple name - check use_alias_map for resolution
                    module_info
                        .use_alias_map
                        .get(&name)
                        .copied()
                        .unwrap_or(name)
                } else {
                    name
                };

                log::trace!(
                    "Resolving TypeAlias: {} -> {}",
                    name.as_str(),
                    resolved_name.as_str()
                );

                // Check visibility if module_info is available
                if let Some(ref module_info) = self.module_info {
                    if let Some(&is_public) = module_info.visibility_map.get(&resolved_name) {
                        if !is_public {
                            // Type is private - report error for accessing it from outside
                            let type_path: Vec<&str> = resolved_name.as_str().split('$').collect();
                            if type_path.len() > 1 {
                                // This is a module member type
                                let module_path: Vec<crate::interner::Symbol> = type_path
                                    [..type_path.len() - 1]
                                    .iter()
                                    .map(ToSymbol::to_symbol)
                                    .collect();
                                let type_name = type_path.last().unwrap().to_symbol();

                                // Report error for private type access
                                self.errors.push(Error::PrivateTypeAccess {
                                    module_path,
                                    type_name,
                                    location: loc.clone(),
                                });
                            }
                        }
                    }
                }

                // Resolve type alias by looking it up in the environment
                match self.lookup(resolved_name, loc.clone()) {
                    Ok(resolved_ty) => {
                        log::trace!(
                            "Resolved TypeAlias {} to {}",
                            resolved_name.as_str(),
                            resolved_ty.to_type()
                        );
                        resolved_ty
                    }
                    Err(_) => {
                        log::warn!(
                            "TypeAlias {} not found, treating as Unknown",
                            resolved_name.as_str()
                        );
                        // If not found, treat as Unknown and convert to intermediate
                        self.gen_intermediate_type_with_location(loc.clone())
                    }
                }
            }
            _ => t.apply_fn(|t| self.convert_unknown_to_intermediate(t, loc.clone())),
        }
    }

    /// Check if a symbol is public based on the visibility map
    fn is_public(&self, name: &Symbol) -> bool {
        self.module_info
            .as_ref()
            .and_then(|info| info.visibility_map.get(name))
            .is_some_and(|vis| *vis)
    }

    fn is_private(&self, name: &Symbol) -> bool {
        !self.is_public(name)
    }

    /// Check if a public function leaks private types in its signature
    fn check_private_type_leak(&mut self, name: Symbol, ty: TypeNodeId, loc: Location) {
        // Check if the function is public
        if !self.is_public(&name) {
            return; // Private functions can use private types
        }

        // Check if the type contains any private type references
        if let Some(type_name) = self.contains_private_type(ty) {
            self.errors.push(Error::PrivateTypeLeak {
                function_name: name,
                private_type: type_name,
                location: loc,
            });
        }
    }

    /// Recursively check if a type contains references to private types
    /// Returns Some(type_name) if a private type is found
    fn contains_private_type(&self, ty: TypeNodeId) -> Option<Symbol> {
        let resolved = Self::substitute_type(ty);
        match resolved.to_type() {
            Type::TypeAlias(name) => {
                // Check if this type alias is private
                if self.is_private(&name) {
                    return Some(name);
                }

                // If it's a qualified name, extract type name and check visibility
                let name_str = name.as_str();
                if name_str.contains("::") {
                    let parts: Vec<&str> = name_str.split("::").collect();
                    if parts.len() >= 2 {
                        let module_path: Vec<Symbol> = parts[..parts.len() - 1]
                            .iter()
                            .map(|s| s.to_symbol())
                            .collect();
                        let type_name = parts[parts.len() - 1].to_symbol();

                        let module_path_str = module_path
                            .iter()
                            .map(|s| s.as_str())
                            .collect::<Vec<_>>()
                            .join("::");
                        let mangled_name =
                            format!("{}::{}", module_path_str, type_name.as_str()).to_symbol();

                        if self.is_private(&mangled_name) {
                            return Some(type_name);
                        }
                    }
                }
                None
            }
            Type::Function { arg, ret } => {
                // Check argument type (can be a single type or a record of multiple args)
                if let Some(private_type) = self.contains_private_type(arg) {
                    return Some(private_type);
                }
                // Check return type
                self.contains_private_type(ret)
            }
            Type::Tuple(ref elements) => {
                for elem_ty in elements.iter() {
                    if let Some(private_type) = self.contains_private_type(*elem_ty) {
                        return Some(private_type);
                    }
                }
                None
            }
            Type::Array(elem_ty) => self.contains_private_type(elem_ty),
            Type::Record(ref fields) => {
                for field in fields.iter() {
                    if let Some(private_type) = self.contains_private_type(field.ty) {
                        return Some(private_type);
                    }
                }
                None
            }
            Type::Union(ref variants) => {
                for variant_ty in variants.iter() {
                    if let Some(private_type) = self.contains_private_type(*variant_ty) {
                        return Some(private_type);
                    }
                }
                None
            }
            Type::Ref(inner_ty) => self.contains_private_type(inner_ty),
            Type::Code(inner_ty) => self.contains_private_type(inner_ty),
            Type::Boxed(inner_ty) => self.contains_private_type(inner_ty),
            Type::UserSum { name, variants } => {
                // Check if the user-defined sum type itself is private
                if self.is_private(&name) {
                    return Some(name);
                }

                // Check payload types of variants
                for (_variant_name, payload_ty_opt) in variants.iter() {
                    if let Some(payload_ty) = payload_ty_opt {
                        if let Some(private_type) = self.contains_private_type(*payload_ty) {
                            return Some(private_type);
                        }
                    }
                }
                None
            }
            Type::Intermediate(_)
            | Type::Primitive(_)
            | Type::TypeScheme(_)
            | Type::Any
            | Type::Failure
            | Type::Unknown => None,
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
        // Resolve type aliases before unification
        let resolved_t1 = self.resolve_type_alias(t1);
        let resolved_t2 = self.resolve_type_alias(t2);

        unify_types(resolved_t1, resolved_t2)
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

                // Check for private type leak in public function declarations
                // Use the original type before resolution to catch TypeAlias references
                if let Pattern::Single(name) = &tpat.pat {
                    eprintln!(
                        "[DEBUG] Checking private type leak for Let binding: {}",
                        name.as_str()
                    );
                    eprintln!(
                        "[DEBUG] Original type before resolution: {:?}",
                        tpat.ty.to_type()
                    );
                    self.check_private_type_leak(*name, tpat.ty, loc_p.clone());
                }

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

                // Check if public function leaks private type in its declared signature
                self.check_private_type_leak(id.id, id.ty, loc.clone());

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
                    if let Some(payload_ty) = constructor_info.payload_type {
                        // Constructor with payload: type is `payload_type -> sum_type`
                        let fn_type = Type::Function {
                            arg: payload_ty,
                            ret: constructor_info.sum_type,
                        }
                        .into_id_with_location(loc.clone());
                        return Ok(fn_type);
                    } else {
                        // Constructor without payload: type is the sum type itself
                        return Ok(constructor_info.sum_type);
                    }
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
                            crate::ast::MatchPattern::Variable(_) => {
                                // Variable pattern binds the whole value
                                // This should typically be handled by Constructor pattern
                                self.infer_type_unwrapping(arm.body)
                            }
                            crate::ast::MatchPattern::Constructor(constructor_name, binding) => {
                                // Handle constructor patterns for union types
                                // Find the type associated with this constructor in the union
                                let binding_ty = self
                                    .get_constructor_type_from_union(scrut_ty, *constructor_name);

                                if let Some(inner_pattern) = binding {
                                    // Add bindings for the inner pattern
                                    self.env.extend();
                                    self.add_pattern_bindings(inner_pattern, binding_ty);
                                    let body_ty = self.infer_type_unwrapping(arm.body);
                                    self.env.to_outer();
                                    body_ty
                                } else {
                                    self.infer_type_unwrapping(arm.body)
                                }
                            }
                            crate::ast::MatchPattern::Tuple(patterns) => {
                                // Tuple pattern in a match arm for multi-scrutinee matching
                                // The scrutinee should be a tuple and we need to bind variables
                                // from each sub-pattern
                                self.env.extend();

                                // Get the scrutinee type and check it's a tuple
                                let resolved_scrut_ty = scrut_ty.get_root().to_type();
                                if let Type::Tuple(elem_types) = resolved_scrut_ty {
                                    // Type check each pattern element against corresponding
                                    // scrutinee element
                                    for (pat, elem_ty) in patterns.iter().zip(elem_types.iter()) {
                                        self.check_pattern_against_type(pat, *elem_ty, &loc);
                                    }
                                } else {
                                    // If scrutinee is not a tuple, check each pattern against
                                    // the whole type (for error reporting)
                                    for pat in patterns.iter() {
                                        self.check_pattern_against_type(pat, scrut_ty, &loc);
                                    }
                                }

                                let body_ty = self.infer_type_unwrapping(arm.body);
                                self.env.to_outer();
                                body_ty
                            }
                        }
                    })
                    .collect();

                // Record Match expression for exhaustiveness checking after type resolution
                self.match_expressions.push((e, scrut_ty));

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

    /// Check if a match expression is exhaustive
    /// Returns a list of missing constructor names if not exhaustive
    fn check_match_exhaustiveness(
        &self,
        scrutinee_ty: TypeNodeId,
        arms: &[crate::ast::MatchArm],
    ) -> Option<Vec<Symbol>> {
        // Get all constructors required for the scrutinee type
        let required_constructors = self.get_all_constructors(scrutinee_ty);

        // If there are no constructors (e.g., primitive types), no exhaustiveness check needed
        if required_constructors.is_empty() {
            return None;
        }

        // Check if there's a wildcard pattern (covers everything)
        let has_wildcard = arms.iter().any(|arm| {
            matches!(
                &arm.pattern,
                crate::ast::MatchPattern::Wildcard
                    | crate::ast::MatchPattern::Variable(_)
                    | crate::ast::MatchPattern::Tuple(_)
            )
        });

        // If there's a wildcard, the match is exhaustive
        if has_wildcard {
            return None;
        }

        // Collect constructors covered by patterns (only Constructor patterns contribute)
        let covered_constructors: Vec<Symbol> = arms
            .iter()
            .filter_map(|arm| {
                if let crate::ast::MatchPattern::Constructor(name, _) = &arm.pattern {
                    Some(*name)
                } else {
                    None
                }
            })
            .collect();

        // Find missing constructors
        let missing: Vec<Symbol> = required_constructors
            .into_iter()
            .filter(|req| !covered_constructors.contains(req))
            .collect();

        if missing.is_empty() {
            None
        } else {
            Some(missing)
        }
    }

    /// Get all constructor names for a type
    /// For union types like `float | string`, returns ["float", "string"]
    /// For UserSum types, returns all constructor names
    fn get_all_constructors(&self, ty: TypeNodeId) -> Vec<Symbol> {
        // First resolve type aliases, then substitute intermediate types
        let resolved = self.resolve_type_alias(ty);
        let substituted = Self::substitute_type(resolved);

        match substituted.to_type() {
            Type::Union(variants) => {
                // For union types, get the constructor name for each variant
                variants
                    .iter()
                    .filter_map(|v| {
                        let v_resolved = Self::substitute_type(*v);
                        Self::type_constructor_name(&v_resolved.to_type())
                    })
                    .collect()
            }
            Type::UserSum { name: _, variants } => {
                // For UserSum types, collect all constructor names
                variants.iter().map(|(name, _)| *name).collect()
            }
            _ => {
                // For non-union/sum types, no constructors to check
                Vec::new()
            }
        }
    }

    /// Check exhaustiveness for all recorded Match expressions
    /// This should be called after type resolution (substitute_all_intermediates)
    pub fn check_all_match_exhaustiveness(&mut self) {
        let match_expressions = std::mem::take(&mut self.match_expressions);

        let errors: Vec<_> = match_expressions
            .into_iter()
            .filter_map(|(match_expr, scrut_ty)| {
                if let Expr::Match(_scrutinee, arms) = &match_expr.to_expr() {
                    let resolved_scrut_ty = self.resolve_type_alias(scrut_ty);
                    let substituted_scrut_ty = Self::substitute_type(resolved_scrut_ty);

                    self.check_match_exhaustiveness(substituted_scrut_ty, arms)
                        .map(|missing| Error::NonExhaustiveMatch {
                            missing_constructors: missing,
                            location: match_expr.to_location(),
                        })
                } else {
                    None
                }
            })
            .collect();

        self.errors.extend(errors);
    }
}

pub fn infer_root(
    e: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    file_path: PathBuf,
    type_declarations: Option<&crate::ast::program::TypeDeclarationMap>,
    type_aliases: Option<&crate::ast::program::TypeAliasMap>,
    module_info: Option<crate::ast::program::ModuleInfo>,
) -> InferContext {
    let mut ctx = InferContext::new(
        builtin_types,
        file_path.clone(),
        type_declarations,
        type_aliases,
        module_info,
    );
    let _t = ctx
        .infer_type(e)
        .unwrap_or(Type::Failure.into_id_with_location(e.to_location()));
    ctx.substitute_all_intermediates();
    ctx.check_all_match_exhaustiveness();
    ctx
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interner::ToSymbol;
    use crate::types::Type;
    use crate::utils::metadata::{Location, Span};

    fn create_test_context() -> InferContext {
        InferContext::new(&[], PathBuf::from("test"), None, None, None)
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
