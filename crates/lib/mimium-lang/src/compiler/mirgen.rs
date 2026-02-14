use super::intrinsics;
use super::typing::{InferContext, infer_root};

use crate::compiler::parser;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::plugin::MacroFunction;
use crate::utils::miniprint::MiniPrint;
use crate::{function, interpreter, numeric, unit};
pub mod convert_pronoun;
pub mod convert_qualified_names;
pub(crate) mod pattern_destructor;
pub(crate) mod recursecheck;

// use super::pattern_destructor::destruct_let_pattern;
use crate::mir::{self, Argument, Instruction, Mir, VPtr, VReg, Value};

use state_tree::tree::StateTreeSkeleton;

use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;

use crate::types::{PType, RecordTypeField, Type};
use crate::utils::environment::{Environment, LookupRes};
use crate::utils::error::ReportableError;
use crate::utils::metadata::{GLOBAL_LABEL, Location, Span};

use crate::ast::{Expr, Literal};

// Decision Tree for pattern matching compilation
// Used to compile multi-scrutinee pattern matching into nested switches

/// A single pattern cell in the pattern matrix
#[derive(Debug, Clone)]
enum PatternCell {
    /// Literal integer value
    Literal(i64),
    /// Wildcard or variable - matches anything
    Wildcard,
    /// Variable binding - matches anything and binds
    Variable(Symbol),
    /// Constructor with optional nested pattern
    Constructor {
        tag: i64,
        payload_ty: Option<TypeNodeId>,
        inner: Option<Box<PatternCell>>,
    },
    /// Constructor already matched by tag; keep payload type and pattern for binding
    Payload {
        payload_ty: TypeNodeId,
        inner: Box<PatternCell>,
    },
    /// Tuple of patterns (for nested tuples in constructor payloads)
    Tuple(Vec<PatternCell>),
}

/// A row in the pattern matrix
#[derive(Debug, Clone)]
struct PatternRow {
    /// Pattern cells for each scrutinee element
    cells: Vec<PatternCell>,
    /// Index of the original match arm
    arm_index: usize,
    /// The body expression to evaluate
    body: ExprNodeId,
}

/// Binding information for pattern variables
#[derive(Debug, Clone)]
struct BindingInfo {
    /// Variable name to bind
    var: Symbol,
    /// Index of the tuple element (scrutinee column)
    column_index: usize,
    /// If binding comes from a constructor payload, contains payload type and optional tuple index
    payload: Option<(TypeNodeId, Option<usize>)>,
}

/// Decision tree node
#[derive(Debug)]
enum DecisionTree {
    /// Leaf node - evaluate arm body with bindings
    Leaf {
        arm_index: usize,
        body: ExprNodeId,
        /// Variable bindings to apply before evaluating body
        bindings: Vec<BindingInfo>,
    },
    /// Switch on scrutinee at given index
    Switch {
        scrutinee_index: usize,
        cases: Vec<(i64, Box<DecisionTree>)>,
        default: Option<Box<DecisionTree>>,
    },
    /// No match - unreachable
    Fail,
}

// pub mod closure_convert;
// pub mod feedconvert;
// pub mod hir_solve_stage;
type StateSkeleton = StateTreeSkeleton<mir::StateType>;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
struct FunctionId(pub u64);
#[derive(Debug, Default)]
struct ContextData {
    pub func_i: FunctionId,
    pub current_bb: usize,
    pub next_state_offset: Option<u64>,
    pub push_sum: u64,
}
#[derive(Debug, Default, Clone)]
struct DefaultArgData {
    pub name: Symbol,
    pub fid: FunctionId,
    pub ty: TypeNodeId,
}

/// Key for identifying a monomorphized function instance.
/// Consists of the original function name and the mangled type signature.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct MonomorphKey {
    pub original_name: Symbol,
    pub type_signature: String,
}

#[derive(Debug)]
struct Context {
    typeenv: InferContext,
    valenv: Environment<VPtr>,
    fn_label: Option<Symbol>,
    anonymous_fncount: u64,
    reg_count: VReg,
    program: Mir,
    default_args_map: BTreeMap<FunctionId, Vec<DefaultArgData>>,
    /// Map from monomorphization key to the function ID of the specialized function
    monomorph_map: BTreeMap<MonomorphKey, FunctionId>,
    data: Vec<ContextData>,
    data_i: usize,
}
enum AssignDestination {
    Local(VPtr),
    UpValue(u64, VPtr),
    Global(VPtr),
}
impl Context {
    pub fn new(typeenv: InferContext, file_path: Option<PathBuf>) -> Self {
        Self {
            typeenv,
            valenv: Environment::new(),
            program: Mir::new(file_path),
            reg_count: 0,
            fn_label: None,
            anonymous_fncount: 0,
            default_args_map: BTreeMap::new(),
            monomorph_map: BTreeMap::new(),
            data: vec![ContextData::default()],
            data_i: 0,
        }
    }
    fn get_loc_from_span(&self, span: &Span) -> Location {
        Location::new(
            span.clone(),
            self.program.file_path.clone().unwrap_or_default(),
        )
    }
    fn get_ctxdata(&mut self) -> &mut ContextData {
        self.data.get_mut(self.data_i).unwrap()
    }

    fn consume_fnlabel(&mut self) -> Symbol {
        let res = self.fn_label.unwrap_or_else(|| {
            let res = format!("lambda_{}", self.anonymous_fncount);
            self.anonymous_fncount += 1;
            res.to_symbol()
        });
        self.fn_label = None;
        res
    }

    fn get_current_fn(&mut self) -> &mut mir::Function {
        let i = self.get_ctxdata().func_i.0 as usize;
        &mut self.program.functions[i]
    }
    fn try_make_delay(
        &mut self,
        f: &VPtr,
        args: &[ExprNodeId],
    ) -> Option<(VPtr, Vec<StateSkeleton>)> {
        let _rt = match f.as_ref() {
            Value::ExtFunction(name, ft) if *name == "delay".to_symbol() => ft,
            _ => return None,
        };

        let (max, src, time) = match args {
            [max, src, time] => (max, src, time),
            _ => return None,
        };
        match max.to_expr() {
            Expr::Literal(Literal::Float(max)) => {
                //need to evaluate args first before calculate state offset because the argument for time contains stateful function call.
                let (args, astates) = self.eval_args(&[*src, *time]);
                let max_time = max.as_str().parse::<f64>().unwrap();
                let new_skeleton = StateSkeleton::Delay {
                    len: max_time as u64,
                };
                self.consume_and_insert_pushoffset();
                self.get_ctxdata().next_state_offset = Some(new_skeleton.total_size());

                let (args, _types): (Vec<VPtr>, Vec<TypeNodeId>) = args.into_iter().unzip();
                Some((
                    self.push_inst(Instruction::Delay(
                        max_time as u64,
                        args[0].clone(),
                        args[1].clone(),
                    )),
                    [astates, vec![new_skeleton]].concat(),
                ))
            }
            _ => unreachable!("unbounded delay access, should be an error at typing stage."),
        }
    }
    fn make_binop_intrinsic(
        &self,
        label: Symbol,
        args: &[(VPtr, TypeNodeId)],
    ) -> Option<Instruction> {
        debug_assert_eq!(args.len(), 2);
        let a0 = args[0].0.clone();
        let a1 = args[1].0.clone();
        match label.as_str() {
            intrinsics::ADD => Some(Instruction::AddF(a0, a1)),
            intrinsics::SUB => Some(Instruction::SubF(a0, a1)),
            intrinsics::MULT => Some(Instruction::MulF(a0, a1)),
            intrinsics::DIV => Some(Instruction::DivF(a0, a1)),
            intrinsics::POW => Some(Instruction::PowF(a0, a1)),
            intrinsics::MODULO => Some(Instruction::ModF(a0, a1)),
            intrinsics::GT => Some(Instruction::Gt(a0, a1)),
            intrinsics::GE => Some(Instruction::Ge(a0, a1)),
            intrinsics::LT => Some(Instruction::Lt(a0, a1)),
            intrinsics::LE => Some(Instruction::Le(a0, a1)),
            intrinsics::EQ => Some(Instruction::Eq(a0, a1)),
            intrinsics::NE => Some(Instruction::Ne(a0, a1)),
            intrinsics::AND => Some(Instruction::And(a0, a1)),
            intrinsics::OR => Some(Instruction::Or(a0, a1)),
            _ => None,
        }
    }

    fn is_tuple_arithmetic_binop_label(label: Symbol) -> bool {
        matches!(
            label.as_str(),
            intrinsics::ADD | intrinsics::SUB | intrinsics::MULT | intrinsics::DIV
        )
    }

    fn make_tuple_arithmetic_binop_leaf(
        &mut self,
        label: Symbol,
        lhs_val: VPtr,
        lhs_ty: TypeNodeId,
        rhs_val: VPtr,
        rhs_ty: TypeNodeId,
    ) -> Option<VPtr> {
        let lhs_scalar = match lhs_ty.to_type() {
            Type::Primitive(PType::Numeric) => lhs_val,
            Type::Primitive(PType::Int) => self.push_inst(Instruction::CastItoF(lhs_val)),
            _ => return None,
        };
        let rhs_scalar = match rhs_ty.to_type() {
            Type::Primitive(PType::Numeric) => rhs_val,
            Type::Primitive(PType::Int) => self.push_inst(Instruction::CastItoF(rhs_val)),
            _ => return None,
        };

        Some(match label.as_str() {
            intrinsics::ADD => self.push_inst(Instruction::AddF(lhs_scalar, rhs_scalar)),
            intrinsics::SUB => self.push_inst(Instruction::SubF(lhs_scalar, rhs_scalar)),
            intrinsics::MULT => self.push_inst(Instruction::MulF(lhs_scalar, rhs_scalar)),
            intrinsics::DIV => self.push_inst(Instruction::DivF(lhs_scalar, rhs_scalar)),
            _ => unreachable!("already filtered by is_tuple_arithmetic_binop_label"),
        })
    }

    fn make_tuple_arithmetic_binop_intrinsic_rec(
        &mut self,
        label: Symbol,
        lhs_val: VPtr,
        lhs_ty: TypeNodeId,
        rhs_val: VPtr,
        rhs_ty: TypeNodeId,
        ret_ty: TypeNodeId,
    ) -> Option<VPtr> {
        let lhs_tuple = match lhs_ty.to_type() {
            Type::Tuple(elems) => Some(elems),
            _ => None,
        };
        let rhs_tuple = match rhs_ty.to_type() {
            Type::Tuple(elems) => Some(elems),
            _ => None,
        };

        match ret_ty.to_type() {
            Type::Tuple(ret_elem_types) => {
                if lhs_tuple.is_none() && rhs_tuple.is_none() {
                    return None;
                }

                let tuple_len = lhs_tuple
                    .as_ref()
                    .map_or_else(|| rhs_tuple.as_ref().map_or(0, |v| v.len()), |v| v.len());
                if let (Some(lhs_elems), Some(rhs_elems)) = (&lhs_tuple, &rhs_tuple)
                    && lhs_elems.len() != rhs_elems.len()
                {
                    return None;
                }
                if tuple_len > 16 || ret_elem_types.len() != tuple_len {
                    return None;
                }

                let lhs_scalar_slot = if lhs_tuple.is_none() {
                    let slot = self.push_inst(Instruction::Alloc(lhs_ty));
                    self.push_inst(Instruction::Store(slot.clone(), lhs_val.clone(), lhs_ty));
                    Some(slot)
                } else {
                    None
                };
                let rhs_scalar_slot = if rhs_tuple.is_none() {
                    let slot = self.push_inst(Instruction::Alloc(rhs_ty));
                    self.push_inst(Instruction::Store(slot.clone(), rhs_val.clone(), rhs_ty));
                    Some(slot)
                } else {
                    None
                };

                let tuple_ptr = self.push_inst(Instruction::Alloc(ret_ty));
                ret_elem_types
                    .iter()
                    .enumerate()
                    .try_for_each(|(idx, ret_elem_ty)| {
                        let (lhs_elem, lhs_elem_ty) = if let Some(lhs_elems) = &lhs_tuple {
                            (
                                self.push_inst(Instruction::GetElement {
                                    value: lhs_val.clone(),
                                    ty: lhs_ty,
                                    tuple_offset: idx as u64,
                                }),
                                lhs_elems[idx],
                            )
                        } else {
                            (
                                self.push_inst(Instruction::Load(
                                    lhs_scalar_slot
                                        .as_ref()
                                        .expect("scalar slot exists when lhs is not tuple")
                                        .clone(),
                                    lhs_ty,
                                )),
                                lhs_ty,
                            )
                        };

                        let (rhs_elem, rhs_elem_ty) = if let Some(rhs_elems) = &rhs_tuple {
                            (
                                self.push_inst(Instruction::GetElement {
                                    value: rhs_val.clone(),
                                    ty: rhs_ty,
                                    tuple_offset: idx as u64,
                                }),
                                rhs_elems[idx],
                            )
                        } else {
                            (
                                self.push_inst(Instruction::Load(
                                    rhs_scalar_slot
                                        .as_ref()
                                        .expect("scalar slot exists when rhs is not tuple")
                                        .clone(),
                                    rhs_ty,
                                )),
                                rhs_ty,
                            )
                        };

                        self.make_tuple_arithmetic_binop_intrinsic_rec(
                            label,
                            lhs_elem,
                            lhs_elem_ty,
                            rhs_elem,
                            rhs_elem_ty,
                            *ret_elem_ty,
                        )
                        .map(|elem_val| {
                            let dst = self.push_inst(Instruction::GetElement {
                                value: tuple_ptr.clone(),
                                ty: ret_ty,
                                tuple_offset: idx as u64,
                            });
                            self.push_inst(Instruction::Store(dst, elem_val, *ret_elem_ty));
                        })
                        .ok_or(())
                    })
                    .ok()?;

                Some(tuple_ptr)
            }
            _ => {
                if lhs_tuple.is_some() || rhs_tuple.is_some() {
                    None
                } else {
                    self.make_tuple_arithmetic_binop_leaf(
                        label, lhs_val, lhs_ty, rhs_val, rhs_ty,
                    )
                }
            }
        }
    }

    fn make_tuple_arithmetic_binop_intrinsic(
        &mut self,
        label: Symbol,
        raw_args: &[(VPtr, TypeNodeId)],
        ret_ty: TypeNodeId,
    ) -> Option<VPtr> {
        if raw_args.len() != 2 || !Self::is_tuple_arithmetic_binop_label(label) {
            return None;
        }

        self.make_tuple_arithmetic_binop_intrinsic_rec(
            label,
            raw_args[0].0.clone(),
            raw_args[0].1,
            raw_args[1].0.clone(),
            raw_args[1].1,
            ret_ty,
        )
    }

    fn make_uniop_intrinsic(
        &mut self,
        label: Symbol,
        args: &[(VPtr, TypeNodeId)],
    ) -> (Option<Instruction>, Vec<StateSkeleton>) {
        debug_assert_eq!(args.len(), 1);
        let a0 = args[0].0.clone();
        match label.as_str() {
            intrinsics::NEG => (Some(Instruction::NegF(a0)), vec![]),
            intrinsics::SQRT => (Some(Instruction::SqrtF(a0)), vec![]),
            intrinsics::LOG => (Some(Instruction::LogF(a0)), vec![]),
            intrinsics::ABS => (Some(Instruction::AbsF(a0)), vec![]),
            intrinsics::SIN => (Some(Instruction::SinF(a0)), vec![]),
            intrinsics::COS => (Some(Instruction::CosF(a0)), vec![]),
            intrinsics::MEM => {
                let skeleton = StateSkeleton::Mem(mir::StateType::from(numeric!()));
                (Some(Instruction::Mem(a0)), vec![skeleton])
            }
            _ => (None, vec![]),
        }
    }

    fn make_intrinsics(
        &mut self,
        label: Symbol,
        raw_args: &[(VPtr, TypeNodeId)],
        args: &[(VPtr, TypeNodeId)],
        ret_ty: TypeNodeId,
    ) -> (Option<VPtr>, Vec<StateSkeleton>) {
        if let Some(v) = self.make_tuple_arithmetic_binop_intrinsic(label, raw_args, ret_ty) {
            return (Some(v), vec![]);
        }

        let (inst, states) = match args.len() {
            1 => self.make_uniop_intrinsic(label, args),
            2 => (self.make_binop_intrinsic(label, args), vec![]),
            _ => return (None, vec![]),
        };
        let vptr = inst.map(|i| self.push_inst(i));
        (vptr, states)
    }
    fn get_current_basicblock(&mut self) -> &mut mir::Block {
        let bbid = self.get_ctxdata().current_bb;
        self.get_current_fn()
            .body
            .get_mut(bbid)
            .expect("no basic block found")
    }
    fn add_new_basicblock(&mut self) {
        let idx = self.get_current_fn().add_new_basicblock();
        self.get_ctxdata().current_bb = idx;
    }
    fn gen_new_register(&mut self) -> VPtr {
        let res = Arc::new(Value::Register(self.reg_count));
        self.reg_count += 1;
        res
    }
    fn push_inst(&mut self, inst: Instruction) -> VPtr {
        let res = self.gen_new_register();
        self.get_current_basicblock().0.push((res.clone(), inst));
        res
    }
    fn add_bind(&mut self, bind: (Symbol, VPtr)) {
        self.valenv.add_bind(&[bind]);
    }

    /// Recursively insert CloseHeapClosure instructions for heap-allocated objects (closures)
    /// based on type information. This handles nested structures like tuples and records.
    fn insert_close_recursively(&mut self, v: VPtr, ty: TypeNodeId) {
        match ty.to_type() {
            Type::Function { arg, ret } => {
                // This is a closure - insert CloseHeapClosure for the closure itself
                self.push_inst(Instruction::CloseHeapClosure(v.clone()));

                // For higher-order functions, we don't need to recurse into arg/ret here
                // because the closure value itself is a single heap object.
                // The arguments/return values will be processed when they are actually used.
            }
            Type::Boxed(inner) => {
                // Boxed value going out of scope — decrement its reference count
                self.push_inst(Instruction::BoxRelease {
                    ptr: v.clone(),
                    inner_type: inner,
                });
            }
            Type::UserSum { .. } => {
                // UserSum value going out of scope - need to release any boxed values within
                // We emit a ReleaseUserSum instruction that will walk the structure at runtime
                // and decrement reference counts for any heap-allocated objects
                self.push_inst(Instruction::ReleaseUserSum {
                    value: v.clone(),
                    ty,
                });
            }
            Type::Tuple(elem_types) => {
                // Recursively process each element of the tuple
                for (i, elem_ty) in elem_types.iter().enumerate() {
                    let elem_v = self.push_inst(Instruction::GetElement {
                        value: v.clone(),
                        ty,
                        tuple_offset: i as u64,
                    });
                    self.insert_close_recursively(elem_v, *elem_ty);
                }
            }
            Type::Record(fields) => {
                // Recursively process each field of the record
                for (i, field) in fields.iter().enumerate() {
                    let field_v = self.push_inst(Instruction::GetElement {
                        value: v.clone(),
                        ty,
                        tuple_offset: i as u64,
                    });
                    self.insert_close_recursively(field_v, field.ty);
                }
            }
            _ => {
                // Other types (primitives, arrays, etc.) don't need closing
            }
        }
    }

    /// Recursively insert CloneHeap instructions for heap-allocated objects.
    /// This increments the reference count so that `release_heap_closures` at
    /// scope exit will not free objects that are still reachable from another scope.
    fn insert_clone_recursively(&mut self, v: VPtr, ty: TypeNodeId) {
        match ty.to_type() {
            Type::Function { .. } => {
                self.push_inst(Instruction::CloneHeap(v.clone()));
            }
            Type::Boxed(_inner) => {
                // Boxed value being duplicated — increment its reference count
                self.push_inst(Instruction::BoxClone { ptr: v.clone() });
            }
            Type::Tuple(elem_types) => {
                for (i, elem_ty) in elem_types.iter().enumerate() {
                    let elem_v = self.push_inst(Instruction::GetElement {
                        value: v.clone(),
                        ty,
                        tuple_offset: i as u64,
                    });
                    self.insert_clone_recursively(elem_v, *elem_ty);
                }
            }
            Type::Record(fields) => {
                for (i, field) in fields.iter().enumerate() {
                    let field_v = self.push_inst(Instruction::GetElement {
                        value: v.clone(),
                        ty,
                        tuple_offset: i as u64,
                    });
                    self.insert_clone_recursively(field_v, field.ty);
                }
            }
            Type::UserSum { .. } => {
                // For UserSum (variant types with recursive/boxed payload),
                // insert a dynamic clone instruction that will traverse the value at runtime
                // and clone any boxed references it contains
                self.push_inst(Instruction::CloneUserSum {
                    value: v.clone(),
                    ty,
                });
            }
            _ => {}
        }
    }

    fn add_bind_pattern(
        &mut self,
        pattern: &TypedPattern,
        v: VPtr,
        ty: TypeNodeId,
        is_global: bool,
    ) {
        let ty = InferContext::substitute_type(ty);
        let TypedPattern { pat, .. } = pattern;
        let span = pattern.to_span();
        match (pat, ty.to_type()) {
            (Pattern::Placeholder, _) => {}
            (Pattern::Single(id), t) => {
                if is_global && !matches!(v.as_ref(), Value::Function(_)) {
                    let gv = Arc::new(Value::Global(v.clone()));
                    if t.is_function() {
                        //globally allocated closures are immidiately closed, not to be disposed
                        self.insert_close_recursively(v.clone(), ty);
                    }
                    self.push_inst(Instruction::SetGlobal(gv.clone(), v.clone(), ty));
                    self.add_bind((*id, gv))
                } else if id.as_str() != GLOBAL_LABEL {
                    self.add_bind((*id, v))
                }
            }
            (Pattern::Tuple(patterns), Type::Tuple(tvec)) => {
                for ((i, pat), cty) in patterns.iter().enumerate().zip(tvec.iter()) {
                    let elem_v = self.push_inst(Instruction::GetElement {
                        value: v.clone(),
                        ty,
                        tuple_offset: i as u64,
                    });
                    let tid = Type::Unknown.into_id_with_location(self.get_loc_from_span(&span));
                    let tpat = TypedPattern::new(pat.clone(), tid);
                    self.add_bind_pattern(&tpat, elem_v, *cty, is_global);
                }
            }
            (Pattern::Record(patterns), Type::Record(kvvec)) => {
                for (k, pat) in patterns.iter() {
                    let i = kvvec
                        .iter()
                        .position(|RecordTypeField { key, .. }| key == k);
                    if let Some(offset) = i {
                        let elem_v = self.push_inst(Instruction::GetElement {
                            value: v.clone(),
                            ty,
                            tuple_offset: offset as u64,
                        });
                        let tid =
                            Type::Unknown.into_id_with_location(self.get_loc_from_span(&span));
                        let tpat = TypedPattern::new(pat.clone(), tid);
                        let elem_t = kvvec[offset].ty;
                        self.add_bind_pattern(&tpat, elem_v, elem_t, is_global);
                    };
                }
            }
            _ => {
                panic!("typing error in the previous stage")
            }
        }
    }

    fn make_new_function(
        &mut self,
        name: Symbol,
        args: &[Argument],
        state_skeleton: Vec<StateSkeleton>,
        parent_i: Option<FunctionId>,
    ) -> FunctionId {
        let index = self.program.functions.len();
        let newf = mir::Function::new(
            index,
            name,
            args,
            state_skeleton,
            parent_i.map(|FunctionId(f)| f as _),
        );
        self.program.functions.push(newf);
        FunctionId(index as _)
    }

    fn do_in_child_ctx<
        F: FnMut(&mut Self, FunctionId) -> (VPtr, TypeNodeId, Vec<StateSkeleton>),
    >(
        &mut self,
        fname: Symbol,
        abinds: &[(Symbol, TypeNodeId, Option<ExprNodeId>)],
        state_skeleton: Vec<StateSkeleton>,
        mut action: F,
    ) -> (FunctionId, VPtr, Vec<StateSkeleton>) {
        self.valenv.extend();
        self.valenv.add_bind(
            abinds
                .iter()
                .enumerate()
                .map(|(i, (s, _ty, _default))| (*s, Arc::new(Value::Argument(i))))
                .collect::<Vec<_>>()
                .as_slice(),
        );
        let args = abinds
            .iter()
            .map(|(s, ty, _default)| Argument(*s, ty.get_root()))
            .collect::<Vec<_>>();
        let parent_i = self.get_ctxdata().func_i;
        let c_idx = self.make_new_function(fname, &args, state_skeleton, Some(parent_i));

        let def_args = abinds
            .iter()
            .filter_map(|(s, ty, default)| {
                default.map(|d| {
                    // we assume default argument expression does not contain stateful function call
                    let (fid, _state) = self.new_default_args_getter(c_idx, *s, d);
                    DefaultArgData {
                        name: *s,
                        fid,
                        ty: *ty,
                    }
                })
            })
            .collect::<Vec<_>>();
        self.default_args_map.insert(c_idx, def_args);

        self.data.push(ContextData {
            func_i: c_idx,
            ..Default::default()
        });
        self.data_i += 1;
        //do action
        let (fptr, ty, states) = action(self, c_idx);

        // TODO: ideally, type should be infered before the actual action
        let f = self.program.functions.get_mut(c_idx.0 as usize).unwrap();
        f.return_type.get_or_init(|| ty);

        //post action
        let _ = self.data.pop();
        self.data_i -= 1;
        log::trace!("end of lexical scope {fname}");
        self.valenv.to_outer();
        (c_idx, fptr, states)
    }
    fn get_default_args_getter_name(name: Symbol, fid: FunctionId) -> Symbol {
        format!("__default_{}_{name}", fid.0).to_symbol()
    }
    fn new_default_args_getter(
        &mut self,
        fid: FunctionId,
        name: Symbol,
        e: ExprNodeId,
    ) -> (FunctionId, Vec<StateSkeleton>) {
        let (fid, _v, states) = self.do_in_child_ctx(
            Self::get_default_args_getter_name(name, fid),
            &[],
            vec![],
            |ctx, c_idx| {
                let (v, ty, states) = ctx.eval_expr(e);
                let _v = ctx.push_inst(Instruction::Return(v, ty));
                let f = Arc::new(Value::Function(c_idx.0 as usize));
                (f, ty, states)
            },
        );
        (fid, states)
    }
    fn get_default_arg_call(&mut self, name: Symbol, fid: FunctionId) -> Option<VPtr> {
        let args = self.default_args_map.get(&fid);
        args.cloned().and_then(|defv_fn_ids| {
            defv_fn_ids
                .iter()
                .find(|default_arg_data| default_arg_data.name == name)
                .map(|default_arg_data| {
                    let fid = self.push_inst(Instruction::Uinteger(default_arg_data.fid.0));
                    self.push_inst(Instruction::Call(fid, vec![], default_arg_data.ty))
                })
        })
    }
    fn lookup(&self, key: &Symbol) -> LookupRes<VPtr> {
        match self.valenv.lookup_cls(key) {
            LookupRes::Local(v) => LookupRes::Local(v.clone()),
            LookupRes::UpValue(level, v) => LookupRes::UpValue(level, v.clone()),
            LookupRes::Global(v) => LookupRes::Global(v.clone()),
            LookupRes::None => LookupRes::None,
        }
    }

    /// Get or create a monomorphized version of a generic function.
    /// Returns the function ID of the specialized function.
    fn get_or_create_monomorphized_function(
        &mut self,
        original_name: Symbol,
        arg_type: TypeNodeId,
        ret_type: TypeNodeId,
        original_fid: FunctionId,
    ) -> FunctionId {
        let type_signature = format!(
            "{}_{}",
            arg_type.to_mangled_string(),
            ret_type.to_mangled_string()
        );

        let key = MonomorphKey {
            original_name,
            type_signature: type_signature.clone(),
        };

        if let Some(fid) = self.monomorph_map.get(&key) {
            return *fid;
        }

        // Create a new specialized function name
        let specialized_name =
            format!("{}_mono_{}", original_name.as_str(), type_signature).to_symbol();

        log::debug!(
            "Creating monomorphized function: {} for types ({} -> {})",
            specialized_name,
            arg_type.to_type(),
            ret_type.to_type()
        );

        // Clone the original function and specialize it
        let original_fn = self.program.functions[original_fid.0 as usize].clone();
        let new_fid = FunctionId(self.program.functions.len() as u64);

        let mut specialized_fn = original_fn.clone();
        specialized_fn.label = specialized_name;

        self.program.functions.push(specialized_fn);
        self.monomorph_map.insert(key, new_fid);

        new_fid
    }

    pub fn eval_literal(&mut self, lit: &Literal, _span: &Span) -> VPtr {
        match lit {
            Literal::String(s) => self.push_inst(Instruction::String(*s)),
            Literal::Int(i) => self.push_inst(Instruction::Integer(*i)),
            Literal::Float(f) => self.push_inst(Instruction::Float(
                f.as_str().parse::<f64>().expect("illegal float format"),
            )),
            Literal::Now => {
                let ftype = numeric!();
                let fntype = function!(vec![], ftype);
                let getnow = Arc::new(Value::ExtFunction("_mimium_getnow".to_symbol(), fntype));
                self.push_inst(Instruction::CallIndirect(getnow, vec![], ftype))
            }
            Literal::SampleRate => {
                let ftype = numeric!();
                let fntype = function!(vec![], ftype);
                let samplerate = Arc::new(Value::ExtFunction(
                    "_mimium_getsamplerate".to_symbol(),
                    fntype,
                ));
                self.push_inst(Instruction::CallIndirect(samplerate, vec![], ftype))
            }
            Literal::SelfLit | Literal::PlaceHolder => unreachable!(),
        }
    }
    fn eval_rvar(&mut self, e: ExprNodeId, t: TypeNodeId) -> VPtr {
        let span = &e.to_span();
        let loc = self.get_loc_from_span(span);
        // After convert_qualified_names, all module names are resolved to simple Var.
        // QualifiedVar should have been converted to Var with mangled name.
        let name = match e.to_expr() {
            Expr::Var(name) => name,
            Expr::QualifiedVar(_path) => {
                unreachable!("Qualified Var should be removed in the previous step.")
            }
            _ => unreachable!("eval_rvar called on non-variable expr"),
        };
        log::trace!("rv t:{} {}", name, t.to_type());

        // Check if this is a constructor from a user-defined sum type
        if let Some(constructor_info) = self.typeenv.constructor_env.get(&name) {
            if constructor_info.payload_type.is_none() {
                // No-payload constructor: generate a TaggedUnionWrap with no value.
                // This allocates a full-sized union on the register stack with the correct tag.
                let tag = constructor_info.tag_index as u64;
                return self.push_inst(Instruction::TaggedUnionWrap {
                    tag,
                    value: Arc::new(Value::None),
                    union_type: constructor_info.sum_type,
                });
            }
            // For constructors with payload, we return a special value that will be
            // handled by Apply to generate TaggedUnionWrap
            return Arc::new(Value::Constructor(
                name,
                constructor_info.tag_index,
                constructor_info.sum_type,
            ));
        }

        match self.lookup(&name) {
            LookupRes::Local(v) => match v.as_ref() {
                Value::Function(i) => {
                    let reg = self.push_inst(Instruction::Uinteger(*i as u64));
                    // TODO: Calculate actual closure size based on upvalues and state
                    self.push_inst(Instruction::MakeClosure {
                        fn_proto: reg,
                        size: 64,
                    })
                }
                _ => {
                    let ptr = self.eval_expr_as_address(e);
                    self.push_inst(Instruction::Load(ptr, t))
                }
            },
            LookupRes::UpValue(level, v) => {
                (0..level)
                    .rev()
                    .fold(v.clone(), |upv, i| match upv.as_ref() {
                        Value::Function(_fi) => v.clone(),
                        _ => {
                            let res = self.gen_new_register();
                            let current = self.data.get_mut(self.data_i - i).unwrap();
                            let currentf = self
                                .program
                                .functions
                                .get_mut(current.func_i.0 as usize)
                                .unwrap();
                            let upi = currentf.get_or_insert_upvalue(&upv) as _;
                            let currentbb = currentf.body.get_mut(current.current_bb).unwrap();
                            currentbb
                                .0
                                .push((res.clone(), Instruction::GetUpValue(upi, t)));
                            res
                        }
                    })
            }
            LookupRes::Global(v) => match v.as_ref() {
                Value::Global(_gv) => self.push_inst(Instruction::GetGlobal(v.clone(), t)),
                Value::Function(_) | Value::Register(_) => v.clone(),
                _ => unreachable!("non global_value"),
            },
            LookupRes::None => {
                let ty = self.typeenv.lookup(name, loc).expect(
                    format!(
                        "variable \"{name}\" not found. it should be detected at type checking stage"
                    )
                    .as_str(),
                );
                Arc::new(Value::ExtFunction(name, ty))
            }
        }
    }
    /// Evaluates an assignee expression and returns a VPtr that is a pointer to the destination.
    fn eval_destination_ptr(&mut self, assignee: ExprNodeId) -> AssignDestination {
        match assignee.to_expr() {
            Expr::Var(name) => {
                // For a simple variable, lookup its pointer from the environment.
                match self.lookup(&name) {
                    LookupRes::Local(v_ptr) => AssignDestination::Local(v_ptr.clone()),
                    LookupRes::Global(v_ptr) => AssignDestination::Global(v_ptr.clone()),
                    LookupRes::UpValue(_level, v_ptr) => {
                        let currentf = self.get_current_fn();
                        let upi = currentf.get_or_insert_upvalue(&v_ptr) as _;
                        AssignDestination::UpValue(upi, v_ptr.clone())
                    }
                    LookupRes::None => {
                        unreachable!("Invalid assignment target: variable not found")
                    }
                }
            }
            Expr::Proj(expr, idx) => {
                let base_ptr = self.eval_expr_as_address(expr);
                let tuple_ty_id = self.typeenv.infer_type(expr).unwrap();
                let ptr = self.push_inst(Instruction::GetElement {
                    value: base_ptr.clone(),
                    ty: tuple_ty_id,
                    tuple_offset: idx as u64,
                });
                AssignDestination::Local(ptr)
            }
            Expr::FieldAccess(expr, accesskey) => {
                // For a field access, we need to calculate the pointer to the field.
                let base_ptr = self.eval_expr_as_address(expr);

                let record_ty_id = self.typeenv.infer_type(expr).unwrap();
                let record_ty = record_ty_id.to_type();

                if let Type::Record(fields) = record_ty {
                    let offset = fields
                        .iter()
                        .position(|RecordTypeField { key, .. }| *key == accesskey)
                        .expect("field access to non-existing field");

                    // Use GetElementPtr to calculate the pointer to the specific field.
                    let ptr = self.push_inst(Instruction::GetElement {
                        value: base_ptr.clone(),
                        ty: record_ty_id,
                        tuple_offset: offset as u64,
                    });
                    AssignDestination::Local(ptr)
                } else {
                    panic!("Expected record type for field access assignment, but got {record_ty}");
                }
            }
            Expr::ArrayAccess(_, _) => {
                unimplemented!("Assignment to array is not implemented yet.")
            }
            _ => unreachable!("Invalid assignee expression"),
        }
    }

    fn eval_assign(&mut self, assignee: ExprNodeId, src: VPtr, t: TypeNodeId) {
        match self.eval_destination_ptr(assignee) {
            AssignDestination::Local(value) => {
                self.push_inst(Instruction::Store(value, src, t));
            }
            AssignDestination::UpValue(upi, _value) => {
                self.push_inst(Instruction::SetUpValue(upi, src, t));
            }
            AssignDestination::Global(value) => {
                self.push_inst(Instruction::SetGlobal(value, src, t));
            }
        }
    }
    fn consume_and_insert_pushoffset(&mut self) {
        if let Some(offset) = self.get_ctxdata().next_state_offset.take() {
            self.get_ctxdata().push_sum += offset;
            //insert pushstateoffset
            self.get_current_basicblock()
                .0
                .push((Arc::new(Value::None), Instruction::PushStateOffset(offset)));
        }
    }
    fn emit_fncall(
        &mut self,
        idx: u64,
        args: Vec<(VPtr, TypeNodeId)>,
        ret_t: TypeNodeId,
    ) -> (VPtr, Vec<StateSkeleton>) {
        // stack size of the function to be called
        let target_fn = &self.program.functions[idx as usize];
        let is_stateful = target_fn.is_stateful();
        let child_skeleton = target_fn.state_skeleton.clone();
        if is_stateful {
            self.consume_and_insert_pushoffset();
        }

        let f = self.push_inst(Instruction::Uinteger(idx));

        let res = self.push_inst(Instruction::Call(f.clone(), args, ret_t));

        if is_stateful {
            self.get_ctxdata().next_state_offset = Some(child_skeleton.total_size());
        }
        let s = if is_stateful {
            vec![child_skeleton]
        } else {
            vec![]
        };
        (res, s)
    }

    fn eval_args(&mut self, args: &[ExprNodeId]) -> (Vec<(VPtr, TypeNodeId)>, Vec<StateSkeleton>) {
        let res = args
            .iter()
            .map(|a_meta| {
                let (v, t, s) = self.eval_expr(*a_meta);
                let res = match v.as_ref() {
                    Value::Function(idx) => {
                        let f = self.push_inst(Instruction::Uinteger(*idx as u64));
                        // TODO: Calculate actual closure size
                        self.push_inst(Instruction::MakeClosure {
                            fn_proto: f,
                            size: 64,
                        })
                    }
                    _ => v.clone(),
                };
                // Clone heap-allocated values (closures and boxed values) before passing to function
                // This implements call-by-value semantics with reference counting
                if t.to_type().contains_function() || t.to_type().contains_boxed() {
                    self.insert_clone_recursively(res.clone(), t);
                }
                // Close heap-allocated values after cloning (for higher-order functions and boxed values)
                if t.to_type().contains_function() || t.to_type().contains_boxed() {
                    self.insert_close_recursively(res.clone(), t);
                }
                (res, t, s)
            })
            .collect::<Vec<_>>();

        let ats = res
            .iter()
            .map(|(a, t, _)| (a.clone(), *t))
            .collect::<Vec<_>>();
        let states = res.into_iter().flat_map(|(_, _, s)| s).collect::<Vec<_>>();
        (ats, states)
    }
    fn eval_block(&mut self, block: Option<ExprNodeId>) -> (VPtr, TypeNodeId, Vec<StateSkeleton>) {
        self.add_new_basicblock();
        let (e, rt, s) = match block {
            Some(e) => self.eval_expr(e),
            None => (Arc::new(Value::None), unit!(), vec![]),
        };
        //if returning non-closure function, make closure
        let e = match e.as_ref() {
            Value::Function(idx) => {
                let cpos = self.push_inst(Instruction::Uinteger(*idx as u64));
                // TODO: Calculate actual closure size
                self.push_inst(Instruction::MakeClosure {
                    fn_proto: cpos,
                    size: 64,
                })
            }
            _ => e,
        };
        (e, rt, s)
    }
    fn alloc_aggregates(
        &mut self,
        items: &[ExprNodeId],
        ty: TypeNodeId,
    ) -> (VPtr, TypeNodeId, Vec<StateSkeleton>) {
        log::trace!("alloc_aggregates: items = {items:?}, ty = {ty:?}");
        let len = items.len();
        if len == 0 {
            return (
                Arc::new(Value::None),
                Type::Record(vec![]).into_id(),
                vec![],
            );
        }
        let alloc_insert_point = self.get_current_basicblock().0.len();
        let dst = self.gen_new_register();
        let mut states = vec![];
        for (i, e) in items.iter().enumerate() {
            let (v, elem_ty, s) = self.eval_expr(*e);
            let ptr = self.push_inst(Instruction::GetElement {
                value: dst.clone(),
                ty, // lazyly set after loops,
                tuple_offset: i as u64,
            });
            states.extend(s);
            self.push_inst(Instruction::Store(ptr, v, elem_ty));
        }
        self.get_current_basicblock()
            .0
            .insert(alloc_insert_point, (dst.clone(), Instruction::Alloc(ty)));

        // pass only the head of the tuple, and the length can be known
        // from the type information.
        (dst, ty, states)
    }
    /// Evaluates an expression as an l-value, returning a pointer to its memory location.
    fn eval_expr_as_address(&mut self, e: ExprNodeId) -> VPtr {
        match e.to_expr() {
            Expr::Var(name) => {
                // do not load here
                match self.lookup(&name) {
                    LookupRes::Local(ptr) => ptr,
                    LookupRes::Global(ptr) => ptr,
                    _ => unreachable!("Cannot get address of this expression"),
                }
            }
            Expr::FieldAccess(base_expr, accesskey) => {
                let base_ptr = self.eval_expr_as_address(base_expr);

                let record_ty_id = self.typeenv.infer_type(base_expr).unwrap();
                let record_ty = record_ty_id.to_type();

                if let Type::Record(fields) = record_ty {
                    let offset = fields
                        .iter()
                        .position(|f| f.key == accesskey)
                        .expect("Field not found");

                    self.push_inst(Instruction::GetElement {
                        value: base_ptr,
                        ty: record_ty_id,
                        tuple_offset: offset as u64,
                    })
                } else {
                    panic!("Cannot access field on a non-record type");
                }
            }
            Expr::ArrayAccess(_, _) => {
                unimplemented!("Array element assignment is not implemented yet.")
            }
            _ => unreachable!("This expression cannot be used as an l-value"),
        }
    }
    fn unpack_argument(
        &mut self,
        f_val: VPtr,
        arg_val: VPtr,
        at: TypeNodeId,
        ty: TypeNodeId,
    ) -> Vec<(VPtr, TypeNodeId)> {
        log::trace!("Unpacking argument {ty} for {at}");
        // Check if the argument is a tuple or record that we need to unpack
        match ty.to_type() {
            Type::Tuple(tys) => tys
                .into_iter()
                .enumerate()
                .map(|(i, t)| {
                    let elem_val = self.push_inst(Instruction::GetElement {
                        value: arg_val.clone(),
                        ty,
                        tuple_offset: i as u64,
                    });
                    (elem_val, t)
                })
                .collect(),
            Type::Record(kvs) => {
                enum SearchRes {
                    Found(usize),
                    Default,
                }
                if let Type::Record(param_types) = at.to_type() {
                    let search_res = param_types.iter().map(|param| {
                        kvs.iter()
                            .enumerate()
                            .find_map(|(i, kv)| {
                                (param.key == kv.key).then_some((SearchRes::Found(i), kv))
                            })
                            .or(param.has_default.then_some((SearchRes::Default, param)))
                    });
                    search_res.map(
                                            |searchres| match searchres {
                                                Some((SearchRes::Found(i), kv)) => {
                                                    log::trace!("non-default argument {} found", kv.key);

                                                    let field_val = self.push_inst(
                                                                    Instruction::GetElement {
                                                                        value: arg_val.clone(),
                                                                        ty,
                                                                        tuple_offset: i as u64,
                                                                    },
                                                                );
                                                                (field_val, kv.ty)
                                                            },
                                                Some((SearchRes::Default, kv)) => {
                                                  if let Value::Function(fid) = f_val.as_ref() {
                                                     let fid=      FunctionId(*fid as u64);
                                                        log::trace!("searching default argument for {} in function {}", kv.key, self.program.functions[fid.0 as usize].label.as_str());
                                                        let default_val = self.get_default_arg_call(kv.key, fid).expect(format!("msg: default argument {} not found", kv.key).as_str());
                                                        (default_val, kv.ty)
                                                    } else {
                                                        log::error!("default argument cannot be supported with closure currently");
                                                        (Arc::new(Value::None), Type::Failure.into_id())
                                                    }
                                                }
                                                None=>{
                                                    panic!("parameter pack failed, possible type inference bug")
                                                }
                                            }).collect::<Vec<_>>()
                } else {
                    unreachable!("parameter pack failed, possible type inference bug")
                }
            }
            _ => vec![(arg_val, ty)],
        }
    }

    pub fn eval_expr(&mut self, e: ExprNodeId) -> (VPtr, TypeNodeId, Vec<StateSkeleton>) {
        let span = e.to_span();
        let ty = self
            .typeenv
            .infer_type(e)
            .expect("type inference failed, should be an error at type checker stage");
        let ty = InferContext::substitute_type(ty);
        match &e.to_expr() {
            Expr::Literal(lit) => {
                let v = self.eval_literal(lit, &span);
                (v, ty, vec![])
            }
            Expr::Var(_name) => {
                // When the type checker says a variable has type Boxed(T),
                // the MIR bind_pattern has already unboxed it to type T.
                // Use the inner type for the actual load.
                let load_ty = if let Type::Boxed(inner) = ty.to_type() {
                    inner
                } else {
                    ty
                };
                (self.eval_rvar(e, load_ty), load_ty, vec![])
            }
            Expr::QualifiedVar(_path) => {
                unreachable!("Qualified Var should be removed in the previous step.")
            }
            Expr::Block(b) => {
                if let Some(block) = b {
                    self.eval_expr(*block)
                } else {
                    (Arc::new(Value::None), unit!(), vec![])
                }
            }
            Expr::Tuple(items) => self.alloc_aggregates(items, ty),
            Expr::Proj(tup, idx) => {
                let i = *idx as usize;
                let (tup_v, tup_ty, states) = self.eval_expr(*tup);
                let elem_ty = match tup_ty.to_type() {
                    Type::Tuple(tys) if i < tys.len() => tys[i],
                    _ => panic!(
                        "expected tuple type for projection,perhaps type error bugs in the previous stage"
                    ),
                };
                let res = self.push_inst(Instruction::GetElement {
                    value: tup_v.clone(),
                    ty: tup_ty,
                    tuple_offset: i as u64,
                });
                (res, elem_ty, states)
            }
            Expr::RecordLiteral(fields) => {
                self.alloc_aggregates(&fields.iter().map(|f| f.expr).collect::<Vec<_>>(), ty)
            }
            Expr::ImcompleteRecord(fields) => {
                // For incomplete records, we also aggregate the available fields
                // The default values will be handled in the type system and during record construction
                self.alloc_aggregates(&fields.iter().map(|f| f.expr).collect::<Vec<_>>(), ty)
            }
            Expr::RecordUpdate(_, _) => {
                // Record update syntax should be expanded during conversion phase
                // This case should not be reached after syntax sugar expansion
                unreachable!("RecordUpdate should be expanded during syntax sugar conversion")
            }
            Expr::FieldAccess(expr, accesskey) => {
                let (expr_v, expr_ty, states) = self.eval_expr(*expr);
                match expr_ty.to_type() {
                    Type::Record(fields) => {
                        let offset = fields
                            .iter()
                            .position(|RecordTypeField { key, .. }| *key == *accesskey)
                            .expect("field access to non-existing field");

                        let res = self.push_inst(Instruction::GetElement {
                            value: expr_v.clone(),
                            ty: expr_ty,
                            tuple_offset: offset as u64,
                        });
                        (res, fields[offset].ty, states)
                    }
                    _ => panic!("expected record type for field access"),
                }
            }
            Expr::ArrayLiteral(items) => {
                // For now, handle array literals similar to tuples

                let (vts, states): (Vec<_>, Vec<_>) = items
                    .iter()
                    .map(|item| {
                        let (v, t, s) = self.eval_expr(*item);
                        ((v, t), s)
                    })
                    .unzip();
                let (values, tys): (Vec<_>, Vec<_>) = vts.into_iter().unzip();
                // Assume all array elements have the same type (first element's type)
                debug_assert!(tys.windows(2).all(|w| w[0] == w[1]));
                let elem_ty = if !tys.is_empty() { tys[0] } else { numeric!() };
                let reg = self.push_inst(Instruction::Array(values.clone(), elem_ty));
                (
                    reg,
                    Type::Array(elem_ty).into_id(),
                    states.into_iter().flatten().collect(),
                )
            }
            Expr::ArrayAccess(array, index) => {
                let (array_v, _array_ty, states) = self.eval_expr(*array);
                let (index_v, _ty, states2) = self.eval_expr(*index);

                // Get element at the specified index
                let result = self.push_inst(Instruction::GetArrayElem(
                    array_v.clone(),
                    index_v.clone(),
                    ty,
                ));
                (result, ty, [states, states2].concat())
            }
            Expr::Apply(f, args) => {
                let (f_val, ft, app_state) = self.eval_expr(*f);

                // Check if this is a constructor call with payload
                if let Value::Constructor(name, tag_index, sum_type) = f_val.as_ref() {
                    // Evaluate the payload arguments
                    let (arg_vals, arg_states) = self.eval_args(args);

                    // Build the payload: single arg → use directly, multiple args → construct a tuple
                    let (payload_val, payload_ty) = if arg_vals.len() == 1 {
                        arg_vals.into_iter().next().unwrap()
                    } else {
                        // Multiple arguments: construct a tuple payload
                        let elem_tys: Vec<TypeNodeId> = arg_vals.iter().map(|(_, t)| *t).collect();
                        let tuple_ty = Type::Tuple(elem_tys).into_id();
                        let tuple_ptr = self.push_inst(Instruction::Alloc(tuple_ty));
                        for (i, (elem_val, elem_ty)) in arg_vals.iter().enumerate() {
                            let dest = self.push_inst(Instruction::GetElement {
                                value: tuple_ptr.clone(),
                                ty: tuple_ty,
                                tuple_offset: i as u64,
                            });
                            self.push_inst(Instruction::Store(dest, elem_val.clone(), *elem_ty));
                        }
                        let loaded = self.push_inst(Instruction::Load(tuple_ptr, tuple_ty));
                        (loaded, tuple_ty)
                    };

                    // Box any Boxed fields in the payload before wrapping
                    let payload_val = self.box_fields_if_needed(payload_val, payload_ty, *name);

                    // Generate TaggedUnionWrap instruction
                    let result = self.push_inst(Instruction::TaggedUnionWrap {
                        tag: *tag_index as u64,
                        value: payload_val,
                        union_type: *sum_type,
                    });
                    return (result, *sum_type, [app_state, arg_states].concat());
                }

                let del = self.try_make_delay(&f_val, args);
                if let Some((d, states)) = del {
                    return (d, numeric!(), states);
                }

                // Get function parameter info
                let (at, rt) = if let Type::Function { arg, ret } = ft.to_type() {
                    (arg, ret)
                } else {
                    panic!("non function type {} {} ", ft.to_type(), ty.to_type());
                };

                // Check if this is a generic function that needs monomorphization
                let needs_monomorphization =
                    at.to_type().contains_type_scheme() || rt.to_type().contains_type_scheme();

                // If we have a generic external function (like `map`), we need to monomorphize it
                let (f_to_call, monomorphized_rt) = if needs_monomorphization {
                    if let Value::ExtFunction(fn_name, _fn_ty) = f_val.as_ref() {
                        // Infer the concrete types from the arguments and expected return type
                        // For now, use the types from type inference (ty for return, args types for arguments)
                        let concrete_arg_ty = self
                            .typeenv
                            .infer_type(*args.first().expect("map needs args"))
                            .unwrap_or(at);
                        let concrete_ret_ty = ty;

                        log::debug!(
                            "Monomorphizing generic function '{}' with arg type: {}, ret type: {}",
                            fn_name,
                            concrete_arg_ty.to_type(),
                            concrete_ret_ty.to_type()
                        );

                        // Create a monomorphized version of the external function
                        let mangled_name = format!(
                            "{}_mono_{}_{}",
                            fn_name.as_str(),
                            concrete_arg_ty.to_mangled_string(),
                            concrete_ret_ty.to_mangled_string()
                        )
                        .to_symbol();

                        let concrete_fn_ty = Type::Function {
                            arg: concrete_arg_ty,
                            ret: concrete_ret_ty,
                        }
                        .into_id();

                        let monomorphized_fn =
                            Arc::new(Value::ExtFunction(mangled_name, concrete_fn_ty));
                        (monomorphized_fn, concrete_ret_ty)
                    } else {
                        todo!(
                            "Monomorphization of non-external generic functions not yet implemented"
                        );
                        (f_val.clone(), rt)
                    }
                } else {
                    (f_val.clone(), ty)
                };

                // Handle parameter packing/unpacking if needed
                // How can we distinguish when the function takes a single tuple and argument is just a single tuple
                let (atvvec, arg_states) = if args.len() == 1 {
                    let (ats, states) = self.eval_args(args);
                    let (arg_val, ty) = ats.first().unwrap().clone();
                    if ty.to_type().can_be_unpacked() {
                        (self.unpack_argument(f_val, arg_val, at, ty), states)
                    } else {
                        (vec![(arg_val, ty)], states)
                    }
                } else {
                    self.eval_args(args)
                };

                // Coerce arguments based on subtype relationships (union wrapping, int->float, etc.)
                let raw_atvvec = atvvec.clone();
                let atvvec = self.coerce_args_for_call(atvvec, at);

                let (res, state) = match f_to_call.as_ref() {
                    Value::Global(v) => match v.as_ref() {
                        Value::Function(idx) => {
                            self.emit_fncall(*idx as u64, atvvec.clone(), monomorphized_rt)
                        }
                        Value::Register(_) => (
                            self.push_inst(Instruction::CallIndirect(
                                v.clone(),
                                atvvec.clone(),
                                monomorphized_rt,
                            )),
                            vec![],
                        ),
                        _ => {
                            panic!("calling non-function global value")
                        }
                    },
                    Value::Register(_) => {
                        //closure
                        //do not increment state size for closure
                        (
                            self.push_inst(Instruction::CallIndirect(
                                f_to_call.clone(),
                                atvvec.clone(),
                                monomorphized_rt,
                            )),
                            vec![],
                        )
                    }
                    Value::Function(idx) => {
                        self.emit_fncall(*idx as u64, atvvec.clone(), monomorphized_rt)
                    }
                    Value::ExtFunction(label, _ty) => {
                        let (res, states) = if let (Some(res), states) = self.make_intrinsics(
                            *label,
                            &raw_atvvec,
                            &atvvec,
                            monomorphized_rt,
                        ) {
                            (res, states)
                        } else {
                            // we assume non-builtin external functions are stateless for now
                            (
                                self.push_inst(Instruction::Call(
                                    f_to_call.clone(),
                                    atvvec.clone(),
                                    monomorphized_rt,
                                )),
                                vec![],
                            )
                        };
                        (res, states)
                    }
                    // Value::ExternalClosure(i) => todo!(),
                    Value::None => unreachable!(),
                    _ => todo!(),
                };
                (
                    res,
                    monomorphized_rt,
                    [app_state, arg_states, state].concat(),
                )
            }

            Expr::Lambda(ids, _rett, body) => {
                let (atype, rt) = match ty.to_type() {
                    Type::Function { arg, ret } => (arg, ret),
                    _ => panic!(),
                };
                let binds = match ids.len() {
                    0 => vec![],
                    1 => {
                        let id = ids[0].clone();
                        let label = id.id;
                        vec![(label, atype, id.default_value)]
                    }
                    _ => {
                        let tys = atype
                            .to_type()
                            .get_as_tuple()
                            .expect("must be tuple or record type. type inference failed");
                        //multiple arguments
                        ids.iter()
                            .zip(tys.iter())
                            .map(|(id, ty)| {
                                let label = id.id;
                                (label, *ty, id.default_value)
                            })
                            .collect()
                    }
                };

                let name = self.consume_fnlabel();
                let (c_idx, f, _astates) =
                    self.do_in_child_ctx(name, &binds, vec![], |ctx, c_idx| {
                        let (res, _, states) = ctx.eval_expr(*body);

                        let child = ctx.program.functions.get_mut(c_idx.0 as usize).unwrap();
                        //todo set skeleton not by modifying but in initialization
                        if let StateTreeSkeleton::FnCall(child) = &mut child.state_skeleton {
                            *child = states.clone().into_iter().map(Box::new).collect();
                        }

                        let push_sum = ctx.get_ctxdata().push_sum;
                        if push_sum > 0 {
                            ctx.get_current_basicblock().0.push((
                                Arc::new(mir::Value::None),
                                Instruction::PopStateOffset(push_sum),
                            )); //todo:offset size
                        }
                        match (res.as_ref(), rt.to_type()) {
                            (_, Type::Primitive(PType::Unit)) => {
                                let _ =
                                    ctx.push_inst(Instruction::Return(Arc::new(Value::None), rt));
                            }
                            (Value::State(v), _) => {
                                let _ = ctx.push_inst(Instruction::ReturnFeed(v.clone(), rt));
                            }
                            (Value::Function(i), _) => {
                                let idx = ctx.push_inst(Instruction::Uinteger(*i as u64));
                                // TODO: Calculate actual closure size
                                let cls = ctx.push_inst(Instruction::MakeClosure {
                                    fn_proto: idx,
                                    size: 64,
                                });
                                ctx.insert_close_recursively(cls.clone(), rt);
                                ctx.insert_clone_recursively(cls.clone(), rt);
                                let _ = ctx.push_inst(Instruction::Return(cls, rt));
                            }
                            (_, _) => {
                                if rt.to_type().contains_function() || rt.to_type().contains_boxed()
                                {
                                    ctx.insert_close_recursively(res.clone(), rt);
                                    ctx.insert_clone_recursively(res.clone(), rt);
                                    let _ = ctx.push_inst(Instruction::Return(res.clone(), rt));
                                } else {
                                    let _ = ctx.push_inst(Instruction::Return(res.clone(), rt));
                                }
                            }
                        };

                        let f = Arc::new(Value::Function(c_idx.0 as usize));
                        (f, rt, states)
                    });
                let child = self.program.functions.get_mut(c_idx.0 as usize).unwrap();
                let res = if child.upindexes.is_empty() {
                    //todo:make Closure
                    f
                } else {
                    let idxcell = self.push_inst(Instruction::Uinteger(c_idx.0));
                    // TODO: Calculate actual closure size
                    self.push_inst(Instruction::MakeClosure {
                        fn_proto: idxcell,
                        size: 64,
                    })
                };
                (res, ty, vec![])
            }
            Expr::Feed(id, expr) => {
                debug_assert!(self.get_ctxdata().next_state_offset.is_none());

                let res = self.push_inst(Instruction::GetState(ty));
                let skeleton = StateTreeSkeleton::Feed(mir::StateType::from(ty));
                self.add_bind((*id, res.clone()));
                self.get_ctxdata().next_state_offset = Some(skeleton.total_size());
                let (retv, _t, states) = self.eval_expr(*expr);

                (
                    Arc::new(Value::State(retv)),
                    ty,
                    [states, vec![skeleton]].concat(),
                )
            }
            Expr::Let(pat, body, then) => {
                if let Ok(tid) = TypedId::try_from(pat.clone()) {
                    self.fn_label = Some(tid.id);
                    log::trace!(
                        "mirgen let {}",
                        self.fn_label.map_or("".to_string(), |s| s.to_string())
                    )
                };
                // let insert_bb = self.get_ctxdata().current_bb;
                // let insert_pos = if self.program.functions.is_empty() {
                //     0
                // } else {
                //     self.get_current_basicblock().0.len()
                // };
                let (bodyv, t, states) = self.eval_expr(*body);
                self.fn_label = None;

                let is_global = self.get_ctxdata().func_i.0 == 0;
                let is_function = matches!(bodyv.as_ref(), Value::Function(_));

                let ptr = if !is_global && !is_function {
                    // ローカル変数の場合、常にAllocaとStoreを使う
                    let ptr = self.push_inst(Instruction::Alloc(t));
                    self.push_inst(Instruction::Store(ptr.clone(), bodyv, t));
                    self.add_bind_pattern(pat, ptr.clone(), t, false);
                    Some((ptr, t))
                } else {
                    // グローバル変数や関数はこれまで通りの扱い
                    self.add_bind_pattern(pat, bodyv, t, is_global);
                    None
                };

                let result = if let Some(then_e) = then {
                    let (r, t_ret, s) = self.eval_expr(*then_e);
                    (r, t_ret, [states, s].concat())
                } else {
                    (Arc::new(Value::None), unit!(), states)
                };

                // ローカル変数がスコープから外れるときに解放
                if let Some((ptr, ty)) = ptr {
                    if ty.to_type().contains_boxed() || ty.to_type().contains_function() {
                        // Load the value and release it
                        let value = self.push_inst(Instruction::Load(ptr, ty));
                        self.insert_close_recursively(value, ty);
                    }
                }

                result
            }
            Expr::LetRec(id, body, then) => {
                let is_global = self.get_ctxdata().func_i.0 == 0;
                self.fn_label = Some(id.id);
                let nextfunid = self.program.functions.len();
                let t = self
                    .typeenv
                    .infer_type(e)
                    .expect("type inference failed, should be an error at type checker stage");
                let t = InferContext::substitute_type(t);

                let v = if is_global {
                    Arc::new(Value::Function(nextfunid))
                } else {
                    self.push_inst(Instruction::Alloc(t))
                };
                let bind = (id.id, v.clone());
                self.add_bind(bind);
                let (b, _bt, states) = self.eval_expr(*body);

                if !is_global {
                    let _ = self.push_inst(Instruction::Store(v.clone(), b.clone(), t));
                }
                if let Some(then_e) = then {
                    let (r, t, s) = self.eval_expr(*then_e);
                    (r, t, [states, s].concat())
                } else {
                    (Arc::new(Value::None), unit!(), states)
                }
            }
            Expr::Assign(assignee, body) => {
                let (src, ty, states) = self.eval_expr(*body);
                self.eval_assign(*assignee, src, ty);
                (Arc::new(Value::None), unit!(), states)
            }
            Expr::Then(body, then) => {
                let (_, _, states) = self.eval_expr(*body);
                match then {
                    Some(t) => {
                        let (r, t, s) = self.eval_expr(*t);
                        (r, t, [states, s].concat())
                    }
                    None => (Arc::new(Value::None), unit!(), states),
                }
            }
            Expr::If(cond, then, else_) => {
                let (c, _, state_c) = self.eval_expr(*cond);
                let cond_bidx = self.get_ctxdata().current_bb;

                // This is just a placeholder. At this point, the locations of
                // the block are not determined yet. These 0s will be
                // overwritten later.
                let _ = self.push_inst(Instruction::JmpIf(c, 0, 0, 0));
                //todo: state offset for branches
                //insert then block
                let then_bidx = cond_bidx + 1;
                let (t, _, state_t) = self.eval_block(Some(*then));
                //jmp to ret is inserted in bytecodegen
                //insert else block
                let else_bidx = self.get_ctxdata().current_bb + 1;
                let (e, _, state_e) = self.eval_block(*else_);
                let then_size = state_t.iter().map(|s| s.total_size()).sum::<u64>();
                let else_size = state_e.iter().map(|s| s.total_size()).sum::<u64>();
                let branch_state = match then_size.cmp(&else_size) {
                    std::cmp::Ordering::Greater => {
                        let elseb = self.get_current_fn().body.get_mut(else_bidx).unwrap();
                        elseb.0.push((
                            Arc::new(Value::None),
                            Instruction::PushStateOffset(then_size - else_size),
                        ));
                        state_t.clone()
                    }
                    std::cmp::Ordering::Less => {
                        let thenb = self.get_current_fn().body.get_mut(then_bidx).unwrap();
                        thenb.0.push((
                            Arc::new(Value::None),
                            Instruction::PushStateOffset(else_size - then_size),
                        ));
                        state_e.clone()
                    }
                    std::cmp::Ordering::Equal => state_t.clone(),
                };
                //insert return block
                self.add_new_basicblock();
                let res = self.push_inst(Instruction::Phi(t, e));
                let phi_bidx = self.get_ctxdata().current_bb;

                // overwrite JmpIf
                let jmp_if = self
                    .get_current_fn()
                    .body
                    .get_mut(cond_bidx)
                    .expect("no basic block found")
                    .0
                    .last_mut()
                    .expect("the block contains no inst?");
                match &mut jmp_if.1 {
                    Instruction::JmpIf(_, then_dst, else_dst, phi_dst) => {
                        *then_dst = then_bidx as _;
                        *else_dst = else_bidx as _;
                        *phi_dst = phi_bidx as _;
                    }
                    _ => panic!("the last block should be Jmp"),
                }

                (res, ty, [state_c, branch_state].concat())
            }
            Expr::Match(scrutinee, arms) => {
                // For now, implement match as a chain of if-else comparisons
                // This is a simple implementation for Phase 1 (integer patterns)
                self.eval_match(*scrutinee, arms, ty)
            }
            Expr::Bracket(_) | Expr::Escape(_) | Expr::MacroExpand(_, _) => {
                unreachable!("Macro code should be expanded before mirgen")
            }
            Expr::BinOp(_, _, _) | Expr::UniOp(_, _) | Expr::Paren(_) => {
                unreachable!(
                    "syntactic sugar for infix&unary operators are removed before this stage"
                )
            }
            Expr::Error => {
                self.push_inst(Instruction::Error);
                (Arc::new(Value::None), unit!(), vec![])
            }
        }
    }

    /// Extract integer value from a literal for use in switch cases
    fn literal_to_i64(lit: &crate::ast::Literal) -> i64 {
        use crate::ast::Literal;
        match lit {
            Literal::Int(i) => *i,
            Literal::Float(f) => f.as_str().parse::<f64>().expect("illegal float format") as i64,
            _ => todo!("Only integer/float literals are supported in match patterns"),
        }
    }

    /// Check if an argument needs to be wrapped in a union type.
    /// Returns Some(tag_index) if wrapping is needed, None otherwise.
    fn needs_union_wrapping(&self, arg_ty: TypeNodeId, param_ty: TypeNodeId) -> Option<u64> {
        use crate::types::Type;

        let arg_ty = arg_ty.get_root();
        let param_ty = param_ty.get_root();

        // If parameter is not a union, no wrapping needed
        let Type::Union(variants) = param_ty.to_type() else {
            return None;
        };

        // If argument type is already the union type, no wrapping needed
        if arg_ty == param_ty {
            return None;
        }

        // Find which variant in the union matches the argument type.
        // For MIR generation, we use simple type comparison since type inference
        // has already ensured compatibility. We first try exact match, then
        // structural compatibility for common cases.
        for (i, variant_ty) in variants.iter().enumerate() {
            let variant_ty = variant_ty.get_root();

            // Exact match
            if arg_ty == variant_ty {
                return Some(i as u64);
            }

            // Simple structural comparison for common cases
            if Self::types_compatible(arg_ty, variant_ty) {
                return Some(i as u64);
            }
        }

        // If no match found, return None (caller will handle this case)
        None
    }

    /// Simple type compatibility check for union variant matching.
    /// This is a lightweight check that handles common cases without deep recursion.
    fn types_compatible(t1: TypeNodeId, t2: TypeNodeId) -> bool {
        use crate::types::{PType, Type};

        let t1 = t1.get_root();
        let t2 = t2.get_root();

        if t1 == t2 {
            return true;
        }

        match (t1.to_type(), t2.to_type()) {
            // Int is compatible with Numeric (int -> float promotion) - check before general primitive match
            (Type::Primitive(PType::Int), Type::Primitive(PType::Numeric)) => true,

            // Same primitives are compatible
            (Type::Primitive(p1), Type::Primitive(p2)) => p1 == p2,

            // Intermediate types (unresolved type variables) - match anything
            (Type::Intermediate(_), _) | (_, Type::Intermediate(_)) => true,

            // Any and Failure match everything
            (Type::Any, _) | (_, Type::Any) => true,
            (Type::Failure, _) | (_, Type::Failure) => true,

            // For other cases, require exact TypeNodeId match
            _ => false,
        }
    }

    /// Coerce a value from one type to another based on subtype relationship.
    /// Handles:
    /// - Union wrapping: wrapping a value into a tagged union (e.g., float -> float | string)
    /// - Primitive coercion: int -> float promotion
    /// Type inference guarantees the coercion is valid.
    fn coerce_value(&mut self, value: VPtr, arg_ty: TypeNodeId, param_ty: TypeNodeId) -> VPtr {
        use crate::types::{PType, Type};

        let arg_ty = arg_ty.get_root();
        let param_ty = param_ty.get_root();

        // Fast path: identical types need no coercion
        if arg_ty == param_ty {
            return value;
        }

        // Only check coercion for types that actually need it
        match param_ty.to_type() {
            Type::Union(_) => {
                // Union wrapping - need to find which variant matches
                if let Some(tag) = self.needs_union_wrapping(arg_ty, param_ty) {
                    log::debug!(
                        "Wrapping {:?} (type {}) in union type {} with tag {}",
                        value,
                        arg_ty.to_type(),
                        param_ty.to_type(),
                        tag
                    );
                    self.push_inst(Instruction::TaggedUnionWrap {
                        tag,
                        value,
                        union_type: param_ty,
                    })
                } else {
                    value
                }
            }
            Type::Primitive(PType::Numeric) => {
                // int -> float promotion
                match arg_ty.to_type() {
                    Type::Primitive(PType::Int) => self.push_inst(Instruction::CastItoF(value)),
                    _ => value,
                }
            }
            // For all other types, type inference guarantees compatibility
            _ => value,
        }
    }

    /// Coerce function arguments to match expected parameter types.
    /// Handles subtype coercion including union wrapping and primitive type promotion.
    fn coerce_args_for_call(
        &mut self,
        args: Vec<(VPtr, TypeNodeId)>,
        param_ty: TypeNodeId,
    ) -> Vec<(VPtr, TypeNodeId)> {
        use crate::types::Type;

        match param_ty.to_type() {
            Type::Tuple(param_types) if args.len() == param_types.len() => {
                // Multiple parameters - coerce each one individually
                args.into_iter()
                    .zip(param_types.iter())
                    .map(|((val, arg_ty), &expected_ty)| {
                        let coerced_val = self.coerce_value(val, arg_ty, expected_ty);
                        (coerced_val, expected_ty)
                    })
                    .collect()
            }
            Type::Record(fields) if args.len() == fields.len() => {
                // Record parameters - coerce each field
                args.into_iter()
                    .zip(fields.iter())
                    .map(|((val, arg_ty), field)| {
                        let coerced_val = self.coerce_value(val, arg_ty, field.ty);
                        (coerced_val, field.ty)
                    })
                    .collect()
            }
            _ if args.len() == 1 => {
                // Single parameter - coerce it if needed
                let (val, arg_ty) = args.into_iter().next().unwrap();
                let coerced_val = self.coerce_value(val, arg_ty, param_ty);
                vec![(coerced_val, param_ty)]
            }
            _ => args, // No coercion needed
        }
    }

    /// Auto-box fields in a constructor payload that need Boxed wrapping.
    ///
    /// When a constructor's registered payload type contains Boxed fields (from
    /// `type rec` declarations), the actual argument values are plain (non-heap)
    /// values. This function inserts BoxAlloc instructions to heap-allocate them.
    ///
    /// For a single-field payload that is directly Boxed, wraps it directly.
    /// For a tuple payload, reconstructs the tuple with any Boxed fields wrapped.
    fn box_fields_if_needed(
        &mut self,
        payload_val: VPtr,
        actual_ty: TypeNodeId,
        constructor_name: Symbol,
    ) -> VPtr {
        use crate::types::Type;

        // Look up the constructor's registered payload type (which includes Boxed)
        let expected_payload_ty = self
            .typeenv
            .constructor_env
            .get(&constructor_name)
            .and_then(|info| info.payload_type);

        let Some(expected_ty) = expected_payload_ty else {
            return payload_val;
        };

        match expected_ty.to_type() {
            Type::Boxed(inner) => {
                // Entire payload is Boxed — wrap it
                self.push_inst(Instruction::BoxAlloc {
                    value: payload_val,
                    inner_type: inner,
                })
            }
            Type::Tuple(expected_elems) => {
                // Check if any element is Boxed; if so we need to rebuild the tuple
                let actual_elems = match actual_ty.to_type() {
                    Type::Tuple(elems) => elems,
                    _ => return payload_val, // not a tuple, nothing to do
                };

                let has_boxed = expected_elems
                    .iter()
                    .any(|e| matches!(e.to_type(), Type::Boxed(_)));

                if !has_boxed {
                    return payload_val;
                }

                // Rebuild the tuple, boxing any Boxed-typed fields
                let mut new_elems: Vec<(VPtr, TypeNodeId)> = Vec::new();
                for (i, (expected_elem_ty, actual_elem_ty)) in
                    expected_elems.iter().zip(actual_elems.iter()).enumerate()
                {
                    let elem_val = self.push_inst(Instruction::GetElement {
                        value: payload_val.clone(),
                        ty: actual_ty,
                        tuple_offset: i as u64,
                    });

                    let elem_val = if let Type::Boxed(inner) = expected_elem_ty.to_type() {
                        self.push_inst(Instruction::BoxAlloc {
                            value: elem_val,
                            inner_type: inner,
                        })
                    } else {
                        elem_val
                    };

                    new_elems.push((elem_val, *expected_elem_ty));
                }

                // Build a new tuple from the (possibly boxed) elements
                let tuple_ptr = self.push_inst(Instruction::Alloc(expected_ty));
                for (i, (elem_val, elem_ty)) in new_elems.iter().enumerate() {
                    let dest = self.push_inst(Instruction::GetElement {
                        value: tuple_ptr.clone(),
                        ty: expected_ty,
                        tuple_offset: i as u64,
                    });
                    self.push_inst(Instruction::Store(dest, elem_val.clone(), *elem_ty));
                }
                self.push_inst(Instruction::Load(tuple_ptr, expected_ty))
            }
            _ => payload_val,
        }
    }

    /// Bind variables from a match pattern to extracted values
    /// Handles variable bindings, tuple patterns, and nested patterns
    fn bind_pattern(&mut self, pattern: &crate::ast::MatchPattern, value: VPtr, ty: TypeNodeId) {
        use crate::ast::MatchPattern;
        use crate::compiler::EvalStage;
        use crate::types::Type;

        match pattern {
            MatchPattern::Variable(var) => {
                // Allocate stack space and store the value, then bind the pointer
                let ptr = self.push_inst(Instruction::Alloc(ty));
                self.push_inst(Instruction::Store(ptr.clone(), value, ty));
                self.add_bind((*var, ptr));
                // Also add to type environment so infer_type can find it
                // MIR generation is at Stage 1 (Stage 0 is for macro evaluation)
                self.typeenv
                    .env
                    .add_bind(&[(*var, (ty, EvalStage::Stage(1)))]);
            }
            MatchPattern::Wildcard => {
                // No binding needed
            }
            MatchPattern::Literal(_) => {
                // No binding for literal patterns
            }
            MatchPattern::Tuple(patterns) => {
                // For tuple patterns, extract each element and bind recursively
                if let Type::Tuple(elem_types) = ty.to_type() {
                    for (i, (pat, elem_ty)) in patterns.iter().zip(elem_types.iter()).enumerate() {
                        let elem_val = self.push_inst(Instruction::GetElement {
                            value: value.clone(),
                            ty,
                            tuple_offset: i as u64,
                        });
                        // If the element type is Boxed, unbox it before binding
                        let (elem_val, bind_ty) = if let Type::Boxed(inner) = elem_ty.to_type() {
                            let unboxed = self.push_inst(Instruction::BoxLoad {
                                ptr: elem_val,
                                inner_type: inner,
                            });
                            (unboxed, inner)
                        } else {
                            (elem_val, *elem_ty)
                        };
                        self.bind_pattern(pat, elem_val, bind_ty);
                    }
                }
            }
            MatchPattern::Constructor(_, inner) => {
                // For nested constructor patterns, recursively bind the inner pattern
                if let Some(inner_pat) = inner {
                    self.bind_pattern(inner_pat, value, ty);
                }
            }
        }
    }

    /// Evaluate a match expression on a union type using tagged union operations
    fn eval_union_match(
        &mut self,
        scrut_val: VPtr,
        scrut_ty: TypeNodeId,
        arms: &[crate::ast::MatchArm],
        result_ty: TypeNodeId,
        mut states: Vec<StateSkeleton>,
    ) -> (VPtr, TypeNodeId, Vec<StateSkeleton>) {
        use crate::ast::MatchPattern;
        use crate::interner::ToSymbol;
        use crate::types::Type;

        // Build a map from constructor names to (tag_index, variant_type)
        // For UserSum types, variant_type is None (no payload)
        let mut constructor_map: std::collections::HashMap<Symbol, (i64, Option<TypeNodeId>)> =
            std::collections::HashMap::new();

        // For UserSum types, the scrutinee is already the tag value (integer)
        // For Union types, we need to extract the tag from a tagged union
        let tag_val = match scrut_ty.to_type() {
            Type::UserSum { name: _, variants } => {
                // UserSum: get tag from tagged union
                let tag_val = self.push_inst(Instruction::TaggedUnionGetTag(scrut_val.clone()));
                for (tag_idx, (variant_name, payload_ty)) in variants.iter().enumerate() {
                    constructor_map.insert(*variant_name, (tag_idx as i64, *payload_ty));
                }
                tag_val
            }
            Type::Union(variants) => {
                // Union: extract tag from tagged union
                let tag_val = self.push_inst(Instruction::TaggedUnionGetTag(scrut_val.clone()));
                for (tag_idx, variant_ty) in variants.iter().enumerate() {
                    use crate::types::PType;
                    let constructor_name = match variant_ty.to_type() {
                        Type::Primitive(PType::Numeric) => "float".to_symbol(),
                        Type::Primitive(PType::String) => "string".to_symbol(),
                        Type::Primitive(PType::Int) => "int".to_symbol(),
                        _ => continue,
                    };
                    constructor_map.insert(constructor_name, (tag_idx as i64, Some(*variant_ty)));
                }
                tag_val
            }
            _ => panic!("eval_union_match called on non-union type"),
        };

        // Collect constructor pattern arms and map them to tag values
        let tag_arms: Vec<_> = arms
            .iter()
            .filter_map(|arm| match &arm.pattern {
                MatchPattern::Constructor(name, _) => {
                    if let Some(&(tag, variant_ty)) = constructor_map.get(name) {
                        Some((arm, tag, variant_ty))
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect();

        // Find wildcard arm (default case)
        let default_arm = arms
            .iter()
            .find(|arm| matches!(&arm.pattern, MatchPattern::Wildcard));

        // Record current block where Switch will be placed
        let switch_bidx = self.get_ctxdata().current_bb;

        // Placeholder Switch instruction on the tag - will be updated later
        let _ = self.push_inst(Instruction::Switch {
            scrutinee: tag_val.clone(),
            cases: vec![],
            default_block: None,
            merge_block: 0,
        });

        // Generate blocks for each constructor pattern
        let (case_blocks, case_results, case_states): (Vec<_>, Vec<_>, Vec<_>) = tag_arms
            .iter()
            .map(|(arm, tag, variant_ty)| {
                self.add_new_basicblock();
                let block_idx = self.get_ctxdata().current_bb as u64;

                // Reset state offset at the start of each arm
                // This ensures each arm starts with a clean state context
                self.get_ctxdata().next_state_offset = None;
                self.get_ctxdata().push_sum = 0;

                // Extract value from the tagged union if there's a binding pattern and payload type
                if let MatchPattern::Constructor(_, Some(inner_pattern)) = &arm.pattern
                    && let Some(vt) = *variant_ty
                {
                    let bound_val =
                        self.push_inst(Instruction::TaggedUnionGetValue(scrut_val.clone(), vt));
                    // Bind the pattern to the extracted value
                    self.bind_pattern(inner_pattern, bound_val, vt);
                }

                let (result_val, _, arm_states) = self.eval_expr(arm.body);
                ((*tag, block_idx), result_val, arm_states)
            })
            .fold(
                (vec![], vec![], vec![]),
                |(mut blocks, mut results, mut states), (block, result, arm_states)| {
                    blocks.push(block);
                    results.push(result);
                    states.push(arm_states);
                    (blocks, results, states)
                },
            );

        let mut case_results = case_results;
        let mut all_arm_states = case_states;

        // Handle default block - only create one if there's an explicit wildcard pattern
        let default_block_idx = if let Some(arm) = default_arm {
            // Explicit wildcard default case
            self.add_new_basicblock();
            let block_idx = self.get_ctxdata().current_bb as u64;

            // Reset state offset for default arm
            self.get_ctxdata().next_state_offset = None;
            self.get_ctxdata().push_sum = 0;

            let (result_val, _, arm_states) = self.eval_expr(arm.body);
            all_arm_states.push(arm_states);
            case_results.push(result_val);
            Some(block_idx)
        } else {
            // Exhaustive match - no default block needed
            None
        };

        // Calculate maximum state size across all arms
        let arm_state_sizes: Vec<u64> = all_arm_states
            .iter()
            .map(|states| states.iter().map(|s| s.total_size()).sum::<u64>())
            .collect();
        let max_state_size = arm_state_sizes.iter().copied().max().unwrap_or(0);

        // Insert PushStateOffset for arms with smaller state sizes
        // This ensures all arms have the same state offset when merging
        for (i, ((_tag, block_idx), state_size)) in
            case_blocks.iter().zip(arm_state_sizes.iter()).enumerate()
        {
            if *state_size < max_state_size {
                let offset = max_state_size - state_size;
                let block = self
                    .get_current_fn()
                    .body
                    .get_mut(*block_idx as usize)
                    .unwrap();
                // Insert PushStateOffset at the end of the block (before result)
                block
                    .0
                    .push((Arc::new(Value::None), Instruction::PushStateOffset(offset)));
            }
        }

        // Handle default block state adjustment if it exists
        if let Some(default_idx) = default_block_idx {
            let default_state_size = arm_state_sizes.last().copied().unwrap_or(0);
            if default_state_size < max_state_size {
                let offset = max_state_size - default_state_size;
                let block = self
                    .get_current_fn()
                    .body
                    .get_mut(default_idx as usize)
                    .unwrap();
                block
                    .0
                    .push((Arc::new(Value::None), Instruction::PushStateOffset(offset)));
            }
        }

        // Generate merge block with PhiSwitch
        self.add_new_basicblock();
        let merge_block_idx = self.get_ctxdata().current_bb as u64;
        let res = self.push_inst(Instruction::PhiSwitch(case_results));

        // Update Switch instruction with correct block indices
        let switch_inst = self
            .get_current_fn()
            .body
            .get_mut(switch_bidx)
            .expect("no basic block found")
            .0
            .last_mut()
            .expect("block contains no inst");

        match &mut switch_inst.1 {
            Instruction::Switch {
                cases,
                default_block,
                merge_block,
                ..
            } => {
                *cases = case_blocks;
                *default_block = default_block_idx;
                *merge_block = merge_block_idx;
            }
            _ => panic!("expected Switch instruction"),
        }

        // Use the largest arm's state as the result state
        // This represents the maximum state size across all branches
        // But we need to collect all states from all arms for the function's state signature
        for arm_states in all_arm_states {
            states.extend(arm_states);
        }

        (res, result_ty, states)
    }

    /// Evaluate a match expression using Switch instruction
    fn eval_match(
        &mut self,
        scrutinee: ExprNodeId,
        arms: &[crate::ast::MatchArm],
        result_ty: TypeNodeId,
    ) -> (VPtr, TypeNodeId, Vec<StateSkeleton>) {
        use crate::ast::MatchPattern;
        use crate::types::Type;

        if arms.is_empty() {
            return (Arc::new(Value::None), unit!(), vec![]);
        }

        let (scrut_val, scrut_ty, mut states) = self.eval_expr(scrutinee);

        // Check if scrutinee is a union type or user-defined sum type
        let is_union_match = matches!(scrut_ty.to_type(), Type::Union(_) | Type::UserSum { .. });

        if is_union_match {
            // Phase 2: Union type matching with constructor patterns
            return self.eval_union_match(scrut_val, scrut_ty, arms, result_ty, states);
        }

        // Check if scrutinee is a tuple type - Phase 6: multi-scrutinee matching
        if let Type::Tuple(elem_types) = scrut_ty.to_type() {
            let (v, state) = self.eval_tuple_match(scrut_val, scrut_ty, &elem_types, arms, states);
            return (v, result_ty, state);
        }

        // Cast float scrutinee to int for switch instruction
        // This ensures JmpTable always receives integer values
        let scrut_val = if matches!(scrut_ty.to_type(), Type::Primitive(PType::Numeric)) {
            self.push_inst(Instruction::CastFtoI(scrut_val))
        } else {
            scrut_val
        };

        // Phase 1: Integer literal matching (existing implementation)
        // Collect literal cases (as i64) and find wildcard (default) arm
        let literal_arms: Vec<_> = arms
            .iter()
            .filter_map(|arm| match &arm.pattern {
                MatchPattern::Literal(lit) => Some((arm, Self::literal_to_i64(lit))),
                MatchPattern::Wildcard | MatchPattern::Variable(_) | MatchPattern::Tuple(_) => None,
                MatchPattern::Constructor(_, _) => {
                    // Constructor patterns should not appear in non-union matches
                    None
                }
            })
            .collect();

        let default_arm = arms
            .iter()
            .find(|arm| matches!(&arm.pattern, MatchPattern::Wildcard));

        // Record current block where Switch will be placed
        let switch_bidx = self.get_ctxdata().current_bb;

        // Placeholder Switch instruction - will be updated later
        let _ = self.push_inst(Instruction::Switch {
            scrutinee: scrut_val.clone(),
            cases: vec![],
            default_block: None,
            merge_block: 0,
        });

        // Generate blocks for each literal case
        let (case_blocks, case_results, case_states): (Vec<_>, Vec<_>, Vec<_>) = literal_arms
            .iter()
            .map(|(arm, lit_val)| {
                self.add_new_basicblock();
                let block_idx = self.get_ctxdata().current_bb as u64;
                let (result_val, _, arm_states) = self.eval_expr(arm.body);
                ((*lit_val, block_idx), result_val, arm_states)
            })
            .fold(
                (vec![], vec![], vec![]),
                |(mut blocks, mut results, mut states), (block, result, arm_states)| {
                    blocks.push(block);
                    results.push(result);
                    states.extend(arm_states);
                    (blocks, results, states)
                },
            );

        let mut case_results = case_results;
        let mut all_states = case_states;

        // Handle default block - only create one if there's an explicit wildcard pattern
        let default_block_idx = if let Some(arm) = default_arm {
            // Wildcard pattern - just evaluate the body
            self.add_new_basicblock();
            let block_idx = self.get_ctxdata().current_bb as u64;
            let (result_val, _, arm_states) = self.eval_expr(arm.body);
            all_states.extend(arm_states);
            case_results.push(result_val);
            Some(block_idx)
        } else {
            // Exhaustive match - no default block needed
            None
        };

        // Generate merge block with PhiSwitch
        self.add_new_basicblock();
        let merge_block_idx = self.get_ctxdata().current_bb as u64;
        let res = self.push_inst(Instruction::PhiSwitch(case_results));

        // Update Switch instruction with correct block indices
        let switch_inst = self
            .get_current_fn()
            .body
            .get_mut(switch_bidx)
            .expect("no basic block found")
            .0
            .last_mut()
            .expect("block contains no inst");

        match &mut switch_inst.1 {
            Instruction::Switch {
                cases,
                default_block,
                merge_block,
                ..
            } => {
                *cases = case_blocks;
                *default_block = default_block_idx;
                *merge_block = merge_block_idx;
            }
            _ => panic!("expected Switch instruction"),
        }

        states.extend(all_states);
        (res, result_ty, states)
    }

    /// Evaluate a match expression with tuple scrutinee (multi-scrutinee matching)
    /// Uses Decision Tree algorithm for efficient pattern matching
    fn eval_tuple_match(
        &mut self,
        scrut_val: VPtr,
        scrut_ty: TypeNodeId,
        elem_types: &[TypeNodeId],
        arms: &[crate::ast::MatchArm],

        mut states: Vec<StateSkeleton>,
    ) -> (VPtr, Vec<StateSkeleton>) {
        if arms.is_empty() {
            return (Arc::new(Value::None), vec![]);
        }

        // Build pattern matrix from match arms
        let pattern_matrix = self.build_pattern_matrix(arms, elem_types.len());

        // Build decision tree from pattern matrix
        // remaining_cols starts as [0, 1, 2, ..., num_columns-1]
        let remaining_cols: Vec<usize> = (0..elem_types.len()).collect();
        let decision_tree = Self::build_decision_tree(&pattern_matrix, &remaining_cols);

        // Compile decision tree to MIR
        // Pass the tuple value and type so each block can extract elements locally
        let (res, tree_states) =
            self.compile_decision_tree(&decision_tree, &scrut_val, scrut_ty, elem_types);

        states.extend(tree_states);
        (res, states)
    }

    /// Build pattern matrix from match arms
    fn build_pattern_matrix(
        &self,
        arms: &[crate::ast::MatchArm],
        num_columns: usize,
    ) -> Vec<PatternRow> {
        use crate::ast::MatchPattern;

        arms.iter()
            .enumerate()
            .map(|(arm_index, arm)| {
                let cells = match &arm.pattern {
                    MatchPattern::Tuple(patterns) => patterns
                        .iter()
                        .map(|p| self.match_pattern_to_cell(p))
                        .collect(),
                    MatchPattern::Wildcard => vec![PatternCell::Wildcard; num_columns],
                    MatchPattern::Variable(v) => vec![PatternCell::Variable(*v); num_columns],
                    _ => vec![PatternCell::Wildcard; num_columns],
                };

                PatternRow {
                    cells,
                    arm_index,
                    body: arm.body,
                }
            })
            .collect()
    }

    /// Convert a MatchPattern to a PatternCell
    fn match_pattern_to_cell(&self, pattern: &crate::ast::MatchPattern) -> PatternCell {
        use crate::ast::MatchPattern;

        match pattern {
            MatchPattern::Literal(lit) => PatternCell::Literal(Self::literal_to_i64(lit)),
            MatchPattern::Wildcard => PatternCell::Wildcard,
            MatchPattern::Variable(v) => PatternCell::Variable(*v),
            MatchPattern::Constructor(name, inner) => {
                // Get tag from constructor name
                let (tag, payload_ty) = self
                    .typeenv
                    .constructor_env
                    .get(name)
                    .map(|info| (info.tag_index as i64, info.payload_type))
                    .unwrap_or((0, None));
                PatternCell::Constructor {
                    tag,
                    payload_ty,
                    inner: inner
                        .as_ref()
                        .map(|p| Box::new(self.match_pattern_to_cell(p))),
                }
            }
            MatchPattern::Tuple(patterns) => PatternCell::Tuple(
                patterns
                    .iter()
                    .map(|p| self.match_pattern_to_cell(p))
                    .collect(),
            ),
        }
    }

    /// Collect variable bindings from a pattern cell recursively
    fn collect_bindings_from_cell(cell: &PatternCell, col_idx: usize) -> Vec<BindingInfo> {
        match cell {
            PatternCell::Variable(v) => vec![BindingInfo {
                var: *v,
                column_index: col_idx,
                payload: None,
            }],
            PatternCell::Constructor {
                payload_ty, inner, ..
            } => {
                if let (Some(payload_ty), Some(inner_cell)) = (payload_ty, inner.as_ref()) {
                    Self::collect_bindings_from_payload(inner_cell, col_idx, *payload_ty)
                } else {
                    vec![]
                }
            }
            PatternCell::Payload { payload_ty, inner } => {
                Self::collect_bindings_from_payload(inner, col_idx, *payload_ty)
            }
            PatternCell::Tuple(_) => vec![],
            _ => vec![],
        }
    }

    /// Collect bindings from a constructor payload pattern.
    /// `payload_ty` is the runtime type needed for `TaggedUnionGetValue`.
    fn collect_bindings_from_payload(
        cell: &PatternCell,
        col_idx: usize,
        payload_ty: TypeNodeId,
    ) -> Vec<BindingInfo> {
        match cell {
            PatternCell::Variable(v) => vec![BindingInfo {
                var: *v,
                column_index: col_idx,
                payload: Some((payload_ty, None)),
            }],
            PatternCell::Tuple(cells) => cells
                .iter()
                .enumerate()
                .flat_map(|(elem_idx, c)| {
                    Self::collect_bindings_from_payload(c, col_idx, payload_ty)
                        .into_iter()
                        .map(move |mut b| {
                            b.payload = Some((payload_ty, Some(elem_idx)));
                            b
                        })
                })
                .collect(),
            _ => vec![],
        }
    }

    /// Build decision tree from pattern matrix using column-based algorithm
    /// `remaining_cols` tracks which original column indices are still available
    fn build_decision_tree(matrix: &[PatternRow], remaining_cols: &[usize]) -> DecisionTree {
        if matrix.is_empty() {
            return DecisionTree::Fail;
        }

        // Find first remaining column with concrete patterns
        let discriminating_col_pos =
            remaining_cols
                .iter()
                .enumerate()
                .find_map(|(pos, &orig_col)| {
                    let has_concrete = matrix.iter().any(|row| {
                        orig_col < row.cells.len()
                            && !matches!(
                                &row.cells[orig_col],
                                PatternCell::Wildcard
                                    | PatternCell::Variable(_)
                                    | PatternCell::Tuple(_)
                                    | PatternCell::Payload { .. }
                            )
                    });
                    if has_concrete { Some(pos) } else { None }
                });

        match discriminating_col_pos {
            None => {
                // All patterns are wildcards/variables/matched constructors - take first row
                let row = &matrix[0];
                // Collect bindings from all patterns including constructor payloads
                let bindings = remaining_cols
                    .iter()
                    .flat_map(|&orig_col| {
                        if orig_col < row.cells.len() {
                            Self::collect_bindings_from_cell(&row.cells[orig_col], orig_col)
                        } else {
                            vec![]
                        }
                    })
                    .collect();

                DecisionTree::Leaf {
                    arm_index: row.arm_index,
                    body: row.body,
                    bindings,
                }
            }
            Some(pos) => {
                let orig_col = remaining_cols[pos];

                // Step 1: Collect all concrete values and wildcards separately
                let mut concrete_rows: BTreeMap<i64, Vec<PatternRow>> = BTreeMap::new();
                let mut wildcard_rows: Vec<PatternRow> = Vec::new();

                for row in matrix {
                    if orig_col >= row.cells.len() {
                        wildcard_rows.push(row.clone());
                        continue;
                    }

                    match &row.cells[orig_col] {
                        PatternCell::Literal(val) => {
                            concrete_rows.entry(*val).or_default().push(row.clone());
                        }
                        PatternCell::Constructor { tag, .. } => {
                            concrete_rows.entry(*tag).or_default().push(row.clone());
                        }
                        PatternCell::Wildcard | PatternCell::Variable(_) => {
                            wildcard_rows.push(row.clone());
                        }
                        PatternCell::Payload { .. } => {
                            // Already matched by constructor tag; treat as wildcard here.
                            wildcard_rows.push(row.clone());
                        }
                        PatternCell::Tuple(_) => {
                            // Nested tuple - treat as wildcard for now
                            wildcard_rows.push(row.clone());
                        }
                    }
                }

                // Build remaining_cols for subtrees (remove the current column)
                let new_remaining_cols: Vec<usize> = remaining_cols
                    .iter()
                    .enumerate()
                    .filter_map(|(i, &c)| if i != pos { Some(c) } else { None })
                    .collect();

                // Step 2: Create specialized matrices for each case
                // Wildcards apply to ALL cases, so append them to each concrete case.
                // For Constructor patterns, replace the matched constructor cell with `Payload`
                // so leaf binding collection knows how to extract payload values.
                // Note: we keep `remaining_cols` (not `new_remaining_cols`) for case subtrees.
                let case_trees: Vec<(i64, Box<DecisionTree>)> = concrete_rows
                    .into_iter()
                    .map(|(val, rows)| {
                        // Transform rows: replace Constructor cells with their inner patterns
                        let mut transformed_rows: Vec<PatternRow> = rows
                            .into_iter()
                            .map(|mut row| {
                                if orig_col < row.cells.len() {
                                    if let PatternCell::Constructor {
                                        payload_ty, inner, ..
                                    } = &row.cells[orig_col]
                                    {
                                        row.cells[orig_col] = match (payload_ty, inner.as_ref()) {
                                            (Some(p_ty), Some(inner_cell)) => {
                                                PatternCell::Payload {
                                                    payload_ty: *p_ty,
                                                    inner: Box::new((**inner_cell).clone()),
                                                }
                                            }
                                            _ => PatternCell::Wildcard,
                                        };
                                    } else {
                                        // For literals, replace with Wildcard
                                        row.cells[orig_col] = PatternCell::Wildcard;
                                    }
                                }
                                row
                            })
                            .collect();
                        // Add wildcard rows to this case
                        transformed_rows.extend(wildcard_rows.iter().cloned());
                        // Use remaining_cols (not new_remaining_cols) since we replaced the cell
                        let subtree = Self::build_decision_tree(&transformed_rows, remaining_cols);
                        (val, Box::new(subtree))
                    })
                    .collect();

                // Build default subtree
                let default_tree = if !wildcard_rows.is_empty() {
                    Some(Box::new(Self::build_decision_tree(
                        &wildcard_rows,
                        &new_remaining_cols,
                    )))
                } else {
                    None
                };

                DecisionTree::Switch {
                    scrutinee_index: orig_col,
                    cases: case_trees,
                    default: default_tree,
                }
            }
        }
    }

    /// Compile decision tree to MIR instructions
    /// Takes the original tuple value and type so each block can extract elements locally
    fn compile_decision_tree(
        &mut self,
        tree: &DecisionTree,
        tuple_val: &VPtr,
        tuple_ty: TypeNodeId,
        elem_types: &[TypeNodeId],
    ) -> (VPtr, Vec<StateSkeleton>) {
        match tree {
            DecisionTree::Leaf {
                arm_index: _,
                body,
                bindings,
            } => {
                // Bind variables by extracting from tuple
                for binding in bindings {
                    let col_idx = binding.column_index;
                    if col_idx >= elem_types.len() {
                        continue;
                    }

                    match binding.payload {
                        Some((payload_ty, tuple_index)) => {
                            // Payload binding - extract from tagged union payload.
                            let enum_ptr = self.push_inst(Instruction::GetElement {
                                value: tuple_val.clone(),
                                ty: tuple_ty,
                                tuple_offset: col_idx as u64,
                            });
                            // Load the enum value from pointer
                            let enum_val_ty = elem_types[col_idx];
                            let enum_val = self.push_inst(Instruction::Load(enum_ptr, enum_val_ty));
                            let payload = self
                                .push_inst(Instruction::TaggedUnionGetValue(enum_val, payload_ty));

                            if let Some(elem_idx) = tuple_index {
                                let payload_elem = self.push_inst(Instruction::GetElement {
                                    value: payload.clone(),
                                    ty: payload_ty,
                                    tuple_offset: elem_idx as u64,
                                });
                                self.add_bind((binding.var, payload_elem));
                            } else {
                                self.add_bind((binding.var, payload));
                            }
                        }
                        None => {
                            // Regular variable binding from tuple element
                            let elem_val = self.push_inst(Instruction::GetElement {
                                value: tuple_val.clone(),
                                ty: tuple_ty,
                                tuple_offset: col_idx as u64,
                            });
                            self.add_bind((binding.var, elem_val));
                        }
                    }
                }
                // Evaluate body
                let (result, _, states) = self.eval_expr(*body);
                (result, states)
            }

            DecisionTree::Switch {
                scrutinee_index,
                cases,
                default,
            } => {
                if cases.is_empty() {
                    // No cases - just use default or fail
                    if let Some(default_tree) = default {
                        return self.compile_decision_tree(
                            default_tree,
                            tuple_val,
                            tuple_ty,
                            elem_types,
                        );
                    } else {
                        return (Arc::new(Value::None), vec![]);
                    }
                }

                // Get scrutinee element value from tuple
                let elem_val = self.push_inst(Instruction::GetElement {
                    value: tuple_val.clone(),
                    ty: tuple_ty,
                    tuple_offset: *scrutinee_index as u64,
                });

                // Check if the element type is an enum (UserSum) - if so, extract the tag
                let elem_ty = if *scrutinee_index < elem_types.len() {
                    elem_types[*scrutinee_index]
                } else {
                    tuple_ty
                };

                let scrut_int = if matches!(elem_ty.to_type(), Type::UserSum { .. }) {
                    // Load the value from the pointer, then extract the tag
                    let loaded_val = self.push_inst(Instruction::Load(elem_val.clone(), elem_ty));
                    // Tagged union tags are stored as integer RawVal already.
                    self.push_inst(Instruction::TaggedUnionGetTag(loaded_val))
                } else {
                    // For literals (numbers), just cast to int
                    self.push_inst(Instruction::CastFtoI(elem_val))
                };

                // Record current block for Switch instruction
                let switch_bb = self.get_ctxdata().current_bb;

                // Placeholder Switch
                let _ = self.push_inst(Instruction::Switch {
                    scrutinee: scrut_int.clone(),
                    cases: vec![],
                    default_block: None,
                    merge_block: 0,
                });

                // Generate blocks for each case
                let mut case_blocks: Vec<(i64, u64)> = Vec::new();
                let mut case_results: Vec<VPtr> = Vec::new();
                let mut all_states: Vec<StateSkeleton> = Vec::new();

                for (val, subtree) in cases {
                    self.add_new_basicblock();
                    let block_idx = self.get_ctxdata().current_bb as u64;
                    let (result, states) =
                        self.compile_decision_tree(subtree, tuple_val, tuple_ty, elem_types);
                    case_blocks.push((*val, block_idx));
                    case_results.push(result);
                    all_states.extend(states);
                }

                // Generate default block if present
                let default_block_idx = if let Some(default_tree) = default {
                    self.add_new_basicblock();
                    let block_idx = self.get_ctxdata().current_bb as u64;
                    let (result, states) =
                        self.compile_decision_tree(default_tree, tuple_val, tuple_ty, elem_types);
                    case_results.push(result);
                    all_states.extend(states);
                    Some(block_idx)
                } else {
                    None
                };

                // Generate merge block
                self.add_new_basicblock();
                let merge_block_idx = self.get_ctxdata().current_bb as u64;
                let res = self.push_inst(Instruction::PhiSwitch(case_results));

                // Update Switch instruction
                let switch_inst = self
                    .get_current_fn()
                    .body
                    .get_mut(switch_bb)
                    .expect("no basic block found")
                    .0
                    .last_mut()
                    .expect("block contains no inst");

                if let Instruction::Switch {
                    cases,
                    default_block,
                    merge_block,
                    ..
                } = &mut switch_inst.1
                {
                    *cases = case_blocks;
                    *default_block = default_block_idx;
                    *merge_block = merge_block_idx;
                }

                (res, all_states)
            }

            DecisionTree::Fail => (Arc::new(Value::None), vec![]),
        }
    }
}

fn is_toplevel_macro(typeenv: &mut InferContext, top_ast: ExprNodeId) -> bool {
    typeenv.infer_type(top_ast).is_ok_and(|t| {
        log::trace!("toplevel type: {}", t.to_type());
        matches!(t.to_type(), Type::Code(_))
    })
}

pub fn typecheck(
    root_expr_id: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    file_path: Option<PathBuf>,
) -> (ExprNodeId, InferContext, Vec<Box<dyn ReportableError>>) {
    let (expr, convert_errs) =
        convert_pronoun::convert_pronoun(root_expr_id, file_path.clone().unwrap_or_default());
    let expr = recursecheck::convert_recurse(expr, file_path.clone().unwrap_or_default());
    // let expr = destruct_let_pattern(expr);
    let infer_ctx = infer_root(
        expr,
        builtin_types,
        file_path.clone().unwrap_or_default(),
        None,
        None,
        None,
    );
    let errors = infer_ctx
        .errors
        .iter()
        .cloned()
        .map(|e| -> Box<dyn ReportableError> { Box::new(e) })
        .chain(
            convert_errs
                .into_iter()
                .map(|e| -> Box<dyn ReportableError> { Box::new(e) }),
        )
        .collect::<Vec<_>>();
    (expr, infer_ctx, errors)
}

pub fn typecheck_with_module_info(
    root_expr_id: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    file_path: Option<PathBuf>,
    module_info: crate::ast::program::ModuleInfo,
) -> (ExprNodeId, InferContext, Vec<Box<dyn ReportableError>>) {
    // Extract builtin names to pass to qualified name resolution
    let builtin_names: Vec<Symbol> = builtin_types.iter().map(|(name, _)| *name).collect();
    // Use the extended version that resolves qualified names
    let (expr, convert_errs) = convert_pronoun::convert_pronoun_with_module(
        root_expr_id,
        file_path.clone().unwrap_or_default(),
        &module_info,
        &builtin_names,
    );
    let expr = recursecheck::convert_recurse(expr, file_path.clone().unwrap_or_default());
    // Type checker needs module_info for type declarations (user-defined sum types).
    let infer_ctx = super::typing::infer_root(
        expr,
        builtin_types,
        file_path.clone().unwrap_or_default(),
        Some(&module_info.type_declarations),
        Some(&module_info.type_aliases),
        Some(module_info.clone()),
    );
    let errors = infer_ctx
        .errors
        .iter()
        .cloned()
        .map(|e| -> Box<dyn ReportableError> { Box::new(e) })
        .chain(convert_errs)
        .collect::<Vec<_>>();
    (expr, infer_ctx, errors)
}

/// Generate MIR from AST.
/// The input ast (`root_expr_id`) should contain global context. (See [[parser::add_global_context]].)
/// MIR generator itself does not emit any error, the any compile errors are analyzed before generating MIR, mostly in type checker.
/// Note that the AST may contain partial error nodes, to do type check and report them as possible.
pub fn compile(
    root_expr_id: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    macro_env: &[Box<dyn MacroFunction>],
    file_path: Option<PathBuf>,
) -> Result<Mir, Vec<Box<dyn ReportableError>>> {
    compile_with_module_info(
        root_expr_id,
        builtin_types,
        macro_env,
        file_path,
        crate::ast::program::ModuleInfo::new(),
    )
}

/// Generate MIR from AST with module information (visibility and use aliases).
pub fn compile_with_module_info(
    root_expr_id: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
    macro_env: &[Box<dyn MacroFunction>],
    file_path: Option<PathBuf>,
    module_info: crate::ast::program::ModuleInfo,
) -> Result<Mir, Vec<Box<dyn ReportableError>>> {
    let expr = root_expr_id.wrap_to_staged_expr();
    let (expr, mut infer_ctx, errors) =
        typecheck_with_module_info(expr, builtin_types, file_path.clone(), module_info);
    if errors.is_empty() {
        let top_type = infer_ctx.infer_type(expr).unwrap();
        let expr =
            interpreter::expand_macro(expr, top_type, macro_env, infer_ctx.constructor_env.clone());

        log::trace!(
            "ast after macro expansion: {:?}",
            expr.to_expr().simple_print()
        );
        let expr = parser::add_global_context(expr, file_path.clone().unwrap_or_default());

        let mut ctx = Context::new(infer_ctx, file_path.clone());
        let _res = ctx.eval_expr(expr);
        ctx.program.file_path = file_path.clone();
        Ok(ctx.program.clone())
    } else {
        Err(errors)
    }
}

// #[cfg(test)]
// mod test;
