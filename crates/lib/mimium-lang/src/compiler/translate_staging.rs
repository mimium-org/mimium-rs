//! Bracket/Escape to codegen-combinator translation pass.
//!
//! This pass runs **after type checking** and transforms `Bracket`/`Escape`
//! nodes into calls to code-generation combinators (`code_lit_f`, `code_var`,
//! `code_app`, etc.).  The result is a pure stage-0 AST that, when compiled
//! and executed on the VM, produces the stage-1 AST as an `ExprNodeId`.
//!
//! # Staging model
//!
//! After `wrap_to_staged_expr` and `into_then_expr`, a program with
//! `#stage(macro)` / `#stage(main)` declarations has the structure:
//!
//! ```text
//! Bracket(                     // from wrap_to_staged_expr
//!   Escape(                    // from Main->Macro transition
//!     <macro definitions>
//!     Bracket(                 // from Macro->Main transition
//!       <main code with Escape calls for macros>
//!     )
//!   )
//! )
//! ```
//!
//! The translation uses two mutually recursive functions:
//!
//! - `translate_stage0(e)` — walk stage-0 code, recursing into children;
//!   when hitting `Bracket(inner)`, switch to `translate_code(inner)`.
//! - `translate_code(e)` — convert a stage-1 expression into stage-0
//!   combinator calls that build the AST for `e`;
//!   when hitting `Escape(inner)`, switch back to `translate_stage0(inner)`.

use crate::ast::{Expr, Literal, MatchArm, MatchPattern, RecordField};
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::types::Type;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Translate all `Bracket`/`Escape` nodes in `expr` into codegen-combinator
/// calls.  The top-level expression is expected to be at stage 0 (the output
/// of `wrap_to_staged_expr` followed by type checking).
pub fn translate(expr: ExprNodeId) -> ExprNodeId {
    translate_stage0(expr)
}

// ---------------------------------------------------------------------------
// Stage-0 walker
// ---------------------------------------------------------------------------

/// Strip `Code(T)` wrappers from a type.
///
/// After staging translation, `Code` values are represented as plain `Numeric`
/// (float) in the VM.  Type annotations that reference `Code(T)` (e.g.
/// explicit `` `float `` return types in user source) must be stripped so the
/// stage-0 type checker sees `Numeric` instead of `Code(Numeric)`.
/// Recursively strip `Code(T)` wrappers from a type.
///
/// After staging translation, `Code` values are represented as plain `Numeric`
/// (float) in the VM.  Type annotations that reference `Code(T)` — whether at
/// the top level or nested inside function/tuple/record types — must be
/// rewritten so the stage-0 type checker sees `Numeric` instead of `Code(T)`.
fn strip_code_type(ty: TypeNodeId) -> TypeNodeId {
    use crate::types::{PType, RecordTypeField};
    match ty.to_type() {
        Type::Code(_) => crate::numeric!(),
        Type::Function { arg, ret } => {
            let new_arg = strip_code_type(arg);
            let new_ret = strip_code_type(ret);
            if new_arg == arg && new_ret == ret {
                ty
            } else {
                Type::Function {
                    arg: new_arg,
                    ret: new_ret,
                }
                .into_id()
            }
        }
        Type::Tuple(elems) => {
            let new_elems: Vec<TypeNodeId> = elems.iter().map(|e| strip_code_type(*e)).collect();
            if new_elems == elems {
                ty
            } else {
                Type::Tuple(new_elems).into_id()
            }
        }
        Type::Record(fields) => {
            let new_fields: Vec<RecordTypeField> = fields
                .iter()
                .map(|f| RecordTypeField {
                    key: f.key,
                    ty: strip_code_type(f.ty),
                    has_default: f.has_default,
                })
                .collect();
            Type::Record(new_fields).into_id()
        }
        Type::Array(inner) => {
            let new_inner = strip_code_type(inner);
            if new_inner == inner {
                ty
            } else {
                Type::Array(new_inner).into_id()
            }
        }
        Type::Ref(inner) => {
            let new_inner = strip_code_type(inner);
            if new_inner == inner {
                ty
            } else {
                Type::Ref(new_inner).into_id()
            }
        }
        _ => ty,
    }
}

/// Strip `Code` from a `TypedId`'s type annotation.
fn strip_code_typed_id(id: TypedId) -> TypedId {
    let new_ty = strip_code_type(id.ty);
    if new_ty == id.ty {
        id
    } else {
        TypedId { ty: new_ty, ..id }
    }
}

/// Strip `Code` from a `TypedPattern`'s type annotation.
fn strip_code_typed_pattern(tp: TypedPattern) -> TypedPattern {
    let new_ty = strip_code_type(tp.ty);
    if new_ty == tp.ty {
        tp
    } else {
        TypedPattern { ty: new_ty, ..tp }
    }
}

/// Walk an expression at stage 0, leaving most nodes unchanged but
/// translating any `Bracket(inner)` into combinator calls via
/// `translate_code`.
fn translate_stage0(expr: ExprNodeId) -> ExprNodeId {
    match expr.to_expr() {
        // The key transition: bracket enters code-construction mode.
        Expr::Bracket(inner) => translate_code(inner),

        // Escape at stage 0 is currently invalid in the language.
        // (Escape at stage 0 should have been caught by the type checker.)
        Expr::Escape(_) => {
            log::warn!("translate_staging: unexpected Escape at stage 0");
            expr
        }

        // -- structural recursion for stage-0 nodes -------------------------
        Expr::Let(tp, val, then) => {
            let new_tp = strip_code_typed_pattern(tp);
            let new_val = translate_stage0(val);
            let new_then = then.map(translate_stage0);
            Expr::Let(new_tp, new_val, new_then).into_id_without_span()
        }
        Expr::LetRec(id, val, then) => {
            let new_id = strip_code_typed_id(id);
            let new_val = translate_stage0(val);
            let new_then = then.map(translate_stage0);
            Expr::LetRec(new_id, new_val, new_then).into_id_without_span()
        }
        Expr::Lambda(params, rtype, body) => {
            let new_params: Vec<TypedId> = params.into_iter().map(strip_code_typed_id).collect();
            let new_rtype = rtype.map(strip_code_type);
            let new_body = translate_stage0(body);
            Expr::Lambda(new_params, new_rtype, new_body).into_id_without_span()
        }
        Expr::Apply(f, args) => {
            let new_f = translate_stage0(f);
            let new_args = args.into_iter().map(translate_stage0).collect();
            Expr::Apply(new_f, new_args).into_id_without_span()
        }
        Expr::If(cond, then, else_opt) => {
            let new_cond = translate_stage0(cond);
            let new_then = translate_stage0(then);
            let new_else = else_opt.map(translate_stage0);
            Expr::If(new_cond, new_then, new_else).into_id_without_span()
        }
        Expr::Then(e1, e2) => {
            let new_e1 = translate_stage0(e1);
            let new_e2 = e2.map(translate_stage0);
            Expr::Then(new_e1, new_e2).into_id_without_span()
        }
        Expr::Block(inner) => {
            let new_inner = inner.map(translate_stage0);
            Expr::Block(new_inner).into_id_without_span()
        }
        Expr::Tuple(elems) => {
            let new_elems = elems.into_iter().map(translate_stage0).collect();
            Expr::Tuple(new_elems).into_id_without_span()
        }
        Expr::ArrayLiteral(elems) => {
            let new_elems = elems.into_iter().map(translate_stage0).collect();
            Expr::ArrayLiteral(new_elems).into_id_without_span()
        }
        Expr::RecordLiteral(fields) => {
            let new_fields = fields
                .into_iter()
                .map(|f| RecordField {
                    name: f.name,
                    expr: translate_stage0(f.expr),
                })
                .collect();
            Expr::RecordLiteral(new_fields).into_id_without_span()
        }
        Expr::Proj(inner, idx) => {
            let new_inner = translate_stage0(inner);
            Expr::Proj(new_inner, idx).into_id_without_span()
        }
        Expr::ArrayAccess(arr, idx) => {
            let new_arr = translate_stage0(arr);
            let new_idx = translate_stage0(idx);
            Expr::ArrayAccess(new_arr, new_idx).into_id_without_span()
        }
        Expr::FieldAccess(inner, name) => {
            let new_inner = translate_stage0(inner);
            Expr::FieldAccess(new_inner, name).into_id_without_span()
        }
        Expr::Assign(lhs, rhs) => {
            let new_lhs = translate_stage0(lhs);
            let new_rhs = translate_stage0(rhs);
            Expr::Assign(new_lhs, new_rhs).into_id_without_span()
        }
        Expr::Feed(id, body) => {
            let new_body = translate_stage0(body);
            Expr::Feed(id, new_body).into_id_without_span()
        }
        Expr::Match(scrutinee, arms) => {
            let new_scrutinee = translate_stage0(scrutinee);
            let new_arms = arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern,
                    body: translate_stage0(arm.body),
                })
                .collect();
            Expr::Match(new_scrutinee, new_arms).into_id_without_span()
        }
        Expr::Paren(inner) => {
            let new_inner = translate_stage0(inner);
            Expr::Paren(new_inner).into_id_without_span()
        }

        // Leaves: literals, variables, errors — return as-is.
        Expr::Literal(_) | Expr::Var(_) | Expr::QualifiedVar(_) | Expr::Error => expr,

        // These should already be desugared before this pass.
        Expr::BinOp(..) | Expr::UniOp(..) | Expr::MacroExpand(..) => {
            log::warn!("translate_staging: unexpected desugared-only node at stage 0");
            expr
        }
        Expr::ImcompleteRecord(fields) => {
            let new_fields = fields
                .into_iter()
                .map(|f| RecordField {
                    name: f.name,
                    expr: translate_stage0(f.expr),
                })
                .collect();
            Expr::ImcompleteRecord(new_fields).into_id_without_span()
        }
        Expr::RecordUpdate(record, fields) => {
            let new_record = translate_stage0(record);
            let new_fields = fields
                .into_iter()
                .map(|f| RecordField {
                    name: f.name,
                    expr: translate_stage0(f.expr),
                })
                .collect();
            Expr::RecordUpdate(new_record, new_fields).into_id_without_span()
        }
    }
}

// ---------------------------------------------------------------------------
// Code-construction translator (stage 1+)
// ---------------------------------------------------------------------------

/// Translate a stage-1 expression into stage-0 combinator calls that, when
/// executed, build the corresponding AST fragment as an `ExprNodeId`.
fn translate_code(expr: ExprNodeId) -> ExprNodeId {
    match expr.to_expr() {
        // -- Escape: drop back to stage 0 -----------------------------------
        //
        // The inner expression is stage-0 code that, when executed, produces
        // an `ExprNodeId` (code value).  We translate it at stage 0.
        Expr::Escape(inner) => translate_stage0(inner),

        // -- Nested bracket: increment stage further -------------------------
        //
        // In the interpreter's `rebuild`, Bracket strips itself and
        // increments the stage.  For multi-stage (≥3) programs we would need
        // nested combinator layers.  For the 2-stage system this should not
        // occur in practice; fall through to leaf.
        Expr::Bracket(inner) => {
            log::warn!("translate_staging: nested Bracket in translate_code (multi-stage)");
            // For now, treat as a code_block wrapping the inner translation.
            make_apply1("code_block", translate_code(inner))
        }

        // -- Literals -------------------------------------------------------
        Expr::Literal(lit) => match lit {
            Literal::Float(sym) => make_apply1(
                "code_lit_f",
                Expr::Literal(Literal::Float(sym)).into_id_without_span(),
            ),
            Literal::Int(i) => make_apply1(
                "code_lit_i",
                Expr::Literal(Literal::Int(i)).into_id_without_span(),
            ),
            Literal::String(sym) => make_apply1(
                "code_lit_s",
                Expr::Literal(Literal::String(sym)).into_id_without_span(),
            ),
            Literal::SelfLit => make_apply0("code_self"),
            Literal::Now => make_apply0("code_now"),
            Literal::SampleRate => make_apply0("code_samplerate"),
            Literal::PlaceHolder => {
                // Placeholder should not appear inside brackets in normal programs.
                log::warn!("translate_staging: PlaceHolder literal in translate_code");
                expr
            }
        },

        // -- Variables ------------------------------------------------------
        //
        // Every variable reference inside a bracket becomes `code_var(name)`,
        // which builds `Expr::Var(name)` in the generated AST.
        Expr::Var(name) => make_apply_str("code_var", name),

        // -- Function application -------------------------------------------
        Expr::Apply(f, args) => {
            let translated_f = translate_code(f);
            match args.len() {
                0 => {
                    // No-arg apply: code_app(f, [])
                    let empty_arr = Expr::ArrayLiteral(vec![]).into_id_without_span();
                    make_apply("code_app", vec![translated_f, empty_arr])
                }
                1 => {
                    let translated_arg = translate_code(args.into_iter().next().unwrap());
                    make_apply("code_app1", vec![translated_f, translated_arg])
                }
                2 => {
                    let mut iter = args.into_iter();
                    let a1 = translate_code(iter.next().unwrap());
                    let a2 = translate_code(iter.next().unwrap());
                    make_apply("code_app2", vec![translated_f, a1, a2])
                }
                _ => {
                    let translated_args: Vec<ExprNodeId> =
                        args.into_iter().map(translate_code).collect();
                    let arr = Expr::ArrayLiteral(translated_args).into_id_without_span();
                    make_apply("code_app", vec![translated_f, arr])
                }
            }
        }

        // -- Lambda ---------------------------------------------------------
        Expr::Lambda(params, _rtype, body) => {
            let translated_body = translate_code(body);
            match params.len() {
                1 => {
                    let name_lit = sym_to_string_literal(params[0].id);
                    make_apply("code_lam1_finish", vec![name_lit, translated_body])
                }
                _ => {
                    let name_lits: Vec<ExprNodeId> =
                        params.iter().map(|p| sym_to_string_literal(p.id)).collect();
                    let names_arr = Expr::ArrayLiteral(name_lits).into_id_without_span();
                    make_apply("code_lam_finish", vec![names_arr, translated_body])
                }
            }
        }

        // -- Let binding ----------------------------------------------------
        Expr::Let(tp, val, then) => {
            let translated_val = translate_code(val);
            let translated_body = then
                .map(translate_code)
                .unwrap_or_else(|| pattern_to_var_code(&tp.pat));
            translate_let_pattern(&tp.pat, translated_val, translated_body)
        }

        // -- LetRec ---------------------------------------------------------
        Expr::LetRec(id, val, then) => {
            let name_lit = sym_to_string_literal(id.id);
            let translated_val = translate_code(val);
            match then {
                Some(body) => {
                    let translated_body = translate_code(body);
                    make_apply(
                        "code_letrec",
                        vec![name_lit, translated_val, translated_body],
                    )
                }
                None => {
                    let var_ref = make_apply_str("code_var", id.id);
                    make_apply("code_letrec", vec![name_lit, translated_val, var_ref])
                }
            }
        }

        // -- If expression --------------------------------------------------
        Expr::If(cond, then, else_opt) => {
            let translated_cond = translate_code(cond);
            let translated_then = translate_code(then);
            let translated_else = else_opt
                .map(translate_code)
                // When there is no else branch, generate `code_lit_f(0.0)` as
                // a placeholder unit-like value.
                .unwrap_or_else(|| {
                    let zero =
                        Expr::Literal(Literal::Float("0.0".to_symbol())).into_id_without_span();
                    make_apply1("code_lit_f", zero)
                });
            make_apply(
                "code_if",
                vec![translated_cond, translated_then, translated_else],
            )
        }

        // -- Then (sequencing) ----------------------------------------------
        Expr::Then(e1, e2) => {
            let translated_e1 = translate_code(e1);
            match e2 {
                Some(e) => {
                    let translated_e2 = translate_code(e);
                    make_apply("code_then", vec![translated_e1, translated_e2])
                }
                None => translated_e1,
            }
        }

        // -- Assign ---------------------------------------------------------
        Expr::Assign(lhs, rhs) => {
            let translated_lhs = translate_code(lhs);
            let translated_rhs = translate_code(rhs);
            make_apply("code_assign", vec![translated_lhs, translated_rhs])
        }

        // -- Tuple ----------------------------------------------------------
        Expr::Tuple(elems) => {
            let translated: Vec<ExprNodeId> = elems.into_iter().map(translate_code).collect();
            let arr = Expr::ArrayLiteral(translated).into_id_without_span();
            make_apply1("code_tuple", arr)
        }

        // -- Projection -----------------------------------------------------
        Expr::Proj(inner, idx) => {
            let translated = translate_code(inner);
            let idx_lit = Expr::Literal(Literal::Int(idx)).into_id_without_span();
            make_apply("code_proj", vec![translated, idx_lit])
        }

        // -- Array literal --------------------------------------------------
        Expr::ArrayLiteral(elems) => {
            let translated: Vec<ExprNodeId> = elems.into_iter().map(translate_code).collect();
            let arr = Expr::ArrayLiteral(translated).into_id_without_span();
            make_apply1("code_array", arr)
        }

        // -- Array access ---------------------------------------------------
        Expr::ArrayAccess(arr, idx) => {
            let translated_arr = translate_code(arr);
            let translated_idx = translate_code(idx);
            make_apply("code_array_access", vec![translated_arr, translated_idx])
        }

        // -- Record literal -------------------------------------------------
        Expr::RecordLiteral(fields) => {
            let names: Vec<ExprNodeId> = fields
                .iter()
                .map(|f| sym_to_string_literal(f.name))
                .collect();
            let vals: Vec<ExprNodeId> =
                fields.into_iter().map(|f| translate_code(f.expr)).collect();
            let names_arr = Expr::ArrayLiteral(names).into_id_without_span();
            let vals_arr = Expr::ArrayLiteral(vals).into_id_without_span();
            make_apply("code_record", vec![names_arr, vals_arr])
        }

        // -- Field access ---------------------------------------------------
        Expr::FieldAccess(inner, name) => {
            let translated = translate_code(inner);
            let name_lit = sym_to_string_literal(name);
            make_apply("code_field_access", vec![translated, name_lit])
        }

        // -- Feed -----------------------------------------------------------
        Expr::Feed(id, body) => {
            let name_lit = sym_to_string_literal(id);
            let translated_body = translate_code(body);
            make_apply("code_feed", vec![name_lit, translated_body])
        }

        // -- Block ----------------------------------------------------------
        Expr::Block(inner) => match inner {
            Some(e) => {
                let translated = translate_code(e);
                make_apply1("code_block", translated)
            }
            None => {
                // Empty block: produce a placeholder.
                let zero = Expr::Literal(Literal::Float("0.0".to_symbol())).into_id_without_span();
                make_apply1("code_lit_f", zero)
            }
        },

        // -- Match ----------------------------------------------------------
        Expr::Match(scrutinee, arms) => translate_code_match(scrutinee, arms),

        // -- Paren ----------------------------------------------------------
        // Paren should already be stripped, but handle it gracefully.
        Expr::Paren(inner) => translate_code(inner),

        // -- Nodes that should not appear inside brackets -------------------
        Expr::QualifiedVar(_) => {
            log::warn!("translate_staging: QualifiedVar in translate_code");
            expr
        }
        Expr::BinOp(..) | Expr::UniOp(..) | Expr::MacroExpand(..) => {
            log::warn!("translate_staging: desugared-only node in translate_code");
            expr
        }
        Expr::ImcompleteRecord(fields) => {
            let names: Vec<ExprNodeId> = fields
                .iter()
                .map(|f| sym_to_string_literal(f.name))
                .collect();
            let vals: Vec<ExprNodeId> =
                fields.into_iter().map(|f| translate_code(f.expr)).collect();
            let names_arr = Expr::ArrayLiteral(names).into_id_without_span();
            let vals_arr = Expr::ArrayLiteral(vals).into_id_without_span();
            make_apply("code_imcomplete_record", vec![names_arr, vals_arr])
        }
        Expr::RecordUpdate(record, fields) => {
            let translated_record = translate_code(record);
            let names: Vec<ExprNodeId> = fields
                .iter()
                .map(|f| sym_to_string_literal(f.name))
                .collect();
            let vals: Vec<ExprNodeId> =
                fields.into_iter().map(|f| translate_code(f.expr)).collect();
            let names_arr = Expr::ArrayLiteral(names).into_id_without_span();
            let vals_arr = Expr::ArrayLiteral(vals).into_id_without_span();
            make_apply(
                "code_record_update",
                vec![translated_record, names_arr, vals_arr],
            )
        }
        Expr::Error => expr,
    }
}

// ---------------------------------------------------------------------------
// Match expression translation
// ---------------------------------------------------------------------------

/// Translate a match expression inside a bracket into combinator calls.
///
/// Since match patterns are structural data rather than expressions, we
/// construct the entire `Expr::Match` node by translating the scrutinee and
/// each arm body, while preserving patterns verbatim.
fn translate_code_match(scrutinee: ExprNodeId, arms: Vec<MatchArm>) -> ExprNodeId {
    // Strategy: build the match AST node using a dedicated combinator that
    // receives the translated scrutinee code, pattern data, and translated
    // arm bodies.
    //
    // For now, we build the Expr::Match node directly via the individual
    // translated parts.  The scrutinee and arm bodies are code values
    // (ExprNodeId); the patterns are preserved as-is.
    //
    // We embed this as stage-0 code that calls a helper to assemble the
    // match node.  Since we don't have a dedicated code_match combinator
    // yet, we construct the node inline using the AST interner.

    let translated_scrutinee = translate_code(scrutinee);
    let translated_arm_bodies: Vec<ExprNodeId> =
        arms.iter().map(|arm| translate_code(arm.body)).collect();

    // Build pattern literals as match pattern data.
    // We need to pass patterns through to the generated code.  Since patterns
    // are not expressions, we serialize them as data that the match-building
    // code can reconstruct.

    // For now, build a code_match combinator call that takes:
    //   arg0: translated scrutinee (code value)
    //   arg1: array of translated arm bodies (code values)
    //   arg2: array of pattern descriptors (as integers/strings)
    //
    // Since a proper code_match combinator is complex, we use a simpler
    // approach: encode each arm as a pair of (pattern_tag, body_code).

    // Encode pattern as an integer tag + optional sub-data.
    let pattern_tags: Vec<ExprNodeId> = arms
        .iter()
        .map(|arm| encode_match_pattern(&arm.pattern))
        .collect();

    let scrutinee_arg = translated_scrutinee;
    let bodies_arr = Expr::ArrayLiteral(translated_arm_bodies).into_id_without_span();
    let patterns_arr = Expr::ArrayLiteral(pattern_tags).into_id_without_span();

    make_apply("code_match", vec![scrutinee_arg, patterns_arr, bodies_arr])
}

/// Encode a `MatchPattern` as a literal expression for passing to the
/// code_match combinator.
///
/// Encoding scheme (may be extended):
/// - Wildcard: Int(0)
/// - Variable(name): String(name) — the combinator binds the variable
/// - Literal(lit): the literal expression itself
/// - Constructor(name, None): Tuple([Int(1), String(name)])
/// - Constructor(name, Some(inner)): Tuple([Int(1), String(name), encode(inner)])
/// - Tuple(pats): Tuple([Int(2), ...encoded sub-patterns])
fn encode_match_pattern(pat: &MatchPattern) -> ExprNodeId {
    match pat {
        MatchPattern::Wildcard => Expr::Literal(Literal::Int(0)).into_id_without_span(),
        MatchPattern::Variable(name) => {
            Expr::Literal(Literal::String(*name)).into_id_without_span()
        }
        MatchPattern::Literal(lit) => Expr::Literal(lit.clone()).into_id_without_span(),
        MatchPattern::Constructor(name, inner) => {
            let tag = Expr::Literal(Literal::Int(1)).into_id_without_span();
            let name_lit = Expr::Literal(Literal::String(*name)).into_id_without_span();
            let mut elems = vec![tag, name_lit];
            if let Some(inner_pat) = inner {
                elems.push(encode_match_pattern(inner_pat));
            }
            Expr::Tuple(elems).into_id_without_span()
        }
        MatchPattern::Tuple(pats) => {
            let tag = Expr::Literal(Literal::Int(2)).into_id_without_span();
            let mut elems = vec![tag];
            elems.extend(pats.iter().map(encode_match_pattern));
            Expr::Tuple(elems).into_id_without_span()
        }
    }
}

// ---------------------------------------------------------------------------
// AST-construction helpers
// ---------------------------------------------------------------------------

/// Create `Apply(Var(combinator_name), args)`.
fn make_apply(name: &str, args: Vec<ExprNodeId>) -> ExprNodeId {
    let f = Expr::Var(name.to_symbol()).into_id_without_span();
    Expr::Apply(f, args).into_id_without_span()
}

/// Create `Apply(Var(combinator_name), [arg])`.
fn make_apply1(name: &str, arg: ExprNodeId) -> ExprNodeId {
    make_apply(name, vec![arg])
}

/// Create `Apply(Var(combinator_name), [])` (zero-argument call).
fn make_apply0(name: &str) -> ExprNodeId {
    make_apply(name, vec![])
}

/// Create `Apply(Var(combinator_name), [Literal::String(sym)])`.
fn make_apply_str(name: &str, sym: Symbol) -> ExprNodeId {
    let lit = sym_to_string_literal(sym);
    make_apply1(name, lit)
}

/// Create a `Literal::String(sym)` expression node.
fn sym_to_string_literal(sym: Symbol) -> ExprNodeId {
    Expr::Literal(Literal::String(sym)).into_id_without_span()
}

/// Extract the variable name from a `Pattern`, returning a string literal.
fn pattern_to_string_literal(pat: &Pattern) -> ExprNodeId {
    sym_to_string_literal(pattern_to_symbol(pat))
}

/// Extract the primary symbol from a `Pattern`.
fn pattern_to_symbol(pat: &Pattern) -> Symbol {
    match pat {
        Pattern::Single(name) => *name,
        Pattern::Placeholder => "_".to_symbol(),
        Pattern::Tuple(pats) => pats
            .first()
            .map(pattern_to_symbol)
            .unwrap_or_else(|| "_".to_symbol()),
        Pattern::Record(fields) => fields
            .first()
            .map(|(name, _)| *name)
            .unwrap_or_else(|| "_".to_symbol()),
        Pattern::Error => "_".to_symbol(),
    }
}

/// Generate code combinator calls for a `let` binding with an arbitrary pattern.
///
/// For simple patterns (`Single`, `Placeholder`), emits `code_let(name, val, body)`.
/// For tuple patterns, emits `code_let_tuple(names, val, body)` — but when sub-patterns
/// are themselves nested tuples, the top level is flattened with temp names and additional
/// `code_let_tuple` calls are prepended to the body to destructure the nested elements.
fn translate_let_pattern(
    pat: &Pattern,
    translated_val: ExprNodeId,
    translated_body: ExprNodeId,
) -> ExprNodeId {
    match pat {
        Pattern::Single(name) => {
            let name_lit = sym_to_string_literal(*name);
            make_apply("code_let", vec![name_lit, translated_val, translated_body])
        }
        Pattern::Placeholder => {
            // Bind to "_" — the value is ignored but the expression still evaluates.
            let name_lit = sym_to_string_literal("_".to_symbol());
            make_apply("code_let", vec![name_lit, translated_val, translated_body])
        }
        Pattern::Tuple(pats) => translate_let_tuple_pattern(pats, translated_val, translated_body),
        Pattern::Record(_) | Pattern::Error => {
            // Fallback: use the primary symbol.
            let name_lit = pattern_to_string_literal(pat);
            make_apply("code_let", vec![name_lit, translated_val, translated_body])
        }
    }
}

// Thread-local counter for generating unique temp variable names.
thread_local! {
    static DESUGAR_COUNTER: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
}

fn fresh_desugar_name() -> Symbol {
    DESUGAR_COUNTER.with(|c| {
        let n = c.get();
        c.set(n + 1);
        format!("__dt{n}").to_symbol()
    })
}

/// Translate a `let` with a tuple pattern into `code_let_tuple` calls.
///
/// When all sub-patterns are flat (`Single` or `Placeholder`), a single
/// `code_let_tuple([names], val, body)` is emitted.  When some sub-patterns
/// are themselves tuples (nested destructuring), the top-level tuple uses
/// temp names for those positions, and additional destructuring `let` calls
/// are inserted between the outer binding and the original body.
fn translate_let_tuple_pattern(
    pats: &[Pattern],
    translated_val: ExprNodeId,
    translated_body: ExprNodeId,
) -> ExprNodeId {
    // Determine the top-level names and collect any nested sub-pattern work.
    let mut top_names: Vec<ExprNodeId> = Vec::with_capacity(pats.len());
    // (position, sub_patterns, temp_name) for nested tuples.
    let mut nested: Vec<(usize, &[Pattern], Symbol)> = Vec::new();

    for (i, pat) in pats.iter().enumerate() {
        match pat {
            Pattern::Single(name) => {
                top_names.push(sym_to_string_literal(*name));
            }
            Pattern::Placeholder => {
                top_names.push(sym_to_string_literal("_".to_symbol()));
            }
            Pattern::Tuple(sub_pats) => {
                let tmp = fresh_desugar_name();
                top_names.push(sym_to_string_literal(tmp));
                nested.push((i, sub_pats.as_slice(), tmp));
            }
            Pattern::Record(_) | Pattern::Error => {
                let name = pattern_to_symbol(pat);
                top_names.push(sym_to_string_literal(name));
            }
        }
    }

    // Build the body chain: innermost is the original translated_body.
    // For each nested tuple (processed in reverse to build inside-out),
    // wrap: translate_let_tuple_pattern(sub_pats, code_var(tmp), current_body)
    let mut body = translated_body;
    for (_i, sub_pats, tmp) in nested.into_iter().rev() {
        let tmp_var = make_apply_str("code_var", tmp);
        body = translate_let_tuple_pattern(sub_pats, tmp_var, body);
    }

    let names_arr = Expr::ArrayLiteral(top_names).into_id_without_span();
    make_apply("code_let_tuple", vec![names_arr, translated_val, body])
}

/// Produce a code value representing the pattern's variables.
///
/// For `Single(x)` → `code_var(x)`.  For `Tuple(a, b)` → `code_tuple([code_var(a), code_var(b)])`.
fn pattern_to_var_code(pat: &Pattern) -> ExprNodeId {
    match pat {
        Pattern::Single(name) => make_apply_str("code_var", *name),
        Pattern::Placeholder => {
            // Placeholder without body — produce a unit-like value.
            let zero = Expr::Literal(Literal::Float("0.0".to_symbol())).into_id_without_span();
            make_apply1("code_lit_f", zero)
        }
        Pattern::Tuple(pats) => {
            let var_refs: Vec<ExprNodeId> = pats.iter().map(pattern_to_var_code).collect();
            let arr = Expr::ArrayLiteral(var_refs).into_id_without_span();
            make_apply1("code_tuple", arr)
        }
        Pattern::Record(fields) => {
            let names: Vec<ExprNodeId> = fields
                .iter()
                .map(|(name, _)| sym_to_string_literal(*name))
                .collect();
            let vals: Vec<ExprNodeId> = fields
                .iter()
                .map(|(_, pat)| pattern_to_var_code(pat))
                .collect();
            let names_arr = Expr::ArrayLiteral(names).into_id_without_span();
            let vals_arr = Expr::ArrayLiteral(vals).into_id_without_span();
            make_apply("code_record", vec![names_arr, vals_arr])
        }
        Pattern::Error => {
            let zero = Expr::Literal(Literal::Float("0.0".to_symbol())).into_id_without_span();
            make_apply1("code_lit_f", zero)
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interner::ToSymbol;
    use crate::pattern::TypedId;
    use crate::types::Type;
    use crate::utils::metadata::Location;

    fn default_loc() -> Location {
        Location::default()
    }

    fn unknown_ty() -> crate::interner::TypeNodeId {
        Type::Unknown.into_id_with_location(default_loc())
    }

    #[allow(dead_code)]
    /// Helper: run a translation and return the resulting Expr.
    fn translate_and_get(expr: ExprNodeId) -> Expr {
        translate(expr).to_expr()
    }

    #[test]
    fn simple_literal_in_bracket() {
        // Bracket(Literal(42.0))
        // => translate_code(Literal(42.0))
        // => Apply(code_lit_f, [Literal(42.0)])
        let lit = Expr::Literal(Literal::Float("42.0".to_symbol())).into_id_without_span();
        let bracket = Expr::Bracket(lit).into_id_without_span();
        let result = translate(bracket);
        match result.to_expr() {
            Expr::Apply(f, args) => {
                assert!(matches!(f.to_expr(), Expr::Var(name) if name.as_str() == "code_lit_f"));
                assert_eq!(args.len(), 1);
                assert!(matches!(
                    args[0].to_expr(),
                    Expr::Literal(Literal::Float(_))
                ));
            }
            other => panic!("expected Apply, got {:?}", other),
        }
    }

    #[test]
    fn variable_in_bracket() {
        // Bracket(Var(x))
        // => Apply(code_var, [Literal::String("x")])
        let var = Expr::Var("x".to_symbol()).into_id_without_span();
        let bracket = Expr::Bracket(var).into_id_without_span();
        let result = translate(bracket);
        match result.to_expr() {
            Expr::Apply(f, args) => {
                assert!(matches!(f.to_expr(), Expr::Var(name) if name.as_str() == "code_var"));
                assert_eq!(args.len(), 1);
                match args[0].to_expr() {
                    Expr::Literal(Literal::String(s)) => assert_eq!(s.as_str(), "x"),
                    other => panic!("expected String literal, got {:?}", other),
                }
            }
            other => panic!("expected Apply, got {:?}", other),
        }
    }

    #[test]
    fn escape_in_bracket() {
        // Bracket(Escape(Var(x)))
        // => translate_code(Escape(Var(x)))
        // => translate_stage0(Var(x))
        // => Var(x)   (stage-0 code left as-is)
        let var = Expr::Var("x".to_symbol()).into_id_without_span();
        let escape = Expr::Escape(var).into_id_without_span();
        let bracket = Expr::Bracket(escape).into_id_without_span();
        let result = translate(bracket);
        match result.to_expr() {
            Expr::Var(name) => assert_eq!(name.as_str(), "x"),
            other => panic!("expected Var, got {:?}", other),
        }
    }

    #[test]
    fn apply_in_bracket() {
        // Bracket(Apply(Var(f), [Var(x)]))
        // => Apply(code_app1, [Apply(code_var, ["f"]), Apply(code_var, ["x"])])
        let f = Expr::Var("f".to_symbol()).into_id_without_span();
        let x = Expr::Var("x".to_symbol()).into_id_without_span();
        let apply = Expr::Apply(f, vec![x]).into_id_without_span();
        let bracket = Expr::Bracket(apply).into_id_without_span();
        let result = translate(bracket);
        match result.to_expr() {
            Expr::Apply(func, args) => {
                assert!(matches!(func.to_expr(), Expr::Var(name) if name.as_str() == "code_app1"));
                assert_eq!(args.len(), 2);
            }
            other => panic!("expected Apply(code_app1, ...), got {other:?}"),
        }
    }

    #[test]
    fn lambda_in_bracket() {
        // Bracket(Lambda([x], _, Var(x)))
        // => Apply(code_lam1_finish, [Literal::String("x"), Apply(code_var, ["x"])])
        let x_id = TypedId::new("x".to_symbol(), unknown_ty());
        let body = Expr::Var("x".to_symbol()).into_id_without_span();
        let lam = Expr::Lambda(vec![x_id], None, body).into_id_without_span();
        let bracket = Expr::Bracket(lam).into_id_without_span();
        let result = translate(bracket);
        match result.to_expr() {
            Expr::Apply(func, args) => {
                assert!(
                    matches!(func.to_expr(), Expr::Var(name) if name.as_str() == "code_lam1_finish")
                );
                assert_eq!(args.len(), 2);
                // First arg: string "x"
                match args[0].to_expr() {
                    Expr::Literal(Literal::String(s)) => assert_eq!(s.as_str(), "x"),
                    other => panic!("expected String literal, got {other:?}"),
                }
            }
            other => panic!("expected Apply(code_lam1_finish, ...), got {other:?}"),
        }
    }

    #[test]
    fn macro_pattern_bracket_escape_bracket() {
        // Simulates the common macro pattern:
        // Bracket(Escape(LetRec(make_42, Lambda([], Bracket(Lit(42.0))),
        //                       Bracket(Var(dsp)))))
        //
        // Should translate to:
        // LetRec(make_42, Lambda([], Apply(code_lit_f, [42.0])),
        //        Apply(code_var, ["dsp"]))

        let lit_42 = Expr::Literal(Literal::Float("42.0".to_symbol())).into_id_without_span();
        let bracket_inner = Expr::Bracket(lit_42).into_id_without_span();
        let lambda = Expr::Lambda(vec![], None, bracket_inner).into_id_without_span();

        let dsp_var = Expr::Var("dsp".to_symbol()).into_id_without_span();
        let bracket_main = Expr::Bracket(dsp_var).into_id_without_span();

        let make_42_id = TypedId::new("make_42".to_symbol(), unknown_ty());
        let letrec = Expr::LetRec(make_42_id, lambda, Some(bracket_main)).into_id_without_span();
        let escape = Expr::Escape(letrec).into_id_without_span();
        let outer_bracket = Expr::Bracket(escape).into_id_without_span();

        let result = translate(outer_bracket);

        // Result should be:
        // LetRec(make_42, Lambda([], Apply(code_lit_f, [42.0])),
        //        Apply(code_var, ["dsp"]))
        match result.to_expr() {
            Expr::LetRec(id, val, then) => {
                assert_eq!(id.id.as_str(), "make_42");
                // val should be Lambda([], Apply(code_lit_f, [42.0]))
                match val.to_expr() {
                    Expr::Lambda(params, _, body) => {
                        assert_eq!(params.len(), 0);
                        match body.to_expr() {
                            Expr::Apply(f, _) => {
                                assert!(
                                    matches!(f.to_expr(), Expr::Var(name) if name.as_str() == "code_lit_f")
                                );
                            }
                            other => panic!("expected Apply(code_lit_f, ...), got {other:?}"),
                        }
                    }
                    other => panic!("expected Lambda, got {other:?}"),
                }
                // then should be Apply(code_var, ["dsp"])
                let then = then.expect("expected Some(then)");
                match then.to_expr() {
                    Expr::Apply(f, args) => {
                        assert!(
                            matches!(f.to_expr(), Expr::Var(name) if name.as_str() == "code_var")
                        );
                        assert_eq!(args.len(), 1);
                    }
                    other => panic!("expected Apply(code_var, ...), got {other:?}"),
                }
            }
            other => panic!("expected LetRec, got {other:?}"),
        }
    }
}
