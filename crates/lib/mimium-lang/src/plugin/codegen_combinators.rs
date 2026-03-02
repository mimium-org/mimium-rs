//! Code-generation combinators for the MetaOCaml-style staging system.
//!
//! These external functions build AST nodes (`ExprNodeId`) at stage 0 when
//! executed on the VM.  Each combinator takes runtime values representing code
//! fragments and composes them into larger AST fragments.
//!
//! The combinators are registered as VM external closures so that the
//! `translate_staging` pass can emit calls to them in place of
//! `Bracket`/`Escape` nodes.

use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expr, Literal, RecordField},
    integer,
    interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId},
    numeric,
    pattern::{Pattern, TypedId, TypedPattern},
    plugin::{ExtClsInfo, ExtFunTypeInfo},
    runtime::vm::{Machine, ReturnCode},
    string_t,
    types::Type,
};

use super::EvalStage;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Create an `ExprNodeId` from an `Expr` without span information.
fn expr_to_id(expr: Expr) -> ExprNodeId {
    expr.into_id_without_span()
}

/// Read a `Symbol` from a raw string index on the VM stack.
///
/// The VM stores strings as indices into `prog.strings`.  This helper
/// converts that index to an interned `Symbol`.
fn raw_to_symbol(machine: &Machine, raw: u64) -> Symbol {
    let idx = raw as usize;
    let s = &machine.prog.strings[idx];
    s.to_symbol()
}

// ---------------------------------------------------------------------------
// Individual combinators
// ---------------------------------------------------------------------------

/// `code_lit_f(x: float) -> Code(float)`
///
/// Wraps a float literal into a code value.
fn code_lit_f(machine: &mut Machine) -> ReturnCode {
    let v = Machine::get_as::<f64>(machine.get_stack(0));
    let sym = v.to_string().to_symbol();
    let expr = expr_to_id(Expr::Literal(Literal::Float(sym)));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_lit_i(x: int) -> Code(int)`
///
/// Wraps an integer literal into a code value.
fn code_lit_i(machine: &mut Machine) -> ReturnCode {
    let v = Machine::get_as::<i64>(machine.get_stack(0));
    let expr = expr_to_id(Expr::Literal(Literal::Int(v)));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_lit_s(x: string) -> Code(string)`
///
/// Wraps a string literal into a code value.
fn code_lit_s(machine: &mut Machine) -> ReturnCode {
    let raw = machine.get_stack(0);
    let sym = raw_to_symbol(machine, raw);
    let expr = expr_to_id(Expr::Literal(Literal::String(sym)));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_var(name: string) -> Code(a)`
///
/// Create a variable reference code value.
fn code_var(machine: &mut Machine) -> ReturnCode {
    let raw = machine.get_stack(0);
    let sym = raw_to_symbol(machine, raw);
    let expr = expr_to_id(Expr::Var(sym));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_app(f_code: Code(a->b), args_array: [Code(a)]) -> Code(b)`
///
/// Construct a function application AST node from code values.
/// The second argument is an array of code values for the arguments.
fn code_app(machine: &mut Machine) -> ReturnCode {
    let f_raw = machine.get_stack(0);
    let args_raw = machine.get_stack(1);
    let f_expr = machine.get_code(f_raw);

    let arr = machine.arrays.get_array(args_raw);
    let len = arr.get_length_array();
    let data = arr.get_data().to_vec();
    let arg_exprs: Vec<ExprNodeId> = (0..len as usize)
        .map(|i| machine.get_code(data[i]))
        .collect();

    let expr = expr_to_id(Expr::Apply(f_expr, arg_exprs));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_app1(f_code: Code(a->b), arg_code: Code(a)) -> Code(b)`
///
/// Optimized single-argument application (avoids array allocation).
fn code_app1(machine: &mut Machine) -> ReturnCode {
    let f_raw = machine.get_stack(0);
    let arg_raw = machine.get_stack(1);
    let f_expr = machine.get_code(f_raw);
    let arg_expr = machine.get_code(arg_raw);
    let expr = expr_to_id(Expr::Apply(f_expr, vec![arg_expr]));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_app2(f: Code(a->b->c), a1: Code(a), a2: Code(b)) -> Code(c)`
///
/// Optimized two-argument application.
fn code_app2(machine: &mut Machine) -> ReturnCode {
    let f_raw = machine.get_stack(0);
    let a1_raw = machine.get_stack(1);
    let a2_raw = machine.get_stack(2);
    let f_expr = machine.get_code(f_raw);
    let a1_expr = machine.get_code(a1_raw);
    let a2_expr = machine.get_code(a2_raw);
    let expr = expr_to_id(Expr::Apply(f_expr, vec![a1_expr, a2_expr]));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_lam1(name: string, body_fn: (Code(a) -> Code(b))) -> Code(a->b)`
///
/// HOAS-style lambda construction.  Creates a fresh variable, wraps it in a
/// `Code` value, passes it to `body_fn`, then assembles `Expr::Lambda`.
///
/// **Note**: the body function is a VM closure that must be called at stage 0
/// to obtain the body code value.
#[allow(dead_code)]
fn code_lam1(machine: &mut Machine) -> ReturnCode {
    let name_raw = machine.get_stack(0);
    let _cls_raw = machine.get_stack(1);
    let name_sym = raw_to_symbol(machine, name_raw);

    // Create a Code value for the parameter variable.
    let param_var_expr = expr_to_id(Expr::Var(name_sym));
    let param_code = machine.alloc_code(param_var_expr);

    // Push the code value as the closure argument and call it.
    // The closure is at stack position 1 relative to base_pointer.
    // We need to invoke it with `param_code` as the argument.
    //
    // After the closure call, the return value (the body Code) will be on
    // the stack.
    machine.set_stack(2, param_code);

    // Call the closure at position 1 with 1 arg starting at position 2.
    // The call_function mechanism will handle the rest.
    // However, external functions cannot easily invoke VM closures directly
    // because of the borrow-checker constraints on `machine`.
    //
    // Instead, we use a two-step protocol:
    // 1. This combinator stores the parameter Code and returns a sentinel.
    // 2. The translate_staging pass generates code that calls the body
    //    function explicitly and then calls `code_lam1_finish` to wrap the
    //    result in Expr::Lambda.
    //
    // For now, we implement the simpler "exploded" approach: the
    // translate_staging pass does NOT use HOAS.  Instead it directly
    // constructs: `code_lam1_finish(name, body_code)`.
    //
    // See `code_lam1_finish` below.
    unreachable!("code_lam1 with HOAS is not called directly; use code_lam1_finish instead.");
}

/// `code_lam1_finish(name_str: string, body_code: Code(b)) -> Code(a->b)`
///
/// Non-HOAS variant: directly assemble `Expr::Lambda` from a name and a body
/// code value.  The `translate_staging` pass arranges scope so that the body
/// code value is the result of translating the lambda body with the parameter
/// already in the `code_variables` set.
fn code_lam1_finish(machine: &mut Machine) -> ReturnCode {
    let name_raw = machine.get_stack(0);
    let body_raw = machine.get_stack(1);
    let name_sym = raw_to_symbol(machine, name_raw);
    let body_expr = machine.get_code(body_raw);

    let param = TypedId::new(name_sym, Type::Unknown.into_id());
    let expr = expr_to_id(Expr::Lambda(vec![param], None, body_expr));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_lam_finish(names: [string], body_code: Code(b)) -> Code(a->b)`
///
/// Multi-parameter lambda construction.
fn code_lam_finish(machine: &mut Machine) -> ReturnCode {
    let names_raw = machine.get_stack(0);
    let body_raw = machine.get_stack(1);
    let body_expr = machine.get_code(body_raw);

    let arr = machine.arrays.get_array(names_raw);
    let len = arr.get_length_array();
    let data = arr.get_data().to_vec();
    let params: Vec<TypedId> = (0..len as usize)
        .map(|i| {
            let sym = raw_to_symbol(machine, data[i]);
            TypedId::new(sym, Type::Unknown.into_id())
        })
        .collect();

    let expr = expr_to_id(Expr::Lambda(params, None, body_expr));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_let(name: string, val_code: Code(a), body_code: Code(b)) -> Code(b)`
fn code_let(machine: &mut Machine) -> ReturnCode {
    let name_raw = machine.get_stack(0);
    let val_raw = machine.get_stack(1);
    let body_raw = machine.get_stack(2);

    let name_sym = raw_to_symbol(machine, name_raw);
    let val_expr = machine.get_code(val_raw);
    let body_expr = machine.get_code(body_raw);

    let pattern = TypedPattern::new(Pattern::Single(name_sym), Type::Unknown.into_id());
    let expr = expr_to_id(Expr::Let(pattern, val_expr, Some(body_expr)));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_let_tuple(names: [string], val_code: Code(a), body_code: Code(b)) -> Code(b)`
///
/// Let-binding with a tuple destructuring pattern.
fn code_let_tuple(machine: &mut Machine) -> ReturnCode {
    let names_raw = machine.get_stack(0);
    let val_raw = machine.get_stack(1);
    let body_raw = machine.get_stack(2);

    let arr = machine.arrays.get_array(names_raw);
    let len = arr.get_length_array();
    let data = arr.get_data().to_vec();
    let sub_pats: Vec<Pattern> = (0..len as usize)
        .map(|i| {
            let sym = raw_to_symbol(machine, data[i]);
            if sym.as_str() == "_" {
                Pattern::Placeholder
            } else {
                Pattern::Single(sym)
            }
        })
        .collect();

    let val_expr = machine.get_code(val_raw);
    let body_expr = machine.get_code(body_raw);

    let pattern = TypedPattern::new(Pattern::Tuple(sub_pats), Type::Unknown.into_id());
    let expr = expr_to_id(Expr::Let(pattern, val_expr, Some(body_expr)));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_letrec(name: string, val_code: Code(a), body_code: Code(b)) -> Code(b)`
fn code_letrec(machine: &mut Machine) -> ReturnCode {
    let name_raw = machine.get_stack(0);
    let val_raw = machine.get_stack(1);
    let body_raw = machine.get_stack(2);

    let name_sym = raw_to_symbol(machine, name_raw);
    let val_expr = machine.get_code(val_raw);
    let body_expr = machine.get_code(body_raw);

    let id = TypedId::new(name_sym, Type::Unknown.into_id());
    let expr = expr_to_id(Expr::LetRec(id, val_expr, Some(body_expr)));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_if(cond: Code(bool), then_code: Code(a), else_code: Code(a)) -> Code(a)`
fn code_if(machine: &mut Machine) -> ReturnCode {
    let cond_raw = machine.get_stack(0);
    let then_raw = machine.get_stack(1);
    let else_raw = machine.get_stack(2);

    let cond_expr = machine.get_code(cond_raw);
    let then_expr = machine.get_code(then_raw);
    let else_expr = machine.get_code(else_raw);

    let expr = expr_to_id(Expr::If(cond_expr, then_expr, Some(else_expr)));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_tuple(elems: [Code(...)]) -> Code(tuple)`
fn code_tuple(machine: &mut Machine) -> ReturnCode {
    let arr_raw = machine.get_stack(0);
    let arr = machine.arrays.get_array(arr_raw);
    let len = arr.get_length_array();
    let data = arr.get_data().to_vec();
    let elems: Vec<ExprNodeId> = (0..len as usize)
        .map(|i| machine.get_code(data[i]))
        .collect();

    let expr = expr_to_id(Expr::Tuple(elems));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_proj(val: Code(tuple), idx: int) -> Code(a)`
fn code_proj(machine: &mut Machine) -> ReturnCode {
    let val_raw = machine.get_stack(0);
    let idx = Machine::get_as::<i64>(machine.get_stack(1));
    let val_expr = machine.get_code(val_raw);

    let expr = expr_to_id(Expr::Proj(val_expr, idx));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_array(elems: [Code(a)]) -> Code([a])`
fn code_array(machine: &mut Machine) -> ReturnCode {
    let arr_raw = machine.get_stack(0);
    let arr = machine.arrays.get_array(arr_raw);
    let len = arr.get_length_array();
    let data = arr.get_data().to_vec();
    let elems: Vec<ExprNodeId> = (0..len as usize)
        .map(|i| machine.get_code(data[i]))
        .collect();

    let expr = expr_to_id(Expr::ArrayLiteral(elems));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_array_access(arr: Code([a]), idx: Code(int)) -> Code(a)`
fn code_array_access(machine: &mut Machine) -> ReturnCode {
    let arr_raw = machine.get_stack(0);
    let idx_raw = machine.get_stack(1);
    let arr_expr = machine.get_code(arr_raw);
    let idx_expr = machine.get_code(idx_raw);

    let expr = expr_to_id(Expr::ArrayAccess(arr_expr, idx_expr));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_then(a: Code(a), b: Code(b)) -> Code(b)`
///
/// Sequence two expressions (the first is evaluated for its side effects).
fn code_then(machine: &mut Machine) -> ReturnCode {
    let a_raw = machine.get_stack(0);
    let b_raw = machine.get_stack(1);
    let a_expr = machine.get_code(a_raw);
    let b_expr = machine.get_code(b_raw);

    let expr = expr_to_id(Expr::Then(a_expr, Some(b_expr)));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_assign(lhs: Code(a), rhs: Code(a)) -> Code(())`
fn code_assign(machine: &mut Machine) -> ReturnCode {
    let lhs_raw = machine.get_stack(0);
    let rhs_raw = machine.get_stack(1);
    let lhs_expr = machine.get_code(lhs_raw);
    let rhs_expr = machine.get_code(rhs_raw);

    let expr = expr_to_id(Expr::Assign(lhs_expr, rhs_expr));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_record(names: [string], values: [Code(...)]) -> Code({...})`
fn code_record(machine: &mut Machine) -> ReturnCode {
    let names_raw = machine.get_stack(0);
    let vals_raw = machine.get_stack(1);

    let names_arr = machine.arrays.get_array(names_raw);
    let names_len = names_arr.get_length_array();
    let names_data = names_arr.get_data().to_vec();

    let vals_arr = machine.arrays.get_array(vals_raw);
    let vals_data = vals_arr.get_data().to_vec();

    let fields: Vec<RecordField> = (0..names_len as usize)
        .map(|i| {
            let name = raw_to_symbol(machine, names_data[i]);
            let expr = machine.get_code(vals_data[i]);
            RecordField { name, expr }
        })
        .collect();

    let expr = expr_to_id(Expr::RecordLiteral(fields));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_imcomplete_record(names: [string], values: [Code(...)]) -> Code({...})`
fn code_imcomplete_record(machine: &mut Machine) -> ReturnCode {
    let names_raw = machine.get_stack(0);
    let vals_raw = machine.get_stack(1);

    let names_arr = machine.arrays.get_array(names_raw);
    let names_len = names_arr.get_length_array();
    let names_data = names_arr.get_data().to_vec();

    let vals_arr = machine.arrays.get_array(vals_raw);
    let vals_data = vals_arr.get_data().to_vec();

    let fields: Vec<RecordField> = (0..names_len as usize)
        .map(|i| {
            let name = raw_to_symbol(machine, names_data[i]);
            let expr = machine.get_code(vals_data[i]);
            RecordField { name, expr }
        })
        .collect();

    let expr = expr_to_id(Expr::ImcompleteRecord(fields));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_record_update(base: Code({...}), names: [string], values: [Code(...)]) -> Code({...})`
fn code_record_update(machine: &mut Machine) -> ReturnCode {
    let base_raw = machine.get_stack(0);
    let names_raw = machine.get_stack(1);
    let vals_raw = machine.get_stack(2);

    let base_expr = machine.get_code(base_raw);

    let names_arr = machine.arrays.get_array(names_raw);
    let names_len = names_arr.get_length_array();
    let names_data = names_arr.get_data().to_vec();

    let vals_arr = machine.arrays.get_array(vals_raw);
    let vals_data = vals_arr.get_data().to_vec();

    let fields: Vec<RecordField> = (0..names_len as usize)
        .map(|i| {
            let name = raw_to_symbol(machine, names_data[i]);
            let expr = machine.get_code(vals_data[i]);
            RecordField { name, expr }
        })
        .collect();

    let expr = expr_to_id(Expr::RecordUpdate(base_expr, fields));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_field_access(val: Code({...}), field: string) -> Code(a)`
fn code_field_access(machine: &mut Machine) -> ReturnCode {
    let val_raw = machine.get_stack(0);
    let field_raw = machine.get_stack(1);
    let val_expr = machine.get_code(val_raw);
    let field_sym = raw_to_symbol(machine, field_raw);

    let expr = expr_to_id(Expr::FieldAccess(val_expr, field_sym));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_feed(name: string, body_code: Code(a)) -> Code(a)`
fn code_feed(machine: &mut Machine) -> ReturnCode {
    let name_raw = machine.get_stack(0);
    let body_raw = machine.get_stack(1);
    let name_sym = raw_to_symbol(machine, name_raw);
    let body_expr = machine.get_code(body_raw);

    let expr = expr_to_id(Expr::Feed(name_sym, body_expr));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_block(inner: Code(a)) -> Code(a)`
fn code_block(machine: &mut Machine) -> ReturnCode {
    let inner_raw = machine.get_stack(0);
    let inner_expr = machine.get_code(inner_raw);

    let expr = expr_to_id(Expr::Block(Some(inner_expr)));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_paren(inner: Code(a)) -> Code(a)`
fn code_paren(machine: &mut Machine) -> ReturnCode {
    let inner_raw = machine.get_stack(0);
    let inner_expr = machine.get_code(inner_raw);

    let expr = expr_to_id(Expr::Paren(inner_expr));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_self() -> Code(float)`
///
/// Construct a `self` literal reference.
fn code_self(machine: &mut Machine) -> ReturnCode {
    let expr = expr_to_id(Expr::Literal(Literal::SelfLit));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_now() -> Code(float)`
///
/// Construct a `now` special variable reference.
fn code_now(machine: &mut Machine) -> ReturnCode {
    let expr = expr_to_id(Expr::Literal(Literal::Now));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_samplerate() -> Code(float)`
fn code_samplerate(machine: &mut Machine) -> ReturnCode {
    let expr = expr_to_id(Expr::Literal(Literal::SampleRate));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

// ---------------------------------------------------------------------------
// lift combinators (VM versions)
// ---------------------------------------------------------------------------

/// `code_lift_f(x: float) -> Code(float)`
///
/// VM-native version of the `lift` function for floats.
/// Equivalent to `code_lit_f` — packages a runtime float as a code literal.
fn code_lift_f(machine: &mut Machine) -> ReturnCode {
    code_lit_f(machine)
}

/// `code_lift_arrayf(arr: [float]) -> Code([float])`
///
/// Lift a runtime float array into a code-level array literal.
/// Each element of the array is converted to a `Literal::Float`, then
/// wrapped in an `Expr::ArrayLiteral`.
fn code_lift_arrayf(machine: &mut Machine) -> ReturnCode {
    let arr_raw = machine.get_stack(0);
    let arr = machine.arrays.get_array(arr_raw);
    let len = arr.get_length_array();
    let elem_word_size = arr.get_elem_word_size() as usize;
    let data = arr.get_data().to_vec();
    let elems: Vec<ExprNodeId> = (0..len as usize)
        .map(|i| {
            let start = i * elem_word_size;
            let end = start + elem_word_size;
            let words = &data[start..end];
            if elem_word_size == 1 {
                let f = f64::from_bits(words[0]);
                expr_to_id(Expr::Literal(Literal::Float(format!("{f}").to_symbol())))
            } else {
                let tuple_elems = words
                    .iter()
                    .map(|w| {
                        let f = f64::from_bits(*w);
                        expr_to_id(Expr::Literal(Literal::Float(format!("{f}").to_symbol())))
                    })
                    .collect::<Vec<_>>();
                expr_to_id(Expr::Tuple(tuple_elems))
            }
        })
        .collect();
    let expr = expr_to_id(Expr::ArrayLiteral(elems));
    let code_val = machine.alloc_code(expr);
    machine.set_stack(0, code_val);
    1
}

/// `code_lift(x) -> Code(x)` — polymorphic lift.
///
/// At runtime, tries to interpret the value as an array first. If the raw
/// value corresponds to a valid array in `Machine::arrays`, each element is
/// lifted as a `Literal::Float` and wrapped in `Expr::ArrayLiteral`.
/// Otherwise the value is treated as a float and lifted via `code_lit_f`.
fn code_lift(machine: &mut Machine) -> ReturnCode {
    let nargs = machine.get_current_ext_call_nargs() as usize;
    if nargs > 1 {
        let elems: Vec<ExprNodeId> = (0..nargs)
            .map(|i| {
                let f = f64::from_bits(machine.get_stack(i as i64));
                expr_to_id(Expr::Literal(Literal::Float(format!("{f}").to_symbol())))
            })
            .collect();
        let expr = expr_to_id(Expr::Tuple(elems));
        let code_val = machine.alloc_code(expr);
        machine.set_stack(0, code_val);
        return 1;
    }

    let val = machine.get_stack(0);
    if let Some(arr) = machine.arrays.try_get_array(val) {
        let len = arr.get_length_array();
        let elem_word_size = arr.get_elem_word_size() as usize;
        let data = arr.get_data().to_vec();
        let elems: Vec<ExprNodeId> = (0..len as usize)
            .map(|i| {
                let start = i * elem_word_size;
                let end = start + elem_word_size;
                let words = &data[start..end];
                if elem_word_size == 1 {
                    let f = f64::from_bits(words[0]);
                    expr_to_id(Expr::Literal(Literal::Float(format!("{f}").to_symbol())))
                } else {
                    let tuple_elems = words
                        .iter()
                        .map(|w| {
                            let f = f64::from_bits(*w);
                            expr_to_id(Expr::Literal(Literal::Float(format!("{f}").to_symbol())))
                        })
                        .collect::<Vec<_>>();
                    expr_to_id(Expr::Tuple(tuple_elems))
                }
            })
            .collect();
        let expr = expr_to_id(Expr::ArrayLiteral(elems));
        let code_val = machine.alloc_code(expr);
        machine.set_stack(0, code_val);
    } else {
        code_lit_f(machine);
    }
    1
}

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

/// Helper to create an `ExtClsInfo` from a plain function pointer.
fn mk_cls(name: &str, fun: fn(&mut Machine) -> ReturnCode, ty: TypeNodeId) -> ExtClsInfo {
    let name_sym = name.to_symbol();
    ExtClsInfo {
        name: name_sym,
        ty,
        fun: Rc::new(RefCell::new(fun)),
    }
}

/// Produce the full set of code-generation combinator signatures.
///
/// These are registered as macro-stage (`Stage(0)`) external closures so that
/// the `translate_staging` pass can emit calls to them.
///
/// Although "code values" are semantically AST fragment handles, at the VM
/// level they are plain `RawVal` words (indices into `Machine::code_values`).
/// We use `Float` (`Numeric`) as their compile-time type so that the MIR
/// generator treats them as single-word values — which matches the actual
/// representation.
pub fn codegen_combinator_signatures() -> Vec<ExtClsInfo> {
    use crate::types::{Type, TypeSchemeId};

    // Shorthand type builders
    let f = numeric!(); // "code value" — 1 word, same footprint as Float
    let s = string_t!();
    let i = integer!();
    let af = Type::Array(f).into_id(); // [Float] — array of code values
    let as_ = Type::Array(s).into_id(); // [String]
    // Polymorphic type variable for `lift` — uses a dedicated TypeSchemeId
    // so the type checker instantiates it fresh at each call site.
    let ts = Type::TypeScheme(TypeSchemeId(u64::MAX - 1)).into_id();

    /// Helper: build `(args...) -> ret` function type.
    fn fty(args: Vec<TypeNodeId>, ret: TypeNodeId) -> TypeNodeId {
        Type::Function {
            arg: Type::Tuple(args).into_id(),
            ret,
        }
        .into_id()
    }

    vec![
        mk_cls("code_lit_f", code_lit_f, fty(vec![f], f)),
        mk_cls("code_lit_i", code_lit_i, fty(vec![i], f)),
        mk_cls("code_lit_s", code_lit_s, fty(vec![s], f)),
        mk_cls("code_var", code_var, fty(vec![s], f)),
        mk_cls("code_app", code_app, fty(vec![f, af], f)),
        mk_cls("code_app1", code_app1, fty(vec![f, f], f)),
        mk_cls("code_app2", code_app2, fty(vec![f, f, f], f)),
        mk_cls("code_lam1_finish", code_lam1_finish, fty(vec![s, f], f)),
        mk_cls("code_lam_finish", code_lam_finish, fty(vec![as_, f], f)),
        mk_cls("code_let", code_let, fty(vec![s, f, f], f)),
        mk_cls("code_let_tuple", code_let_tuple, fty(vec![as_, f, f], f)),
        mk_cls("code_letrec", code_letrec, fty(vec![s, f, f], f)),
        mk_cls("code_if", code_if, fty(vec![f, f, f], f)),
        mk_cls("code_tuple", code_tuple, fty(vec![af], f)),
        mk_cls("code_proj", code_proj, fty(vec![f, i], f)),
        mk_cls("code_array", code_array, fty(vec![af], f)),
        mk_cls("code_array_access", code_array_access, fty(vec![f, f], f)),
        mk_cls("code_then", code_then, fty(vec![f, f], f)),
        mk_cls("code_assign", code_assign, fty(vec![f, f], f)),
        mk_cls("code_record", code_record, fty(vec![as_, af], f)),
        mk_cls(
            "code_imcomplete_record",
            code_imcomplete_record,
            fty(vec![as_, af], f),
        ),
        mk_cls(
            "code_record_update",
            code_record_update,
            fty(vec![f, as_, af], f),
        ),
        mk_cls("code_field_access", code_field_access, fty(vec![f, s], f)),
        mk_cls("code_feed", code_feed, fty(vec![s, f], f)),
        mk_cls("code_block", code_block, fty(vec![f], f)),
        mk_cls("code_paren", code_paren, fty(vec![f], f)),
        mk_cls("code_self", code_self, fty(vec![], f)),
        mk_cls("code_now", code_now, fty(vec![], f)),
        mk_cls("code_samplerate", code_samplerate, fty(vec![], f)),
        mk_cls("code_lift_f", code_lift_f, fty(vec![f], f)),
        // lift variants — shadow builtin macro types with VM-compatible types
        mk_cls("lift_f", code_lift_f, fty(vec![f], f)),
        mk_cls("lift_arrayf", code_lift_arrayf, fty(vec![af], f)),
        // Polymorphic lift: (T) -> Numeric where T is instantiated by the type
        // checker. The return is always a code-value index (Numeric) regardless
        // of input type. At the VM level, both floats and arrays are 1-word
        // values, so the calling convention is uniform; the implementation
        // distinguishes them at runtime via `try_get_array`.
        mk_cls("lift", code_lift, fty(vec![ts], f)),
    ]
}

/// Return type info for the codegen combinators (all at macro stage).
pub fn codegen_combinator_type_infos() -> Vec<ExtFunTypeInfo> {
    codegen_combinator_signatures()
        .iter()
        .map(|cls| ExtFunTypeInfo::new(cls.name, cls.ty, EvalStage::Stage(0)))
        .collect()
}
