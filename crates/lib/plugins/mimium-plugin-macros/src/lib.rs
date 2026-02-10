//! Procedural macros for mimium plugin development.
//!
//! This crate provides attribute macros that simplify the definition of plugin
//! functions by automatically generating the boilerplate required to bridge
//! between idiomatic Rust and mimium's runtime FFI.
//!
//! # Overview
//!
//! Plugin authors write methods using normal Rust types (e.g. `f64` arguments
//! and return values). The [`mimium_plugin_fn`] macro transforms each annotated
//! method into a pair:
//!
//! 1. A private helper retaining the original signature and body.
//! 2. A public wrapper with the `(&mut Self, &mut Machine) -> ReturnCode`
//!    signature expected by [`SystemPluginFnType`](mimium_lang::plugin::SystemPluginFnType).
//!
//! Internally the wrapper creates a [`RuntimeHandle`](mimium_lang::runtime::ffi::RuntimeHandle)
//! and performs all argument extraction / return-value writing through it,
//! ensuring that the actual plugin logic is decoupled from the VM implementation.
//!
//! # Example
//!
//! ```rust,ignore
//! use mimium_plugin_macros::mimium_plugin_fn;
//!
//! struct MyPlugin { value: f64 }
//!
//! impl MyPlugin {
//!     #[mimium_plugin_fn]
//!     pub fn add_value(&mut self, x: f64) -> f64 {
//!         self.value + x
//!     }
//! }
//! ```

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Error, FnArg, Ident, ItemFn, Pat, ReturnType, Type, TypeTuple, parse_macro_input};

/// Transforms a plugin method into a VM-compatible wrapper.
///
/// The annotated method should use idiomatic Rust types for its parameters
/// and return value.  The macro generates:
///
/// 1. A private `#[doc(hidden)]` helper named `__{name}_impl` that preserves
///    the original signature and body (including early returns).
/// 2. A public wrapper keeping the original method name, with the signature
///    `(&mut self, &mut Machine) -> ReturnCode`.
///
/// # Supported types
///
/// | Position      | Type              | Conversion                                  |
/// |---------------|-------------------|---------------------------------------------|
/// | Argument      | `f64`             | `RuntimeHandle::get_arg_f64(N)`             |
/// | Return        | `f64`             | `RuntimeHandle::set_return_f64(0, v)`       |
/// | Return        | `(f64, f64, ...)` | One `set_return_f64` call per element       |
/// | Return        | `()`              | No stack write, return code 0               |
///
/// # Example
///
/// ```rust,ignore
/// #[mimium_plugin_fn]
/// pub fn get_sampler(&mut self, pos: f64, sample_idx: f64) -> f64 {
///     let idx = sample_idx as usize;
///     self.samples.get(idx)
///         .map(|v| interpolate(v, pos))
///         .unwrap_or(0.0)
/// }
/// ```
#[proc_macro_attribute]
pub fn mimium_plugin_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    match generate_plugin_fn(&input) {
        Ok(tokens) => tokens.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Parsed parameter information (one per non-self argument).
struct ParamInfo {
    /// Zero-based index in the argument list (excluding self).
    index: usize,
    /// Variable name extracted from the pattern.
    name: Ident,
    /// The declared type (validated to be `f64` for now).
    ty: Box<Type>,
}

/// Classification of the return type for code generation.
enum ReturnKind {
    /// `()` or no explicit return type.
    Unit,
    /// A single `f64`.
    Scalar,
    /// A tuple of `f64` values with the given element count.
    Tuple(usize),
}

/// Main code-generation entry point.
fn generate_plugin_fn(input: &ItemFn) -> syn::Result<proc_macro2::TokenStream> {
    let fn_name = &input.sig.ident;
    let vis = &input.vis;
    let attrs = &input.attrs;
    let body = &input.block;

    // --- validation ---------------------------------------------------------

    let _receiver = require_self_receiver(input)?;

    if !input.sig.generics.params.is_empty() {
        return Err(Error::new_spanned(
            &input.sig.generics,
            "mimium_plugin_fn does not support generic parameters",
        ));
    }
    if input.sig.asyncness.is_some() {
        return Err(Error::new_spanned(
            &input.sig,
            "mimium_plugin_fn does not support async functions",
        ));
    }

    // --- parse parameters & return type -------------------------------------

    let params = parse_params(input)?;
    let return_kind = classify_return_type(&input.sig.output)?;

    // --- code generation ----------------------------------------------------

    let impl_name = format_ident!("__{}_impl", fn_name);

    // All original parameters including self (for the helper method).
    let all_impl_params = &input.sig.inputs;
    let original_output = &input.sig.output;

    // Argument extraction through RuntimeHandle.
    let arg_extractions = params.iter().map(|p| {
        let name = &p.name;
        let ty = &p.ty;
        let idx = p.index as u32;
        quote! {
            let #name: #ty = __handle.get_arg_f64(#idx);
        }
    });

    // Call arguments for the helper method.
    let call_args = params.iter().map(|p| {
        let name = &p.name;
        quote! { #name }
    });

    // Result writing & return code.
    let (result_handling, return_code) = match &return_kind {
        ReturnKind::Unit => {
            let args = call_args.collect::<Vec<_>>();
            (quote! { self.#impl_name(#(#args),*); }, quote! { 0i64 })
        }
        ReturnKind::Scalar => {
            let args = call_args.collect::<Vec<_>>();
            (
                quote! {
                    let __result = self.#impl_name(#(#args),*);
                    __handle.set_return_f64(0, __result);
                },
                quote! { 1i64 },
            )
        }
        ReturnKind::Tuple(n) => {
            let args = call_args.collect::<Vec<_>>();
            let writes = (0..*n).map(|i| {
                let idx = syn::Index::from(i);
                let stack_pos = i as u32;
                quote! {
                    __handle.set_return_f64(#stack_pos, __result.#idx);
                }
            });
            let n_i64 = *n as i64;
            (
                quote! {
                    let __result = self.#impl_name(#(#args),*);
                    #(#writes)*
                },
                quote! { #n_i64 },
            )
        }
    };

    // --- emit both methods --------------------------------------------------

    Ok(quote! {
        #[doc(hidden)]
        #[inline(always)]
        fn #impl_name(#all_impl_params) #original_output
            #body

        #(#attrs)*
        #vis fn #fn_name(
            &mut self,
            __machine: &mut ::mimium_lang::runtime::vm::Machine,
        ) -> ::mimium_lang::runtime::vm::ReturnCode {
            // SAFETY: __machine is valid for the duration of this call and
            // no other mutable alias exists.
            let mut __handle = unsafe {
                ::mimium_lang::runtime::vm_ffi::runtime_handle_from_machine(__machine)
            };
            #(#arg_extractions)*
            #result_handling
            #return_code
        }
    })
}

// ---------------------------------------------------------------------------
// Parsing & validation helpers
// ---------------------------------------------------------------------------

/// Ensure the first parameter is a `self` receiver and return it.
fn require_self_receiver(input: &ItemFn) -> syn::Result<&syn::Receiver> {
    let first = input.sig.inputs.first().ok_or_else(|| {
        Error::new_spanned(
            &input.sig,
            "mimium_plugin_fn requires a method with a `self` parameter",
        )
    })?;
    match first {
        FnArg::Receiver(r) => Ok(r),
        other => Err(Error::new_spanned(
            other,
            "mimium_plugin_fn: first parameter must be `&self` or `&mut self`",
        )),
    }
}

/// Parse non-self parameters into [`ParamInfo`] entries.
fn parse_params(input: &ItemFn) -> syn::Result<Vec<ParamInfo>> {
    input
        .sig
        .inputs
        .iter()
        .skip(1) // skip self
        .enumerate()
        .map(|(i, arg)| {
            let pat_type = match arg {
                FnArg::Typed(pt) => pt,
                _ => {
                    return Err(Error::new_spanned(
                        arg,
                        "mimium_plugin_fn: unexpected second self parameter",
                    ));
                }
            };
            validate_arg_type(&pat_type.ty)?;
            let name = extract_ident(&pat_type.pat)?;
            Ok(ParamInfo {
                index: i,
                name,
                ty: pat_type.ty.clone(),
            })
        })
        .collect()
}

/// Validate that an argument type is currently supported.
fn validate_arg_type(ty: &Type) -> syn::Result<()> {
    match ty {
        Type::Path(tp) if tp.path.is_ident("f64") => Ok(()),
        _ => Err(Error::new_spanned(
            ty,
            "mimium_plugin_fn: only `f64` arguments are currently supported \
             (String and Closure support is planned)",
        )),
    }
}

/// Extract a simple identifier from a pattern, rejecting wildcards and
/// destructuring patterns.
fn extract_ident(pat: &Pat) -> syn::Result<Ident> {
    match pat {
        Pat::Ident(pi) => Ok(pi.ident.clone()),
        _ => Err(Error::new_spanned(
            pat,
            "mimium_plugin_fn: parameter must be a simple identifier (not a pattern)",
        )),
    }
}

/// Classify the return type for code generation.
fn classify_return_type(output: &ReturnType) -> syn::Result<ReturnKind> {
    match output {
        ReturnType::Default => Ok(ReturnKind::Unit),
        ReturnType::Type(_, ty) => classify_type(ty),
    }
}

fn classify_type(ty: &Type) -> syn::Result<ReturnKind> {
    match ty {
        // Explicit `()`
        Type::Tuple(TypeTuple { elems, .. }) if elems.is_empty() => Ok(ReturnKind::Unit),
        // `f64`
        Type::Path(tp) if tp.path.is_ident("f64") => Ok(ReturnKind::Scalar),
        // `(f64, f64, ...)` — all elements must be f64
        Type::Tuple(TypeTuple { elems, .. }) => {
            for elem in elems {
                match elem {
                    Type::Path(tp) if tp.path.is_ident("f64") => {}
                    _ => {
                        return Err(Error::new_spanned(
                            elem,
                            "mimium_plugin_fn: tuple return elements must all be `f64`",
                        ));
                    }
                }
            }
            Ok(ReturnKind::Tuple(elems.len()))
        }
        _ => Err(Error::new_spanned(
            ty,
            "mimium_plugin_fn: unsupported return type. \
             Currently supported: f64, (f64, ...), ()",
        )),
    }
}
