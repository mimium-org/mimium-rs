//! Procedural macros for mimium plugin development.
//!
//! This crate provides attribute macros that simplify the definition of plugin
//! functions by automatically generating the boilerplate required to bridge
//! between idiomatic Rust and mimium's runtime FFI.
//!
//! # Overview
//!
//! ## Runtime Functions (`mimium_plugin_fn`)
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
//! ## Macro Functions (`mimium_plugin_macro`)
//!
//! For compile-time code generation, the [`mimium_plugin_macro`] attribute transforms
//! methods with the signature `(&mut self, &[(Value, TypeNodeId)]) -> Value` into
//! FFI-compatible wrappers that handle serialization/deserialization automatically.
//!
//! # Example
//!
//! ```rust,ignore
//! use mimium_plugin_macros::{mimium_plugin_fn, mimium_plugin_macro};
//!
//! struct MyPlugin { value: f64 }
//!
//! impl MyPlugin {
//!     #[mimium_plugin_fn]
//!     pub fn add_value(&mut self, x: f64) -> f64 {
//!         self.value + x
//!     }
//!
//!     #[mimium_plugin_macro]
//!     pub fn my_macro(&mut self, args: &[(Value, TypeNodeId)]) -> Value {
//!         // Generate code based on arguments
//!         Value::Code(...)
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
// ===========================================================================
// Macro Function Support (`mimium_plugin_macro`)
// ===========================================================================

/// Transforms a compile-time macro method into a validated form.
///
/// The annotated method should have the signature:
/// ```rust,ignore
/// fn name(&mut self, args: &[(Value, TypeNodeId)]) -> Value
/// ```
///
/// This macro validates the signature and preserves the method as-is.
/// To expose the macro via FFI, manually create a bridge function like:
///
/// ```rust,ignore
/// #[unsafe(no_mangle)]
/// pub extern "C" fn ffi_my_macro(
///     instance: *mut std::ffi::c_void,
///     args_ptr: *const u8,
///     args_len: usize,
///     out_ptr: *mut *mut u8,
///     out_len: *mut usize,
/// ) -> i32 {
///     unsafe {
///         let plugin = &mut *(instance as *mut MyPlugin);
///         let args_bytes = std::slice::from_raw_parts(args_ptr, args_len);
///         let args = mimium_lang::runtime::ffi_serde::deserialize_macro_args(args_bytes)
///             .unwrap_or_else(|_| return -1);
///         let result = plugin.my_macro(&args);
///         let result_bytes = mimium_lang::runtime::ffi_serde::serialize_value(&result)
///             .unwrap_or_else(|_| return -2);
///         let boxed = result_bytes.into_boxed_slice();
///         *out_len = boxed.len();
///         *out_ptr = Box::into_raw(boxed) as *mut u8;
///         0
///     }
/// }
/// ```
///
/// # Example
///
/// ```rust,ignore
/// use mimium_plugin_macros::mimium_plugin_macro;
/// use mimium_lang::interpreter::Value;
/// use mimium_lang::interner::TypeNodeId;
///
/// struct SamplerPlugin;
///
/// impl SamplerPlugin {
///     #[mimium_plugin_macro]
///     pub fn make_sampler(&mut self, args: &[(Value, TypeNodeId)]) -> Value {
///         // Extract first argument as string
///         let path = match &args[0].0 {
///             Value::String(s) => s.to_string(),
///             _ => unreachable!(),
///         };
///         // Load sample and return generated code
///         Value::Code(...)
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn mimium_plugin_macro(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    match validate_macro_fn(&input) {
        Ok(_) => {
            // Return the original function as-is, converted back to TokenStream
            quote! { #input }.into()
        }
        Err(e) => e.to_compile_error().into(),
    }
}

/// Validates the signature of a macro function.
fn validate_macro_fn(input: &ItemFn) -> syn::Result<()> {
    // Check that function takes &mut self as first parameter
    let _receiver = require_self_receiver(input)?;

    if !input.sig.generics.params.is_empty() {
        return Err(Error::new_spanned(
            &input.sig.generics,
            "mimium_plugin_macro does not support generic parameters",
        ));
    }

    // Validate signature: should have one parameter of type &[(Value, TypeNodeId)]
    if input.sig.inputs.len() != 2 {
        return Err(Error::new_spanned(
            &input.sig.inputs,
            "mimium_plugin_macro: expected signature (&mut self, &[(Value, TypeNodeId)]) -> Value",
        ));
    }

    // Validate return type: should be Value
    match &input.sig.output {
        ReturnType::Type(_, ty) => {
            if !is_value_type(ty) {
                return Err(Error::new_spanned(
                    ty,
                    "mimium_plugin_macro: return type must be `Value`",
                ));
            }
        }
        ReturnType::Default => {
            return Err(Error::new_spanned(
                &input.sig,
                "mimium_plugin_macro: must return `Value`",
            ));
        }
    }

    Ok(())
}

/// Check if a type is `Value` (from mimium_lang::interpreter).
fn is_value_type(ty: &Type) -> bool {
    match ty {
        Type::Path(tp) => tp
            .path
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "Value"),
        _ => false,
    }
}
