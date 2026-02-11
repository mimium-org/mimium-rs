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

// ===========================================================================
// Dynamic Plugin ABI Generation (`mimium_export_plugin!`)
// ===========================================================================

/// Generate all FFI export functions for a dynamic plugin.
///
/// This macro eliminates the ~200 lines of boilerplate that every plugin must
/// write to expose itself via the dynamic plugin ABI.  It generates:
///
/// - `mimium_plugin_metadata()` — static metadata export
/// - `mimium_plugin_create()` — plugin constructor
/// - `mimium_plugin_set_interner()` — interner sharing
/// - `mimium_plugin_destroy()` — destructor
/// - `mimium_plugin_get_function()` — runtime function dispatcher
/// - `mimium_plugin_get_macro()` — macro function dispatcher
/// - `mimium_plugin_get_type_infos()` — type info export
/// - One `ffi_*` wrapper per runtime function
/// - One `ffi_*` wrapper per macro function
///
/// # Syntax
///
/// ```rust,ignore
/// mimium_export_plugin! {
///     plugin_type: MyPlugin,
///     plugin_name: "my-plugin",
///     plugin_author: "my-org",
///     // Optional: set to "try_new" for fallible constructors.
///     // Default is "default".
///     // constructor: "default",
///     capabilities: {
///         has_audio_worker: false,
///         has_macros: true,
///         has_runtime_functions: true,
///     },
///     // Runtime functions mapped from FFI name to method name.
///     runtime_functions: [
///         ("__get_slider", get_slider),
///         ("__probe_intercept", probe_intercept),
///     ],
///     // Macro functions mapped from FFI name to method name.
///     macro_functions: [
///         ("Slider", make_slider),
///         ("Probe", make_probe_macro),
///     ],
///     // Type info entries: (name, type_expr, stage).
///     type_infos: [
///         { name: "Slider", ty: MyPlugin::slider_signature(), stage: 0 },
///         { name: "Probe", ty: MyPlugin::probe_signature(), stage: 0 },
///         { name: "__get_slider", ty_expr: function!(vec![numeric!()], numeric!()), stage: 1 },
///     ],
/// }
/// ```
#[proc_macro]
pub fn mimium_export_plugin(input: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(input as PluginExportInput);
    match generate_plugin_exports(&parsed) {
        Ok(tokens) => tokens.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

// ---------------------------------------------------------------------------
// Parsing structures for mimium_export_plugin!
// ---------------------------------------------------------------------------

struct PluginExportInput {
    plugin_type: syn::Path,
    plugin_name: syn::LitStr,
    plugin_author: syn::LitStr,
    constructor: ConstructorKind,
    capabilities: Capabilities,
    runtime_functions: Vec<FunctionMapping>,
    macro_functions: Vec<FunctionMapping>,
    type_infos: Vec<TypeInfoEntry>,
    wasm_audio_handle: Option<WasmAudioHandle>,
}

#[derive(Clone)]
enum ConstructorKind {
    Default,
    TryNew,
}

struct Capabilities {
    has_audio_worker: syn::LitBool,
    has_macros: syn::LitBool,
    has_runtime_functions: syn::LitBool,
}

struct FunctionMapping {
    ffi_name: syn::LitStr,
    method_name: syn::Ident,
}

enum TypeInfoEntry {
    /// Uses a SysPluginSignature method (name, method call expr, stage)
    Signature {
        name_str: syn::LitStr,
        sig_expr: syn::Expr,
        stage: syn::LitInt,
    },
    /// Uses a direct type expression (name, ty_expr, stage)
    TypeExpr {
        name_str: syn::LitStr,
        ty_expr: syn::Expr,
        stage: syn::LitInt,
    },
}

/// Configuration for auto-generating `into_wasm_plugin_fn_map` on an audio
/// handle type.
///
/// When specified in `mimium_export_plugin!`, the macro generates an
/// `into_wasm_plugin_fn_map` method that creates `WasmPluginFn` closures
/// forwarding `&[f64]` arguments to the handle's methods.
///
/// All handle methods referenced here must accept `f64` arguments to match
/// the WASM trampoline calling convention.
struct WasmAudioHandle {
    handle_type: syn::Path,
    functions: Vec<WasmHandleFn>,
}

/// A single function entry for WASM audio handle code generation.
struct WasmHandleFn {
    /// The FFI function name (e.g. `"__get_slider"`).
    ffi_name: syn::LitStr,
    /// The method name on the audio handle type (e.g. `get_slider`).
    method_name: syn::Ident,
    /// Number of `f64` arguments the method takes.
    nargs: usize,
}

// Custom parser for the plugin export macro input
impl syn::parse::Parse for PluginExportInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut plugin_type: Option<syn::Path> = None;
        let mut plugin_name: Option<syn::LitStr> = None;
        let mut plugin_author: Option<syn::LitStr> = None;
        let mut constructor = ConstructorKind::Default;
        let mut capabilities: Option<Capabilities> = None;
        let mut runtime_functions = Vec::new();
        let mut macro_functions = Vec::new();
        let mut type_infos = Vec::new();
        let mut wasm_audio_handle: Option<WasmAudioHandle> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<syn::Token![:]>()?;

            match key.to_string().as_str() {
                "plugin_type" => {
                    plugin_type = Some(input.parse()?);
                }
                "plugin_name" => {
                    plugin_name = Some(input.parse()?);
                }
                "plugin_author" => {
                    plugin_author = Some(input.parse()?);
                }
                "constructor" => {
                    let lit: syn::LitStr = input.parse()?;
                    constructor = match lit.value().as_str() {
                        "default" => ConstructorKind::Default,
                        "try_new" => ConstructorKind::TryNew,
                        _ => {
                            return Err(Error::new_spanned(
                                lit,
                                "constructor must be \"default\" or \"try_new\"",
                            ));
                        }
                    };
                }
                "capabilities" => {
                    let content;
                    syn::braced!(content in input);
                    capabilities = Some(parse_capabilities(&content)?);
                }
                "runtime_functions" => {
                    let content;
                    syn::bracketed!(content in input);
                    runtime_functions = parse_function_mappings(&content)?;
                }
                "macro_functions" => {
                    let content;
                    syn::bracketed!(content in input);
                    macro_functions = parse_function_mappings(&content)?;
                }
                "type_infos" => {
                    let content;
                    syn::bracketed!(content in input);
                    type_infos = parse_type_infos(&content)?;
                }
                "wasm_audio_handle" => {
                    let content;
                    syn::braced!(content in input);
                    wasm_audio_handle = Some(parse_wasm_audio_handle(&content)?);
                }
                other => {
                    return Err(Error::new(key.span(), format!("unknown field: {other}")));
                }
            }

            // consume optional trailing comma
            let _ = input.parse::<syn::Token![,]>();
        }

        Ok(PluginExportInput {
            plugin_type: plugin_type
                .ok_or_else(|| Error::new(proc_macro2::Span::call_site(), "missing plugin_type"))?,
            plugin_name: plugin_name
                .ok_or_else(|| Error::new(proc_macro2::Span::call_site(), "missing plugin_name"))?,
            plugin_author: plugin_author.ok_or_else(|| {
                Error::new(proc_macro2::Span::call_site(), "missing plugin_author")
            })?,
            constructor,
            capabilities: capabilities.ok_or_else(|| {
                Error::new(proc_macro2::Span::call_site(), "missing capabilities")
            })?,
            runtime_functions,
            macro_functions,
            type_infos,
            wasm_audio_handle,
        })
    }
}

fn parse_capabilities(input: syn::parse::ParseStream) -> syn::Result<Capabilities> {
    let mut has_audio_worker = None;
    let mut has_macros = None;
    let mut has_runtime_functions = None;

    while !input.is_empty() {
        let key: Ident = input.parse()?;
        input.parse::<syn::Token![:]>()?;
        let val: syn::LitBool = input.parse()?;
        let _ = input.parse::<syn::Token![,]>();

        match key.to_string().as_str() {
            "has_audio_worker" => has_audio_worker = Some(val),
            "has_macros" => has_macros = Some(val),
            "has_runtime_functions" => has_runtime_functions = Some(val),
            _ => return Err(Error::new_spanned(key, "unknown capability")),
        }
    }

    Ok(Capabilities {
        has_audio_worker: has_audio_worker
            .unwrap_or(syn::LitBool::new(false, proc_macro2::Span::call_site())),
        has_macros: has_macros.unwrap_or(syn::LitBool::new(false, proc_macro2::Span::call_site())),
        has_runtime_functions: has_runtime_functions
            .unwrap_or(syn::LitBool::new(false, proc_macro2::Span::call_site())),
    })
}

fn parse_function_mappings(input: syn::parse::ParseStream) -> syn::Result<Vec<FunctionMapping>> {
    let mut result = Vec::new();
    while !input.is_empty() {
        let content;
        syn::parenthesized!(content in input);
        let ffi_name: syn::LitStr = content.parse()?;
        content.parse::<syn::Token![,]>()?;
        let method_name: Ident = content.parse()?;
        result.push(FunctionMapping {
            ffi_name,
            method_name,
        });
        let _ = input.parse::<syn::Token![,]>();
    }
    Ok(result)
}

fn parse_type_infos(input: syn::parse::ParseStream) -> syn::Result<Vec<TypeInfoEntry>> {
    let mut result = Vec::new();
    while !input.is_empty() {
        let content;
        syn::braced!(content in input);

        // Parse fields inside braces
        let mut name_str: Option<syn::LitStr> = None;
        let mut sig_expr: Option<syn::Expr> = None;
        let mut ty_expr: Option<syn::Expr> = None;
        let mut stage: Option<syn::LitInt> = None;

        while !content.is_empty() {
            let key: Ident = content.parse()?;
            content.parse::<syn::Token![:]>()?;

            match key.to_string().as_str() {
                "name" => {
                    name_str = Some(content.parse()?);
                }
                "sig" => {
                    sig_expr = Some(content.parse()?);
                }
                "ty_expr" => {
                    ty_expr = Some(content.parse()?);
                }
                "stage" => {
                    stage = Some(content.parse()?);
                }
                _ => return Err(Error::new_spanned(key, "unknown type_info field")),
            }
            let _ = content.parse::<syn::Token![,]>();
        }

        let name_str = name_str
            .ok_or_else(|| Error::new(proc_macro2::Span::call_site(), "type_info: missing name"))?;
        let stage = stage.ok_or_else(|| {
            Error::new(proc_macro2::Span::call_site(), "type_info: missing stage")
        })?;

        if let Some(sig_expr) = sig_expr {
            result.push(TypeInfoEntry::Signature {
                name_str,
                sig_expr,
                stage,
            });
        } else if let Some(ty_expr) = ty_expr {
            result.push(TypeInfoEntry::TypeExpr {
                name_str,
                ty_expr,
                stage,
            });
        } else {
            return Err(Error::new(
                proc_macro2::Span::call_site(),
                "type_info: must have `sig` or `ty_expr`",
            ));
        }

        let _ = input.parse::<syn::Token![,]>();
    }
    Ok(result)
}

/// Parse the `wasm_audio_handle` section.
///
/// ```text
/// wasm_audio_handle: {
///     handle_type: MyAudioHandle,
///     functions: [
///         ("__get_slider", get_slider, 1),
///         ("__probe_intercept", probe_intercept, 2),
///     ],
/// },
/// ```
fn parse_wasm_audio_handle(input: syn::parse::ParseStream) -> syn::Result<WasmAudioHandle> {
    let mut handle_type: Option<syn::Path> = None;
    let mut functions = Vec::new();

    while !input.is_empty() {
        let key: Ident = input.parse()?;
        input.parse::<syn::Token![:]>()?;

        match key.to_string().as_str() {
            "handle_type" => {
                handle_type = Some(input.parse()?);
            }
            "functions" => {
                let content;
                syn::bracketed!(content in input);
                while !content.is_empty() {
                    let inner;
                    syn::parenthesized!(inner in content);
                    let ffi_name: syn::LitStr = inner.parse()?;
                    inner.parse::<syn::Token![,]>()?;
                    let method_name: Ident = inner.parse()?;
                    inner.parse::<syn::Token![,]>()?;
                    let nargs_lit: syn::LitInt = inner.parse()?;
                    let nargs: usize = nargs_lit.base10_parse()?;
                    functions.push(WasmHandleFn {
                        ffi_name,
                        method_name,
                        nargs,
                    });
                    let _ = content.parse::<syn::Token![,]>();
                }
            }
            _ => return Err(Error::new_spanned(key, "unknown wasm_audio_handle field")),
        }
        let _ = input.parse::<syn::Token![,]>();
    }

    Ok(WasmAudioHandle {
        handle_type: handle_type.ok_or_else(|| {
            Error::new(
                proc_macro2::Span::call_site(),
                "wasm_audio_handle: missing handle_type",
            )
        })?,
        functions,
    })
}

// ---------------------------------------------------------------------------
// Code generation
// ---------------------------------------------------------------------------

fn generate_plugin_exports(input: &PluginExportInput) -> syn::Result<proc_macro2::TokenStream> {
    let plugin_ty = &input.plugin_type;
    let plugin_name_lit = &input.plugin_name;
    let plugin_author_lit = &input.plugin_author;
    let has_audio_worker = &input.capabilities.has_audio_worker;
    let has_macros = &input.capabilities.has_macros;
    let has_runtime_fns = &input.capabilities.has_runtime_functions;

    // -- mimium_plugin_create
    let create_body = match &input.constructor {
        ConstructorKind::Default => quote! {
            let plugin = Box::new(<#plugin_ty>::default());
            Box::into_raw(plugin) as *mut ::mimium_lang::plugin::loader::PluginInstance
        },
        ConstructorKind::TryNew => quote! {
            match <#plugin_ty>::try_new() {
                Some(plugin) => Box::into_raw(Box::new(plugin)) as *mut ::mimium_lang::plugin::loader::PluginInstance,
                None => std::ptr::null_mut(),
            }
        },
    };

    // -- ffi runtime function wrappers
    let ffi_runtime_wrappers = input.runtime_functions.iter().map(|f| {
        let ffi_fn_name = format_ident!("__ffi_{}", f.method_name);
        let method = &f.method_name;
        quote! {
            unsafe extern "C" fn #ffi_fn_name(
                instance: *mut ::mimium_lang::plugin::loader::PluginInstance,
                runtime: *mut ::std::ffi::c_void,
            ) -> i64 {
                if instance.is_null() || runtime.is_null() {
                    return 0;
                }
                let machine = unsafe { &mut *(runtime as *mut ::mimium_lang::runtime::vm::Machine) };
                let plugin = unsafe { &mut *(instance as *mut #plugin_ty) };
                plugin.#method(machine)
            }
        }
    });

    // -- mimium_plugin_get_function match arms
    let get_function_arms = input.runtime_functions.iter().map(|f| {
        let ffi_name_str = &f.ffi_name;
        let ffi_fn_name = format_ident!("__ffi_{}", f.method_name);
        quote! {
            #ffi_name_str => Some(#ffi_fn_name),
        }
    });

    // -- ffi macro bridges
    let ffi_macro_wrappers = input.macro_functions.iter().map(|f| {
        let ffi_fn_name = format_ident!("__ffi_macro_{}", f.method_name);
        let method = &f.method_name;
        quote! {
            unsafe extern "C" fn #ffi_fn_name(
                instance: *mut ::std::ffi::c_void,
                args_ptr: *const u8,
                args_len: usize,
                out_ptr: *mut *mut u8,
                out_len: *mut usize,
            ) -> i32 {
                if instance.is_null() || args_ptr.is_null() || out_ptr.is_null() || out_len.is_null() {
                    return -3;
                }
                unsafe {
                    let plugin = &mut *(instance as *mut #plugin_ty);
                    let args_bytes = ::std::slice::from_raw_parts(args_ptr, args_len);
                    let args = match ::mimium_lang::runtime::ffi_serde::deserialize_macro_args(args_bytes) {
                        Ok(a) => a,
                        Err(e) => {
                            ::mimium_lang::log::error!("Failed to deserialize macro args: {e}");
                            return -1;
                        }
                    };
                    let result = plugin.#method(&args);
                    let result_bytes = match ::mimium_lang::runtime::ffi_serde::serialize_value(&result) {
                        Ok(b) => b,
                        Err(e) => {
                            ::mimium_lang::log::error!("Failed to serialize macro result: {e}");
                            return -2;
                        }
                    };
                    let boxed = result_bytes.into_boxed_slice();
                    *out_len = boxed.len();
                    *out_ptr = Box::into_raw(boxed) as *mut u8;
                    0
                }
            }
        }
    });

    // -- mimium_plugin_get_macro match arms
    let get_macro_arms = input.macro_functions.iter().map(|f| {
        let ffi_name_str = &f.ffi_name;
        let ffi_fn_name = format_ident!("__ffi_macro_{}", f.method_name);
        // Also match the method name as an alias
        let method_str = f.method_name.to_string();
        quote! {
            #ffi_name_str | #method_str => Some(#ffi_fn_name),
        }
    });

    // -- type info entries
    let type_info_entries = input.type_infos.iter().map(|entry| match entry {
        TypeInfoEntry::Signature {
            name_str: _,
            sig_expr,
            stage,
        } => {
            quote! {
                {
                    let sig = #sig_expr;
                    add_info(sig.get_name(), sig.get_type(), #stage, &mut storage, &mut infos);
                }
            }
        }
        TypeInfoEntry::TypeExpr {
            name_str,
            ty_expr,
            stage,
        } => {
            quote! {
                {
                    let ty = #ty_expr;
                    add_info(#name_str, ty, #stage, &mut storage, &mut infos);
                }
            }
        }
    });

    // Build the full plugin name string with null terminator
    let plugin_name_with_nul = format!("{}\0", plugin_name_lit.value());
    let plugin_author_with_nul = format!("{}\0", plugin_author_lit.value());

    // Generate optional WASM audio handle impl
    let wasm_handle_impl = generate_wasm_handle_impl(&input.wasm_audio_handle);

    Ok(quote! {
        #[cfg(not(target_arch = "wasm32"))]
        const _: () = {
            use ::std::ffi::{CString, c_char};
            use ::mimium_lang::plugin::loader::{
                PluginCapabilities, PluginInstance, PluginMetadata,
                PluginFunctionFn, PluginMacroFn, FfiTypeInfo,
            };

            static PLUGIN_NAME: &str = #plugin_name_with_nul;
            static PLUGIN_VERSION: &str = env!("CARGO_PKG_VERSION");
            static PLUGIN_AUTHOR: &str = #plugin_author_with_nul;

            #[unsafe(no_mangle)]
            pub extern "C" fn mimium_plugin_metadata() -> *const PluginMetadata {
                use ::std::sync::OnceLock;
                static METADATA: OnceLock<(CString, PluginMetadata)> = OnceLock::new();
                let (_version_cstr, metadata) = METADATA.get_or_init(|| {
                    let version_cstr = CString::new(PLUGIN_VERSION).expect("Version string is valid");
                    let metadata = PluginMetadata {
                        name: PLUGIN_NAME.as_ptr() as *const c_char,
                        version: version_cstr.as_ptr(),
                        author: PLUGIN_AUTHOR.as_ptr() as *const c_char,
                        capabilities: PluginCapabilities {
                            has_audio_worker: #has_audio_worker,
                            has_macros: #has_macros,
                            has_runtime_functions: #has_runtime_fns,
                        },
                    };
                    (version_cstr, metadata)
                });
                metadata
            }

            #[unsafe(no_mangle)]
            pub extern "C" fn mimium_plugin_create() -> *mut PluginInstance {
                #create_body
            }

            #[unsafe(no_mangle)]
            pub unsafe extern "C" fn mimium_plugin_set_interner(
                globals_ptr: *const ::std::ffi::c_void,
            ) {
                unsafe { ::mimium_lang::interner::set_external_session_globals(globals_ptr) };
            }

            #[unsafe(no_mangle)]
            pub extern "C" fn mimium_plugin_destroy(instance: *mut PluginInstance) {
                if !instance.is_null() {
                    unsafe {
                        let _ = Box::from_raw(instance as *mut #plugin_ty);
                    }
                }
            }

            // -- Runtime function FFI wrappers
            #(#ffi_runtime_wrappers)*

            #[unsafe(no_mangle)]
            pub unsafe extern "C" fn mimium_plugin_get_function(
                name: *const c_char,
            ) -> Option<PluginFunctionFn> {
                use ::std::ffi::CStr;
                if name.is_null() { return None; }
                let name_str = unsafe { CStr::from_ptr(name) }.to_str().ok()?;
                match name_str {
                    #(#get_function_arms)*
                    _ => None,
                }
            }

            // -- Macro function FFI bridges
            #(#ffi_macro_wrappers)*

            #[unsafe(no_mangle)]
            pub unsafe extern "C" fn mimium_plugin_get_macro(
                name: *const c_char,
            ) -> Option<PluginMacroFn> {
                use ::std::ffi::CStr;
                if name.is_null() { return None; }
                let name_str = unsafe { CStr::from_ptr(name) }.to_str().ok()?;
                match name_str {
                    #(#get_macro_arms)*
                    _ => None,
                }
            }

            // -- Type info export
            static mut TYPE_INFO_STORAGE: Option<Vec<(CString, Vec<u8>)>> = None;
            static mut FFI_TYPE_INFOS: Option<Vec<FfiTypeInfo>> = None;

            #[unsafe(no_mangle)]
            pub extern "C" fn mimium_plugin_get_type_infos(
                out_len: *mut usize,
            ) -> *const FfiTypeInfo {
                if out_len.is_null() {
                    return ::std::ptr::null();
                }
                unsafe {
                    if (*::std::ptr::addr_of!(TYPE_INFO_STORAGE)).is_none() {
                        let mut storage: Vec<(CString, Vec<u8>)> = Vec::new();
                        let mut infos: Vec<FfiTypeInfo> = Vec::new();

                        let add_info = |name_str: &str,
                                        ty: ::mimium_lang::interner::TypeNodeId,
                                        stage: u8,
                                        storage: &mut Vec<(CString, Vec<u8>)>,
                                        infos: &mut Vec<FfiTypeInfo>| -> Option<()> {
                            let name_cstr = CString::new(name_str).ok()?;
                            let type_bytes = ::bincode::serialize(&ty).ok()?;
                            let name_ptr = name_cstr.as_ptr();
                            let type_ptr = type_bytes.as_ptr();
                            let type_len = type_bytes.len();
                            storage.push((name_cstr, type_bytes));
                            infos.push(FfiTypeInfo {
                                name: name_ptr,
                                type_data: type_ptr,
                                type_len,
                                stage,
                            });
                            Some(())
                        };

                        #(#type_info_entries)*

                        TYPE_INFO_STORAGE = Some(storage);
                        FFI_TYPE_INFOS = Some(infos);
                    }

                    let infos = (*::std::ptr::addr_of!(FFI_TYPE_INFOS)).as_ref().unwrap();
                    *out_len = infos.len();
                    infos.as_ptr()
                }
            }
        };

        #wasm_handle_impl
    })
}

/// Generate an `into_wasm_plugin_fn_map` impl block for the audio handle type.
///
/// Each function entry becomes a closure that:
/// 1. Locks the `Arc<Mutex<Handle>>`
/// 2. Extracts the required number of `f64` args from the `&[f64]` slice
/// 3. Calls the handle's method with those args
/// 4. Returns the result as `Option<f64>`
fn generate_wasm_handle_impl(
    wasm_handle: &Option<WasmAudioHandle>,
) -> proc_macro2::TokenStream {
    let wasm_handle = match wasm_handle {
        Some(h) => h,
        None => return quote! {},
    };

    let handle_ty = &wasm_handle.handle_type;

    let closure_entries = wasm_handle.functions.iter().map(|f| {
        let ffi_name_str = &f.ffi_name;
        let method = &f.method_name;
        let nargs = f.nargs;

        // Generate arg forwarding: args[0], args[1], ..., args[n-1]
        let arg_indices: Vec<proc_macro2::TokenStream> = (0..nargs)
            .map(|i| {
                let idx = syn::Index::from(i);
                quote! { args[#idx] }
            })
            .collect();

        quote! {
            {
                let h = handle.clone();
                map.insert(
                    #ffi_name_str.to_string(),
                    ::std::sync::Arc::new(move |args: &[f64]| -> Option<f64> {
                        if args.len() >= #nargs {
                            Some(h.lock().ok()?.#method(#(#arg_indices),*))
                        } else {
                            None
                        }
                    }) as ::mimium_lang::runtime::wasm::WasmPluginFn,
                );
            }
        }
    });

    quote! {
        #[cfg(not(target_arch = "wasm32"))]
        impl #handle_ty {
            /// Convert this audio handle into a `WasmPluginFnMap` for WASM
            /// trampoline registration.
            ///
            /// Auto-generated by `mimium_export_plugin!`.
            pub fn into_wasm_plugin_fn_map(self) -> ::mimium_lang::runtime::wasm::WasmPluginFnMap {
                let handle = ::std::sync::Arc::new(::std::sync::Mutex::new(self));
                let mut map = ::mimium_lang::runtime::wasm::WasmPluginFnMap::new();
                #(#closure_entries)*
                map
            }
        }
    }
}