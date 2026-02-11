//! ## mimium MIDI Plugin
//!
//! MIDI plugin currently implements a functionality for binding midi note signal to a tuple of float value.
//! Processing for raw MIDI events like midi plugin in VST cannot be realized for now.

use atomic_float::AtomicF64;
use midir::{MidiInput, MidiInputConnection, MidiInputPort};
use mimium_lang::{
    ast::{Expr, Literal},
    function,
    interner::{ToSymbol, TypeNodeId},
    interpreter::Value,
    log, numeric,
    plugin::{SysPluginSignature, SystemPlugin, SystemPluginFnType, SystemPluginMacroType},
    runtime::vm,
    string_t,
    types::{PType, RecordTypeField, Type},
    unit,
};
use mimium_plugin_macros::mimium_plugin_fn;
use std::{
    cell::OnceCell,
    sync::{Arc, atomic::Ordering},
};
use wmidi::MidiMessage;

type NoteCallBack = Arc<dyn Fn(f64, f64) + Send + Sync>;

#[derive(Default)]
struct NoteCallBacks(pub [Vec<NoteCallBack>; 16]);

impl NoteCallBacks {
    pub fn invoke_note_callback(&self, chan: u8, note: u8, vel: u8) {
        if chan < 15 {
            self.0[chan as usize]
                .iter()
                .for_each(|cb| cb(note as f64, vel as f64));
        };
    }
}

struct NoteCell {
    channel: u8,
    pitch: AtomicF64,
    velocity: AtomicF64,
}
/// Main module for Midi Plugin.
pub struct MidiPlugin {
    input: Option<MidiInput>,
    port: OnceCell<MidiInputPort>,
    port_name: Option<String>,
    note_callbacks: Option<NoteCallBacks>,
    connection: Option<MidiInputConnection<NoteCallBacks>>,
    // New fields for macro-based MIDI note handling
    midi_note_cells: Vec<Arc<NoteCell>>,
    midi_note_channels: Vec<u8>,
}

impl MidiPlugin {
    const GET_MIDI_NOTE: &'static str = "__get_midi_note";

    pub fn try_new() -> Option<Self> {
        let input_res = MidiInput::new("mimium midi plugin");
        match input_res {
            Ok(input) => Some(Self {
                input: Some(input),
                port: OnceCell::new(),
                port_name: None,
                note_callbacks: Some(Default::default()),
                connection: None,
                midi_note_cells: Vec::new(),
                midi_note_channels: Vec::new(),
            }),
            Err(_e) => None,
        }
    }
    fn add_note_callback(&mut self, chan: u8, cb: NoteCallBack) {
        match self.note_callbacks.as_mut() {
            Some(v) if chan < 15 => {
                v.0[chan as usize].push(cb);
            }
            _ => {}
        }
    }
    /// This function is exposed to mimium as "set_midi_port(port:string)".
    /// Until this function is called, MIDI plugin tries to the default device.
    pub fn set_midi_port(&mut self, vm: &mut vm::Machine) -> vm::ReturnCode {
        let idx = vm.get_stack(0);
        let pname = vm.prog.strings[idx as usize].clone();

        self.port_name = Some(pname);
        0
    }
    /// Macro function for midi_note_mono! that generates unique IDs and returns runtime code
    /// Arguments: channel:float[0-15], default_note:float[0-127], default_velocity:float[0-127]
    /// Returns: Code that evaluates to a record {pitch:float, velocity:float}
    pub fn midi_note_mono_macro(&mut self, v: &[(Value, TypeNodeId)]) -> Value {
        let zero = Value::Code(
            Expr::Literal(Literal::Float(0.0.to_string().to_symbol())).into_id_without_span(),
        );
        if v.len() != 3 {
            log::error!(
                "midi_note_mono! expects 3 arguments (channel, default_note, default_velocity)"
            );
            return zero;
        }

        let (ch, default_note, default_vel) = match (v[0].0.clone(), v[1].0.clone(), v[2].0.clone())
        {
            (Value::Number(ch), Value::Number(note), Value::Number(vel)) => (ch, note, vel),
            _ => {
                log::error!("midi_note_mono! arguments must be numbers");
                return zero;
            }
        };
        let (uid, cell) = if let Some((uid, cell)) = self
            .midi_note_cells
            .iter()
            .enumerate()
            .find(|(_, c)| c.channel == ch as u8)
        {
            (uid, cell.clone())
        } else {
            let cell = Arc::new(NoteCell {
                channel: ch as u8,
                pitch: AtomicF64::new(default_note),
                velocity: AtomicF64::new(default_vel),
            });
            let uid = self.midi_note_cells.len();
            self.midi_note_cells.push(cell.clone());
            (uid, cell)
        };

        // Register the callback
        let cell_c = cell.clone();
        self.add_note_callback(
            ch as u8,
            Arc::new(move |note, vel| {
                cell_c.pitch.store(note, Ordering::Relaxed);
                cell_c.velocity.store(vel, Ordering::Relaxed);
            }),
        );

        // Generate code that calls __get_midi_note(uid)
        Value::Code(
            Expr::Apply(
                Expr::Var(Self::GET_MIDI_NOTE.to_symbol()).into_id_without_span(),
                vec![
                    Expr::Literal(Literal::Float(uid.to_string().to_symbol()))
                        .into_id_without_span(),
                ],
            )
            .into_id_without_span(),
        )
    }

    /// Runtime function to get MIDI note values by UID.
    /// Arguments: uid:float (index into midi_note_cells)
    /// Returns: (pitch, velocity) tuple
    #[mimium_plugin_fn]
    pub fn get_midi_note(&mut self, uid: f64) -> (f64, f64) {
        let uid = uid as usize;
        match self.midi_note_cells.get(uid) {
            Some(cell) => {
                let pitch = cell.pitch.load(Ordering::Relaxed);
                let velocity = cell.velocity.load(Ordering::Relaxed);
                (pitch, velocity)
            }
            None => {
                log::error!("Invalid MIDI note UID: {uid}");
                (0.0, 0.0)
            }
        }
    }
}

impl Drop for MidiPlugin {
    fn drop(&mut self) {
        if let Some(c) = self.connection.take() {
            c.close();
        }
    }
}

impl SystemPlugin for MidiPlugin {
    fn after_main(&mut self, _machine: &mut vm::Machine) -> vm::ReturnCode {
        if self.connection.is_some() {
            return 0;
        }
        let input = self.input.as_ref().unwrap();
        let ports = input.ports();

        let port_opt = match (&self.port_name, ports.is_empty()) {
            (Some(pname), false) => ports.iter().find(|port| {
                let name = input.port_name(port).unwrap_or_default();
                &name == pname
            }),
            (None, false) => {
                log::info!("trying to connect default MIDI input device...");
                ports.first()
            }
            (_, true) => None,
        };
        if let Some(p) = port_opt {
            let name = input.port_name(p).unwrap_or_default();
            log::info!("Midi Input: Connected to {name}");
            let res = self.input.take().unwrap().connect(
                p,
                &name,
                |_stamp, message, cbs: &mut NoteCallBacks| {
                    let msg = MidiMessage::from_bytes(message);
                    if let Ok(m) = msg {
                        match m {
                            MidiMessage::NoteOff(channel, note, _vel) => {
                                cbs.invoke_note_callback(channel.index(), u8::from(note), 0);
                            }
                            MidiMessage::NoteOn(channel, note, vel) => {
                                cbs.invoke_note_callback(
                                    channel.index(),
                                    u8::from(note),
                                    vel.into(),
                                );
                            }
                            _ => {}
                        }
                    }
                },
                self.note_callbacks.take().unwrap(),
            );
            match res {
                Ok(c) => self.connection = Some(c),
                Err(e) => {
                    log::error!("{e}")
                }
            }
            let _ = self.port.set(p.clone());
        } else {
            log::warn!("No MIDI devices found.")
        }
        0
    }

    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        // set_midi_port function
        let ty = function!(vec![string_t!()], unit!());
        let fun: SystemPluginFnType<Self> = Self::set_midi_port;
        let setport = SysPluginSignature::new("set_midi_port", fun, ty);

        // New midi_note_mono! macro (returns code that evaluates to record)
        let midi_note_macro_f: SystemPluginMacroType<Self> = Self::midi_note_mono_macro;
        let record_ty = Type::Record(vec![
            RecordTypeField::new("pitch".to_symbol(), numeric!(), false),
            RecordTypeField::new("velocity".to_symbol(), numeric!(), false),
        ])
        .into_id();
        let midi_note_macro = SysPluginSignature::new_macro(
            "midi_note_mono",
            midi_note_macro_f,
            function!(
                vec![numeric!(), numeric!(), numeric!()],
                Type::Code(record_ty).into_id()
            ),
        );

        // Runtime function __get_midi_note (returns record)
        let get_midi_note_f: SystemPluginFnType<Self> = Self::get_midi_note;
        let get_midi_note = SysPluginSignature::new(
            Self::GET_MIDI_NOTE,
            get_midi_note_f,
            function!(vec![numeric!()], record_ty),
        );

        vec![setport, midi_note_macro, get_midi_note]
    }
}

// -------------------------------------------------------------------------
// Signature helpers for FFI type info export
// -------------------------------------------------------------------------

impl MidiPlugin {
    /// Returns the signature for the `midi_note_mono!` macro.
    ///
    /// Used when exporting type information to the dynamic plugin loader.
    pub fn midi_note_mono_signature() -> SysPluginSignature {
        let midi_note_macro_f: SystemPluginMacroType<Self> = Self::midi_note_mono_macro;
        let record_ty = Type::Record(vec![
            RecordTypeField::new("pitch".to_symbol(), numeric!(), false),
            RecordTypeField::new("velocity".to_symbol(), numeric!(), false),
        ])
        .into_id();
        SysPluginSignature::new_macro(
            "midi_note_mono",
            midi_note_macro_f,
            function!(
                vec![numeric!(), numeric!(), numeric!()],
                Type::Code(record_ty).into_id()
            ),
        )
    }
}

// -------------------------------------------------------------------------
// Dynamic Plugin ABI
// -------------------------------------------------------------------------

#[cfg(not(target_arch = "wasm32"))]
use std::ffi::{CString, c_char};

#[cfg(not(target_arch = "wasm32"))]
use mimium_lang::plugin::loader::{PluginCapabilities, PluginInstance, PluginMetadata};

#[cfg(not(target_arch = "wasm32"))]
static PLUGIN_NAME: &str = "mimium-midi\0";
#[cfg(not(target_arch = "wasm32"))]
static PLUGIN_VERSION: &str = env!("CARGO_PKG_VERSION");
#[cfg(not(target_arch = "wasm32"))]
static PLUGIN_AUTHOR: &str = "mimium-org\0";

#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub extern "C" fn mimium_plugin_metadata() -> *const PluginMetadata {
    use std::sync::OnceLock;

    static METADATA: OnceLock<(CString, PluginMetadata)> = OnceLock::new();

    let (_version_cstr, metadata) = METADATA.get_or_init(|| {
        let version_cstr = CString::new(PLUGIN_VERSION).expect("Version string is valid");
        let metadata = PluginMetadata {
            name: PLUGIN_NAME.as_ptr() as *const c_char,
            version: version_cstr.as_ptr(),
            author: PLUGIN_AUTHOR.as_ptr() as *const c_char,
            capabilities: PluginCapabilities {
                has_audio_worker: false,
                has_macros: true,
                has_runtime_functions: true,
            },
        };
        (version_cstr, metadata)
    });

    metadata
}

#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub extern "C" fn mimium_plugin_create() -> *mut PluginInstance {
    match MidiPlugin::try_new() {
        Some(plugin) => Box::into_raw(Box::new(plugin)) as *mut PluginInstance,
        None => {
            log::warn!("Midi is not supported on this platform.");
            std::ptr::null_mut()
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub extern "C" fn mimium_plugin_destroy(instance: *mut PluginInstance) {
    if !instance.is_null() {
        // SAFETY: instance was created by mimium_plugin_create
        unsafe {
            let _ = Box::from_raw(instance as *mut MidiPlugin);
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
use mimium_lang::plugin::loader::PluginFunctionFn;

/// Get a plugin function by name.
///
/// # Safety
///
/// - `name` must be a valid pointer to a null-terminated C string
/// - The returned function pointer, if any, must only be called with valid arguments
#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mimium_plugin_get_function(
    name: *const c_char,
) -> Option<PluginFunctionFn> {
    use std::ffi::CStr;

    if name.is_null() {
        return None;
    }

    let name_str = unsafe { CStr::from_ptr(name) }.to_str().ok()?;

    match name_str {
        "set_midi_port" => Some(ffi_set_midi_port),
        "__get_midi_note" => Some(ffi_get_midi_note),
        _ => None,
    }
}

#[cfg(not(target_arch = "wasm32"))]
use mimium_lang::plugin::loader::PluginMacroFn;

/// Get a plugin macro function by name.
///
/// # Safety
///
/// - `name` must be a valid pointer to a null-terminated C string
/// - The returned function pointer, if any, must only be called with valid arguments
#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mimium_plugin_get_macro(name: *const c_char) -> Option<PluginMacroFn> {
    use std::ffi::CStr;

    if name.is_null() {
        return None;
    }

    let name_str = unsafe { CStr::from_ptr(name) }.to_str().ok()?;

    match name_str {
        "midi_note_mono_macro" => Some(ffi_midi_note_mono_macro),
        _ => None,
    }
}

/// FFI wrapper for `set_midi_port`.
#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" fn ffi_set_midi_port(
    instance: *mut PluginInstance,
    runtime: *mut std::ffi::c_void,
) -> i64 {
    use mimium_lang::runtime::vm::Machine;

    if instance.is_null() || runtime.is_null() {
        return 0;
    }

    let machine = unsafe { &mut *(runtime as *mut Machine) };
    let plugin = unsafe { &mut *(instance as *mut MidiPlugin) };

    plugin.set_midi_port(machine) as i64
}

/// FFI wrapper for `__get_midi_note`.
#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" fn ffi_get_midi_note(
    instance: *mut PluginInstance,
    runtime: *mut std::ffi::c_void,
) -> i64 {
    use mimium_lang::runtime::vm::Machine;

    if instance.is_null() || runtime.is_null() {
        return 0;
    }

    let machine = unsafe { &mut *(runtime as *mut Machine) };
    let plugin = unsafe { &mut *(instance as *mut MidiPlugin) };

    plugin.get_midi_note(machine)
}

/// FFI bridge for `midi_note_mono!` macro function.
///
/// # Safety
///
/// - `instance` must be a valid pointer to a MidiPlugin instance
/// - `args_ptr` and `args_len` must describe a valid byte buffer containing serialized arguments
/// - `out_ptr` and `out_len` must be valid pointers to write the result
#[cfg(not(target_arch = "wasm32"))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ffi_midi_note_mono_macro(
    instance: *mut std::ffi::c_void,
    args_ptr: *const u8,
    args_len: usize,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    use mimium_lang::runtime::ffi_serde::{deserialize_macro_args, serialize_value};

    if instance.is_null() || args_ptr.is_null() || out_ptr.is_null() || out_len.is_null() {
        log::error!("ffi_midi_note_mono_macro: Null pointer detected");
        return -3;
    }

    unsafe {
        let plugin = &mut *(instance as *mut MidiPlugin);

        let args_bytes = std::slice::from_raw_parts(args_ptr, args_len);
        let args = match deserialize_macro_args(args_bytes) {
            Ok(a) => a,
            Err(e) => {
                log::error!(
                    "Failed to deserialize macro arguments for midi_note_mono: {e}"
                );
                return -1;
            }
        };

        let result = plugin.midi_note_mono_macro(&args);

        let result_bytes = match serialize_value(&result) {
            Ok(b) => b,
            Err(e) => {
                log::error!("Failed to serialize macro result for midi_note_mono: {e}");
                return -2;
            }
        };

        let boxed = result_bytes.into_boxed_slice();
        *out_len = boxed.len();
        *out_ptr = Box::into_raw(boxed) as *mut u8;

        0
    }
}

// -------------------------------------------------------------------------
// Type information export
// -------------------------------------------------------------------------

#[cfg(not(target_arch = "wasm32"))]
mod ffi_exports {
    use super::*;

    /// Static storage for type information strings (must outlive the plugin).
    static mut TYPE_INFO_STORAGE: Option<Vec<(std::ffi::CString, Vec<u8>)>> = None;

    /// Static storage for FfiTypeInfo array (must outlive the plugin).
    static mut FFI_TYPE_INFOS: Option<Vec<mimium_lang::plugin::loader::FfiTypeInfo>> = None;

    /// Export type information for the plugin.
    ///
    /// Returns an array of FfiTypeInfo structures that describe the types of
    /// all macros and functions provided by the plugin.
    #[unsafe(no_mangle)]
    pub extern "C" fn mimium_plugin_get_type_infos(
        out_len: *mut usize,
    ) -> *const mimium_lang::plugin::loader::FfiTypeInfo {
        use mimium_lang::plugin::loader::FfiTypeInfo;
        if out_len.is_null() {
            return std::ptr::null();
        }

        unsafe {
            if (*std::ptr::addr_of!(TYPE_INFO_STORAGE)).is_none() {
                let mut storage = Vec::new();
                let mut infos = Vec::new();

                // midi_note_mono! macro type info
                let sig = MidiPlugin::midi_note_mono_signature();
                let name_cstr = match std::ffi::CString::new(sig.get_name()) {
                    Ok(s) => s,
                    Err(_) => return std::ptr::null(),
                };
                let type_bytes = match bincode::serialize(&sig.get_type()) {
                    Ok(b) => b,
                    Err(_) => return std::ptr::null(),
                };

                let name_ptr = name_cstr.as_ptr();
                let type_ptr = type_bytes.as_ptr();
                let type_len = type_bytes.len();

                storage.push((name_cstr, type_bytes));
                infos.push(FfiTypeInfo {
                    name: name_ptr,
                    type_data: type_ptr,
                    type_len,
                    stage: 0, // Stage(0) = Macro/CompileTime
                });

                TYPE_INFO_STORAGE = Some(storage);
                FFI_TYPE_INFOS = Some(infos);
            }

            let infos = (*std::ptr::addr_of!(FFI_TYPE_INFOS)).as_ref().unwrap();
            *out_len = infos.len();
            infos.as_ptr()
        }
    }
}
