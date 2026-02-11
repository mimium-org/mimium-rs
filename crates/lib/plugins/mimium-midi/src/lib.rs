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
use mimium_plugin_macros::{mimium_export_plugin, mimium_plugin_fn};
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
    pub const GET_MIDI_NOTE: &'static str = "__get_midi_note";

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
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

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
// Dynamic Plugin ABI (generated by mimium_export_plugin!)
// -------------------------------------------------------------------------

mimium_export_plugin! {
    plugin_type: MidiPlugin,
    plugin_name: "mimium-midi",
    plugin_author: "mimium-org",
    constructor: "try_new",
    capabilities: {
        has_audio_worker: false,
        has_macros: true,
        has_runtime_functions: true,
    },
    runtime_functions: [
        ("set_midi_port", set_midi_port),
        ("__get_midi_note", get_midi_note),
    ],
    macro_functions: [
        ("midi_note_mono", midi_note_mono_macro),
    ],
    type_infos: [
        { name: "midi_note_mono", sig: MidiPlugin::midi_note_mono_signature(), stage: 0 },
        { name: "set_midi_port", ty_expr: function!(vec![string_t!()], unit!()), stage: 1 },
        {
            name: "__get_midi_note",
            ty_expr: function!(
                vec![numeric!()],
                Type::Record(vec![
                    RecordTypeField::new("pitch".to_symbol(), numeric!(), false),
                    RecordTypeField::new("velocity".to_symbol(), numeric!(), false),
                ]).into_id()
            ),
            stage: 1
        },
    ],
}
