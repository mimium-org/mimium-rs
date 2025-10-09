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
    plugin::{ExtClsInfo, SysPluginSignature, SystemPlugin, SystemPluginFnType, SystemPluginMacroType},
    runtime::vm,
    string_t, tuple,
    types::{PType, RecordTypeField, Type},
    unit,
};
use std::{
    cell::{OnceCell, RefCell},
    rc::Rc,
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

/// Main module for Midi Plugin.
pub struct MidiPlugin {
    input: Option<MidiInput>,
    port: OnceCell<MidiInputPort>,
    port_name: Option<String>,
    note_callbacks: Option<NoteCallBacks>,
    connection: Option<MidiInputConnection<NoteCallBacks>>,
    // New fields for macro-based MIDI note handling
    midi_note_cells: Vec<Arc<(AtomicF64, AtomicF64)>>,
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
    /// This function is exposed to mimium as "bind_midi_note_mono".
    /// Arguments: channel:float[0-15], default_note:float[0-127], default:velocity[0-127]
    /// Return value: Closure(()->(float,float))
    /// If none of the midi device are connected, the returned closure just returns default value continuously.
    pub fn bind_midi_note_mono(&mut self, vm: &mut vm::Machine) -> vm::ReturnCode {
        let ch = vm::Machine::get_as::<f64>(vm.get_stack(0));
        let default_note = vm::Machine::get_as::<f64>(vm.get_stack(1));
        let default_vel = vm::Machine::get_as::<f64>(vm.get_stack(2));

        let cell = Arc::new((AtomicF64::new(default_note), AtomicF64::new(default_vel)));
        let cell_c = cell.clone();
        self.add_note_callback(
            ch as u8,
            Arc::new(move |note, vel| {
                cell_c.0.store(note, Ordering::Relaxed);
                cell_c.1.store(vel, Ordering::Relaxed);
            }),
        );
        let cls = move |vm: &mut vm::Machine| -> vm::ReturnCode {
            let note = cell.0.load(Ordering::Relaxed);
            let vel = cell.1.load(Ordering::Relaxed);
            vm.set_stack(0, vm::Machine::to_value(note));
            vm.set_stack(1, vm::Machine::to_value(vel));
            2
        };
        let ty = function!(vec![], tuple!(numeric!(), numeric!()));
        let rcls = vm.wrap_extern_cls(ExtClsInfo::new(
            "get_midi_val".to_symbol(),
            ty,
            Rc::new(RefCell::new(cls)),
        ));
        vm.set_stack(0, vm::Machine::to_value(rcls));
        1
    }

    /// Macro function for midi_note_mono! that generates unique IDs and returns runtime code
    /// Arguments: channel:float[0-15], default_note:float[0-127], default_velocity:float[0-127]
    /// Returns: Code that evaluates to a record {pitch:float, velocity:float}
    pub fn midi_note_mono_macro(&mut self, v: &[(Value, TypeNodeId)]) -> Value {
        if v.len() != 3 {
            log::error!("midi_note_mono! expects 3 arguments (channel, default_note, default_velocity)");
            return Value::Number(0.0);
        }

        let (ch, default_note, default_vel) = match (v[0].0.clone(), v[1].0.clone(), v[2].0.clone()) {
            (Value::Number(ch), Value::Number(note), Value::Number(vel)) => (ch, note, vel),
            _ => {
                log::error!("midi_note_mono! arguments must be numbers");
                return Value::Number(0.0);
            }
        };

        // Create a new cell for this MIDI note instance
        let cell = Arc::new((AtomicF64::new(default_note), AtomicF64::new(default_vel)));
        let uid = self.midi_note_cells.len();
        self.midi_note_cells.push(cell.clone());
        self.midi_note_channels.push(ch as u8);

        // Register the callback
        let cell_c = cell.clone();
        self.add_note_callback(
            ch as u8,
            Arc::new(move |note, vel| {
                cell_c.0.store(note, Ordering::Relaxed);
                cell_c.1.store(vel, Ordering::Relaxed);
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

    /// Runtime function to get MIDI note values by UID
    /// Arguments: uid:float (index into midi_note_cells)
    /// Returns: record {pitch:float, velocity:float}
    pub fn get_midi_note(&mut self, vm: &mut vm::Machine) -> vm::ReturnCode {
        let uid = vm::Machine::get_as::<f64>(vm.get_stack(0)) as usize;

        match self.midi_note_cells.get(uid) {
            Some(cell) => {
                let pitch = cell.0.load(Ordering::Relaxed);
                let velocity = cell.1.load(Ordering::Relaxed);
                // Return as a tuple for now (records are represented as tuples in the VM)
                vm.set_stack(0, vm::Machine::to_value(pitch));
                vm.set_stack(1, vm::Machine::to_value(velocity));
                2
            }
            None => {
                log::error!("Invalid MIDI note UID: {}", uid);
                vm.set_stack(0, vm::Machine::to_value(0.0));
                vm.set_stack(1, vm::Machine::to_value(0.0));
                2
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
                    log::error!("{}", e)
                }
            }
            let _ = self.port.set(p.clone());
        } else {
            log::warn!("No MIDI devices found.")
        }
        0
    }

    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        // Legacy bind_midi_note_mono (returns closure that returns tuple)
        let ty = function!(
            vec![numeric!(), numeric!(), numeric!()],
            function!(vec![], tuple!(numeric!(), numeric!()))
        );
        let fun: SystemPluginFnType<Self> = Self::bind_midi_note_mono;
        let bindnote = SysPluginSignature::new("bind_midi_note_mono", fun, ty);
        
        // set_midi_port function
        let ty = function!(vec![string_t!()], unit!());
        let fun: SystemPluginFnType<Self> = Self::set_midi_port;
        let setport = SysPluginSignature::new("set_midi_port", fun, ty);

        // New midi_note_mono! macro (returns code that evaluates to record)
        let midi_note_macro_f: SystemPluginMacroType<Self> = Self::midi_note_mono_macro;
        let record_ty = Type::Record(vec![
            RecordTypeField::new("pitch".to_symbol(), numeric!(), false),
            RecordTypeField::new("velocity".to_symbol(), numeric!(), false),
        ]).into_id();
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

        vec![setport, bindnote, midi_note_macro, get_midi_note]
    }
}
