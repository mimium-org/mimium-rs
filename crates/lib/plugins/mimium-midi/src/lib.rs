//! ## mimium MIDI Plugin
//!
//! MIDI plugin currently implements a functionality for binding midi note signal to a tuple of float value.
//! Processing for raw MIDI events like midi plugin in VST cannot be realized for now.

use atomic_float::AtomicF64;
use midir::{MidiInput, MidiInputConnection, MidiInputPort};
use mimium_lang::{
    function,
    interner::ToSymbol,
    log, numeric,
    plugin::{ExtClsInfo, SysPluginSignature, SystemPlugin, SystemPluginFnType},
    runtime::vm,
    string_t, tuple,
    types::{PType, Type},
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
}

impl MidiPlugin {
    pub fn try_new() -> Option<Self> {
        let input_res = MidiInput::new("mimium midi plugin");
        match input_res {
            Ok(input) => Some(Self {
                input: Some(input),
                port: OnceCell::new(),
                port_name: None,
                note_callbacks: Some(Default::default()),
                connection: None,
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
        let ty = function!(
            vec![numeric!(), numeric!(), numeric!()],
            function!(vec![], tuple!(numeric!(), numeric!()))
        );
        let fun: SystemPluginFnType<Self> = Self::bind_midi_note_mono;
        let bindnote = SysPluginSignature::new("bind_midi_note_mono", fun, ty);
        let ty = function!(vec![string_t!()], unit!());
        let fun: SystemPluginFnType<Self> = Self::set_midi_port;
        let setport = SysPluginSignature::new("set_midi_port", fun, ty);
        vec![setport, bindnote]
    }
}
