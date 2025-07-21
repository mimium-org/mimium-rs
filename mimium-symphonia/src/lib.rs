use std::cell::RefCell;
use std::rc::Rc;

use mimium_lang::interner::ToSymbol;
use mimium_lang::plugin::{ExtClsInfo, ExtFunInfo, ExtFunType, Plugin};
use mimium_lang::runtime::vm::{self, Machine, ReturnCode};
use mimium_lang::types::{PType, Type};
use mimium_lang::utils::fileloader;
use mimium_lang::{function, numeric, string_t};
use symphonia::core::audio::{Layout, SampleBuffer, SignalSpec};
use symphonia::core::codecs::{CODEC_TYPE_NULL, CodecParameters, Decoder, DecoderOptions};
use symphonia::core::errors::Error as SymphoniaError;
use symphonia::core::formats::{FormatOptions, SeekMode, SeekTo};
use symphonia::core::io::{MediaSource, MediaSourceStream, MediaSourceStreamOptions};
use symphonia::core::meta::MetadataOptions;
use symphonia::core::probe::{Hint, ProbeResult};
use symphonia::core::units::Time;
type DecoderSet = (Box<dyn Decoder>, ProbeResult, u32);
mod filemanager;
use filemanager::FileManager;

fn get_default_decoder(path: &str) -> Result<DecoderSet, Box<dyn std::error::Error>> {
    let flmgr = filemanager::get_global_file_manager();
    let src = flmgr.open_file_stream(path).expect("failed to open file");
    let ms: Box<dyn MediaSource> = Box::new(src);
    let mss_opts = MediaSourceStreamOptions::default();
    let mss = MediaSourceStream::new(ms, mss_opts);
    let hint = Hint::new();
    //  hint.with_extension("mp3");
    // Use the default options for metadata and format readers.
    let meta_opts: MetadataOptions = Default::default();
    let fmt_opts: FormatOptions = Default::default();

    // Probe the media source.
    let probed = symphonia::default::get_probe().format(&hint, mss, &fmt_opts, &meta_opts)?;

    let format = probed.format.as_ref();
    let track = format
        .tracks()
        .iter()
        .find(|t| t.codec_params.codec != CODEC_TYPE_NULL)
        .ok_or(SymphoniaError::Unsupported("no supported audio tracks"))?;
    let id = track.id;
    // Use the default options for the decoder.
    let dec_opts: DecoderOptions = Default::default();
    // Create a decoder for the track.
    let decoder = symphonia::default::get_codecs().make(&track.codec_params, &dec_opts)?;

    Ok((decoder, probed, id))
}

fn load_wavfile_to_vec(path: &str) -> Result<Vec<f64>, SymphoniaError> {
    let (mut decoder, mut probed, id) = get_default_decoder(path).expect("failed to find file");
    let max_frames = decoder.codec_params().max_frames_per_packet.unwrap();
    let _ = probed.format.seek(
        SeekMode::Accurate,
        SeekTo::Time {
            time: Time::from_ss(0, 0).unwrap(),
            track_id: Some(id),
        },
    );
    let CodecParameters {
        channels,
        sample_rate,
        channel_layout,
        ..
    } = probed.format.default_track().unwrap().codec_params;
    let mut res = Vec::<f64>::new();
    let mut buf = SampleBuffer::<f64>::new(
        max_frames,
        SignalSpec::new_with_layout(
            sample_rate.unwrap_or(48000),
            channel_layout.unwrap_or(Layout::Mono),
        ),
    );
    if channels.is_some_and(|c| c.count() != 1) {
        panic!("gen_sampler_mono only supports mono format.")
    }
    loop {
        match probed.format.next_packet() {
            Ok(packet) => {
                // Decode the packet into audio samples.
                let _ = decoder.decode(&packet).map(|decoded| {
                    buf.copy_interleaved_ref(decoded.clone());
                    res.extend_from_slice(buf.samples());
                });
            }
            Err(e) => match e {
                SymphoniaError::IoError(ioerror)
                    if ioerror.kind() == std::io::ErrorKind::UnexpectedEof =>
                {
                    break; //file reached to the end of stream.
                }
                SymphoniaError::DecodeError(_) => {
                    //contains broken packet but recoverable.
                    continue;
                }
                _ => return Err(e),
            },
        }
    }
    Ok(res)
}

/// helper function that does bilinear interpolation
fn interpolate_vec(vec: &[f64], pos: f64) -> f64 {
    let bound = vec.len();
    let pos_u = pos.floor() as usize;
    //todo: efficient boundary check
    match pos_u {
        _ if pos >= 0.0 && (pos_u + 1) < bound => {
            let frac = pos.fract();
            let frac_rem = 1.0 - frac;
            vec[pos_u] * frac_rem + vec[pos_u + 1] * frac
        }
        _ if pos_u + 1 == bound => vec[pos_u],
        _ => 0.0,
    }
}

fn gen_sampler_mono(machine: &mut Machine) -> ReturnCode {
    //return higher order closure

    let relpath = machine.prog.strings[vm::Machine::get_as::<usize>(machine.get_stack(0))];

    let mmmfilepath = machine
        .prog
        .file_path
        .map_or_else(|| "".to_string(), |s| s.to_string());
    let abspath = fileloader::get_canonical_path(&mmmfilepath, relpath.as_str())
        .inspect_err(|e| {
            panic!("{}", e);
        })
        .unwrap();

    let vec = load_wavfile_to_vec(abspath.to_str().unwrap())
        .inspect_err(|e| {
            panic!("gen_sampler_mono error: {}", e);
        })
        .unwrap(); //the generated vector is moved into the closure

    let res = move |machine: &mut Machine| -> ReturnCode {
        let pos = vm::Machine::get_as::<f64>(machine.get_stack(0));
        // this sampler read with boundary checks.
        let val = interpolate_vec(&vec, pos);
        machine.set_stack(0, Machine::to_value(val));
        1
    };
    let ty = function!(vec![numeric!()], numeric!());
    let idx = machine.wrap_extern_cls(ExtClsInfo::new(
        "sampler_mono".to_symbol(),
        ty,
        Rc::new(RefCell::new(res)),
    ));
    machine.set_stack(0, Machine::to_value(idx));
    1
}

pub struct SamplerPlugin;

impl Plugin for SamplerPlugin {
    fn get_ext_functions(&self) -> Vec<ExtFunInfo> {
        let t = function!(vec![string_t!()], function!(vec![numeric!()], numeric!()));
        let sig = ExtFunInfo::new(
            "gen_sampler_mono".to_symbol(),
            t,
            gen_sampler_mono as ExtFunType,
        );
        vec![sig]
    }

    fn get_ext_closures(&self) -> Vec<ExtClsInfo> {
        vec![]
    }
}
