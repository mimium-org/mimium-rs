use std::collections::HashMap;
use std::sync::Arc;

use mimium_lang::ast::{Expr, Literal};
use mimium_lang::interner::{ToSymbol, TypeNodeId};
use mimium_lang::interpreter::Value;
use mimium_lang::numeric;
use mimium_lang::pattern::TypedId;
use mimium_lang::plugin::{
    SysPluginSignature, SystemPlugin, SystemPluginFnType, SystemPluginMacroType,
};
use mimium_lang::string_t;
use mimium_lang::types::{PType, Type};
use mimium_lang::{function, log};
use mimium_plugin_macros::{mimium_export_plugin, mimium_plugin_fn, mimium_plugin_macro};
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
    let ext = std::path::PathBuf::from(path);

    let mss_opts = MediaSourceStreamOptions::default();
    let mss = MediaSourceStream::new(ms, mss_opts);

    let hint = ext.extension().map_or_else(Hint::new, |ext| {
        ext.to_str().map_or_else(Hint::new, |s| {
            let mut hint = Hint::new();
            hint.with_extension(s);
            hint
        })
    });

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

#[derive(Default)]
pub struct SamplerPlugin {
    sample_cache: HashMap<String, Arc<Vec<f64>>>,
    sample_namemap: HashMap<String, usize>,
}

impl SamplerPlugin {
    pub const GET_SAMPLER: &'static str = "__get_sampler";

    /// Returns a fallback lambda that ignores input and always returns 0.0 (silence)
    fn error_fallback() -> Value {
        Value::Code(
            Expr::Lambda(
                vec![TypedId::new(
                    "_".to_symbol(),
                    Type::Primitive(PType::Numeric).into_id(),
                )],
                None,
                Expr::Literal(Literal::Float("0.0".to_symbol())).into_id_without_span(),
            )
            .into_id_without_span(),
        )
    }

    /// Returns the signature for the Sampler_mono! macro.
    ///
    /// This is used when exporting type information to the dynamic plugin loader.
    pub fn sampler_mono_signature() -> SysPluginSignature {
        let sampler_macrof: SystemPluginMacroType<Self> = Self::make_sampler_mono;
        SysPluginSignature::new_macro(
            "Sampler_mono",
            sampler_macrof,
            function!(
                vec![string_t!()],
                Type::Code(function!(vec![numeric!()], numeric!())).into_id()
            ),
        )
    }

    #[mimium_plugin_macro]
    pub fn make_sampler_mono(&mut self, v: &[(Value, TypeNodeId)]) -> Value {
        assert_eq!(v.len(), 1);
        let rel_path_str = match &v[0].0 {
            Value::String(s) => s.to_string(),
            _ => {
                unreachable!("Sampler_mono! expects a string argument, guaranteed by type checker")
            }
        };

        // Resolve the path (absolute or relative to CWD)
        log::debug!("Attempting to canonicalize path: {rel_path_str}");
        let abs_path = match std::fs::canonicalize(&rel_path_str) {
            Ok(p) => {
                let path_str = p.to_string_lossy().to_string();
                log::debug!("Canonicalized path: {path_str}");
                path_str
            }
            Err(e) => {
                log::error!("Failed to resolve audio file path '{rel_path_str}': {e}");
                return Self::error_fallback();
            }
        };

        // Get the sampler index, loading the file if necessary
        let idx = if let Some(&idx) = self.sample_namemap.get(&abs_path) {
            idx
        } else {
            // Load the audio file at compile time
            let vec = match load_wavfile_to_vec(&abs_path) {
                Ok(v) => Arc::new(v),
                Err(e) => {
                    mimium_lang::log::error!("Failed to load audio file '{abs_path}': {e}");
                    return Self::error_fallback();
                }
            };

            let idx = self.sample_cache.len();
            self.sample_cache.insert(abs_path.clone(), vec);
            self.sample_namemap.insert(abs_path, idx);
            idx
        };

        // Generate code that creates a closure for sampling
        Value::Code(
            Expr::Lambda(
                vec![TypedId::new(
                    "pos".to_symbol(),
                    Type::Primitive(PType::Numeric).into_id(),
                )],
                None,
                Expr::Apply(
                    Expr::Var(Self::GET_SAMPLER.to_symbol()).into_id_without_span(),
                    vec![
                        Expr::Var("pos".to_symbol()).into_id_without_span(),
                        Expr::Literal(Literal::Float(idx.to_string().to_symbol()))
                            .into_id_without_span(),
                    ],
                )
                .into_id_without_span(),
            )
            .into_id_without_span(),
        )
    }

    #[mimium_plugin_fn]
    pub fn get_sampler(&mut self, pos: f64, sample_idx: f64) -> f64 {
        let idx = sample_idx as usize;
        let samples = self.sample_cache.values().nth(idx);
        match samples {
            Some(vec) => interpolate_vec(vec, pos),
            None => {
                mimium_lang::log::error!("Invalid sample index: {idx}");
                0.0
            }
        }
    }
}

impl SystemPlugin for SamplerPlugin {
    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        let sampler_macrof: SystemPluginMacroType<Self> = Self::make_sampler_mono;
        let sampler_macro = SysPluginSignature::new_macro(
            "Sampler_mono",
            sampler_macrof,
            function!(
                vec![string_t!()],
                Type::Code(function!(vec![numeric!()], numeric!())).into_id()
            ),
        );

        let get_samplerf: SystemPluginFnType<Self> = Self::get_sampler;
        let get_sampler = SysPluginSignature::new(
            Self::GET_SAMPLER,
            get_samplerf,
            function!(vec![numeric!(), numeric!()], numeric!()),
        );

        vec![sampler_macro, get_sampler]
    }
}

// -------------------------------------------------------------------------
// Dynamic Plugin ABI (generated by mimium_export_plugin!)
// -------------------------------------------------------------------------

mimium_export_plugin! {
    plugin_type: SamplerPlugin,
    plugin_name: "mimium-symphonia",
    plugin_author: "mimium-org",
    capabilities: {
        has_audio_worker: false,
        has_macros: true,
        has_runtime_functions: true,
    },
    runtime_functions: [
        ("__get_sampler", get_sampler),
    ],
    macro_functions: [
        ("Sampler_mono", make_sampler_mono),
    ],
    type_infos: [
        { name: "Sampler_mono", sig: SamplerPlugin::sampler_mono_signature(), stage: 0 },
        { name: "__get_sampler", ty_expr: function!(vec![numeric!(), numeric!()], numeric!()), stage: 1 },
    ],
}
