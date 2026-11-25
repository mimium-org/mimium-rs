use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, mpsc};

use crate::driver::{Driver, RuntimeData, SampleRate};
use crate::runtime_fn;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{self, BufferSize};

use mimium_lang::compiler::IoChannelInfo;
use mimium_lang::log;
use mimium_lang::plugin::ExtClsInfo;

use mimium_lang::runtime::{ProgramPayload, Time};
use ringbuf::traits::{Consumer, Producer, Split};
use ringbuf::{HeapCons, HeapProd, HeapRb};
pub(crate) const DEFAULT_BUFFER_SIZE: usize = 2048;

fn select_stream_config(
    configs: impl Iterator<Item = cpal::SupportedStreamConfigRange>,
    dsp_channels: u16,
    sample_rate: cpal::SampleRate,
) -> Option<cpal::SupportedStreamConfig> {
    let desired_channels = dsp_channels.max(2);
    configs
        .filter(|config| config.sample_format() == cpal::SampleFormat::F32)
        .map(|config| {
            let rate = sample_rate
                .0
                .clamp(config.min_sample_rate().0, config.max_sample_rate().0);
            let channels = config.channels();
            let priority = (
                rate.abs_diff(sample_rate.0),
                channels.abs_diff(desired_channels),
                channels < desired_channels,
            );
            (priority, config.with_sample_rate(cpal::SampleRate(rate)))
        })
        .min_by_key(|(priority, _)| *priority)
        .map(|(_, config)| config)
}

fn select_compatible_configs(
    inputs: &[cpal::SupportedStreamConfigRange],
    outputs: &[cpal::SupportedStreamConfigRange],
    input_channels: u16,
    output_channels: u16,
    sample_rate: cpal::SampleRate,
) -> Option<(cpal::SupportedStreamConfig, cpal::SupportedStreamConfig)> {
    let desired_input = input_channels.max(2);
    let desired_output = output_channels.max(2);
    inputs
        .iter()
        .filter(|config| config.sample_format() == cpal::SampleFormat::F32)
        .flat_map(|input| {
            outputs
                .iter()
                .filter(|config| config.sample_format() == cpal::SampleFormat::F32)
                .filter_map(move |output| {
                    let min_rate = input.min_sample_rate().0.max(output.min_sample_rate().0);
                    let max_rate = input.max_sample_rate().0.min(output.max_sample_rate().0);
                    if min_rate > max_rate {
                        return None;
                    }
                    let rate = sample_rate.0.clamp(min_rate, max_rate);
                    let input_count = input.channels();
                    let output_count = output.channels();
                    let priority = (
                        rate.abs_diff(sample_rate.0),
                        u32::from(input_count.abs_diff(desired_input))
                            + u32::from(output_count.abs_diff(desired_output)),
                        u8::from(input_count < desired_input)
                            + u8::from(output_count < desired_output),
                    );
                    Some((
                        priority,
                        (*input).with_sample_rate(cpal::SampleRate(rate)),
                        (*output).with_sample_rate(cpal::SampleRate(rate)),
                    ))
                })
        })
        .min_by_key(|(priority, _, _)| *priority)
        .map(|(_, input, output)| (input, output))
}

fn stream_channels(hardware_channels: u16, dsp_channels: u16) -> u16 {
    dsp_channels.max(2).min(hardware_channels)
}

fn stream_buffer_size(
    requested: usize,
    ranges: impl Iterator<Item = cpal::SupportedBufferSize>,
) -> Option<usize> {
    let (min, max) = ranges.fold(
        (1usize, u32::MAX as usize),
        |(min, max), range| match range {
            cpal::SupportedBufferSize::Range {
                min: device_min,
                max: device_max,
            } => (min.max(device_min as usize), max.min(device_max as usize)),
            cpal::SupportedBufferSize::Unknown => (min, max),
        },
    );
    if min <= max {
        Some(requested.clamp(min, max))
    } else {
        None
    }
}

pub struct NativeDriver {
    sr: SampleRate,
    hardware_ichannels: usize,
    hardware_ochannels: usize,
    is_playing: bool,
    istream: Option<cpal::Stream>,
    ostream: Option<cpal::Stream>,
    count: Arc<AtomicU64>,
    buffer_size: usize,
    input_device_name: Option<String>,
    output_device_name: Option<String>,
    swap_prod: Option<mpsc::Sender<ProgramPayload>>,
}
impl NativeDriver {
    pub fn new(
        buffer_size: usize,
        input_device_name: Option<String>,
        output_device_name: Option<String>,
    ) -> Self {
        Self {
            sr: SampleRate::from(48000),
            hardware_ichannels: 1,
            hardware_ochannels: 1,
            is_playing: false,
            istream: None,
            ostream: None,
            count: Default::default(),
            buffer_size,
            input_device_name,
            output_device_name,
            swap_prod: None,
        }
    }
}

impl Default for NativeDriver {
    fn default() -> Self {
        Self::new(DEFAULT_BUFFER_SIZE, None, None)
    }
}

pub fn native_driver(
    buffer_size: usize,
    input_device_name: Option<String>,
    output_device_name: Option<String>,
) -> Box<dyn Driver<Sample = f64>> {
    Box::new(NativeDriver::new(
        buffer_size,
        input_device_name,
        output_device_name,
    ))
}

//Runtime data, which will be created and immidiately send to audio thread.
struct NativeAudioData {
    pub vmdata: RuntimeData,
    iochannels: IoChannelInfo,
    buffer: HeapCons<f64>,
    localbuffer: Vec<f64>,
    count: Arc<AtomicU64>,
    pub swap_channel: mpsc::Receiver<ProgramPayload>,
}
unsafe impl Send for NativeAudioData {}

impl NativeAudioData {
    pub fn new(
        buffer: HeapCons<f64>,
        runtime_data: RuntimeData,
        count: Arc<AtomicU64>,
        buffer_size: usize,
        swap_cons: mpsc::Receiver<ProgramPayload>,
    ) -> Self {
        let vmdata = runtime_data;
        let iochannels = vmdata.io_channels().unwrap_or(IoChannelInfo {
            input: 0,
            output: 0,
        });
        let input_len = buffer_size * iochannels.input as usize;
        let localbuffer: Vec<f64> = vec![0.0f64; input_len];
        Self {
            vmdata,
            iochannels,
            buffer,
            localbuffer,
            count,
            swap_channel: swap_cons,
        }
    }
    pub fn process(&mut self, dst: &mut [f32], h_ochannels: usize) {
        if let Ok(swap) = self.swap_channel.try_recv() {
            self.vmdata.resume_with_program(swap);
        }

        let input_channels = self.iochannels.input as usize;
        let frames_per_chunk = if input_channels > 0 {
            (self.localbuffer.len() / input_channels).max(1)
        } else {
            (dst.len() / h_ochannels).max(1)
        };
        for chunk in dst.chunks_mut(frames_per_chunk * h_ochannels) {
            self.process_chunk(chunk, h_ochannels);
        }
    }

    fn process_chunk(&mut self, dst: &mut [f32], h_ochannels: usize) {
        let frame_count = dst.len() / h_ochannels;
        let input_channels = self.iochannels.input as usize;
        let required_input_len = frame_count * input_channels;

        if required_input_len > 0 {
            let local = &mut self.localbuffer.as_mut_slice()[..required_input_len];
            local.fill(0.0);
            let popped = self.buffer.pop_slice(local);
            local[popped..].fill(0.0);
        }

        for (frame_index, o) in dst.chunks_mut(h_ochannels).enumerate() {
            let input = if input_channels > 0 {
                let start = frame_index * input_channels;
                let end = start + input_channels;
                &self.localbuffer[start..end]
            } else {
                &[]
            };
            self.vmdata.set_input(input);
            let _rc = self
                .vmdata
                .run_dsp(Time(self.count.load(Ordering::Relaxed)));
            let res = self.vmdata.get_output(self.iochannels.output as usize);
            self.count.fetch_add(1, Ordering::Relaxed);
            o.fill(0.0);
            match (h_ochannels, self.iochannels.output as usize) {
                (2, 1) => {
                    o[0] = res[0] as f32;
                    o[1] = res[0] as f32;
                }
                (hout, dspout) if hout >= dspout => {
                    for i in 0..dspout {
                        o[i] = res[i] as f32;
                    }
                }
                (hout, _) => {
                    for i in 0..hout {
                        //truncate output up to hardware channels
                        o[i] = res[i] as f32;
                    }
                }
            }
        }
    }
}
struct NativeAudioReceiver {
    dsp_ichannels: usize,
    localbuffer: Vec<f64>,
    buffer: HeapProd<f64>,
    count: u64,
}
unsafe impl Send for NativeAudioReceiver {}
impl NativeAudioReceiver {
    pub fn new(dsp_ichannels: usize, buffer_size: usize, buffer: HeapProd<f64>) -> Self {
        Self {
            dsp_ichannels,
            localbuffer: vec![0f64; buffer_size * dsp_ichannels],
            buffer,
            count: 0,
        }
    }
    pub fn receive_data(&mut self, data: &[f32], h_ichannels: usize) {
        if self.dsp_ichannels == 0 {
            return;
        }
        let frames_per_chunk = (self.localbuffer.len() / self.dsp_ichannels).max(1);
        for chunk in data.chunks(frames_per_chunk * h_ichannels) {
            self.receive_chunk(chunk, h_ichannels);
        }
    }

    fn receive_chunk(&mut self, data: &[f32], h_ichannels: usize) {
        let frame_count = data.len() / h_ichannels;
        let required_size = frame_count * self.dsp_ichannels;
        debug_assert!(required_size <= self.localbuffer.len());

        if self.dsp_ichannels > 0 {
            for (input_channel, dsp_channel) in data
                .chunks(h_ichannels)
                .zip(self.localbuffer.chunks_mut(self.dsp_ichannels))
            {
                match (h_ichannels, self.dsp_ichannels) {
                    (i1, i2) if i1 == i2 => {
                        dsp_channel
                            .iter_mut()
                            .zip(input_channel.iter().copied())
                            .for_each(|(d, s)| *d = s as f64);
                    }
                    (2, 1) => {
                        dsp_channel[0] = input_channel[0] as f64; //copy lch
                    }
                    (1, 2) => {
                        dsp_channel[0] = input_channel[0] as f64;
                        dsp_channel[1] = input_channel[0] as f64;
                    }
                    (_, _) => {
                        let num_channels_to_copy = input_channel.len().min(dsp_channel.len());
                        for i in 0..num_channels_to_copy {
                            dsp_channel[i] = input_channel[i] as f64;
                        }
                        // fill the remaining with 0.0
                        dsp_channel[num_channels_to_copy..].fill(0.0);
                    }
                }
            }
            let local = &self.localbuffer.as_slice()[..required_size];

            self.buffer.push_slice(local);
            self.count += (data.len() / h_ichannels) as u64;
        }
    }
}
impl NativeDriver {
    fn pick_input_device(&self, host: &cpal::Host) -> Option<cpal::Device> {
        self.input_device_name.as_ref().and_then(|name| {
            host.input_devices().ok().and_then(|devices| {
                devices
                    .filter_map(|device| {
                        let device_name = device.name().ok()?;
                        Some((device, device_name))
                    })
                    .find_map(|(device, device_name)| (device_name == *name).then_some(device))
            })
        })
    }

    fn pick_output_device(&self, host: &cpal::Host) -> Option<cpal::Device> {
        self.output_device_name.as_ref().and_then(|name| {
            host.output_devices().ok().and_then(|devices| {
                devices
                    .filter_map(|device| {
                        let device_name = device.name().ok()?;
                        Some((device, device_name))
                    })
                    .find_map(|(device, device_name)| (device_name == *name).then_some(device))
            })
        })
    }

    fn set_streams(
        &mut self,
        istream: Option<cpal::Stream>,
        ostream: Option<cpal::Stream>,
        // iconfig: Option<cpal::StreamConfig>,
        // oconfig: Option<cpal::StreamConfig>,
    ) {
        self.istream = istream;
        self.ostream = ostream;
        // self.iconfig = iconfig;
        // self.oconfig = oconfig;
    }
}
impl Driver for NativeDriver {
    type Sample = f64;
    fn get_runtimefn_infos(&self) -> Vec<ExtClsInfo> {
        let getnow = runtime_fn::gen_getnowfn(self.count.clone());
        let getsamplerate = runtime_fn::gen_getsampleratefn(self.sr.0.clone());
        vec![getnow, getsamplerate]
    }

    fn init(
        &mut self,
        runtime_data: RuntimeData,
        sample_rate: Option<SampleRate>,
    ) -> Option<IoChannelInfo> {
        let mut runtime_data = runtime_data;
        let host = cpal::default_host();

        let iochannels = runtime_data.io_channels();
        let ichannels = iochannels.map_or(0, |io| io.input) as usize;

        let idevice = self.pick_input_device(&host).or_else(|| {
            if let Some(name) = &self.input_device_name {
                log::warn!(
                    "Configured input device '{name}' was not found. Falling back to default input device."
                );
            }
            host.default_input_device()
        });
        let odevice = self.pick_output_device(&host).or_else(|| {
            if let Some(name) = &self.output_device_name {
                log::warn!(
                    "Configured output device '{name}' was not found. Falling back to default output device."
                );
            }
            host.default_output_device()
        });

        let output_config = odevice.as_ref().and_then(|device| {
            let default = device.default_output_config().ok()?;
            if cfg!(target_os = "macos") {
                Some(default)
            } else {
                device.supported_output_configs().ok().and_then(|configs| {
                    select_stream_config(
                        configs,
                        iochannels.map_or(0, |io| io.output) as u16,
                        sample_rate
                            .as_ref()
                            .map_or(default.sample_rate(), |rate| cpal::SampleRate(rate.get())),
                    )
                })
            }
        });
        let input_config = idevice.as_ref().and_then(|device| {
            let default = device.default_input_config().ok()?;
            let rate = output_config.as_ref().map_or_else(
                || {
                    sample_rate
                        .as_ref()
                        .map_or(default.sample_rate(), |rate| cpal::SampleRate(rate.get()))
                },
                |config| config.sample_rate(),
            );
            device
                .supported_input_configs()
                .ok()
                .and_then(|configs| select_stream_config(configs, ichannels as u16, rate))
        });
        let (input_config, output_config) = if cfg!(target_os = "macos") {
            (input_config, output_config)
        } else {
            let compatible = idevice
                .as_ref()
                .zip(odevice.as_ref())
                .and_then(|(input, output)| {
                    let inputs = input.supported_input_configs().ok()?.collect::<Vec<_>>();
                    let outputs = output.supported_output_configs().ok()?.collect::<Vec<_>>();
                    let rate = sample_rate
                        .as_ref()
                        .map(|rate| cpal::SampleRate(rate.get()))
                        .or_else(|| output_config.as_ref().map(|config| config.sample_rate()))?;
                    select_compatible_configs(
                        &inputs,
                        &outputs,
                        ichannels as u16,
                        iochannels.map_or(0, |io| io.output) as u16,
                        rate,
                    )
                });
            compatible.map_or((input_config, output_config), |(input, output)| {
                (Some(input), Some(output))
            })
        };
        if idevice.is_some() && input_config.is_none() {
            log::error!("No supported f32 input stream configuration found.");
        }
        if odevice.is_some() && output_config.is_none() {
            log::error!("No supported f32 output stream configuration found.");
        }
        if let (Some(input), Some(output)) = (&input_config, &output_config)
            && input.sample_rate() != output.sample_rate()
        {
            log::warn!("Input and output sample rates differ; audio input may drift.");
        }

        let input_range = input_config.as_ref().map(|config| *config.buffer_size());
        let output_range = output_config.as_ref().map(|config| *config.buffer_size());
        let common_buffer_size = stream_buffer_size(
            self.buffer_size,
            input_range.into_iter().chain(output_range),
        );
        if common_buffer_size.is_none() {
            log::warn!("Input and output buffer size ranges do not overlap; using separate sizes.");
        }
        let input_buffer_size = common_buffer_size.unwrap_or_else(|| {
            stream_buffer_size(self.buffer_size, input_range.into_iter())
                .unwrap_or(self.buffer_size)
        });
        let output_buffer_size = common_buffer_size.unwrap_or_else(|| {
            stream_buffer_size(self.buffer_size, output_range.into_iter())
                .unwrap_or(self.buffer_size)
        });
        if input_buffer_size != self.buffer_size || output_buffer_size != self.buffer_size {
            log::warn!(
                "Requested buffer size {} adjusted to input {} and output {} frames.",
                self.buffer_size,
                input_buffer_size,
                output_buffer_size
            );
        }

        let input_buffer_len =
            ichannels.max(1) * input_buffer_size.saturating_add(output_buffer_size);
        let (prod, cons) = HeapRb::<Self::Sample>::new(input_buffer_len).split();
        let in_stream = if let (Some(idevice), Some(input_config)) = (idevice, input_config) {
            let mut iconfig = input_config.config();
            if cfg!(target_os = "macos") {
                iconfig.channels = stream_channels(iconfig.channels, ichannels as u16);
            }
            iconfig.buffer_size = BufferSize::Fixed(input_buffer_size as u32);
            log::info!(
                "input device: {} channel: {} ,buffer size:{:?}",
                idevice.name().unwrap_or_default(),
                iconfig.channels,
                iconfig.buffer_size
            );
            let mut receiver = NativeAudioReceiver::new(ichannels, input_buffer_size, prod);
            self.hardware_ichannels = iconfig.channels as usize;
            let h_ichannels = self.hardware_ichannels;
            self.sr.0.store(iconfig.sample_rate.0, Ordering::Relaxed);
            runtime_data
                .runtime
                .set_sample_rate(iconfig.sample_rate.0 as f64);
            let in_stream = idevice.build_input_stream(
                &iconfig,
                move |data: &[f32], _s: &cpal::InputCallbackInfo| {
                    receiver.receive_data(data, h_ichannels)
                },
                |e| {
                    log::error!("{e}");
                },
                None,
            );
            in_stream.map_err(|e| log::error!("{e}")).ok()
        } else {
            None
        };
        let _ = in_stream.as_ref().map(|i| i.pause());
        let (swap_prod, swap_cons) = mpsc::channel();
        self.swap_prod = Some(swap_prod);
        let out_stream = if let (Some(odevice), Some(output_config)) = (odevice, output_config) {
            let mut oconfig = output_config.config();
            if cfg!(target_os = "macos") {
                let dsp_channels = iochannels.map_or(0, |io| io.output) as u16;
                oconfig.channels = stream_channels(oconfig.channels, dsp_channels);
            }
            let h_ochannels = oconfig.channels as usize;
            self.hardware_ochannels = h_ochannels;
            self.sr.0.store(oconfig.sample_rate.0, Ordering::Relaxed);
            runtime_data
                .runtime
                .set_sample_rate(oconfig.sample_rate.0 as f64);

            let mut processor = NativeAudioData::new(
                cons,
                runtime_data,
                self.count.clone(),
                output_buffer_size,
                swap_cons,
            );
            oconfig.buffer_size = cpal::BufferSize::Fixed(output_buffer_size as u32);
            log::info!(
                "output device {} buffer size:{:?} channels: {} samplerate {}Hz",
                odevice.name().unwrap_or_default(),
                oconfig.buffer_size,
                oconfig.channels,
                oconfig.sample_rate.0
            );
            let out_stream = odevice.build_output_stream(
                &oconfig,
                move |data: &mut [f32], _s: &cpal::OutputCallbackInfo| {
                    processor.process(data, h_ochannels)
                },
                |e| {
                    log::error!("{e}");
                },
                None,
            );
            out_stream.map_err(|e| log::error!("{e}")).ok()
        } else {
            None
        };
        log::info!(
            "in:{:?} out:{:?}",
            in_stream.is_some(),
            out_stream.is_some()
        );
        self.set_streams(in_stream, out_stream);
        iochannels
    }

    fn play(&mut self) -> bool {
        let ires: bool = self
            .istream
            .as_mut()
            .and_then(|is| match is.play() {
                Ok(_) => Some(()),
                Err(e) => {
                    log::error!("{e}");
                    None
                }
            })
            .is_some();
        let ores = self
            .ostream
            .as_mut()
            .and_then(|os| match os.play() {
                Ok(_) => Some(()),
                Err(e) => {
                    log::error!("{e}");
                    None
                }
            })
            .is_some();
        self.is_playing = ires || ores;
        self.is_playing
    }

    fn pause(&mut self) -> bool {
        let _ires: bool = self
            .istream
            .as_mut()
            .and_then(|is| match is.pause() {
                Ok(_) => Some(()),
                Err(e) => {
                    log::error!("{e}");
                    None
                }
            })
            .is_some();
        let _ores = self
            .ostream
            .as_mut()
            .and_then(|os| match os.pause() {
                Ok(_) => Some(()),
                Err(e) => {
                    log::error!("{e}");
                    None
                }
            })
            .is_some();
        self.is_playing = false;
        false
    }

    fn renew_program(&mut self, new_program: ProgramPayload) {
        self.swap_prod
            .as_mut()
            .and_then(|sp| sp.send(new_program).ok());
    }
    fn get_program_channel(&self) -> Option<mpsc::Sender<ProgramPayload>> {
        self.swap_prod.clone()
    }
    fn is_playing(&self) -> bool {
        self.is_playing
    }

    fn get_samplerate(&self) -> u32 {
        self.sr.get()
    }

    fn get_current_sample(&self) -> Time {
        Time(self.count.load(Ordering::Relaxed))
    }
}

#[cfg(test)]
mod channel_tests {
    use super::{
        NativeAudioReceiver, select_compatible_configs, select_stream_config, stream_buffer_size,
        stream_channels,
    };
    use cpal::SupportedBufferSize;
    use ringbuf::HeapRb;
    use ringbuf::traits::{Consumer, Split};

    #[test]
    fn input_callback_larger_than_requested_buffer() {
        let (producer, mut consumer) = HeapRb::<f64>::new(8).split();
        let mut receiver = NativeAudioReceiver::new(2, 1, producer);
        receiver.receive_data(&[1.0, 2.0, 3.0, 4.0, 5.0, 6.0], 2);
        let mut output = [0.0; 6];
        assert_eq!(consumer.pop_slice(&mut output), 6);
        assert_eq!(output, [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
    }

    #[test]
    fn select_supported_float_config_for_dsp() {
        let config = |channels, min_rate, max_rate, format| {
            cpal::SupportedStreamConfigRange::new(
                channels,
                cpal::SampleRate(min_rate),
                cpal::SampleRate(max_rate),
                SupportedBufferSize::Range { min: 64, max: 1024 },
                format,
            )
        };
        let configs = [
            config(2, 44100, 48000, cpal::SampleFormat::I16),
            config(14, 44100, 48000, cpal::SampleFormat::F32),
            config(2, 44100, 48000, cpal::SampleFormat::F32),
            config(1, 44100, 48000, cpal::SampleFormat::F32),
        ];
        let selected =
            select_stream_config(configs.into_iter(), 2, cpal::SampleRate(48000)).unwrap();
        assert_eq!(selected.channels(), 2);
        assert_eq!(selected.sample_format(), cpal::SampleFormat::F32);
        assert_eq!(selected.sample_rate(), cpal::SampleRate(48000));

        let unsupported = [config(2, 44100, 48000, cpal::SampleFormat::I16)];
        assert!(
            select_stream_config(unsupported.into_iter(), 2, cpal::SampleRate(48000)).is_none()
        );

        let constrained = [
            config(1, 44100, 44100, cpal::SampleFormat::F32),
            config(4, 44100, 44100, cpal::SampleFormat::F32),
        ];
        let selected =
            select_stream_config(constrained.into_iter(), 3, cpal::SampleRate(48000)).unwrap();
        assert_eq!(selected.channels(), 4);
        assert_eq!(selected.sample_rate(), cpal::SampleRate(44100));
    }

    #[test]
    fn select_common_sample_rate_for_devices() {
        let config = |channels, min_rate, max_rate| {
            cpal::SupportedStreamConfigRange::new(
                channels,
                cpal::SampleRate(min_rate),
                cpal::SampleRate(max_rate),
                SupportedBufferSize::Range { min: 64, max: 1024 },
                cpal::SampleFormat::F32,
            )
        };
        let inputs = [config(2, 44100, 44100)];
        let outputs = [config(2, 48000, 48000), config(2, 44100, 44100)];
        let (input, output) =
            select_compatible_configs(&inputs, &outputs, 2, 2, cpal::SampleRate(48000)).unwrap();
        assert_eq!(input.sample_rate(), cpal::SampleRate(44100));
        assert_eq!(output.sample_rate(), cpal::SampleRate(44100));
        assert!(
            select_compatible_configs(&inputs, &outputs[..1], 2, 2, cpal::SampleRate(48000))
                .is_none()
        );
    }

    #[test]
    fn stream_channels_match_dsp_within_hardware_limits() {
        assert_eq!(stream_channels(14, 2), 2);
        assert_eq!(stream_channels(14, 1), 2);
        assert_eq!(stream_channels(14, 0), 2);
        assert_eq!(stream_channels(14, 8), 8);
        assert_eq!(stream_channels(2, 8), 2);
        assert_eq!(stream_channels(1, 2), 1);
    }

    #[test]
    fn buffer_size_matches_both_devices() {
        let ranges = [
            SupportedBufferSize::Range { min: 32, max: 2048 },
            SupportedBufferSize::Range { min: 64, max: 1024 },
        ];
        assert_eq!(stream_buffer_size(4096, ranges.into_iter()), Some(1024));
        assert_eq!(stream_buffer_size(16, ranges.into_iter()), Some(64));
        assert_eq!(stream_buffer_size(512, ranges.into_iter()), Some(512));
        assert_eq!(
            stream_buffer_size(512, [SupportedBufferSize::Unknown].into_iter()),
            Some(512)
        );
        let disjoint = [
            SupportedBufferSize::Range { min: 32, max: 64 },
            SupportedBufferSize::Range {
                min: 128,
                max: 1024,
            },
        ];
        assert_eq!(stream_buffer_size(512, disjoint.into_iter()), None);
    }
}
