use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, mpsc};

use crate::driver::{Driver, RuntimeData, SampleRate};
use crate::runtime_fn;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{self, BufferSize, StreamConfig};

use mimium_lang::compiler::IoChannelInfo;
use mimium_lang::log;
use mimium_lang::plugin::ExtClsInfo;

use mimium_lang::runtime::{Time, vm};
use ringbuf::traits::{Consumer, Producer, Split};
use ringbuf::{HeapCons, HeapProd, HeapRb};
pub(crate) const DEFAULT_BUFFER_SIZE: usize = 4096;

pub struct NativeDriver {
    sr: SampleRate,
    hardware_ichannels: usize,
    hardware_ochannels: usize,
    is_playing: bool,
    istream: Option<cpal::Stream>,
    ostream: Option<cpal::Stream>,
    count: Arc<AtomicU64>,
    buffer_size: usize,
    swap_prod: Option<mpsc::Sender<vm::Program>>,
}
impl NativeDriver {
    pub fn new(buffer_size: usize) -> Self {
        Self {
            sr: SampleRate::from(48000),
            hardware_ichannels: 1,
            hardware_ochannels: 1,
            is_playing: false,
            istream: None,
            ostream: None,
            count: Default::default(),
            buffer_size,
            swap_prod: None,
        }
    }
}

impl Default for NativeDriver {
    fn default() -> Self {
        Self::new(DEFAULT_BUFFER_SIZE)
    }
}

pub fn native_driver(buffer_size: usize) -> Box<dyn Driver<Sample = f64>> {
    Box::new(NativeDriver::new(buffer_size))
}

//Runtime data, which will be created and immidiately send to audio thread.
struct NativeAudioData {
    pub vmdata: RuntimeData,
    iochannels: IoChannelInfo,
    buffer: HeapCons<f64>,
    localbuffer: Vec<f64>,
    count: Arc<AtomicU64>,
    pub swap_channel: mpsc::Receiver<vm::Program>,
}
unsafe impl Send for NativeAudioData {}

impl NativeAudioData {
    pub fn new(
        buffer: HeapCons<f64>,
        runtime_data: RuntimeData,
        count: Arc<AtomicU64>,
        h_ochannels: usize,
        swap_cons: mpsc::Receiver<vm::Program>,
    ) -> Self {
        //todo: split as trait interface method
        let vmdata = runtime_data;
        let iochannels = vmdata.vm.prog.iochannels.unwrap_or(IoChannelInfo {
            input: 0,
            output: 0,
        });
        let localbuffer: Vec<f64> = vec![0.0f64; DEFAULT_BUFFER_SIZE * h_ochannels];
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
            self.vmdata = self.vmdata.new_resume(swap);
        }
        // let len = dst.len().min(self.localbuffer.len());
        let len = dst.len();

        let local = &mut self.localbuffer.as_mut_slice()[..len];
        self.buffer.pop_slice(local);
        for (o, s) in dst
            .chunks_mut(h_ochannels)
            .zip(local.chunks(self.iochannels.output as usize))
        {
            self.vmdata
                .vm
                .set_stack_range(0, unsafe { std::mem::transmute::<&[f64], &[u64]>(s) });
            let _rc = self
                .vmdata
                .run_dsp(Time(self.count.load(Ordering::Relaxed)));
            let res = vm::Machine::get_as_array::<f64>(
                self.vmdata.vm.get_top_n(self.iochannels.output as _),
            );
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
    pub fn new(dsp_ichannels: usize, buffer: HeapProd<f64>) -> Self {
        Self {
            dsp_ichannels,
            localbuffer: vec![0f64; DEFAULT_BUFFER_SIZE * dsp_ichannels],
            buffer,
            count: 0,
        }
    }
    pub fn receive_data(&mut self, data: &[f32], h_ichannels: usize) {
        debug_assert_eq!(data.len() / h_ichannels, DEFAULT_BUFFER_SIZE);
        let required_size = data.len() / h_ichannels * self.dsp_ichannels;
        debug_assert_eq!(required_size, DEFAULT_BUFFER_SIZE * self.dsp_ichannels);

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
    fn init_iconfig(device: &cpal::Device, sample_rate: Option<SampleRate>) -> StreamConfig {
        let config_builder = device
            .supported_input_configs()
            .unwrap()
            .next()
            .expect("no supported config");
        sample_rate
            .and_then(|sr| config_builder.try_with_sample_rate(cpal::SampleRate(sr.get())))
            .unwrap_or_else(|| {
                device
                    .default_input_config()
                    .expect("no default input configs")
            })
            .config()
    }
    fn init_oconfig(device: &cpal::Device, sample_rate: Option<SampleRate>) -> StreamConfig {
        sample_rate
            .and_then(|sr| {
                if cfg!(not(target_os = "macos")) {
                    let config_builder = device
                        .supported_output_configs()
                        .unwrap()
                        .max_by(|x, y| x.cmp_default_heuristics(y))
                        .expect("no supported config");
                    config_builder.try_with_sample_rate(cpal::SampleRate(sr.get()))
                } else {
                    // Because the cpal's Device:.supported_output_configs: for CoreAudio is not usable,
                    // we ignore given samplerate and use default configuration.
                    // See https://github.com/RustAudio/cpal/pull/96
                    None
                }
            })
            .map_or_else(
                || device.default_output_config().unwrap().config(),
                |builder| builder.config(),
            )
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
        let host = cpal::default_host();

        let iochannels = runtime_data.vm.prog.iochannels;
        let ichannels = iochannels.map_or(0, |io| io.input) as usize;
        let ochannels = iochannels.map_or(0, |io| io.output) as usize;

        let (prod, cons) = HeapRb::<Self::Sample>::new(ochannels * self.buffer_size).split();

        let idevice = host.default_input_device();
        let in_stream = if let Some(idevice) = idevice {
            let mut iconfig = Self::init_iconfig(&idevice, sample_rate.clone());
            iconfig.buffer_size = BufferSize::Fixed((self.buffer_size) as u32);
            log::info!(
                "input device: {} channel: {} ,buffer size:{:?}",
                idevice.name().unwrap_or_default(),
                ichannels,
                iconfig.buffer_size
            );
            let mut receiver = NativeAudioReceiver::new(ichannels, prod);
            self.hardware_ichannels = iconfig.channels as usize;
            let h_ichannels = self.hardware_ichannels;
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
        let odevice = host.default_output_device();
        let (swap_prod, swap_cons) = mpsc::channel();
        self.swap_prod = Some(swap_prod);
        let out_stream = if let Some(odevice) = odevice {
            let mut oconfig = Self::init_oconfig(&odevice, sample_rate);
            let h_ochannels = oconfig.channels as usize;
            self.hardware_ochannels = h_ochannels;

            let mut processor = NativeAudioData::new(
                cons,
                runtime_data,
                self.count.clone(),
                h_ochannels,
                swap_cons,
            );
            oconfig.buffer_size = cpal::BufferSize::Fixed((self.buffer_size) as u32);
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

    fn renew_vm(&mut self, new_prog: vm::Program) {
        self.swap_prod
            .as_mut()
            .and_then(|sp| sp.send(new_prog).ok());
    }
    fn get_vm_channel(&self) -> Option<mpsc::Sender<vm::Program>> {
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
