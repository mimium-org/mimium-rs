use std::{
    fs::File,
    io::{BufWriter, Write},
    path::Path,
};

use mimium_lang::{ExecContext, compiler::IoChannelInfo, plugin::ExtClsInfo, runtime::vm};

use crate::driver::Driver;

use super::local_buffer::LocalBufferDriver;

pub struct CsvDriver {
    driver: LocalBufferDriver,
    csv_file: Box<dyn Write>,
}

impl CsvDriver {
    pub fn new<P: AsRef<Path>>(times: usize, path: &Option<P>) -> Self {
        let csv_file: Box<dyn std::io::Write> = if let Some(path) = path {
            let csv_file = File::create(path.as_ref()).unwrap();
            Box::new(BufWriter::new(csv_file))
        } else {
            Box::new(BufWriter::new(std::io::stdout()))
        };

        Self {
            driver: LocalBufferDriver::new(times),
            csv_file,
        }
    }
}

impl Driver for CsvDriver {
    type Sample = <LocalBufferDriver as Driver>::Sample;
    fn get_runtimefn_infos(&self) -> Vec<ExtClsInfo> {
        self.driver.get_runtimefn_infos()
    }

    fn init(
        &mut self,
        ctx: ExecContext,
        sample_rate: Option<crate::driver::SampleRate>,
    ) -> Option<IoChannelInfo> {
        let res = self.driver.init(ctx, sample_rate);

        let chunk_size = res.map_or(0, |io| io.output);
        let mut header = String::new();
        for i in 0..chunk_size {
            header.push_str(&format!("ch{i}"));
            if i != chunk_size - 1 {
                header.push(',');
            } else {
                header.push('\n');
            }
        }

        // TODO: these erros should be handled. The Driver interface will
        // probably return Result to propagate runtime errors (e.g. dsp() not
        // found). Let's revisit here after it happens.
        self.csv_file
            .write_all(header.as_bytes())
            .expect("failed to write");
        self.csv_file.flush().expect("failed to flush");

        res
    }

    fn play(&mut self) -> bool {
        let res = self.driver.play();

        let chunk_size = self.driver.vmdata.as_ref().map_or(0, |vmdata| {
            vmdata.vm.prog.iochannels.map_or(0, |io| io.output)
        }) as _;
        let mut line = String::new();
        for sample in self.driver.get_generated_samples().chunks(chunk_size) {
            for (i, v) in sample.iter().enumerate() {
                // :? is to display "0" as "0.0"
                line.push_str(&format!("{v:?}"));
                if i != sample.len() - 1 {
                    line.push(',');
                } else {
                    line.push('\n');
                }
            }
        }

        self.csv_file
            .write_all(line.as_bytes())
            .expect("failed to write");
        self.csv_file.flush().expect("failed to flush");

        res
    }

    fn pause(&mut self) -> bool {
        self.driver.pause()
    }

    fn get_samplerate(&self) -> u32 {
        self.driver.get_samplerate()
    }

    fn get_current_sample(&self) -> mimium_lang::runtime::Time {
        self.driver.get_current_sample()
    }

    fn is_playing(&self) -> bool {
        self.driver.is_playing()
    }
}

pub fn csv_driver<P: AsRef<Path>>(times: usize, path: &Option<P>) -> Box<dyn Driver<Sample = f64>> {
    Box::new(CsvDriver::new(times, path))
}
