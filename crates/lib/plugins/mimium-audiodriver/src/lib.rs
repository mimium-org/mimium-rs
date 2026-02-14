pub mod backends;
pub mod driver;
pub mod runtime_fn;

#[derive(Clone, Debug, Default)]
pub struct AudioDriverOptions {
    pub input_device: Option<String>,
    pub output_device: Option<String>,
    pub buffer_size: Option<usize>,
}

/// Create the default audio driver used by mimium CLI and examples.
pub fn load_default_runtime() -> Box<dyn driver::Driver<Sample = f64>> {
    #[cfg(not(target_arch = "wasm32"))]
    {
        crate::backends::cpal::native_driver(backends::cpal::DEFAULT_BUFFER_SIZE, None, None)
    }
    #[cfg(target_arch = "wasm32")]
    {
        crate::backends::local_buffer::local_buffer_driver(4096)
    }
}

/// Create a runtime audio driver with user-provided options.
pub fn load_runtime_with_options(options: &AudioDriverOptions) -> Box<dyn driver::Driver<Sample = f64>> {
    #[cfg(not(target_arch = "wasm32"))]
    {
        crate::backends::cpal::native_driver(
            options.buffer_size.unwrap_or(backends::cpal::DEFAULT_BUFFER_SIZE),
            options.input_device.clone(),
            options.output_device.clone(),
        )
    }
    #[cfg(target_arch = "wasm32")]
    {
        let _ = options;
        crate::backends::local_buffer::local_buffer_driver(4096)
    }
}
