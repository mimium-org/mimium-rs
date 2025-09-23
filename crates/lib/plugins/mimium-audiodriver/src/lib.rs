pub mod backends;
pub mod driver;
pub mod runtime_fn;

/// Create the default audio driver used by mimium CLI and examples.
pub fn load_default_runtime() -> Box<dyn driver::Driver<Sample = f64>> {
    #[cfg(not(target_arch = "wasm32"))]
    {
        crate::backends::cpal::native_driver(4096)
    }
    #[cfg(target_arch = "wasm32")]
    {
        crate::backends::local_buffer::local_buffer_driver(4096)
    }
}
