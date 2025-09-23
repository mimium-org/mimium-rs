pub mod environment;
pub mod error;
pub mod fileloader;
pub mod half_float;
pub mod metadata;
pub mod miniprint;

#[macro_export]
macro_rules! format_vec {
    ($vec:expr,$sep:expr) => {
        $vec.iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join($sep)
    };
}
