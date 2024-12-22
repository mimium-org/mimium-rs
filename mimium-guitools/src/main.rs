/// Standalone app for testing.
///
use mimium_guitools::plot_window::PlotApp;

// When compiling natively:

#[derive(Debug)]
struct Error(String);
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl std::error::Error for Error {}
fn main() -> eframe::Result {
    // env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).
    #[cfg(not(target_arch = "wasm32"))]
    {
        let native_options = eframe::NativeOptions {
            viewport: egui::ViewportBuilder::default()
                .with_inner_size([400.0, 300.0])
                .with_min_inner_size([300.0, 220.0]), // .with_icon(
            //     // NOTE: Adding an icon is optional
            //     eframe::icon_data::from_png_bytes(&include_bytes!("../assets/icon-256.png")[..])
            //         .expect("Failed to load icon"),)
            ..Default::default()
        };
        eframe::run_native(
            "mimium guitools",
            native_options,
            Box::new(|_cc| Ok(Box::new(PlotApp::new_test()))),
        )
    }
    #[cfg(target_arch = "wasm32")]
    Err(eframe::Error::AppCreation(Box::new(Error(
        "mimium-guitools is not supported on wasm32.".to_string(),
    ))))
}
