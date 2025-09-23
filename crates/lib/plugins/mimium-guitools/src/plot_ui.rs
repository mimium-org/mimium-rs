use egui::Color32;
use egui_plot::{Line, LineStyle, PlotPoints};
use ringbuf::{HeapCons, HeapRb, traits::Consumer};
pub(crate) struct PlotUi {
    label: String,
    color: Color32,
    buf: HeapCons<f64>,
    local_buf: Vec<f64>,
    update_index: usize,
}
impl PlotUi {
    pub fn new(label: &str, buf: HeapCons<f64>, color: Color32) -> Self {
        let local_buf = (0..4096).map(|_| 0.0).collect();

        Self {
            label: label.to_string(),
            color,
            buf,
            local_buf,
            update_index: 0,
        }
    }
    pub fn new_test(label: &str) -> Self {
        let local_buf = (0..4096)
            .map(|t| (std::f64::consts::PI * 2.0 * (t as f64 / 4096.0)).sin())
            .collect();
        Self {
            label: label.to_string(),
            color: Color32::from_rgb(255, 0, 0),
            buf: HeapCons::new(HeapRb::new(10).into()),
            local_buf,
            update_index: 0,
        }
    }
    pub(crate) fn draw_line(&mut self) -> (bool, Line) {
        let mut request_repaint = false;
        while let Some(s) = self.buf.try_pop() {
            request_repaint = true;
            self.local_buf[self.update_index] = s;
            self.update_index = (self.update_index + 1) % self.local_buf.len();
        }

        let line = Line::new(PlotPoints::from_parametric_callback(
            |t| (t, self.local_buf[t as usize]),
            0.0..4096.0,
            4096,
        ))
        // .color(...)
        .color(self.color)
        .style(LineStyle::Solid)
        .name(&self.label);
        (request_repaint, line)
    }
}
