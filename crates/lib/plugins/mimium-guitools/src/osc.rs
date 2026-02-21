use std::{net::UdpSocket, sync::Arc};

use rosc::{OscBundle, OscMessage, OscPacket, OscType, decoder};

use crate::plot_window::FloatParameter;

pub(crate) struct OscSliderReceiver {
    socket: UdpSocket,
    recv_buf: [u8; 2048],
}

impl OscSliderReceiver {
    const DEFAULT_BIND_ADDRESS: &'static str = "0.0.0.0:9000";
    const OSC_BIND_ENV: &'static str = "MIMIUM_GUITOOLS_OSC_BIND";

    pub(crate) fn from_env() -> Option<Self> {
        let bind_address = std::env::var(Self::OSC_BIND_ENV)
            .ok()
            .filter(|addr| !addr.is_empty())
            .unwrap_or_else(|| Self::DEFAULT_BIND_ADDRESS.to_string());
        let socket = UdpSocket::bind(bind_address.as_str())
            .inspect_err(|err| {
                mimium_lang::log::warn!("failed to bind OSC receiver on {bind_address}: {err}")
            })
            .ok()?;
        socket
            .set_nonblocking(true)
            .inspect_err(|err| {
                mimium_lang::log::warn!(
                    "failed to set OSC receiver socket to non-blocking mode: {err}"
                )
            })
            .ok()?;
        Some(Self {
            socket,
            recv_buf: [0; 2048],
        })
    }

    pub(crate) fn poll_and_apply(&mut self, sliders: &[Arc<FloatParameter>]) {
        loop {
            match self.socket.recv_from(&mut self.recv_buf) {
                Ok((size, _)) => {
                    if let Ok((_, packet)) = decoder::decode_udp(&self.recv_buf[..size]) {
                        Self::apply_packet(sliders, packet);
                    }
                }
                Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => break,
                Err(err) => {
                    mimium_lang::log::warn!("OSC receiver socket error: {err}");
                    break;
                }
            }
        }
    }

    fn apply_packet(sliders: &[Arc<FloatParameter>], packet: OscPacket) {
        match packet {
            OscPacket::Message(message) => Self::apply_message(sliders, &message),
            OscPacket::Bundle(OscBundle { content, .. }) => content
                .into_iter()
                .for_each(|packet| Self::apply_packet(sliders, packet)),
        }
    }

    fn apply_message(sliders: &[Arc<FloatParameter>], message: &OscMessage) {
        let slider_name = Self::osc_address_to_slider_name(message.addr.as_str());
        let slider_value = message
            .args
            .first()
            .and_then(Self::osc_numeric_argument_to_f64);
        if let (Some(name), Some(value)) = (slider_name, slider_value) {
            sliders
                .iter()
                .find(|slider| slider.name() == name)
                .iter()
                .for_each(|slider| slider.set(value));
        }
    }

    fn osc_address_to_slider_name(address: &str) -> Option<String> {
        let name = address
            .split('/')
            .filter(|segment| !segment.is_empty())
            .collect::<Vec<_>>()
            .join(".");
        (!name.is_empty()).then_some(name)
    }

    fn osc_numeric_argument_to_f64(argument: &OscType) -> Option<f64> {
        match argument {
            OscType::Float(value) => Some(*value as f64),
            OscType::Double(value) => Some(*value),
            OscType::Int(value) => Some(*value as f64),
            OscType::Long(value) => Some(*value as f64),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::OscSliderReceiver;
    use rosc::{OscMessage, OscType};

    #[test]
    fn osc_path_is_mapped_to_slider_name() {
        let slider_name = OscSliderReceiver::osc_address_to_slider_name("/Params/freq");
        assert_eq!(slider_name.as_deref(), Some("Params.freq"));
    }

    #[test]
    fn osc_message_updates_matching_slider() {
        let mut app = crate::plot_window::PlotApp::default();
        let (slider, _) = app.add_slider("Params.freq", 0.0, -1.0, 1.0);
        let message = OscMessage {
            addr: "/Params/freq".to_string(),
            args: vec![OscType::Float(440.0)],
        };

        OscSliderReceiver::apply_message(&app.sliders, &message);

        assert_eq!(slider.get(), 440.0);
    }
}
