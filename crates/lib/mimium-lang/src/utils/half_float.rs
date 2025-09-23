use half::f16;
use std::fmt::Display;
/// Half-Precision floating point type that can be converted from 64bit float with truncation checking.
pub const ALLOWED_ERROR: f64 = 0.00001;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct HFloat(f16);

impl Display for HFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<f16> for HFloat {
    fn from(value: f16) -> Self {
        Self(value)
    }
}
impl From<HFloat> for f64 {
    fn from(value: HFloat) -> Self {
        value.0.to_f64()
    }
}

impl TryFrom<f64> for HFloat {
    type Error = ();

    fn try_from(value: f64) -> Result<Self, Self::Error> {
        let hv = f16::from_f64(value);
        let error = (hv.to_f64() - value).abs();
        if error < ALLOWED_ERROR {
            Ok(Self(f16::from_f64(value)))
        } else {
            Err(())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn safecast() {
        let f64v: f64 = 2.0;
        let hv = HFloat::try_from(f64v);
        assert!(hv.is_ok());
        assert!((hv.unwrap().0.to_f64() - f64v).abs() < ALLOWED_ERROR)
    }
}
