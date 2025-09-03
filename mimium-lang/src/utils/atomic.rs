//! Generic atomic data structures that can be easily converted from/into inner data types.
//! The ordering of store/load is fixed to [ `std::sync::atomic` ].
//!

use atomic_float;
// use serde::{Deserialize, Serialize, de::DeserializeOwned};
use std::{
    marker::PhantomData,
    sync::{Arc, atomic},
};
pub trait SimpleAtomicTest: Copy + PartialOrd {
    type Atomic;
    type Composed: SimpleAtomic<Primitive = Self> + From<Self> + std::fmt::Debug;
}
pub trait IsAtomicNumber<T>:
    SimpleAtomicTest + std::ops::Add<Output = T> + std::ops::Sub<Output = T> + Default
{
}
pub trait SimpleAtomic {
    const ORDER: atomic::Ordering = atomic::Ordering::Relaxed;
    type Primitive: Copy + PartialOrd;
    type Atomic;
    fn load(&self) -> Self::Primitive;
    fn store(&self, v: Self::Primitive);
}

macro_rules! impl_simple_atomic {
    ($name:ident,$p:ty,$ps:literal,$a:ty ) => {
        #[derive(Debug)]
        // #[serde(from=$ps,into=$ps)]
        pub struct $name {
            value: $a,
            _phantom: PhantomData<$p>,
        }
        impl $name {
            pub fn new(v: $p) -> Self {
                Self {
                    value: <$a>::from(v),
                    _phantom: PhantomData,
                }
            }
        }

        impl From<$p> for $name {
            fn from(v: $p) -> Self {
                $name::new(v)
            }
        }

        impl SimpleAtomic for $name {
            type Primitive = $p;
            type Atomic = $a;
            fn load(&self) -> $p {
                self.value.load(Self::ORDER)
            }
            fn store(&self, v: $p) {
                self.value.store(v, Self::ORDER)
            }
        }
        impl From<$name> for $p {
            fn from(v: $name) -> Self {
                v.load()
            }
        }

        impl Clone for $name {
            fn clone(&self) -> Self {
                Self::from(self.load())
            }
        }
        impl std::default::Default for $name {
            fn default() -> $name {
                $name::from(<$p>::default())
            }
        }
        impl SimpleAtomicTest for $p {
            type Atomic = $a;
            type Composed = $name;
        }
        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                self.load() == other.load()
            }
        }
    };
}
macro_rules! impl_is_num {
    ($t0:ty) => {
        impl IsAtomicNumber<$t0> for $t0 {}
    };
    ($t0:ty,$($t:ty),+) => {
        impl IsAtomicNumber<$t0> for $t0 {}
        impl_is_num!($($t),+);
    };
}
pub fn make_simple_atomic<P: SimpleAtomicTest>(v: P) -> P::Composed {
    P::Composed::from(v)
}

impl_simple_atomic!(Bool, bool, "bool", atomic::AtomicBool);
impl_simple_atomic!(Usize, usize, "usize", atomic::AtomicUsize);
impl_simple_atomic!(I8, i8, "i8", atomic::AtomicI8);
impl_simple_atomic!(U8, u8, "u8", atomic::AtomicU8);
impl_simple_atomic!(I16, i16, "i16", atomic::AtomicI16);
impl_simple_atomic!(U16, u16, "u16", atomic::AtomicU16);
impl_simple_atomic!(I32, i32, "i32", atomic::AtomicI32);
impl_simple_atomic!(U32, u32, "u32", atomic::AtomicU32);
impl_simple_atomic!(F32, f32, "f32", atomic_float::AtomicF32);
impl_simple_atomic!(I64, i64, "i64", atomic::AtomicI64);
impl_simple_atomic!(U64, u64, "u64", atomic::AtomicU64);
impl_simple_atomic!(F64, f64, "f64", atomic_float::AtomicF64);
impl_is_num!(usize, i8, u8, i16, u16, i32, u32, f32, i64, u64, f64);

#[cfg(test)]
mod test {
    use super::*;
    // use serde_json;
    #[test]
    fn boolean() {
        let t = Bool::from(true);
        let v: bool = t.into();
        assert!(v);
    }
    // #[test]
    // fn serialize() {
    //     let t = U64::from(42);
    //     let json = serde_json::to_string_pretty(&t).unwrap();
    //     assert_eq!(json, "42".to_string())
    // }
}
