//! A compact binary encoding of [`StateTreeSkeleton`], so that a skeleton can travel
//! with a compiled program (e.g. as a WASM custom section) across process boundaries.
//!
//! Pre-order, little-endian: each node is a tag byte followed by one u64:
//! `0` Delay(len), `1` Mem(word size), `2` Feed(word size), `3` FnCall(child count).

use crate::tree::{SizedType, StateTreeSkeleton};

pub fn encode<T: SizedType>(skeleton: &StateTreeSkeleton<T>) -> Vec<u8> {
    fn rec<T: SizedType>(s: &StateTreeSkeleton<T>, out: &mut Vec<u8>) {
        let (tag, v) = match s {
            StateTreeSkeleton::Delay { len } => (0u8, *len),
            StateTreeSkeleton::Mem(t) => (1, t.word_size()),
            StateTreeSkeleton::Feed(t) => (2, t.word_size()),
            StateTreeSkeleton::FnCall(c) => (3, c.len() as u64),
        };
        out.push(tag);
        out.extend_from_slice(&v.to_le_bytes());
        if let StateTreeSkeleton::FnCall(children) = s {
            for c in children {
                rec(c, out);
            }
        }
    }
    let mut out = vec![];
    rec(skeleton, &mut out);
    out
}

/// Decode bytes made by [`encode`]. `make` builds the leaf size type from a word count.
/// Returns `None` on malformed or trailing input.
pub fn decode<T: SizedType>(bytes: &[u8], make: impl Fn(u64) -> T + Copy) -> Option<StateTreeSkeleton<T>> {
    fn rec<T: SizedType>(
        b: &[u8],
        pos: &mut usize,
        make: impl Fn(u64) -> T + Copy,
        depth: usize,
    ) -> Option<StateTreeSkeleton<T>> {
        if depth > 10_000 {
            return None;
        }
        let tag = *b.get(*pos)?;
        let v = u64::from_le_bytes(b.get(*pos + 1..*pos + 9)?.try_into().ok()?);
        *pos += 9;
        Some(match tag {
            0 => StateTreeSkeleton::Delay { len: v },
            1 => StateTreeSkeleton::Mem(make(v)),
            2 => StateTreeSkeleton::Feed(make(v)),
            3 => {
                // every child takes at least 9 bytes
                if (v as usize) > (b.len() - *pos) / 9 {
                    return None;
                }
                let mut children = Vec::with_capacity(v as usize);
                for _ in 0..v {
                    children.push(Box::new(rec(b, pos, make, depth + 1)?));
                }
                StateTreeSkeleton::FnCall(children)
            }
            _ => return None,
        })
    }
    let mut pos = 0;
    let s = rec(bytes, &mut pos, make, 0)?;
    (pos == bytes.len()).then_some(s)
}
