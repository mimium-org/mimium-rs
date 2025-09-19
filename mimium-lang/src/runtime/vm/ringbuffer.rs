use std::ptr::slice_from_raw_parts_mut;

use super::RawVal;

/// A minimal implementation of synchronous ringbuffer for a single thread code.
/// Because this Ringbuffer is constructed by re-interpreting the portion of linear memory array temporary, it has limited lifetime.
#[repr(C)]
pub(super) struct Ringbuffer<'a> {
    read_idx: &'a mut u64,
    write_idx: &'a mut u64,
    data: &'a mut [u64],
}

impl<'a> Ringbuffer<'a> {
    pub fn new(head: *mut u64, size_in_samples: u64) -> Self {
        let (read_idx, write_idx, data) = unsafe {
            let read_idx = head.as_mut().unwrap_unchecked();
            let write_ptr = head.offset(1);
            let write_idx = write_ptr.as_mut().unwrap_unchecked();
            let data_head = head.offset(2);
            let data = slice_from_raw_parts_mut(data_head, size_in_samples as usize);
            (read_idx, write_idx, data.as_mut().unwrap())
        };

        Self {
            read_idx,
            write_idx,
            data,
        }
    }
    pub fn process(&mut self, input: RawVal, time_raw: u64) -> RawVal {
        let len = self.data.len() as u64;
        let res = unsafe {
            let time = f64::from_bits(time_raw) as u64;
            let read_idx = *self.read_idx;
            *self.write_idx = (read_idx + time) % len;
            let write_idx = *self.write_idx;
            let res = *self.data.get_unchecked(read_idx as usize);
            *self.data.get_unchecked_mut(write_idx as usize) = input;
            *self.read_idx = (read_idx + 1) % len;
            res
        };
        res
    }
}
