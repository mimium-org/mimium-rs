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

        if len == 0 {
            return 0;
        }

        unsafe {
            let delay_time = f64::from_bits(time_raw);
            let max_delay = (len - 1) as f64;
            let delay_samples = delay_time.clamp(0.0, max_delay) as u64;
            let write_idx = *self.write_idx % len;
            let read_idx = (write_idx + len - delay_samples) % len;
            let res = *self.data.get_unchecked(read_idx as usize);
            *self.data.get_unchecked_mut(write_idx as usize) = input;
            *self.read_idx = read_idx;
            *self.write_idx = (write_idx + 1) % len;
            res
        }
    }

    pub fn process_block(
        &mut self,
        dst: &mut [RawVal],
        src: &[RawVal],
        time: &[RawVal],
        frames: usize,
    ) {
        debug_assert!(dst.len() >= frames);
        debug_assert!(src.len() >= frames);
        debug_assert!(time.len() >= frames);

        (0..frames).for_each(|frame| {
            dst[frame] = self.process(src[frame], time[frame]);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::Ringbuffer;

    fn make_storage(size_in_samples: usize) -> Vec<u64> {
        vec![0; size_in_samples + 2]
    }

    #[test]
    fn process_block_matches_scalar_process() {
        let size = 8usize;
        let inputs = [1.0, 2.0, 3.0, 4.0, 5.0]
            .map(f64::to_bits);
        let times = [0.0, 1.0, 2.0, 3.0, 1.0]
            .map(f64::to_bits);

        let mut scalar_storage = make_storage(size);
        let mut scalar = Ringbuffer::new(scalar_storage.as_mut_ptr(), size as u64);
        let scalar_out = inputs
            .iter()
            .zip(times.iter())
            .map(|(input, time)| scalar.process(*input, *time))
            .collect::<Vec<_>>();

        let mut block_storage = make_storage(size);
        let mut block = Ringbuffer::new(block_storage.as_mut_ptr(), size as u64);
        let mut block_out = vec![0; inputs.len()];
        block.process_block(&mut block_out, &inputs, &times, inputs.len());

        assert_eq!(block_out, scalar_out);
        assert_eq!(block_storage, scalar_storage);
    }
}
