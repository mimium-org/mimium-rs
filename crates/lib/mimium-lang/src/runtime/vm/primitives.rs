use crate::runtime::primitives::{RuntimePrimitives, Word, WordSize};

use super::{Machine, StateOffset, heap};

impl RuntimePrimitives for Machine {
    type HeapRef = heap::HeapIdx;
    type ArrayRef = Word; // VM uses RawVal (u64) for array references
    type StateOffset = StateOffset;
    type TypeId = u8;

    fn heap_alloc(&mut self, size_words: WordSize) -> Self::HeapRef {
        self.heap.insert(heap::HeapObject::new(size_words as usize))
    }

    fn heap_retain(&mut self, obj: Self::HeapRef) {
        heap::heap_retain(&mut self.heap, obj);
    }

    fn heap_release(&mut self, obj: Self::HeapRef) {
        heap::heap_release(&mut self.heap, obj);
    }

    fn heap_load(&mut self, dst: &mut [Word], obj: Self::HeapRef, size_words: WordSize) {
        let size = size_words as usize;
        debug_assert!(dst.len() >= size);
        let data = self
            .heap
            .get(obj)
            .map(|heap_obj| &heap_obj.data[..size])
            .expect("heap_load: invalid heap index");
        dst[..size].copy_from_slice(data);
    }

    fn heap_store(&mut self, obj: Self::HeapRef, src: &[Word], size_words: WordSize) {
        let size = size_words as usize;
        debug_assert!(src.len() >= size);
        let data = self
            .heap
            .get_mut(obj)
            .map(|heap_obj| &mut heap_obj.data[..size])
            .expect("heap_store: invalid heap index");
        data.copy_from_slice(&src[..size]);
    }

    fn box_alloc(&mut self, src: &[Word], size_words: WordSize) -> Self::HeapRef {
        let size = size_words as usize;
        debug_assert!(src.len() >= size);
        let data = src[..size].to_vec();
        self.heap.insert(heap::HeapObject::with_data(data))
    }

    fn box_load(&mut self, dst: &mut [Word], obj: Self::HeapRef, size_words: WordSize) {
        self.heap_load(dst, obj, size_words);
    }

    fn box_clone(&mut self, obj: Self::HeapRef) {
        self.heap_retain(obj);
    }

    fn box_release(&mut self, obj: Self::HeapRef) {
        self.heap_release(obj);
    }

    fn box_store(&mut self, obj: Self::HeapRef, src: &[Word], size_words: WordSize) {
        self.heap_store(obj, src, size_words);
    }

    fn usersum_clone(&mut self, value: &mut [Word], size_words: WordSize, type_id: Self::TypeId) {
        let size = size_words as usize;
        let ty = self
            .prog
            .get_type_from_table(type_id)
            .expect("usersum_clone: invalid type id");
        let data = &value[..size];
        Self::clone_usersum_recursive(data, &ty, &mut self.heap);
    }

    fn usersum_release(&mut self, value: &mut [Word], size_words: WordSize, type_id: Self::TypeId) {
        let size = size_words as usize;
        let ty = self
            .prog
            .get_type_from_table(type_id)
            .expect("usersum_release: invalid type id");
        let data = &value[..size];
        let type_table = &self.prog.type_table;
        Self::release_usersum_recursive(data, &ty, &mut self.heap, type_table);
    }

    fn closure_make(
        &mut self,
        _fn_index: Word,
        _upvalue_count: WordSize,
        _state_size: WordSize,
    ) -> Self::HeapRef {
        // VM backend uses MakeHeapClosure instruction directly, which requires upvalue_map
        // from the execution context. This primitive is intended for WASM backend where
        // closures are allocated differently.
        unimplemented!("closure_make is not used in VM backend (see MakeHeapClosure instruction)")
    }

    fn closure_close(&mut self, obj: Self::HeapRef) {
        self.close_heap_upvalues(obj);
    }

    fn closure_call(
        &mut self,
        _obj: Self::HeapRef,
        _args: &[Word],
        _nargs_words: WordSize,
        _ret: &mut [Word],
        _nret_words: WordSize,
    ) {
        // VM backend uses CallIndirect instruction directly, which integrates with
        // the execution loop and call stack. This primitive is intended for WASM backend
        // where function calls are handled differently.
        unimplemented!("closure_call is not used in VM backend (see CallIndirect instruction)")
    }

    fn state_push(&mut self, offset: Self::StateOffset) {
        self.get_current_state().push_pos(offset);
    }

    fn state_pop(&mut self, offset: Self::StateOffset) {
        self.get_current_state().pop_pos(offset);
    }

    fn state_get(&mut self, dst: &mut [Word], size_words: WordSize) {
        let size = size_words as usize;
        debug_assert!(dst.len() >= size);
        let data = self.get_current_state().get_state(size_words);
        dst[..size].copy_from_slice(&data[..size]);
    }

    fn state_set(&mut self, src: &[Word], size_words: WordSize) {
        let size = size_words as usize;
        debug_assert!(src.len() >= size);
        let dst = self.get_current_state().get_state_mut(size);
        dst.copy_from_slice(&src[..size]);
    }

    fn state_delay(&mut self, dst: &mut [Word], src: &[Word], time: &[Word], max_len: Word) {
        let input = src.first().copied().unwrap_or_default();
        let t = time.first().copied().unwrap_or_default();
        let size_in_samples = max_len;
        let mut ringbuf = self.get_current_state().get_as_ringbuffer(size_in_samples);
        let res = ringbuf.process(input, t);
        if let Some(dst_first) = dst.first_mut() {
            *dst_first = Self::to_value(res);
        }
    }

    fn state_mem(&mut self, dst: &mut [Word], src: &[Word]) {
        let prev = self.get_current_state().get_state_mut(1)[0];
        let next = src.first().copied().unwrap_or_default();
        if let Some(dst_first) = dst.first_mut() {
            *dst_first = prev;
        }
        self.get_current_state().get_state_mut(1)[0] = next;
    }

    fn array_alloc(&mut self, len: Word, elem_size_words: WordSize) -> Self::ArrayRef {
        self.arrays.alloc_array(len, elem_size_words)
    }

    fn array_get_elem(
        &mut self,
        dst: &mut [Word],
        arr: Self::ArrayRef,
        index: Word,
        elem_size_words: WordSize,
    ) {
        let elem_size = elem_size_words as usize;
        debug_assert!(dst.len() >= elem_size);
        let index_val = Self::get_as::<f64>(index) as usize;
        let array = self.arrays.get_array(arr);
        let start = index_val * elem_size;
        debug_assert!(start + elem_size <= array.data.len());
        let data = &array.data[start..start + elem_size];
        dst[..elem_size].copy_from_slice(data);
    }

    fn array_set_elem(
        &mut self,
        arr: Self::ArrayRef,
        index: Word,
        src: &[Word],
        elem_size_words: WordSize,
    ) {
        let elem_size = elem_size_words as usize;
        debug_assert!(src.len() >= elem_size);
        let index_val = Self::get_as::<f64>(index) as usize;
        let array = self.arrays.get_array_mut(arr);
        let start = index_val * elem_size;
        debug_assert!(start + elem_size <= array.data.len());
        array.data[start..start + elem_size].copy_from_slice(&src[..elem_size]);
    }

    fn runtime_get_now(&mut self) -> Word {
        // Runtime globals are provided through external functions in VM backend.
        // For now, return 0. This will be properly wired when plugin infrastructure
        // provides these runtime values.
        Self::to_value(0.0)
    }

    fn runtime_get_samplerate(&mut self) -> Word {
        // Default sample rate. In practice this should be provided by audio driver plugin.
        // The VM backend typically accesses this through external function table.
        Self::to_value(48000.0)
    }
}
