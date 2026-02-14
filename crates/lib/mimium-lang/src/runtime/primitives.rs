//! Backend-agnostic runtime primitives for heap, state, and arrays.
//!
//! This module defines the minimal interface required by code generation so
//! multiple backends (VM, WASM) can share the same runtime behavior.

pub type Word = u64;
pub type WordSize = u64;

/// Interface for runtime operations that are currently VM-specific.
pub trait RuntimePrimitives {
    type HeapRef: Copy + Eq;
    type ArrayRef: Copy + Eq;
    type StateOffset: Copy;
    type TypeId: Copy;

    // Heap operations.
    fn heap_alloc(&mut self, size_words: WordSize) -> Self::HeapRef;
    fn heap_retain(&mut self, obj: Self::HeapRef);
    fn heap_release(&mut self, obj: Self::HeapRef);
    fn heap_load(&mut self, dst: &mut [Word], obj: Self::HeapRef, size_words: WordSize);
    fn heap_store(&mut self, obj: Self::HeapRef, src: &[Word], size_words: WordSize);

    // Boxed value helpers (may map to heap operations).
    fn box_alloc(&mut self, src: &[Word], size_words: WordSize) -> Self::HeapRef;
    fn box_load(&mut self, dst: &mut [Word], obj: Self::HeapRef, size_words: WordSize);
    fn box_clone(&mut self, obj: Self::HeapRef);
    fn box_release(&mut self, obj: Self::HeapRef);
    fn box_store(&mut self, obj: Self::HeapRef, src: &[Word], size_words: WordSize);

    // UserSum traversal helpers.
    fn usersum_clone(&mut self, value: &mut [Word], size_words: WordSize, type_id: Self::TypeId);
    fn usersum_release(&mut self, value: &mut [Word], size_words: WordSize, type_id: Self::TypeId);

    // Closure helpers.
    fn closure_make(
        &mut self,
        fn_index: Word,
        upvalue_count: WordSize,
        state_size: WordSize,
    ) -> Self::HeapRef;
    fn closure_close(&mut self, obj: Self::HeapRef);
    fn closure_call(
        &mut self,
        obj: Self::HeapRef,
        args: &[Word],
        nargs_words: WordSize,
        ret: &mut [Word],
        nret_words: WordSize,
    );

    // State storage and time-based ops.
    fn state_push(&mut self, offset: Self::StateOffset);
    fn state_pop(&mut self, offset: Self::StateOffset);
    fn state_get(&mut self, dst: &mut [Word], size_words: WordSize);
    fn state_set(&mut self, src: &[Word], size_words: WordSize);
    fn state_delay(&mut self, dst: &mut [Word], src: &[Word], time: &[Word], max_len: Word);
    fn state_mem(&mut self, dst: &mut [Word], src: &[Word]);

    // Arrays.
    fn array_alloc(&mut self, len: Word, elem_size_words: WordSize) -> Self::ArrayRef;
    fn array_get_elem(
        &mut self,
        dst: &mut [Word],
        arr: Self::ArrayRef,
        index: Word,
        elem_size_words: WordSize,
    );
    fn array_set_elem(
        &mut self,
        arr: Self::ArrayRef,
        index: Word,
        src: &[Word],
        elem_size_words: WordSize,
    );

    // Runtime globals.
    fn runtime_get_now(&mut self) -> Word;
    fn runtime_get_samplerate(&mut self) -> Word;
}
