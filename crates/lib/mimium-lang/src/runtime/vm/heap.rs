//! Generic heap-allocated object storage with reference counting.
//!
//! This module provides a unified heap storage mechanism for objects that need
//! dynamic lifetime management, such as closures and recursive variant types.
//!
//! ## Design Philosophy
//!
//! No type tags are needed at runtime since mimium is a statically-typed language.
//! Type safety is guaranteed at compile time, and each object type follows a specific
//! data layout convention.
//!
//! ## Data Layout Conventions
//!
//! ### Closure Layout
//! ```text
//! [0]: fn_proto_pos (usize as RawVal)
//! [1]: base_ptr (u64 as RawVal)
//! [2]: is_closed (bool as RawVal, 0=false, 1=true)
//! [3]: upvalue_count (usize as RawVal)
//! [4..4+upvalue_count]: upvalue data (SharedUpValue pointers encoded as RawVal)
//! [4+upvalue_count..]: state_storage data
//! ```
//!
//! ### Tagged Union Layout (for recursive variant types, future)
//! ```text
//! [0]: tag (u64 as RawVal)
//! [1..]: payload data (may contain HeapIdx for recursive references)
//! ```

use crate::runtime::vm::RawVal;
use slotmap::{DefaultKey, SlotMap};

/// Generic heap-allocated object with reference counting.
///
/// No type discrimination is performed at runtime. The layout of `data`
/// is determined by compile-time type information and follows documented
/// layout conventions for each object type.
#[derive(Debug, Clone)]
pub struct HeapObject {
    /// Reference count for automatic memory management
    pub refcount: u64,
    /// Size of the object in words (RawVal count)
    pub size: usize,
    /// Raw data storage following specific layout conventions
    pub data: Vec<RawVal>,
}

impl HeapObject {
    /// Create a new heap object with the given size.
    ///
    /// The data vector is initialized with zeros. Caller is responsible
    /// for filling in the appropriate data according to layout conventions.
    pub fn new(size: usize) -> Self {
        Self {
            refcount: 1,
            size,
            data: vec![0; size],
        }
    }

    /// Create a new heap object with pre-initialized data.
    pub fn with_data(data: Vec<RawVal>) -> Self {
        let size = data.len();
        Self {
            refcount: 1,
            size,
            data,
        }
    }
}

/// Heap storage using slot map for stable indices and efficient memory reuse
pub type HeapStorage = SlotMap<DefaultKey, HeapObject>;

/// Stable index into heap storage
pub type HeapIdx = DefaultKey;

/// Increment the reference count of a heap object.
///
/// This should be called when creating a new reference to a heap object
/// (e.g., copying a closure reference to another register).
pub fn heap_retain(storage: &mut HeapStorage, idx: HeapIdx) {
    if let Some(obj) = storage.get_mut(idx) {
        obj.refcount += 1;
        log::trace!("heap_retain: {:?} refcount -> {}", idx, obj.refcount);
    } else {
        log::warn!("heap_retain: invalid HeapIdx {idx:?}");
    }
}

/// Decrement the reference count of a heap object and free it if the count reaches zero.
///
/// When freeing, this function recursively releases any nested heap references
/// found in the object's data. The caller must ensure that heap references
/// are properly identified (e.g., by layout conventions).
///
/// # Recursive Release
///
/// Currently, this function does not automatically detect nested heap references.
/// Specialized release functions should be used for objects containing heap references:
/// - `heap_release_closure` for closures with captured heap objects
/// - Future: `heap_release_variant` for recursive variant types
pub fn heap_release(storage: &mut HeapStorage, idx: HeapIdx) {
    if let Some(obj) = storage.get_mut(idx) {
        obj.refcount -= 1;
        log::trace!("heap_release: {:?} refcount -> {}", idx, obj.refcount);

        if obj.refcount == 0 {
            log::trace!("heap_release: freeing {idx:?}");
            storage.remove(idx);
        }
    } else {
        log::warn!("heap_release: invalid HeapIdx {idx:?}");
    }
}

/// Release a closure from heap storage, properly handling nested closures in upvalues.
///
/// This function understands the closure layout convention and recursively releases
/// any captured closures found in upvalues.
///
/// # Closure Layout Assumption
/// - [3]: upvalue_count
/// - [4..4+upvalue_count]: upvalues (may contain HeapIdx to other closures)
pub fn heap_release_closure(storage: &mut HeapStorage, idx: HeapIdx) {
    // First, decrement refcount
    let should_free = if let Some(obj) = storage.get_mut(idx) {
        obj.refcount -= 1;
        log::trace!(
            "heap_release_closure: {:?} refcount -> {}",
            idx,
            obj.refcount
        );
        obj.refcount == 0
    } else {
        log::warn!("heap_release_closure: invalid HeapIdx {idx:?}");
        return;
    };

    if should_free {
        // Collect nested closure references before removing
        // Note: This is a simplified version. The actual implementation needs to
        // understand the UpValue structure to properly identify closure references.
        // For now, we just remove the object without recursive handling.
        // TODO: Implement proper recursive release based on UpValue::Closed analysis

        log::trace!("heap_release_closure: freeing {idx:?}");
        storage.remove(idx);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_heap_object_creation() {
        let obj = HeapObject::new(5);
        assert_eq!(obj.refcount, 1);
        assert_eq!(obj.size, 5);
        assert_eq!(obj.data.len(), 5);
        assert!(obj.data.iter().all(|&v| v == 0));
    }

    #[test]
    fn test_heap_object_with_data() {
        let data = vec![1, 2, 3, 4, 5];
        let obj = HeapObject::with_data(data.clone());
        assert_eq!(obj.refcount, 1);
        assert_eq!(obj.size, 5);
        assert_eq!(obj.data, data);
    }

    #[test]
    fn test_heap_retain_and_release() {
        let mut storage = HeapStorage::default();
        let idx = storage.insert(HeapObject::new(3));

        // Initial refcount is 1
        assert_eq!(storage.get(idx).unwrap().refcount, 1);

        // Retain increases refcount
        heap_retain(&mut storage, idx);
        assert_eq!(storage.get(idx).unwrap().refcount, 2);

        heap_retain(&mut storage, idx);
        assert_eq!(storage.get(idx).unwrap().refcount, 3);

        // Release decreases refcount
        heap_release(&mut storage, idx);
        assert_eq!(storage.get(idx).unwrap().refcount, 2);

        heap_release(&mut storage, idx);
        assert_eq!(storage.get(idx).unwrap().refcount, 1);

        // Final release frees the object
        heap_release(&mut storage, idx);
        assert!(storage.get(idx).is_none());
    }

    #[test]
    fn test_heap_release_already_freed() {
        let mut storage = HeapStorage::default();
        let idx = storage.insert(HeapObject::new(3));

        heap_release(&mut storage, idx);
        assert!(storage.get(idx).is_none());

        // Releasing again should not panic (just log warning)
        heap_release(&mut storage, idx);
    }
}
