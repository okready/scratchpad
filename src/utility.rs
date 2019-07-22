// Copyright 2018-2019 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Miscellaneous utility routines and types.

use core::fmt;
use core::mem::{align_of, size_of};

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
use super::{Box, ByteData, Vec};

/// Returns the minimum number of elements of a given type necessary for
/// storage of a given byte count. The actual supported byte count may be
/// larger due to padding.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate scratchpad;
///
/// # fn main() {
/// assert_eq!(array_len_for_bytes!(u64, 32), 4);
/// # }
/// ```
#[macro_export(local_inner_macros)]
macro_rules! array_len_for_bytes {
    ($element:ty, $bytes:expr) => {
        ($bytes + $crate::size_of::<$element>() - 1)
            / $crate::size_of::<$element>()
    };

    ($element:ty, $bytes:expr,) => {
        array_len_for_bytes!($element, $bytes)
    };
}

/// Declares a static array of the specified element type that is large enough
/// for storage of a given byte count. The actual supported byte count may be
/// larger due to padding.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate scratchpad;
///
/// // `BufferType` is the same as `[u64; 4]`.
/// type BufferType = array_type_for_bytes!(u64, 32);
///
/// # fn main() {
/// let buffer: BufferType = [1, 2, 3, 4];
/// # }
/// ```
#[macro_export(local_inner_macros)]
macro_rules! array_type_for_bytes {
    ($element:ty, $bytes:expr) => {
        [$element; array_len_for_bytes!($element, $bytes)]
    };

    ($element:ty, $bytes:expr,) => {
        array_type_for_bytes!($element, $bytes)
    };
}

/// Returns the minimum number of elements of a given type necessary for
/// tracking of at least the specified number of [allocation markers]. The
/// actual supported marker count may be larger due to padding.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate scratchpad;
///
/// use scratchpad::CacheAligned;
///
/// # fn main() {
/// let len = array_len_for_markers!(CacheAligned, 16);
///
/// #[cfg(target_pointer_width = "32")]
/// assert_eq!(len, 1);
///
/// #[cfg(target_pointer_width = "64")]
/// assert_eq!(len, 2);
/// # }
/// ```
///
/// [allocation markers]: trait.Marker.html
#[macro_export(local_inner_macros)]
macro_rules! array_len_for_markers {
    ($element:ty, $marker_count:expr) => {
        array_len_for_bytes!(
            $element,
            $marker_count * $crate::size_of::<usize>(),
        )
    };

    ($element:ty, $marker_count:expr,) => {
        array_len_for_markers!($element, $marker_count)
    };
}

/// Declares a static array of the specified element type that is large enough
/// for storage of at least the specified number of [allocation markers]. The
/// actual supported marker count may be larger due to padding.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate scratchpad;
///
/// use scratchpad::{CacheAligned, ErrorKind, Scratchpad};
///
/// // `BufferType` is the same as `[CacheAligned; 1]` on targets using 32-bit
/// // pointers and `[CacheAligned; 2]` on targets using 64-bit pointers.
/// type BufferType = array_type_for_markers!(CacheAligned, 16);
///
/// # fn main() {
/// #[cfg(target_pointer_width = "32")]
/// let buffer: BufferType = [CacheAligned([1; 64])];
///
/// #[cfg(target_pointer_width = "64")]
/// let buffer: BufferType = [CacheAligned([1; 64]), CacheAligned([2; 64])];
///
/// // Regardless of the target pointer size, the capacity of 16 markers is
/// // still the same.
/// let scratchpad = Scratchpad::new([0u64; 1], buffer);
/// let mut markers = Vec::new();
/// for _ in 0..16 {
///     markers.push(scratchpad.mark_front().unwrap());
/// }
///
/// assert_eq!(
///     scratchpad.mark_front().unwrap_err().kind(),
///     ErrorKind::MarkerLimit,
/// );
/// # }
/// ```
///
/// [allocation markers]: trait.Marker.html
#[macro_export(local_inner_macros)]
macro_rules! array_type_for_markers {
    ($element:ty, $marker_count:expr) => {
        [$element; array_len_for_markers!($element, $marker_count)]
    };

    ($element:ty, $marker_count:expr,) => {
        array_type_for_markers!($element, $marker_count)
    };
}

/// Creates a [`CacheAligned`] instance whose contents are zeroed-out.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate scratchpad;
/// # fn main() {
/// let zeroed = cache_aligned_zeroed!();
/// for i in 0..zeroed.0.len() {
///     assert_eq!(zeroed.0[i], 0);
/// }
/// # }
/// ```
///
/// [`CacheAligned`]: struct.CacheAligned.html
#[macro_export]
macro_rules! cache_aligned_zeroed {
    () => {
        $crate::CacheAligned([0; $crate::CACHE_ALIGNMENT])
    };
}

/// Creates an array of zeroed-out [`CacheAligned`] values large enough for
/// storage of the given byte count. The actual supported byte count may be
/// larger due to padding.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate scratchpad;
/// # fn main() {
/// let zeroed = cache_aligned_zeroed_for_bytes!(256);
/// for element in &zeroed {
///     for i in 0..element.0.len() {
///         assert_eq!(element.0[i], 0);
///     }
/// }
/// # }
/// ```
///
/// [`CacheAligned`]: struct.CacheAligned.html
#[macro_export(local_inner_macros)]
macro_rules! cache_aligned_zeroed_for_bytes {
    ($bytes:expr) => {
        [cache_aligned_zeroed!();
            array_len_for_bytes!($crate::CacheAligned, $bytes)]
    };
    ($bytes:expr,) => {
        cache_aligned_zeroed_for_bytes!($bytes)
    };
}

/// Creates an array of zeroed-out [`CacheAligned`] values large enough for
/// storage of at least the specified number of [allocation markers]. The
/// actual supported marker count may be larger due to padding.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate scratchpad;
/// # fn main() {
/// let zeroed = cache_aligned_zeroed_for_markers!(32);
/// for element in &zeroed {
///     for i in 0..element.0.len() {
///         assert_eq!(element.0[i], 0);
///     }
/// }
/// # }
/// ```
///
/// [`CacheAligned`]: struct.CacheAligned.html
/// [allocation markers]: trait.Marker.html
#[macro_export(local_inner_macros)]
macro_rules! cache_aligned_zeroed_for_markers {
    ($marker_count:expr) => {
        [cache_aligned_zeroed!();
            array_len_for_markers!($crate::CacheAligned, $marker_count)]
    };
    ($marker_count:expr,) => {
        cache_aligned_zeroed_for_markers!($marker_count)
    };
}

/// Assumed cache line byte alignment.
///
/// This may vary from the actual cache line alignment, which can vary between
/// processors types, including those of the same architecture. 64 bytes is
/// typically assumed to be a "safe" target to ensure cache alignment.
///
/// Note that since the `repr(align)` attribute doesn't support named
/// constants, this value is duplicated in the declaration of
/// [`CacheAligned`], so it must be updated in both locations if changed.
///
/// [`CacheAligned`]: struct.CacheAligned.html
pub const CACHE_ALIGNMENT: usize = 64;

/// Cache-aligned storage for [`Buffer`] and [`Tracking`] use.
///
/// Internally, this simply wraps a `u8` array to ensure cache alignment.
/// Arrays and slices of this type can be used directly for either
/// [`Scratchpad`] storage or marker tracking.
///
/// The alignment and size of `CacheAligned` are determined by the
/// [`CACHE_ALIGNMENT`] constant.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate scratchpad;
/// use scratchpad::{CacheAligned, CACHE_ALIGNMENT, Scratchpad};
///
/// fn is_cache_aligned<T>(ptr: *const T) -> bool {
///     ptr as usize & (CACHE_ALIGNMENT - 1) == 0
/// }
///
/// # fn main() {
/// // `CacheAligned` elements are guaranteed to always be aligned to
/// // `CACHE_ALIGNMENT` bytes, regardless of whether they're created on the
/// // stack, heap-allocated, or allocated from a `Scratchpad`.
/// let cache_aligned = cache_aligned_zeroed!();
/// assert!(is_cache_aligned(&cache_aligned));
///
/// let cache_aligned = Box::new(cache_aligned_zeroed!());
/// assert!(is_cache_aligned(&*cache_aligned));
///
/// let scratchpad = Scratchpad::new(
///     [cache_aligned_zeroed!(); 1],
///     [0usize; 1],
/// );
/// let marker = scratchpad.mark_front().expect("marker creation failed");
/// let cache_aligned = marker
///     .allocate(cache_aligned_zeroed!())
///     .expect("allocation failed");
/// assert!(is_cache_aligned(&*cache_aligned));
/// # }
/// ```
///
/// [`Buffer`]: trait.Buffer.html
/// [`CACHE_ALIGNMENT`]: constant.CACHE_ALIGNMENT.html
/// [`Scratchpad`]: struct.Scratchpad.html
/// [`Tracking`]: trait.Tracking.html
#[derive(Clone, Copy)]
#[repr(C, align(64))]
pub struct CacheAligned(pub [u8; CACHE_ALIGNMENT]);

/// Dummy constant used to verify `CacheAligned` size at compile-time.
const _ASSERT_CACHE_ALIGNED_SIZE: [(); size_of::<CacheAligned>()
    - CACHE_ALIGNMENT] = [];
/// Dummy constant used to verify `CacheAligned` alignment at compile-time.
const _ASSERT_CACHE_ALIGNED_ALIGNMENT: [(); align_of::<CacheAligned>()
    - CACHE_ALIGNMENT] = [];

impl fmt::Debug for CacheAligned {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CacheAligned {{ ... }}")
    }
}

/// Returns a boxed slice of a given length whose data is uninitialized.
///
/// # Safety
///
/// The contents of the boxed slice are left uninitialized; reading from and
/// writing to the slice contents can trigger [undefined behavior] if not
/// careful.
///
/// It is strongly recommended to only allocate slices of
/// [`MaybeUninit`]-wrapped types if supported by the version of Rust used, as
/// using slices of non-wrapped types cause undefined behavior. This function
/// will most likely be updated to enforce this restriction in future major
/// releases of this crate.
///
/// # Examples
///
/// ```
/// use scratchpad::uninitialized_boxed_slice;
/// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
/// use std::mem::MaybeUninit;
///
/// # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
/// # let buffer = unsafe { uninitialized_boxed_slice::<u32>(32) };
/// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
/// let buffer = unsafe { uninitialized_boxed_slice::<MaybeUninit<u32>>(32) };
/// assert_eq!(buffer.len(), 32);
/// ```
///
/// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
#[inline]
pub unsafe fn uninitialized_boxed_slice<T>(len: usize) -> Box<[T]>
where
    T: ByteData,
{
    let mut buffer = Vec::with_capacity(len);
    buffer.set_len(len);
    buffer.into_boxed_slice()
}

/// Returns a boxed slice of uninitialized data large enough to hold at least
/// the specified number of bytes.
///
/// # Safety
///
/// The contents of the boxed slice are left uninitialized; reading from and
/// writing to the slice contents can trigger [undefined behavior] if not
/// careful.
///
/// It is strongly recommended to only allocate slices of
/// [`MaybeUninit`]-wrapped types if supported by the version of Rust used, as
/// using slices of non-wrapped types cause undefined behavior. This function
/// will most likely be updated to enforce this restriction in future major
/// releases of this crate.
///
/// # Examples
///
/// ```
/// use scratchpad::uninitialized_boxed_slice_for_bytes;
/// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
/// use std::mem::MaybeUninit;
///
/// # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
/// # let buffer = unsafe { uninitialized_boxed_slice_for_bytes::<u32>(32) };
/// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
/// let buffer = unsafe {
///     uninitialized_boxed_slice_for_bytes::<MaybeUninit<u32>>(32)
/// };
/// assert_eq!(buffer.len(), 8);
/// ```
///
/// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
#[inline]
pub unsafe fn uninitialized_boxed_slice_for_bytes<T>(bytes: usize) -> Box<[T]>
where
    T: ByteData,
{
    uninitialized_boxed_slice(array_len_for_bytes!(T, bytes))
}

/// Returns a boxed slice of uninitialized data large enough for tracking of
/// at least the specified number of [allocation markers].
///
/// # Safety
///
/// The contents of the boxed slice are left uninitialized; reading from and
/// writing to the slice contents can trigger [undefined behavior] if not
/// careful.
///
/// It is strongly recommended to only allocate slices of
/// [`MaybeUninit`]-wrapped types if supported by the version of Rust used, as
/// using slices of non-wrapped types cause undefined behavior. This function
/// will most likely be updated to enforce this restriction in future major
/// releases of this crate.
///
/// # Examples
///
/// ```
/// use scratchpad::uninitialized_boxed_slice_for_markers;
/// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
/// use std::mem::MaybeUninit;
///
/// # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
/// # let buffer = unsafe {
/// #     uninitialized_boxed_slice_for_markers::<usize>(32)
/// # };
/// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
/// let buffer = unsafe {
///     uninitialized_boxed_slice_for_markers::<MaybeUninit<usize>>(32)
/// };
/// assert_eq!(buffer.len(), 32);
/// ```
///
/// [allocation markers]: trait.Marker.html
/// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
#[inline]
pub unsafe fn uninitialized_boxed_slice_for_markers<T>(
    marker_count: usize,
) -> Box<[T]>
where
    T: ByteData,
{
    uninitialized_boxed_slice(array_len_for_markers!(T, marker_count))
}
