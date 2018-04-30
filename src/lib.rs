// Copyright 2018 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Stack-like dynamic memory pool with double-ended allocation support.
//!
//! # Table of Contents
//!
//! - [Overview](#overview)
//! - [Crate Features](#crate-features)
//! - [Examples](#examples)
//! - [Memory Management](#memory-management)
//!   - [Buffer Types](#buffer-types)
//!   - [Macros and Functions for Handling Buffers](#macros-and-functions-for-handling-buffers)
//! - [Data Alignment](#data-alignment)
//!   - [`Buffer` and `Tracking` Alignments](#buffer-and-tracking-alignments)
//!   - [Alignment of Allocated Arrays](#alignment-of-allocated-arrays)
//!   - [Cache Alignment](#cache-alignment)
//! - [Memory Overhead](#memory-overhead)
//! - [Limitations](#limitations)
//! - [Mutability Notes](#mutability-notes)
//!
//! # Overview
//!
//! [`Scratchpad`] provides a method for quick and safe dynamic allocations of
//! arbitrary types without relying on the global heap (e.g. using [`Box`] or
//! [`Vec`]). Allocations are made from a fixed-size region of memory in a
//! stack-like fashion using two separate stacks (one for each end of the
//! allocation buffer) to allow different types of allocations with possibly
//! overlapping lifecycles to be made from each end.
//!
//! Groups of allocations are partitioned using [`Marker`] objects. Markers
//! can be set at the [front][`mark_front()`] or [back][`mark_back()`] of a
//! scratchpad. Allocations can be made from either a front or back marker as
//! long as it is the most-recently created active marker of that type (e.g.
//! creating and allocating from a back marker still allows you to allocate
//! from the most recent front marker, but creating a new front marker blocks
//! allocations from previous front markers until the newer front marker is
//! dropped).
//!
//! No memory is actually freed until the marker is dropped (although an
//! item's [`Drop`] implementation is still called if necessary when the
//! allocation is dropped). Markers can be dropped in any order, but the
//! memory is not made available for reuse until any subsequently set markers
//! of the same type (front versus back) have also been dropped. This behavior
//! allows allocations and frees to be performed relatively quickly.
//!
//! Some cases for which [`Scratchpad`] can be useful include:
//!
//! - **Short-term dynamic allocations.** A function may need to allocate a
//!   variable amount of memory for a relatively short amount of time. Rust
//!   currently does not provide anything analogous to the `alloca()` function
//!   in C (which also has its own pitfalls), and using the global heap can be
//!   slow and introduce fragmentation if other allocations are made before
//!   the short-term allocation is freed. Additionally, if each thread has its
//!   own scratchpad, overhead incurred from synchronization between threads
//!   can be eliminated.
//! - **Grouped allocation lifecycles.** Some applications may incorporate
//!   some cycle in which they allocate an arbitrary amount of data that can
//!   be freed all at once after some period of time. One such case is with
//!   video games, where static data for a level may be allocated and persist
//!   only for the duration in which the level is active. Such allocations can
//!   be made by setting a marker for the level and dropping it when the level
//!   is unloaded. Other allocations with overlapping lifecycles, such as
//!   those persisting across levels, or temporary allocations needed while
//!   loading into one end of the scratchpad, can also be set at the other end
//!   of the same scratchpad at the same time.
//!
//! # Crate Features
//!
//! The following optional features can be set when building this crate:
//!
//! - **`std`**: Allows [`Box`] to be used as the storage type for allocations
//!   and marker tracking. Enabled by default; can be disabled to build the
//!   crate with `#![no_std]`.
//! - **`unstable`**: Enables unstable toolchain features (requires a nightly
//!   compiler). Disabled by default. Enabling this feature includes:
//!   - [`ByteData`] trait implementations for `u128`/`i128`.
//!   - Declaration of the function [`Scratchpad::new()`] as `const`.
//!   - Support for using [`Box`] as the storage type for allocations and
//!     marker tracking, regardless of whether the `std` feature is enabled
//!     (`alloc` library is used if `std` is disabled).
//!
//! # Examples
//!
//! Applications can create per-thread scratchpads for temporary allocations
//! using thread-local variables, reducing fragmentation of the global memory
//! heap and improving performance. The following example shows how one might
//! use temporary storage to interface with a third-party library within some
//! abstraction layer:
//!
//! ```
//! #[macro_use]
//! extern crate scratchpad;
//!
//! use scratchpad::{CacheAligned, Scratchpad};
//! use scratchpad::uninitialized_boxed_slice_for_bytes;
//! use std::mem::{size_of, uninitialized};
//! use std::os::raw::c_void;
//!
//! /// Thread-local scratchpad size, in bytes (1 MB).
//! const THREAD_LOCAL_SCRATCHPAD_SIZE: usize = 1024 * 1024;
//! /// Maximum thread-local scratchpad allocation marker count.
//! const THREAD_LOCAL_SCRATCHPAD_MAX_MARKERS: usize = 8;
//!
//! /// Buffer type for thread-local scratchpad allocations. By using
//! /// `CacheAligned`, we avoid false sharing of cache lines between threads.
//! type ThreadLocalScratchBuffer = Box<[CacheAligned]>;
//!
//! /// Buffer type for tracking of thread-local scratchpad markers (each
//! /// marker requires a `usize` value).
//! type ThreadLocalScratchTracking = array_type_for_markers!(
//!     CacheAligned,
//!     THREAD_LOCAL_SCRATCHPAD_MAX_MARKERS,
//! );
//!
//! thread_local! {
//!     /// Thread-local scratchpad. The initial contents of the allocation
//!     /// buffer and marker tracking buffer are ignored, so we can create
//!     /// them as uninitialized.
//!     pub static THREAD_LOCAL_SCRATCHPAD: Scratchpad<
//!         ThreadLocalScratchBuffer,
//!         ThreadLocalScratchTracking,
//!     > = unsafe { Scratchpad::new(
//!         uninitialized_boxed_slice_for_bytes(THREAD_LOCAL_SCRATCHPAD_SIZE),
//!         uninitialized(),
//!     ) };
//! }
//!
//! /// Rust bindings for part of some fictional third-party API written in
//! /// C/C++.
//! pub mod libfoo {
//!     use std::os::raw::c_void;
//!
//!     #[repr(C)]
//!     pub enum SequenceType {
//!         Integer,
//!         Float,
//!         Double,
//!     }
//!
//!     #[derive(Debug, PartialEq)]
//!     #[repr(C)]
//!     pub enum SequenceResult {
//!         Red,
//!         Green,
//!         Blue,
//!     }
//!
//!     #[repr(C)]
//!     pub struct SequenceParameters {
//!         pub data_type: SequenceType,
//!         pub data_count: usize,
//!         pub data: *const c_void,
//!     }
//!
//!     pub unsafe extern "C" fn process_sequences(
//!         sequence_count: usize,
//!         sequences: *const SequenceParameters,
//!     ) -> SequenceResult {
//!         // ...
//! #         SequenceResult::Red
//!     }
//! }
//!
//! /// Our abstraction of `libfoo::process_sequences()`, in which we only
//! /// ever work with `f32` data.
//! pub fn process_float_sequences<I, E, S>(
//!     sequences: I,
//! ) -> Result<libfoo::SequenceResult, scratchpad::Error>
//! where
//!     I: IntoIterator<Item = S, IntoIter = E>,
//!     E: ExactSizeIterator<Item = S>,
//!     S: AsRef<[f32]>,
//! {
//!     THREAD_LOCAL_SCRATCHPAD.with(|scratchpad| {
//!         // We need an array of `libfoo::SequenceParameters` structs for
//!         // the call to `libfoo::process_sequences()`.
//!         let mut sequences = sequences.into_iter();
//!         let sequences_len = sequences.len();
//!
//!         let marker = scratchpad.mark_front()?;
//!         let foo_sequences = marker.allocate_array_with(
//!             sequences_len,
//!             |index| {
//!                 let data = sequences.next().unwrap();
//!                 let data_ref = data.as_ref();
//!                 libfoo::SequenceParameters {
//!                     data_type: libfoo::SequenceType::Float,
//!                     data_count: data_ref.len(),
//!                     data: data_ref.as_ptr() as *const c_void,
//!                 }
//!             }
//!         )?;
//!
//!         Ok(unsafe { libfoo::process_sequences(
//!             sequences_len,
//!             foo_sequences.as_ptr(),
//!         ) })
//!
//!         // The marker is dropped as it goes out of scope, freeing the
//!         // allocated memory.
//!     })
//! }
//!
//! fn main() {
//!     let sequence_a = [2.22f32, 9.99f32, -1234.56f32];
//!     let sequence_b = [-1.0f32, 8.8f32, 27.0f32, 0.03f32];
//!     let sequence_c = [88.0f32];
//!     let sequences = [&sequence_a[..], &sequence_b[..], &sequence_c[..]];
//!     assert_eq!(
//!         process_float_sequences(&sequences).unwrap(),
//!         libfoo::SequenceResult::Red,
//!     );
//! }
//! ```
//!
//! # Memory Management
//!
//! ## Buffer Types
//!
//! The backing data structures used for allocation storage and marker
//! tracking are declared in the generic parameters of the [`Scratchpad`]
//! type. This allows a fair degree of control over the memory usage of the
//! scratchpad itself.
//!
//! The allocation storage type is provided by the `BufferT` generic
//! parameter, which is constrained by the [`Buffer`] trait. Allocation
//! storage must be a contiguous region of memory, such as a static array,
//! boxed slice, or mutable slice reference. Array element types are
//! constrained to types that implement the [`ByteData`] trait; by default,
//! this only includes basic integer types and the [`CacheAligned`] struct
//! provided by this crate.
//!
//! The marker tracking buffer type is provided by the `TrackingT` generic
//! parameter, which is constrained by the [`Tracking`] trait. This type must
//! allow storage and retrieval of a fixed number of `usize` values. Unlike
//! [`Buffer`], there is no restriction on how the values are stored so long
//! as they can be set and subsequently retrieved by index. Any type
//! implementing [`Buffer`] also implements [`Tracking`] by default.
//!
//! ## Macros and Functions for Handling Buffers
//!
//! Writing out the math needed to determine the number of elements needed for
//! an array or boxed slice of a specific byte capacity or marker tracking
//! capacity can be tedious and error-prone. This crate provides some macros
//! and functions to help reduce the amount of work needed for declaring
//! buffers based on their byte capacity or marker capacity:
//!
//! - [`array_type_for_bytes!()`] and [`array_type_for_markers!()`] can be
//!   used to declare static array types based on their capacity.
//! - [`cache_aligned_zeroed_for_bytes!()`] and
//!   [`cache_aligned_zeroed_for_markers!()`] provide shorthand for creating
//!   static arrays of [`CacheAligned`] elements with their contents zeroed
//!   out. The expansion of this macro is a constant expression.
//! - [`uninitialized_boxed_slice_for_bytes()`] and
//!   [`uninitialized_boxed_slice_for_markers()`] can be used to allocate
//!   memory for a boxed slice of a given capacity without initializing its
//!   contents.
//!
//! Some lower level macros and functions are also available if needed:
//!
//! - [`array_len_for_bytes!()`] and [`array_len_for_markers!()`] return the
//!   number of elements needed for a static array with a given capacity. The
//!   results are constant expressions that can be evaluated at compile-time.
//! - [`cache_aligned_zeroed!()`] provides shorthand for creating a
//!   [`CacheAligned`] value with its contents zeroed out.
//! - [`uninitialized_boxed_slice()`] allocates a boxed slice of a given
//!   number of elements without initializing its contents.
//!
//! # Data Alignment
//!
//! [`Scratchpad`] properly handles the alignment requirements of allocated
//! objects by padding the offset of the allocation within the allocation
//! buffer. This can result in some amount of wasted space if mixing
//! allocations of types that have different alignment requirements. This
//! waste can be minimized if necessary by grouping allocations based on their
//! alignment requirements or by using separate scratchpads for different
//! alignments.
//!
//! ## `Buffer` and `Tracking` Alignments
//!
//! The alignment of the allocation buffer and marker tracking themselves are
//! determined by the element type of the corresponding slice or array used,
//! as specified by the generic parameters of the [`Scratchpad`] type and the
//! parameters of [`Scratchpad::new()`]. If the alignment of the buffer itself
//! doesn't match the alignment needed for the first allocation made, the
//! allocation will be offset from the start of the buffer as needed,
//! resulting in wasted space.
//!
//! To avoid this, use an element type for the buffer's slice or array that
//! provides at least the same alignment as that which will be needed for
//! allocations. For most types, a slice or array of `u64` elements should
//! provide sufficient alignment to avoid any initial wasted space.
//!
//! Allocations that require non-standard alignments may require defining a
//! custom [`ByteData`] type with an appropriate `#[repr(align)]` attribute to
//! avoid any wasted space at the start of the allocation buffer. For example,
//! a [`Scratchpad`] guaranteeing a minimum initial alignment of 16 bytes for
//! SIMD allocations can be created as follows:
//!
//! ```
//! use std::mem::uninitialized;
//! use scratchpad::{ByteData, Scratchpad};
//!
//! #[repr(align(16))]
//! struct Aligned16([u32; 4]);
//!
//! unsafe impl ByteData for Aligned16 {}
//!
//! let scratchpad = Scratchpad::<[Aligned16; 1024], [usize; 4]>::new(
//!     unsafe { uninitialized() },
//!     unsafe { uninitialized() },
//! );
//! ```
//!
//! ## Alignment of Allocated Arrays
//!
//! When allocating a dynamically sized array from a [`Marker`][`Marker`]
//! (i.e. using one of the `allocate_array*()` methods), the array is *only*
//! guaranteed to be aligned based on the requirements of the element type.
//! This means that, for example, it is unsafe to use an array of `u8` values
//! as a buffer in which `f32` values will be written to or read from
//! directly. It is strongly recommended that you only use arrays allocated
//! from a marker as the element type specified, or that the array is
//! allocated using an element type whose alignment is at least as large as
//! the data it will contain.
//!
//! ## Cache Alignment
//!
//! Applications may prefer to keep data aligned to cache lines to avoid
//! performance issues (e.g. multiple cache line loads for data crossing cache
//! line boundaries, false sharing). The crate provides a simple data type,
//! [`CacheAligned`], that can be used as the backing type for both allocation
//! buffers and marker tracking. Simply providing an array or slice of
//! [`CacheAligned`] objects instead of a built-in integer type will help
//! ensure cache alignment of the buffer. The size of a single
//! [`CacheAligned`] element will always match its alignment.
//!
//! This crate uses 64 bytes as the assumed cache line alignment, regardless
//! of the build target. While the actual cache line alignment can vary
//! between processors, 64 bytes is generally assumed to be a "safe" target.
//! This value is exported in the [`CACHE_ALIGNMENT`] constant for
//! applications that wish to reference it.
//!
//! # Memory Overhead
//!
//! Creating a marker requires a single `usize` value (4 bytes on platforms
//! using 32-bit pointers, 8 bytes if using 64-bit pointers) within the
//! scratchpad's tracking buffer. When using a slice or array for marker
//! tracking, this memory is allocated up-front, so the footprint of the
//! [`Scratchpad`] does not change after the scratchpad is created.
//!
//! Each [`Marker`] instance itself contains a reference back to its
//! [`Scratchpad`] and its index within the scratchpad. This comes out to 8
//! bytes on platforms with 32-bit pointers and 16 bytes on platforms with
//! 64-bit pointers.
//!
//! Individual allocations have effectively no overhead. Each [`Allocation`]
//! instance itself only contains a reference to the allocated type, whose
//! size can vary depending on whether the allocation is a single item or an
//! array of items.
//!
//! # Limitations
//!
//! - Due to a lack of support in Rust for generically implementing traits for
//!   any size of a static array of a given type, the [`Buffer`] trait (and,
//!   by association, [`Tracking`] trait) is only implemented for a limited
//!   number of array sizes. Mutable slice references and boxed slices do not
//!   have this restriction, so they can be used for unsupported array sizes.
//! - Using large static arrays as buffers can cause the program stack to
//!   overflow while creating a scratchpad, particularly in debug builds.
//!   Using [`Scratchpad::static_new()`] instead of [`Scratchpad::new()`] can
//!   help avoid such issues, and using either boxed slices or slice
//!   references of externally owned arrays for storage can help avoid such
//!   issues entirely.
//!
//! # Mutability Notes
//!
//! [`Scratchpad`] uses internal mutability when allocating and freeing
//! memory, with allocation methods operating on immutable [`Scratchpad`] and
//! [`Marker`] references. This is necessary to cleanly allow for multiple
//! concurrent allocations, as Rust's mutable borrowing restrictions would
//! otherwise prevent such behavior. Allocation and free operations do not
//! have any side effect on other existing allocations, so there are no
//! special considerations necessary by the user.
//!
//! [`Allocation`]: struct.Allocation.html
//! [`array_len_for_bytes!()`]: macro.array_len_for_bytes.html
//! [`array_len_for_markers!()`]: macro.array_len_for_markers.html
//! [`array_type_for_bytes!()`]: macro.array_type_for_bytes.html
//! [`array_type_for_markers!()`]: macro.array_type_for_markers.html
//! [`Box`]: https://doc.rust-lang.org/alloc/boxed/index.html
//! [`Buffer`]: trait.Buffer.html
//! [`ByteData`]: trait.ByteData.html
//! [`cache_aligned_zeroed!()`]: macro.cache_aligned_zeroed.html
//! [`cache_aligned_zeroed_for_bytes!()`]: macro.cache_aligned_zeroed_for_bytes.html
//! [`cache_aligned_zeroed_for_markers!()`]: macro.cache_aligned_zeroed_for_markers.html
//! [`CACHE_ALIGNMENT`]: constant.CACHE_ALIGNMENT.html
//! [`CacheAligned`]: struct.CacheAligned.html
//! [`Drop`]: https://doc.rust-lang.org/core/ops/trait.Drop.html
//! [`mark_back()`]: struct.Scratchpad.html#method.mark_back
//! [`mark_front()`]: struct.Scratchpad.html#method.mark_front
//! [`Marker`]: trait.Marker.html
//! [`Scratchpad`]: struct.Scratchpad.html
//! [`Scratchpad::new()`]: struct.Scratchpad.html#method.new
//! [`Tracking`]: trait.Tracking.html
//! [`uninitialized_boxed_slice()`]: fn.uninitialized_boxed_slice.html
//! [`uninitialized_boxed_slice_for_bytes()`]: fn.uninitialized_boxed_slice_for_bytes.html
//! [`uninitialized_boxed_slice_for_markers()`]: fn.uninitialized_boxed_slice_for_markers.html
//! [`Vec`]: https://doc.rust-lang.org/alloc/vec/index.html

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(all(feature = "unstable", not(feature = "std")), feature(alloc))]
#![cfg_attr(feature = "unstable", feature(const_fn))]

#[cfg(all(feature = "unstable", not(feature = "std")))]
extern crate alloc;
#[cfg(feature = "std")]
extern crate core;

#[cfg(test)]
extern crate arrayvec;

use core::fmt;
use core::ptr;
use core::slice;

use core::cell::{RefCell, UnsafeCell};
use core::marker::PhantomData;
use core::mem::{align_of, forget, uninitialized};
use core::ops::{Deref, DerefMut};

#[cfg(feature = "std")]
use std::boxed::Box;
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(all(feature = "unstable", not(feature = "std")))]
use alloc::boxed::Box;
#[cfg(all(feature = "unstable", not(feature = "std")))]
use alloc::vec::Vec;

// Re-export `size_of()` for easier use with our exported macros.
pub use core::mem::size_of;

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
#[macro_export]
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
#[macro_export]
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
#[macro_export]
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
/// use scratchpad::{CacheAligned, Error, Scratchpad};
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
/// assert_eq!(scratchpad.mark_front().unwrap_err(), Error::MarkerLimit);
/// # }
/// ```
///
/// [allocation markers]: trait.Marker.html
#[macro_export]
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
#[macro_export]
macro_rules! cache_aligned_zeroed_for_bytes {
    ($bytes:expr) => {
        [
            cache_aligned_zeroed!();
            array_len_for_bytes!($crate::CacheAligned, $bytes)
        ]
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
#[macro_export]
macro_rules! cache_aligned_zeroed_for_markers {
    ($marker_count:expr) => {
        [
            cache_aligned_zeroed!();
            array_len_for_markers!($crate::CacheAligned, $marker_count)
        ]
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

/// [`Scratchpad`] allocation errors.
///
/// [`Scratchpad`]: struct.Scratchpad.html
#[derive(Debug, PartialEq)]
pub enum Error {
    /// Maximum number of scratchpad markers are currently set.
    MarkerLimit,
    /// Allocation cannot be made because the marker is not the most-recently
    /// created active marker.
    MarkerLocked,
    /// Insufficient space in the scratchpad buffer for the allocation.
    InsufficientMemory,
    /// Integer overflow detected (typically due to a very large size or
    /// alignment).
    Overflow,
}

impl fmt::Display for Error {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Error::MarkerLimit => {
                write!(f, "scratchpad marker limit reached")
            }
            &Error::MarkerLocked => {
                write!(f, "marker is not the most recent active marker")
            }
            &Error::InsufficientMemory => {
                write!(f, "insufficient allocation buffer space")
            }
            &Error::Overflow => write!(f, "integer overflow"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {
    fn description(&self) -> &str {
        match self {
            &Error::MarkerLimit => "scratchpad marker limit reached",
            &Error::MarkerLocked => {
                "marker is not the most recent active marker"
            }
            &Error::InsufficientMemory => {
                "insufficient allocation buffer space"
            }
            &Error::Overflow => "integer overflow",
        }
    }
}

/// Cache-aligned storage for [`Buffer`] and [`Tracking`] use.
///
/// Internally, this simply wraps a `u8` array to ensure cache alignment.
/// Arrays and slices of this type can be used directly for either
/// [`Scratchpad`] storage or marker tracking.
///
/// The alignment and size of `CacheAligned` are determined by the
/// [`CACHE_ALIGNMENT`] constant.
///
/// [`Buffer`]: trait.Buffer.html
/// [`CACHE_ALIGNMENT`]: constant.CACHE_ALIGNMENT.html
/// [`Scratchpad`]: struct.Scratchpad.html
/// [`Tracking`]: trait.Tracking.html
#[derive(Clone, Copy)]
#[repr(align(64))]
pub struct CacheAligned(pub [u8; CACHE_ALIGNMENT]);

impl fmt::Debug for CacheAligned {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CacheAligned {{ ... }}")
    }
}

/// Trait for types that can be safely used as the backing data type for
/// storage of arbitrary data.
///
/// `ByteData` is implemented by default for all basic integer types as well
/// as the [`CacheAligned`] struct provided by this crate.
///
/// # Safety
///
/// This trait is used to help constrain implementations of the [`Buffer`]
/// trait to known types that are considered "safe" to use as the backing
/// storage type of a buffer. To properly implement this trait, the type
/// should have the following characteristics:
///
/// - Allow arbitrary bytes within instances of the type to be left
///   uninitialized without any possible side effects (outside of attempts to
///   explicitly read those bytes). In particular, types should not have
///   [`Drop`] trait implementations that rely on the data to be in any
///   particular state.
/// - Allow arbitrary bytes within instances of the type to be written to with
///   arbitrary values without affecting other bytes.
/// - Allow previously written bytes to be read back regardless of whether
///   other bytes have been written to yet (only bytes that have been
///   explicitly written to are expected to be read back).
///
/// [`Buffer`]: trait.Buffer.html
/// [`CacheAligned`]: struct.CacheAligned.html
/// [`Drop`]: https://doc.rust-lang.org/core/ops/trait.Drop.html
pub unsafe trait ByteData: Sized {}

unsafe impl ByteData for u8 {}
unsafe impl ByteData for u16 {}
unsafe impl ByteData for u32 {}
unsafe impl ByteData for u64 {}
#[cfg(feature = "nightly")]
unsafe impl ByteData for u128 {}
unsafe impl ByteData for usize {}
unsafe impl ByteData for i8 {}
unsafe impl ByteData for i16 {}
unsafe impl ByteData for i32 {}
unsafe impl ByteData for i64 {}
#[cfg(feature = "nightly")]
unsafe impl ByteData for i128 {}
unsafe impl ByteData for isize {}
unsafe impl ByteData for CacheAligned {}

/// Trait for [`Scratchpad`] buffer types.
///
/// `Buffer` objects contain the memory from which [`Scratchpad`] allocations
/// are made. [`Scratchpad`] handles all bookkeeping, so a buffer only needs
/// to provide methods for raw access of the buffer memory.
///
/// [`Scratchpad`]: struct.Scratchpad.html
pub trait Buffer {
    /// Returns a byte slice of the buffer contents.
    fn as_bytes(&self) -> &[u8];
    /// Returns a mutable byte slice of the buffer contents.
    fn as_bytes_mut(&mut self) -> &mut [u8];
}

/// [`Buffer`] sub-trait for static arrays.
///
/// This trait is used specifically to restrict the implementation of
/// [`Scratchpad::static_new()`] to static array buffers.
///
/// # Safety
///
/// [`Scratchpad::static_new()`] leaves instances of this type **entirely**
/// uninitialized, so implementing it is fundamentally unsafe. It should only
/// be implemented by static arrays of [`ByteData`] types.
///
/// [`Buffer`]: trait.Buffer.html
/// [`ByteData`]: trait.ByteData.html
/// [`Scratchpad::static_new()`]: struct.Scratchpad.html#method.static_new
pub unsafe trait StaticBuffer: Buffer {}

impl<'a, T> Buffer for &'a mut [T]
where
    T: ByteData,
{
    #[inline]
    fn as_bytes(&self) -> &[u8] {
        unsafe {
            slice::from_raw_parts(
                self.as_ptr() as *const u8,
                self.len() * size_of::<T>(),
            )
        }
    }

    #[inline]
    fn as_bytes_mut(&mut self) -> &mut [u8] {
        unsafe {
            slice::from_raw_parts_mut(
                self.as_mut_ptr() as *mut u8,
                self.len() * size_of::<T>(),
            )
        }
    }
}

#[cfg(any(feature = "std", feature = "unstable"))]
impl<T> Buffer for Box<[T]>
where
    T: ByteData,
{
    #[inline]
    fn as_bytes(&self) -> &[u8] {
        let data = self.as_ref();
        unsafe {
            slice::from_raw_parts(
                data.as_ptr() as *const u8,
                data.len() * size_of::<T>(),
            )
        }
    }

    #[inline]
    fn as_bytes_mut(&mut self) -> &mut [u8] {
        let data = self.as_mut();
        unsafe {
            slice::from_raw_parts_mut(
                data.as_mut_ptr() as *mut u8,
                data.len() * size_of::<T>(),
            )
        }
    }
}

/// Macro for generating `Buffer` implementations for static arrays.
macro_rules! generate_buffer_impl {
    ($size:expr) => {
        impl<T> Buffer for [T; $size]
        where
            T: ByteData,
        {
            #[inline]
            fn as_bytes(&self) -> &[u8] {
                let data = &self[..];
                unsafe { slice::from_raw_parts (
                    data.as_ptr() as *const u8,
                    data.len() * size_of::<T>(),
                ) }
            }

            #[inline]
            fn as_bytes_mut(&mut self) -> &mut [u8] {
                let data = &mut self[..];
                unsafe { slice::from_raw_parts_mut (
                    data.as_mut_ptr() as *mut u8,
                    data.len() * size_of::<T>(),
                ) }
            }
        }

        unsafe impl<T> StaticBuffer for [T; $size]
        where
            T: ByteData,
        {}
    };

    ($size:expr, $($other:tt)*) => {
        generate_buffer_impl!($size);
        generate_buffer_impl!($($other)*);
    };

    () => {};
}

generate_buffer_impl!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
generate_buffer_impl!(11, 12, 13, 14, 15, 16, 17, 18, 19, 20);
generate_buffer_impl!(21, 22, 23, 24, 25, 26, 27, 28, 29, 30);
generate_buffer_impl!(31, 32, 33, 34, 35, 36, 37, 38, 39, 40);
generate_buffer_impl!(41, 42, 43, 44, 45, 46, 47, 48, 49, 50);
generate_buffer_impl!(51, 52, 53, 54, 55, 56, 57, 58, 59, 60);
generate_buffer_impl!(61, 62, 63, 64);
generate_buffer_impl!(0x80, 0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000);
generate_buffer_impl!(0x4000, 0x8000, 0x10000, 0x20000, 0x40000, 0x80000);
generate_buffer_impl!(0x100000, 0x200000, 0x400000, 0x800000, 0x1000000);
generate_buffer_impl!(0x2000000, 0x4000000, 0x8000000, 0x10000000);
generate_buffer_impl!(0x20000000, 0x40000000);

/// Trait for [`Scratchpad`] allocation tracking containers.
///
/// Each [`Marker`] is tracked within a [`Scratchpad`] using only a single
/// `usize` value per allocation. Actual storage of such values can be
/// implemented in any manner (memory does not need to be contiguous, for
/// instance).
///
/// [`Scratchpad`] and [`Marker`] will never call [`get()`] for a given index
/// if [`set()`] has not been previously called for the same index, so values
/// can be left uninitialized prior to [`set()`] calls.
///
/// [`get()`]: #method.get.html
/// [`Marker`]: trait.Marker.html
/// [`Scratchpad`]: struct.Scratchpad.html
/// [`set()`]: #method.set.html
pub trait Tracking: Sized {
    /// Returns the total number of allocations that can be stored in this
    /// container.
    fn capacity(&self) -> usize;
    /// Stores a value at the specified index.
    fn set(&mut self, index: usize, value: usize);
    /// Retrieves the value from the specified index.
    fn get(&self, index: usize) -> usize;
}

impl<T> Tracking for T
where
    T: Buffer,
{
    #[inline]
    fn capacity(&self) -> usize {
        self.as_bytes().len() / size_of::<usize>()
    }

    #[inline]
    fn set(&mut self, index: usize, value: usize) {
        let bytes = self.as_bytes_mut();
        let contents = unsafe {
            slice::from_raw_parts_mut(
                bytes.as_mut_ptr() as *mut usize,
                bytes.len() / size_of::<usize>(),
            )
        };
        contents[index] = value;
    }

    #[inline]
    fn get(&self, index: usize) -> usize {
        let bytes = self.as_bytes();
        let contents = unsafe {
            slice::from_raw_parts(
                bytes.as_ptr() as *const usize,
                bytes.len() / size_of::<usize>(),
            )
        };
        contents[index]
    }
}

/// Front and back stacks for `Marker` tracking (used internally).
struct MarkerStacks<TrackingT>
where
    TrackingT: Tracking,
{
    /// Stack data.
    data: TrackingT,
    /// Front stack offset.
    front: usize,
    /// Back stack offset.
    back: usize,
}

impl<TrackingT> fmt::Debug for MarkerStacks<TrackingT>
where
    TrackingT: Tracking,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "MarkerStacks {{ ... }}")
    }
}

/// Scratchpad [`Marker`] allocation.
///
/// Markers implement the [`Deref`] and [`DerefMut`] traits, allowing the data
/// to be dereferenced explicitly using the unary `*` operator (e.g.
/// `*allocation`) or implicitly by the compiler under various circumstances.
///
/// An allocation is statically bound to the lifetime of the [`Marker`] from
/// which it is allocated, ensuring that no dangling references can be left
/// when the [`Marker`] is dropped.
///
/// [`Marker`]: trait.Marker.html
/// [`Deref`]: https://doc.rust-lang.org/core/ops/trait.Deref.html
/// [`DerefMut`]: https://doc.rust-lang.org/core/ops/trait.DerefMut.html
pub struct Allocation<'marker, 't, T>
where
    T: 't + ?Sized,
{
    /// Allocation data.
    data: &'t mut T,
    /// Dummy reference for ensuring the allocation does not outlive the
    /// `Marker` from which it was allocated.
    _phantom: PhantomData<&'marker ()>,
}

impl<'marker, 't, T> Allocation<'marker, 't, T>
where
    T: 't + Sized,
{
    /// Moves the value out of the `Allocation`.
    ///
    /// Note that this is only implemented for [`Sized`] value types.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let x = {
    ///     let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new(
    ///         [0],
    ///         [0],
    ///     );
    ///     let marker = scratchpad.mark_front().unwrap();
    ///     let allocation = marker.allocate(3.14159).unwrap();
    ///
    ///     allocation.unwrap()
    /// };
    ///
    /// // Value was moved out of the allocation, so it can now outlive the
    /// // scratchpad in which it was initially created.
    /// assert_eq!(x, 3.14159);
    /// ```
    ///
    /// [`Sized`]: https://doc.rust-lang.org/core/marker/trait.Sized.html
    pub fn unwrap(self) -> T {
        unsafe {
            let value = ptr::read(self.data);
            forget(self);
            value
        }
    }
}

impl<'marker, 't, T> Deref for Allocation<'marker, 't, T>
where
    T: 't + ?Sized,
{
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        self.data
    }
}

impl<'marker, 't, T> DerefMut for Allocation<'marker, 't, T>
where
    T: 't + ?Sized,
{
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        self.data
    }
}

impl<'marker, 't, T> Drop for Allocation<'marker, 't, T>
where
    T: 't + ?Sized,
{
    #[inline]
    fn drop(&mut self) {
        unsafe { ptr::drop_in_place(self.data) };
    }
}

impl<'marker, 't, T> fmt::Debug for Allocation<'marker, 't, T>
where
    T: 't + ?Sized + fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Allocation {{ data: {:?} }}", self.data)
    }
}

/// [`Scratchpad`] allocation marker implementation trait.
///
/// This provides the shared interface for the [`MarkerFront`] and
/// [`MarkerBack`] types.
///
/// [`MarkerBack`]: struct.MarkerBack.html
/// [`MarkerFront`]: struct.MarkerFront.html
/// [`Scratchpad`]: struct.Scratchpad.html
pub trait Marker {
    /// Allocates space for the given value, moving it into the allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let x = marker.allocate(3.14159).unwrap();
    /// assert_eq!(*x, 3.14159);
    /// ```
    fn allocate<'marker, 't, T>(
        &'marker self,
        value: T,
    ) -> Result<Allocation<'marker, 't, T>, Error> {
        unsafe {
            self.allocate_uninitialized::<T>().map(|allocation| {
                ptr::write(allocation.data, value);
                allocation
            })
        }
    }

    /// Allocates space for a value, initializing it to its default.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let x = marker.allocate_default::<f64>().unwrap();
    /// assert_eq!(*x, 0.0);
    /// ```
    fn allocate_default<'marker, 't, T: Default>(
        &'marker self,
    ) -> Result<Allocation<'marker, 't, T>, Error> {
        self.allocate(Default::default())
    }

    /// Allocates uninitialized space for the given type.
    ///
    /// # Safety
    ///
    /// Since memory for the allocated data is uninitialized, it can
    /// potentially be in an invalid state for a given type, leading to
    /// undefined program behavior. It is recommended that one of the safe
    /// `allocate*()` methods are used instead if possible.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let mut x = unsafe { marker.allocate_uninitialized().unwrap() };
    /// *x = 3.14159;
    /// assert_eq!(*x, 3.14159);
    /// ```
    unsafe fn allocate_uninitialized<'marker, 't, T>(
        &'marker self,
    ) -> Result<Allocation<'marker, 't, T>, Error> {
        let data = self.allocate_memory(align_of::<T>(), size_of::<T>(), 1)?;

        Ok(Allocation {
            data: &mut *(data as *mut T),
            _phantom: PhantomData,
        })
    }

    /// Allocates space for an array, initializing each element with the given
    /// value.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let x = marker.allocate_array(3, 3.14159).unwrap();
    /// assert_eq!(*x, [3.14159, 3.14159, 3.14159]);
    /// ```
    fn allocate_array<'marker, 't, T: Clone>(
        &'marker self,
        len: usize,
        value: T,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        unsafe {
            self.allocate_array_uninitialized(len).map(|allocation| {
                debug_assert_eq!(allocation.data.len(), len);
                for element in allocation.data.iter_mut() {
                    ptr::write(element, value.clone());
                }
                allocation
            })
        }
    }

    /// Allocates space for an array, initializing each element to its default
    /// value.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let x = marker.allocate_array_default::<f64>(3).unwrap();
    /// assert_eq!(*x, [0.0, 0.0, 0.0]);
    /// ```
    fn allocate_array_default<'marker, 't, T: Default>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        unsafe {
            self.allocate_array_uninitialized(len).map(|allocation| {
                debug_assert_eq!(allocation.data.len(), len);
                for element in allocation.data.iter_mut() {
                    ptr::write(element, Default::default());
                }
                allocation
            })
        }
    }

    /// Allocates space for an array, initializing each element with the
    /// result of a function.
    ///
    /// The function `func` takes a single parameter containing the index of
    /// the element being initialized.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let x = marker.allocate_array_with(3, |index| index as f64).unwrap();
    /// assert_eq!(*x, [0.0, 1.0, 2.0]);
    /// ```
    fn allocate_array_with<'marker, 't, T, F: FnMut(usize) -> T>(
        &'marker self,
        len: usize,
        mut func: F,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        unsafe {
            self.allocate_array_uninitialized(len).map(|allocation| {
                debug_assert_eq!(allocation.data.len(), len);
                for (index, element) in allocation.data.iter_mut().enumerate()
                {
                    ptr::write(element, func(index));
                }
                allocation
            })
        }
    }

    /// Allocates uninitialized space for an array of the given type.
    ///
    /// # Safety
    ///
    /// Since memory for the allocated data is uninitialized, it can
    /// potentially be in an invalid state for a given type, leading to
    /// undefined program behavior. It is recommended that one of the safe
    /// `allocate*()` methods are used instead if possible.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let mut x = unsafe {
    ///     marker.allocate_array_uninitialized(3).unwrap()
    /// };
    /// x[0] = 3.14159;
    /// x[1] = 4.14159;
    /// x[2] = 5.14159;
    /// assert_eq!(*x, [3.14159, 4.14159, 5.14159]);
    /// ```
    unsafe fn allocate_array_uninitialized<'marker, 't, T>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        let data =
            self.allocate_memory(align_of::<T>(), size_of::<T>(), len)?;

        Ok(Allocation {
            data: slice::from_raw_parts_mut(data as *mut T, len),
            _phantom: PhantomData,
        })
    }

    /// Allocates a block of memory of a given size and alignment.
    ///
    /// If successful, returns a tuple containing the allocation address and
    /// allocation index.
    ///
    /// **_This is intended primarily for use by the internal implementation
    /// of this trait, and is not safe for use by external code. Its signature
    /// and behavior are not guaranteed to be consistent across versions of
    /// this crate._**
    #[doc(hidden)]
    unsafe fn allocate_memory(
        &self,
        alignment: usize,
        size: usize,
        len: usize,
    ) -> Result<*mut u8, Error>;
}

/// [`Scratchpad`] marker for allocations from the front of the allocation
/// buffer.
///
/// A `MarkerFront` is created when calling the [`mark_front()`] method on a
/// [`Scratchpad`] instance. Object allocations can only be made from the most
/// recently created `MarkerFront` or [`MarkerBack`] that is still active.
///
/// Markers are statically bound to the lifetime of the [`Scratchpad`] from
/// which they are created, ensuring that no dangling references are left when
/// the [`Scratchpad`] is dropped.
///
/// This struct wraps [`Marker`] trait methods to avoid the need to import
/// [`Marker`] into scope.
///
/// [`mark_front()`]: struct.Scratchpad.html#method.mark_front
/// [`Marker`]: trait.Marker.html
/// [`MarkerBack`]: struct.MarkerBack.html
/// [`Scratchpad`]: struct.Scratchpad.html
#[derive(Debug)]
pub struct MarkerFront<'scratchpad, BufferT, TrackingT>
where
    BufferT: 'scratchpad + Buffer,
    TrackingT: 'scratchpad + Tracking,
{
    /// `Scratchpad` in which the allocation is tracked.
    scratchpad: &'scratchpad Scratchpad<BufferT, TrackingT>,
    /// Marker index.
    index: usize,
}

impl<'scratchpad, BufferT, TrackingT> Marker
    for MarkerFront<'scratchpad, BufferT, TrackingT>
where
    BufferT: 'scratchpad + Buffer,
    TrackingT: 'scratchpad + Tracking,
{
    unsafe fn allocate_memory(
        &self,
        alignment: usize,
        size: usize,
        len: usize,
    ) -> Result<*mut u8, Error> {
        // Make sure the marker is the top-most front marker (no allocations
        // are allowed if a more-recently created front marker is still
        // active).
        let mut markers = self.scratchpad.markers.borrow_mut();
        if markers.front != self.index + 1 {
            return Err(Error::MarkerLocked);
        }

        let alignment_mask = alignment - 1;
        debug_assert_eq!(alignment & alignment_mask, 0);

        // Pad the allocation size to match the requested alignment.
        let size = size.checked_add(alignment_mask).ok_or(Error::Overflow)?
            & !alignment_mask;
        let size = size * len;

        // Compute the buffer range needed to accommodate the allocation size
        // and alignment.
        let buffer = (*self.scratchpad.buffer.get()).as_bytes_mut();
        let buffer_end_offset = if markers.back == markers.data.capacity() {
            buffer.len()
        } else {
            markers.data.get(markers.back)
        };

        let buffer_start = buffer.as_mut_ptr() as usize;
        let buffer_end = buffer_start + buffer_end_offset;

        let start = buffer_start + markers.data.get(self.index);
        debug_assert!(start <= buffer_end);

        let start = start.checked_add(alignment_mask).ok_or(Error::Overflow)?
            & !alignment_mask;
        let end = start.checked_add(size).ok_or(Error::Overflow)?;
        if end > buffer_end {
            return Err(Error::InsufficientMemory);
        }

        // Update this marker's offset and return the allocation.
        markers.data.set(self.index, end - buffer_start);

        Ok(start as *mut u8)
    }
}

impl<'scratchpad, BufferT, TrackingT>
    MarkerFront<'scratchpad, BufferT, TrackingT>
where
    BufferT: 'scratchpad + Buffer,
    TrackingT: 'scratchpad + Tracking,
{
    /// Allocates space for the given value, moving it into the allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let x = marker.allocate(3.14159).unwrap();
    /// assert_eq!(*x, 3.14159);
    /// ```
    #[inline(always)]
    pub fn allocate<'marker, 't, T>(
        &'marker self,
        value: T,
    ) -> Result<Allocation<'marker, 't, T>, Error> {
        Marker::allocate(self, value)
    }

    /// Allocates space for a value, initializing it to its default.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let x = marker.allocate_default::<f64>().unwrap();
    /// assert_eq!(*x, 0.0);
    /// ```
    #[inline(always)]
    pub fn allocate_default<'marker, 't, T: Default>(
        &'marker self,
    ) -> Result<Allocation<'marker, 't, T>, Error> {
        Marker::allocate_default(self)
    }

    /// Allocates uninitialized space for the given type.
    ///
    /// # Safety
    ///
    /// Since memory for the allocated data is uninitialized, it can
    /// potentially be in an invalid state for a given type, leading to
    /// undefined program behavior. It is recommended that one of the safe
    /// `allocate*()` methods are used instead if possible.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let mut x = unsafe { marker.allocate_uninitialized().unwrap() };
    /// *x = 3.14159;
    /// assert_eq!(*x, 3.14159);
    /// ```
    #[inline(always)]
    pub unsafe fn allocate_uninitialized<'marker, 't, T>(
        &'marker self,
    ) -> Result<Allocation<'marker, 't, T>, Error> {
        Marker::allocate_uninitialized(self)
    }

    /// Allocates space for an array, initializing each element with the given
    /// value.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let x = marker.allocate_array(3, 3.14159).unwrap();
    /// assert_eq!(*x, [3.14159, 3.14159, 3.14159]);
    /// ```
    #[inline(always)]
    pub fn allocate_array<'marker, 't, T: Clone>(
        &'marker self,
        len: usize,
        value: T,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        Marker::allocate_array(self, len, value)
    }

    /// Allocates space for an array, initializing each element to its default
    /// value.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let x = marker.allocate_array_default::<f64>(3).unwrap();
    /// assert_eq!(*x, [0.0, 0.0, 0.0]);
    /// ```
    #[inline(always)]
    pub fn allocate_array_default<'marker, 't, T: Default>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        Marker::allocate_array_default(self, len)
    }

    /// Allocates space for an array, initializing each element with the
    /// result of a function.
    ///
    /// The function `func` takes a single parameter containing the index of
    /// the element being initialized.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let x = marker.allocate_array_with(3, |index| index as f64).unwrap();
    /// assert_eq!(*x, [0.0, 1.0, 2.0]);
    /// ```
    #[inline(always)]
    pub fn allocate_array_with<'marker, 't, T, F: FnMut(usize) -> T>(
        &'marker self,
        len: usize,
        func: F,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        Marker::allocate_array_with(self, len, func)
    }

    /// Allocates uninitialized space for an array of the given type.
    ///
    /// # Safety
    ///
    /// Since memory for the allocated data is uninitialized, it can
    /// potentially be in an invalid state for a given type, leading to
    /// undefined program behavior. It is recommended that one of the safe
    /// `allocate*()` methods are used instead if possible.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let mut x = unsafe {
    ///     marker.allocate_array_uninitialized(3).unwrap()
    /// };
    /// x[0] = 3.14159;
    /// x[1] = 4.14159;
    /// x[2] = 5.14159;
    /// assert_eq!(*x, [3.14159, 4.14159, 5.14159]);
    /// ```
    #[inline(always)]
    pub unsafe fn allocate_array_uninitialized<'marker, 't, T>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        Marker::allocate_array_uninitialized(self, len)
    }
}

impl<'scratchpad, BufferT, TrackingT> Drop
    for MarkerFront<'scratchpad, BufferT, TrackingT>
where
    BufferT: 'scratchpad + Buffer,
    TrackingT: 'scratchpad + Tracking,
{
    fn drop(&mut self) {
        let mut markers = self.scratchpad.markers.borrow_mut();

        let mut front = markers.front;
        debug_assert!(self.index < front);
        let mut last_index = front - 1;
        if self.index < last_index {
            // Markers created after this marker still exist, so flag it as
            // unused so it can be freed later.
            markers.data.set(self.index, core::usize::MAX);
            return;
        }

        // Pop the marker entry off the marker stack as well all other unused
        // marker slots at the end of the stack.
        loop {
            front = last_index;
            if front == 0 {
                break;
            }

            last_index -= 1;
            if markers.data.get(last_index) != core::usize::MAX {
                break;
            }
        }

        markers.front = front;
    }
}

/// [`Scratchpad`] marker for allocations from the back of the allocation
/// buffer.
///
/// A `MarkerBack` is created when calling the [`mark_back()`] method on a
/// [`Scratchpad`] instance. Object allocations can only be made from the most
/// recently created [`MarkerFront`] or `MarkerBack` that is still active.
///
/// Markers are statically bound to the lifetime of the [`Scratchpad`] from
/// which they are created, ensuring that no dangling references are left when
/// the [`Scratchpad`] is dropped.
///
/// This struct wraps [`Marker`] trait methods to avoid the need to import
/// [`Marker`] into scope.
///
/// [`mark_back()`]: struct.Scratchpad.html#method.mark_back
/// [`Marker`]: trait.Marker.html
/// [`MarkerFront`]: struct.MarkerFront.html
/// [`Scratchpad`]: struct.Scratchpad.html
#[derive(Debug)]
pub struct MarkerBack<'scratchpad, BufferT, TrackingT>
where
    BufferT: 'scratchpad + Buffer,
    TrackingT: 'scratchpad + Tracking,
{
    /// `Scratchpad` in which the allocation is tracked.
    scratchpad: &'scratchpad Scratchpad<BufferT, TrackingT>,
    /// Marker index.
    index: usize,
}

impl<'scratchpad, BufferT, TrackingT> Marker
    for MarkerBack<'scratchpad, BufferT, TrackingT>
where
    BufferT: 'scratchpad + Buffer,
    TrackingT: 'scratchpad + Tracking,
{
    unsafe fn allocate_memory(
        &self,
        alignment: usize,
        size: usize,
        len: usize,
    ) -> Result<*mut u8, Error> {
        // Make sure the marker is the bottom-most back marker (no allocations
        // are allowed if a more-recently created back marker is still
        // active).
        let mut markers = self.scratchpad.markers.borrow_mut();
        if markers.back != self.index {
            return Err(Error::MarkerLocked);
        }

        let alignment_mask = alignment - 1;
        debug_assert_eq!(alignment & alignment_mask, 0);

        // Pad the allocation size to match the requested alignment.
        let size = size.checked_add(alignment_mask).ok_or(Error::Overflow)?
            & !alignment_mask;
        let size = size * len;

        // Compute the buffer range needed to accommodate the allocation size
        // and alignment.
        let buffer = (*self.scratchpad.buffer.get()).as_bytes_mut();
        let buffer_start = buffer.as_mut_ptr() as usize;
        let buffer_end = buffer_start + markers.data.get(self.index);

        let start_min = if markers.front == 0 {
            buffer_start
        } else {
            buffer_start + markers.data.get(markers.front - 1)
        };

        debug_assert!(start_min <= buffer_end);

        let start = buffer_end.checked_sub(size).ok_or(Error::Overflow)?
            & !alignment_mask;
        if start < start_min {
            return Err(Error::InsufficientMemory);
        }

        // Update this marker's offset and return the allocation.
        markers.data.set(self.index, start - buffer_start);

        Ok(start as *mut u8)
    }
}

impl<'scratchpad, BufferT, TrackingT>
    MarkerBack<'scratchpad, BufferT, TrackingT>
where
    BufferT: 'scratchpad + Buffer,
    TrackingT: 'scratchpad + Tracking,
{
    /// Allocates space for the given value, moving it into the allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let x = marker.allocate(3.14159).unwrap();
    /// assert_eq!(*x, 3.14159);
    /// ```
    #[inline(always)]
    pub fn allocate<'marker, 't, T>(
        &'marker self,
        value: T,
    ) -> Result<Allocation<'marker, 't, T>, Error> {
        Marker::allocate(self, value)
    }

    /// Allocates space for a value, initializing it to its default.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let x = marker.allocate_default::<f64>().unwrap();
    /// assert_eq!(*x, 0.0);
    /// ```
    #[inline(always)]
    pub fn allocate_default<'marker, 't, T: Default>(
        &'marker self,
    ) -> Result<Allocation<'marker, 't, T>, Error> {
        Marker::allocate_default(self)
    }

    /// Allocates uninitialized space for the given type.
    ///
    /// # Safety
    ///
    /// Since memory for the allocated data is uninitialized, it can
    /// potentially be in an invalid state for a given type, leading to
    /// undefined program behavior. It is recommended that one of the safe
    /// `allocate*()` methods are used instead if possible.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let mut x = unsafe { marker.allocate_uninitialized().unwrap() };
    /// *x = 3.14159;
    /// assert_eq!(*x, 3.14159);
    /// ```
    #[inline(always)]
    pub unsafe fn allocate_uninitialized<'marker, 't, T>(
        &'marker self,
    ) -> Result<Allocation<'marker, 't, T>, Error> {
        Marker::allocate_uninitialized(self)
    }

    /// Allocates space for an array, initializing each element with the given
    /// value.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let x = marker.allocate_array(3, 3.14159).unwrap();
    /// assert_eq!(*x, [3.14159, 3.14159, 3.14159]);
    /// ```
    #[inline(always)]
    pub fn allocate_array<'marker, 't, T: Clone>(
        &'marker self,
        len: usize,
        value: T,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        Marker::allocate_array(self, len, value)
    }

    /// Allocates space for an array, initializing each element to its default
    /// value.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let x = marker.allocate_array_default::<f64>(3).unwrap();
    /// assert_eq!(*x, [0.0, 0.0, 0.0]);
    /// ```
    #[inline(always)]
    pub fn allocate_array_default<'marker, 't, T: Default>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        Marker::allocate_array_default(self, len)
    }

    /// Allocates space for an array, initializing each element with the
    /// result of a function.
    ///
    /// The function `func` takes a single parameter containing the index of
    /// the element being initialized.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let x = marker.allocate_array_with(3, |index| index as f64).unwrap();
    /// assert_eq!(*x, [0.0, 1.0, 2.0]);
    /// ```
    #[inline(always)]
    pub fn allocate_array_with<'marker, 't, T, F: FnMut(usize) -> T>(
        &'marker self,
        len: usize,
        func: F,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        Marker::allocate_array_with(self, len, func)
    }

    /// Allocates uninitialized space for an array of the given type.
    ///
    /// # Safety
    ///
    /// Since memory for the allocated data is uninitialized, it can
    /// potentially be in an invalid state for a given type, leading to
    /// undefined program behavior. It is recommended that one of the safe
    /// `allocate*()` methods are used instead if possible.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::new([0; 3], [0]);
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let mut x = unsafe {
    ///     marker.allocate_array_uninitialized(3).unwrap()
    /// };
    /// x[0] = 3.14159;
    /// x[1] = 4.14159;
    /// x[2] = 5.14159;
    /// assert_eq!(*x, [3.14159, 4.14159, 5.14159]);
    /// ```
    #[inline(always)]
    pub unsafe fn allocate_array_uninitialized<'marker, 't, T>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, 't, [T]>, Error> {
        Marker::allocate_array_uninitialized(self, len)
    }
}

impl<'scratchpad, BufferT, TrackingT> Drop
    for MarkerBack<'scratchpad, BufferT, TrackingT>
where
    BufferT: 'scratchpad + Buffer,
    TrackingT: 'scratchpad + Tracking,
{
    fn drop(&mut self) {
        let mut markers = self.scratchpad.markers.borrow_mut();

        let mut back = markers.back;
        debug_assert!(self.index >= back);
        if self.index > back {
            // Markers created after this marker still exist, so flag it as
            // unused so it can be freed later.
            markers.data.set(self.index, core::usize::MAX);
            return;
        }

        // Pop the marker entry off the marker stack as well all other unused
        // marker slots at the end of the stack.
        let capacity = markers.data.capacity();
        loop {
            back += 1;
            if back == capacity {
                break;
            }

            if markers.data.get(back) != core::usize::MAX {
                break;
            }
        }

        markers.back = back;
    }
}

/// Stack-like dynamic memory pool with double-ended allocation support.
///
/// `Scratchpad` manages dynamic allocations from a fixed-size region of
/// memory in a stack-like manner. Allocations can be made simultaneously from
/// either the "front" or "back" of the scratchpad by setting a [`Marker`]
/// using either [`mark_front()`][`mark_front()`] (returning a
/// [`MarkerFront`]) or [`mark_back()`][`mark_back()`] (returning a
/// [`MarkerBack`]). Multiple markers can be set, but only the most-recently
/// set marker of a given type that is still active can be used to allocate
/// objects.
///
/// Individual allocations can be made from the marker, but no memory is
/// actually freed back into the pool until the marker is dropped, where all
/// the memory allocated through the marker is released at once. If the marker
/// is not the most-recently set active marker of its type, its memory will
/// simply be flagged as unused until all markers of the same type that were
/// created after it are also dropped, at which point the memory will be once
/// again made available for allocations.
///
/// `Scratchpad`, [`Marker`] implementations, and [`Allocation`] all make use
/// of static lifetimes to ensure that an object cannot be used after the
/// object from which it was created is dropped (an allocation cannot outlive
/// its marker, and a marker cannot outlive its scratchpad).
///
/// *See also the [crate-level documentation](index.html) for more detailed
/// information about how `Scratchpad` works and can be used.*
///
/// [`Allocation`]: struct.Allocation.html
/// [`mark_back()`]: #method.mark_back
/// [`mark_front()`]: #method.mark_front
/// [`Marker`]: trait.Marker.html
/// [`MarkerBack`]: struct.MarkerBack.html
/// [`MarkerFront`]: struct.MarkerFront.html
pub struct Scratchpad<BufferT, TrackingT>
where
    BufferT: Buffer,
    TrackingT: Tracking,
{
    /// Buffer from which allocations are made.
    buffer: UnsafeCell<BufferT>,
    /// Dual stack containing the offsets of each active marker. If a marker
    /// not at the end of one of the stacks is freed, its offset is set to
    /// `core::usize::MAX` to indicate it is no longer active until the
    /// allocations that came after it (in the same stack) have also been
    /// freed.
    markers: RefCell<MarkerStacks<TrackingT>>,
}

impl<BufferT, TrackingT> Scratchpad<BufferT, TrackingT>
where
    BufferT: Buffer,
    TrackingT: Tracking,
{
    /// Creates a new scratchpad instance.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate scratchpad;
    /// use scratchpad::Scratchpad;
    /// use std::mem::uninitialized;
    ///
    /// # fn main() {
    /// // Creates a scratchpad that can hold up to 256 bytes of data and up
    /// // to 4 allocation markers. The initial contents of each buffer are
    /// // ignored, so we can provide uninitialized data in order to reduce
    /// // the runtime overhead of creating a scratchpad.
    /// let scratchpad = unsafe { Scratchpad::new(
    ///     uninitialized::<array_type_for_bytes!(u64, 256)>(),
    ///     uninitialized::<array_type_for_markers!(usize, 4)>(),
    /// ) };
    /// # }
    /// ```
    #[inline(always)]
    #[cfg(feature = "unstable")]
    pub const fn new(buffer: BufferT, tracking: TrackingT) -> Self {
        Scratchpad {
            buffer: UnsafeCell::new(buffer),
            markers: RefCell::new(MarkerStacks {
                data: tracking,
                front: 0,
                back: core::usize::MAX, // Lazy initialization.
            }),
        }
    }

    /// Creates a new scratchpad instance.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate scratchpad;
    /// use scratchpad::Scratchpad;
    /// use std::mem::uninitialized;
    ///
    /// # fn main() {
    /// // Creates a scratchpad that can hold up to 256 bytes of data and up
    /// // to 4 allocation markers. The initial contents of each buffer are
    /// // ignored, so we can provide uninitialized data in order to reduce
    /// // the runtime overhead of creating a scratchpad.
    /// let scratchpad = unsafe { Scratchpad::new(
    ///     uninitialized::<array_type_for_bytes!(u64, 256)>(),
    ///     uninitialized::<array_type_for_markers!(usize, 4)>(),
    /// ) };
    /// # }
    /// ```
    #[inline(always)]
    #[cfg(not(feature = "unstable"))]
    pub fn new(buffer: BufferT, tracking: TrackingT) -> Self {
        Scratchpad {
            buffer: UnsafeCell::new(buffer),
            markers: RefCell::new(MarkerStacks {
                back: tracking.capacity(),
                data: tracking,
                front: 0,
            }),
        }
    }
}

impl<BufferT, TrackingT> Scratchpad<BufferT, TrackingT>
where
    BufferT: StaticBuffer,
    TrackingT: Tracking + StaticBuffer,
{
    /// Creates a new instance for scratchpad types backed entirely by static
    /// arrays without initializing array memory.
    ///
    /// Since static array [`Buffer`] and [`Tracking`] types are owned by the
    /// scratchpad, and their sizes are known ahead of time to the scratchpad
    /// type, scratchpads using only static arrays for storage can be created
    /// without having to provide any parameters.
    ///
    /// Note that this cannot be `const` as there is no support in Rust for
    /// creating uninitialized values in `const` code. [`Scratchpad::new()`]
    /// must be used with the `unstable` crate feature enabled if `const` code
    /// is required.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate scratchpad;
    /// use scratchpad::Scratchpad;
    ///
    /// # fn main() {
    /// // Creates a scratchpad that can hold up to 256 bytes of data and up
    /// // to 4 allocation markers.
    /// let scratchpad = Scratchpad::<
    ///     array_type_for_bytes!(u64, 256),
    ///     array_type_for_markers!(usize, 4),
    /// >::static_new();
    /// # }
    /// ```
    ///
    /// [`Buffer`]: trait.Buffer.html
    /// [`Tracking`]: trait.Tracking.html
    /// [`Scratchpad::new()`]: #method.new
    #[inline(always)]
    pub fn static_new() -> Self
    {
        Scratchpad {
            buffer: unsafe { uninitialized() },
            markers: RefCell::new(MarkerStacks {
                data: unsafe { uninitialized() },
                front: 0,
                back: size_of::<TrackingT>() / size_of::<usize>(),
            }),
        }
    }
}

impl<BufferT, TrackingT> Scratchpad<BufferT, TrackingT>
where
    BufferT: Buffer,
    TrackingT: Tracking,
{
    /// Creates a marker at the front of the allocation buffer for subsequent
    /// allocations.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    ///
    /// let marker = scratchpad.mark_front().unwrap();
    /// // `marker` can now be used for allocations...
    /// ```
    pub fn mark_front<'scratchpad>(
        &'scratchpad self,
    ) -> Result<MarkerFront<'scratchpad, BufferT, TrackingT>, Error> {
        let mut markers = self.markers.borrow_mut();

        // `markers.back` is lazy-initialized when the "unstable" feature is
        // enabled so that `Scratchpad::new()` can be a `const` function.
        #[cfg(feature = "unstable")]
        {
            if markers.back == core::usize::MAX {
                markers.back = markers.data.capacity();
            }
        }

        let index = markers.front;
        if index == markers.back {
            return Err(Error::MarkerLimit);
        }

        let buffer_offset = if index == 0 {
            0
        } else {
            markers.data.get(index - 1)
        };
        markers.data.set(index, buffer_offset);
        markers.front = index + 1;

        Ok(MarkerFront {
            scratchpad: self,
            index,
        })
    }

    /// Creates a marker at the back of the allocation buffer for subsequent
    /// allocations.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 1], [usize; 1]>::new([0], [0]);
    ///
    /// let marker = scratchpad.mark_back().unwrap();
    /// // `marker` can now be used for allocations...
    /// ```
    pub fn mark_back<'scratchpad>(
        &'scratchpad self,
    ) -> Result<MarkerBack<'scratchpad, BufferT, TrackingT>, Error> {
        let mut markers = self.markers.borrow_mut();

        // `markers.back` is lazy-initialized when the "unstable" feature is
        // enabled so that `Scratchpad::new()` can be a `const` function.
        #[cfg(feature = "unstable")]
        {
            if markers.back == core::usize::MAX {
                markers.back = markers.data.capacity();
            }
        }

        let mut index = markers.back;
        if index == markers.front {
            return Err(Error::MarkerLimit);
        }

        let buffer_offset = if index == markers.data.capacity() {
            unsafe { (*self.buffer.get()).as_bytes().len() }
        } else {
            markers.data.get(index)
        };
        index -= 1;
        markers.data.set(index, buffer_offset);
        markers.back = index;

        Ok(MarkerBack {
            scratchpad: self,
            index,
        })
    }
}

impl<BufferT, TrackingT> fmt::Debug for Scratchpad<BufferT, TrackingT>
where
    BufferT: Buffer,
    TrackingT: Tracking,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Scratchpad {{ buffer.len = {}, markers: {:?} }}",
            unsafe { &*self.buffer.get() }.as_bytes().len(),
            self.markers.borrow(),
        )
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
/// # Examples
///
/// ```
/// use scratchpad::uninitialized_boxed_slice;
///
/// let buffer = unsafe { uninitialized_boxed_slice::<u32>(32) };
/// assert_eq!(buffer.len(), 32);
/// ```
///
/// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
#[cfg(any(feature = "std", feature = "unstable"))]
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
/// # Examples
///
/// ```
/// use scratchpad::uninitialized_boxed_slice_for_bytes;
///
/// let buffer = unsafe { uninitialized_boxed_slice_for_bytes::<u32>(32) };
/// assert_eq!(buffer.len(), 8);
/// ```
///
/// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
#[cfg(any(feature = "std", feature = "unstable"))]
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
/// # Examples
///
/// ```
/// use scratchpad::uninitialized_boxed_slice_for_markers;
///
/// let buffer = unsafe {
///     uninitialized_boxed_slice_for_markers::<usize>(32)
/// };
/// assert_eq!(buffer.len(), 32);
/// ```
///
/// [allocation markers]: trait.Marker.html
/// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
#[cfg(any(feature = "std", feature = "unstable"))]
pub unsafe fn uninitialized_boxed_slice_for_markers<T>(
    marker_count: usize,
) -> Box<[T]>
where
    T: ByteData,
{
    uninitialized_boxed_slice(array_len_for_markers!(T, marker_count))
}

#[cfg(test)]
mod tests;
