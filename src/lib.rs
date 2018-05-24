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
//!   - [General Usage](#general-usage)
//!   - [Additional Functionality](#additional-functionality)
//!   - [Use Cases](#use-cases)
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
//! ## General Usage
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
//! ## Additional Functionality
//!
//! Aside from general memory management routines, the crate also provides a
//! couple additional features:
//!
//! - **Allocation concatenation.** The [`Allocation::concat()`] method allows
//!   for combining two adjacent allocations of a scalar, array, or slice of a
//!   given type into a single slice allocation.
//! - **Allocation extension.** The [`MarkerFront::append()`] and
//!   [`MarkerBack::prepend()`] methods allow for extending allocations at the
//!   end of their respective stacks with new data. Variants of these methods
//!   also exist for cloning ([`append_clone()`], [`prepend_clone()`]) and
//!   copying ([`append_copy()`], [`prepend_copy()`]) the source values
//!   without moving them into an allocation.
//! - **Slice conversion.** Existing allocations can be converted into a slice
//!   allocation using the [`IntoSliceAllocation`] trait without moving or
//!   altering the contents of the allocation.
//! - **String concatenation.** The [`Marker::concat()`] method takes a
//!   collection of strings and, if enough space is available, returns
//!   an allocation containing a [`str`] slice with the concatenated result.
//!
//! ## Use Cases
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
//! ) -> Result<libfoo::SequenceResult, scratchpad::Error<()>>
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
//! instance itself only contains a pointer to its data, whose size can vary
//! depending on whether a fat pointer is necessary (such as for dynamically
//! sized arrays allocated using one of the `allocate_array*()` marker
//! methods).
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
//! [`Allocation::concat()`]: struct.Allocation.html#method.concat
//! [`append_clone()`]: struct.MarkerFront.html#method.append_clone
//! [`append_copy()`]: struct.MarkerFront.html#method.append_copy
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
//! [`IntoSliceAllocation`]: trait.IntoSliceAllocation.html
//! [`mark_back()`]: struct.Scratchpad.html#method.mark_back
//! [`mark_front()`]: struct.Scratchpad.html#method.mark_front
//! [`Marker`]: trait.Marker.html
//! [`Marker::concat()`]: trait.Marker.html#method.concat
//! [`MarkerBack::prepend()`]: struct.MarkerBack.html#method.prepend
//! [`MarkerFront::append()`]: struct.MarkerFront.html#method.append
//! [`prepend_clone()`]: struct.MarkerBack.html#method.prepend_clone
//! [`prepend_copy()`]: struct.MarkerBack.html#method.prepend_copy
//! [`Scratchpad`]: struct.Scratchpad.html
//! [`Scratchpad::new()`]: struct.Scratchpad.html#method.new
//! [`Scratchpad::static_new()`]: struct.Scratchpad.html#method.static_new
//! [`str`]: https://doc.rust-lang.org/std/primitive.str.html
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

mod allocation;
mod error;
mod marker;
mod scratchpad;
mod traits;
#[macro_use]
mod utility;

pub use allocation::*;
pub use error::*;
pub use marker::*;
pub use scratchpad::*;
pub use traits::*;
pub use utility::*;

#[cfg(test)]
mod tests;
