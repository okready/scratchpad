// Copyright 2018-2021 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Stack-like memory allocator with double-ended allocation support.
//!
//! # Table of Contents
//!
//! - [Overview](#overview)
//! - [Basic Walkthrough](#basic-walkthrough)
//!   - [Allocating Pool Storage](#allocating-pool-storage)
//!   - [Creating the Scratchpad Instance](#creating-the-scratchpad-instance)
//!   - [Creating Allocation Markers](#creating-allocation-markers)
//!   - [Allocating Data](#allocating-data)
//!   - [Freeing Memory](#freeing-memory)
//! - [Additional Operations](#additional-operations)
//! - [Optional Crate Features](#optional-crate-features)
//! - [Implementation Notes](#implementation-notes)
//!   - [Memory Management](#memory-management)
//!     - [Buffer Types](#buffer-types)
//!     - [Macros and Functions for Handling Buffers](#macros-and-functions-for-handling-buffers)
//!   - [Data Alignment](#data-alignment)
//!     - [`Buffer` and `Tracking` Alignments](#buffer-and-tracking-alignments)
//!     - [Alignment of Allocated Arrays](#alignment-of-allocated-arrays)
//!     - [Cache Alignment](#cache-alignment)
//!   - [Memory Overhead](#memory-overhead)
//!   - [Limitations](#limitations)
//!   - [Mutability Notes](#mutability-notes)
//! - [Known Issues](#known-issues)
//! - [Example - Temporary Thread-local Allocations](#example---temporary-thread-local-allocations)
//!
//! # Overview
//!
//! [`Scratchpad`] provides a method for quick and safe dynamic allocations of
//! arbitrary types without relying on the global heap (e.g. using [`Box`] or
//! [`Vec`]). Allocations are made from a fixed-size region of memory in a
//! stack-like fashion using two separate stacks (one for each end of the
//! allocation buffer) to allow different types of allocations with
//! independent lifecycles to be made from each end.
//!
//! Such allocators are commonly used in game development, but are also useful
//! in general for short-lived allocations or groups of allocations that share
//! a common lifetime. While not quite as flexible as heap allocations,
//! allocations from a stack allocator are usually much faster and are
//! isolated from the rest of the heap, reducing memory fragmentation.
//!
//! In addition to general allocation operations, this crate also allows for
//! adding to existing allocations and concatenating adjacent allocations.
//!
//! # Basic Walkthrough
//!
//! The following is a step-by-step example of how to create a scratchpad and
//! use it for basic allocation operations.
//!
//! ## Allocating Pool Storage
//!
//! A scratchpad instance relies on two buffers of memory:
//!
//! - The pool from which memory is allocated.
//! - A separate buffer for tracking allocation "markers". A marker defines a
//!   context in which a group of allocations is made. Objects are allocated
//!   through markers, and memory is only ever truly "freed" back to the pool
//!   when the marker is dropped.
//!
//! To keep things flexible, the user is required to provide either a static
//! array, boxed slice, or borrowed slice reference for each. Any basic
//! integer type (`u8`, `i64`, etc.) can be used as the array or slice element
//! type, as well as the [`CacheAligned`] type provided by this crate if
//! cache-aligned memory is desired.
//!
//! A handful of [macros and
//! functions](#macros-and-functions-for-handling-buffers) are provided to
//! help with defining buffers with specific storage requirements. Macros and
//! functions related specifically to allocation pool storage are suffixed
//! with "`_for_bytes`", while macros and functions related specifically to
//! marker tracking storage are suffixed with "`_for_markers`". Here, we use
//! [`uninitialized_boxed_slice_for_bytes()`] and
//! [`uninitialized_boxed_slice_for_markers()`] to create two boxed slices of
//! cache-aligned memory for our allocation pool and tracking buffer.
//!
//! ```
//! use scratchpad::uninitialized_boxed_slice_for_bytes;
//! use scratchpad::uninitialized_boxed_slice_for_markers;
//! use scratchpad::CacheAligned;
//! # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
//! use std::mem::MaybeUninit;
//!
//! const POOL_SIZE: usize = 1usize * 1024 * 1024; // 1 MB
//! const MARKERS_MAX: usize = 16;
//!
//! # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
//! # let pool = unsafe {
//! #     uninitialized_boxed_slice_for_bytes::<CacheAligned>(POOL_SIZE)
//! # };
//! # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
//! # let tracking = unsafe {
//! #     uninitialized_boxed_slice_for_markers::<CacheAligned>(MARKERS_MAX)
//! # };
//! # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
//! let pool = unsafe {
//!     uninitialized_boxed_slice_for_bytes::<MaybeUninit<CacheAligned>>(
//!         POOL_SIZE
//!     )
//! };
//! # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
//! let tracking = unsafe {
//!     uninitialized_boxed_slice_for_markers::<MaybeUninit<CacheAligned>>(
//!         MARKERS_MAX
//!     )
//! };
//! ```
//!
//! ## Creating the Scratchpad Instance
//!
//! Now that we've defined the storage that our scratchpad will use, we can
//! create a [`Scratchpad`] instance using this storage. Simply call
//! [`Scratchpad::new()`], passing the allocation pool and marker tracking
//! buffer as arguments.
//!
//! ```
//! use scratchpad::Scratchpad;
//!
//! # let pool = [0usize; 1];
//! # let tracking = [0usize; 1];
//! let scratchpad = Scratchpad::new(pool, tracking);
//! ```
//!
//! If you decide to use static arrays for both allocation storage and marker
//! tracking, [`Scratchpad::static_new()`] can be used to create the
//! scratchpad without having to construct the arrays separately.
//!
//! Note using large static array buffers can cause the program to run
//! out of stack space when using [`new()`][`Scratchpad::new()`] or
//! [`static_new()`][`Scratchpad::static_new()`].
//! [`Scratchpad::static_new_in_place()`] provides the ability to create a
//! scratchpad within a specific block of (uninitialized) memory while
//! guaranteeing that the allocation and marker tracking buffers never touch
//! the stack. Keep in mind that this function is `unsafe`, so you may need to
//! use either boxed slices or slices of externally owned arrays instead of
//! static arrays if you wish to avoid `unsafe` code.
//!
//! ## Creating Allocation Markers
//!
//! When we're ready to allocate from the scratchpad, we must first create a
//! [`Marker`]. A marker defines a context in which allocations will be made.
//! Multiple allocations can be made from a single marker, but the memory
//! isn't freed back to the scratchpad until the marker is dropped, even if
//! the individual allocations are dropped. This helps keep things fast, as
//! the scratchpad only needs to perform a limited amount of tracking of
//! allocated memory.
//!
//! Markers can be created from either the "front" or "back" of a scratchpad.
//! Front markers, created using [`Scratchpad::mark_front()`], allocate memory
//! from the start of the allocation pool upward, while back markers, created
//! using [`Scratchpad::mark_back()`], allocate memory from the end of the
//! allocation pool downward. The stacks of such allocations are handled
//! separately, so front markers don't affect back markers and vice-versa
//! (with the exception of how much space is still available in the
//! scratchpad).
//!
//! Here, we create a front marker for subsequent allocations.
//!
//! ```
//! # use scratchpad::Scratchpad;
//! # let scratchpad = Scratchpad::<[usize; 1], [usize; 1]>::static_new();
//! let marker = scratchpad.mark_front().unwrap();
//! ```
//!
//! ## Allocating Data
//!
//! Each marker provides a variety of allocation functions, allowing
//! allocations of single elements or dynamically sized arrays of any type.
//! Allocation functions can be found in the [`Marker`] docs, and are prefixed
//! with "`allocate_`" (most `Marker` trait functions are also wrapped by
//! methods of the [`MarkerFront`] and [`MarkerBack`] types as well, so you
//! don't need to import the `Marker` trait into scope to use them).
//!
//! Allocations are wrapped in an [`Allocation`] instance. `Allocation`
//! implements [`Deref`] and [`DerefMut`], allowing access to the wrapped data
//! either explicitly using the unary `*` operator (e.g. `*allocation`) or
//! implicitly, such as when calling methods provided by the allocated data
//! type (e.g. `allocation.len()` for retrieving the number of elements in an
//! `Allocation<[i32]>`). Allocations also implement [`StableDeref`], allowing
//! them to be used with crates that support the trait such as [`owning_ref`]
//! and [`rental`].
//!
//! ```
//! # use scratchpad::Scratchpad;
//! # let scratchpad = Scratchpad::<[usize; 16], [usize; 1]>::static_new();
//! # let marker = scratchpad.mark_front().unwrap();
//! // Allocate a single `f32`.
//! let f32_allocation = marker.allocate(3.14159f32).unwrap();
//! assert_eq!(*f32_allocation, 3.14159f32);
//!
//! // Allocate an array of `i32` values.
//! let i32_array_allocation = marker
//!     .allocate_array_with(5, |index| index as i32 * 2)
//!     .unwrap();
//! assert_eq!(*i32_array_allocation, [0, 2, 4, 6, 8]);
//! ```
//!
//! One thing to note is that allocations can only be made from the most
//! recently created marker belonging to a given stack ("front" or "back"). A
//! marker can be used again for allocations once any more-recent markers from
//! the same stack are dropped. Creating a new front marker doesn't prevent
//! allocations from being made from any back markers, and vice-versa.
//!
//! ```
//! # use scratchpad::Scratchpad;
//! # let scratchpad = Scratchpad::<[usize; 16], [usize; 2]>::static_new();
//! // Allocations can be made from a marker immediately after it's created...
//! let first_marker = scratchpad.mark_front().unwrap();
//! let result = first_marker.allocate(3.14159f32);
//! assert!(result.is_ok());
//!
//! {
//!     // ...but creating a second marker will prevent any new allocations
//!     // from being made using the first marker...
//!     let second_marker = scratchpad.mark_front().unwrap();
//!     let result = first_marker.allocate([1, 2, 3, 4, 5]);
//!     assert!(result.is_err());
//!
//! } // ...until the second marker is dropped (such as when going out of
//!   // scope here).
//!
//! let result = first_marker.allocate([1, 2, 3, 4, 5]);
//! assert!(result.is_ok());
//!
//! // Creating a back marker does not prevent us from allocating from our
//! // front marker though.
//! let back_marker = scratchpad.mark_back().unwrap();
//! let result = first_marker.allocate([6, 7, 8, 9, 10]);
//! assert!(result.is_ok());
//! ```
//!
//! ## Freeing Memory
//!
//! Memory is freed *only* when the *most recently created* [`Marker`] from a
//! given stack ("front" or "back") is dropped. Despite this, markers are
//! still allowed to be dropped out-of-order; if a marker that is not the most
//! recently created marker from its stack is dropped, its memory will simply
//! be unusable until the more recently created markers are also freed. This
//! can fragment the memory pool, so it is recommended to drop markers in the
//! reverse order in which they are created when possible.
//!
//! Dropping an [`Allocation`] will not cause the scratchpad memory used to be
//! freed, but it will immediately call any [`Drop`] implementation for the
//! allocated type. Allocations can also be dropped in any order.
//!
//! Each [`Allocation`] is bound to the lifetime of the [`Marker`] from which
//! it is created, and each [`Marker`] is bound to the lifetime of the
//! [`Scratchpad`] from which it is created, ensuring allocations and markers
//! cannot outlive their parent objects.
//!
//! # Additional Operations
//!
//! Aside from general allocation and use of data, this crate provides support
//! for a handful of additional operations:
//!
//! - Allocations that are adjacent in memory in the same scratchpad can be
//!   combined using [`Allocation::concat()`] and its unsafe counterpart,
//!   [`Allocation::concat_unchecked()`].
//! - Data can be added to the most recent allocation from a given stack:
//!   - Data can be appended to the most recent allocation from a front marker
//!     using [`MarkerFront::append()`], [`MarkerFront::append_clone()`], and
//!     [`MarkerFront::append_copy()`].
//!   - Data can be prepended to the most recent allocation from a back marker
//!     using [`MarkerBack::prepend()`], [`MarkerBack::prepend_clone()`], and
//!     [`MarkerBack::prepend_copy()`].
//!   - The [`Marker`] trait provides generic [`extend()`],
//!     [`extend_clone()`], and [`extend_copy()`] methods that either append
//!     or prepend depending on whether the marker is a front or back marker.
//! - Arbitrary numbers of slices can be concatenated into a single allocation
//!   at once using [`Marker::concat_slices()`],
//!   [`Marker::concat_slices_clone()`], and [`Marker::concat_slices_copy()`].
//! - Allocations can be converted into slices using
//!   [`Allocation::into_slice_like_allocation()`].
//! - The data in an allocation can be moved out of the allocation using
//!   [`Allocation::unwrap()`].
//!
//! # Optional Crate Features
//!
//! The following optional features can be set when building this crate:
//!
//! - **`std`**: Implements various traits for [`Box`] and [`Vec`] types,
//!   allowing boxed slices to be used as storage for allocations and marker
//!   tracking as well as both boxes and vectors to be used as slice sources
//!   for [`Marker`] methods that take slices as parameters. Enabled by
//!   default; can be disabled to build the crate with `#![no_std]`.
//! - **`alloc`**: Implements the same support for [`Box`] and [`Vec`] as the
//!   `std` feature, but uses the `alloc` crate directly, allowing for use in
//!   `no_std` code. Requires Rust 1.36.0 or later. Disabled by default.
//! - **`unstable`**: Enables unstable Rust features (requires a nightly
//!   toolchain). Disabled by default. Enabling this feature includes:
//!   - Declaration of the function [`Scratchpad::new()`] as `const`.
//!   - Support for various features that were still unstable with legacy Rust
//!     releases:
//!     - [`Box`] and [`Vec`] support for `no_std` code (enabled without the
//!       `unstable` feature when using Rust 1.36.0 or later with the `alloc`
//!       feature enabled).
//!     - [`ByteData`] trait implementations for `u128` and `i128` (enabled
//!       without the `unstable` feature when using Rust 1.26.0 or later).
//!     - [`ByteData`] trait implementation for all
//!       [`std::mem::MaybeUninit`][`MaybeUninit`] types wrapping other
//!       [`ByteData`] types (enabled without the `unstable` feature when
//!       using Rust 1.36.0 or later).
//!
//! # Implementation Notes
//!
//! ## Memory Management
//!
//! Scratchpad memory usage is explicitly controlled by the user. While this
//! adds a bit to the complexity of setting up a scratchpad for use, it allows
//! for scratchpads to be used under a variety of constraints, such as within
//! low-memory embedded environments.
//!
//! ### Buffer Types
//!
//! The backing data structures used for allocation storage and marker
//! tracking are specified by the generic parameters of the [`Scratchpad`]
//! type.
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
//! ### Macros and Functions for Handling Buffers
//!
//! Writing out the math needed to determine the number of elements needed for
//! an array or boxed slice of a specific byte or marker capacity can be
//! tedious and error-prone. This crate provides some macros and functions to
//! help reduce the amount of work needed for declaring buffers based on their
//! byte capacity or marker capacity:
//!
//! - [`array_type_for_bytes!()`] and [`array_type_for_markers!()`] can be
//!   used to declare static array types based on their capacity.
//! - [`cache_aligned_zeroed_for_bytes!()`] and
//!   [`cache_aligned_zeroed_for_markers!()`] provide shorthand for creating
//!   static arrays of [`CacheAligned`] elements with their contents zeroed
//!   out.
//! - [`uninitialized_boxed_slice_for_bytes()`] and
//!   [`uninitialized_boxed_slice_for_markers()`] can be used to allocate
//!   memory for a boxed slice of a given capacity without initializing its
//!   contents.
//!
//! Some lower level macros and functions are also available if needed:
//!
//! - [`array_len_for_bytes!()`] and [`array_len_for_markers!()`] return the
//!   number of elements needed for a static array with a given capacity.
//! - [`cache_aligned_zeroed!()`] provides shorthand for creating a single
//!   [`CacheAligned`] value with its contents zeroed out.
//! - [`uninitialized_boxed_slice()`] allocates a boxed slice of a given
//!   number of elements without initializing its contents.
//!
//! Additionally, all of the macros listed above evaluate to constant
//! expressions when given constant expressions for input values.
//!
//! ## Data Alignment
//!
//! The alignment requirements of allocated objects are handled by padding the
//! offset of the allocation within the allocation buffer. This can result in
//! some amount of wasted space if mixing allocations of types that have
//! different alignment requirements. This waste can be minimized if necessary
//! by grouping allocations based on their alignment requirements or by using
//! separate scratchpads for different alignments.
//!
//! ### `Buffer` and `Tracking` Alignments
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
//! # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
//! # use std::mem::uninitialized;
//! # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
//! use std::mem::MaybeUninit;
//! use scratchpad::{ByteData, Scratchpad};
//!
//! #[repr(C, align(16))]
//! struct Aligned16([u32; 4]);
//!
//! unsafe impl ByteData for Aligned16 {}
//!
//! # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
//! # let scratchpad = Scratchpad::<[Aligned16; 1024], [usize; 4]>::new(
//! #     unsafe { uninitialized() },
//! #     unsafe { uninitialized() },
//! # );
//! # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
//! let scratchpad = Scratchpad::<
//!     [MaybeUninit<Aligned16>; 1024],
//!     [MaybeUninit<usize>; 4],
//! >::new(
//!     unsafe { MaybeUninit::uninit().assume_init() },
//!     unsafe { MaybeUninit::uninit().assume_init() },
//! );
//! ```
//!
//! ### Alignment of Allocated Arrays
//!
//! When allocating a dynamically sized array from a [`Marker`][`Marker`]
//! (i.e. using one of the `allocate_array*()` or `allocate_slice*()`
//! methods), the array is *only* guaranteed to be aligned based on the
//! requirements of the element type. This means that, for example, it is
//! unsafe to use an array of `u8` values as a buffer in which `f32` values
//! will be written to or read from directly. It is strongly recommended that
//! you only use arrays allocated from a marker as the element type specified,
//! or that the array is allocated using an element type whose alignment is at
//! least as large as the data it will contain.
//!
//! ### Cache Alignment
//!
//! Applications may prefer to keep data aligned to cache lines to avoid
//! performance issues (e.g. multiple cache line loads for data crossing cache
//! line boundaries, false sharing). The crate provides a custom data type,
//! [`CacheAligned`], that can be used as the backing type for both allocation
//! buffers and marker tracking. Simply providing an array or slice of
//! [`CacheAligned`] objects instead of a built-in integer type will help
//! ensure cache alignment of the buffer.
//!
//! This crate uses 64 bytes as the assumed cache line alignment, regardless
//! of the build target. While the actual cache line alignment can vary
//! between processors, 64 bytes is generally assumed to be a "safe" target.
//! This value is exported in the [`CACHE_ALIGNMENT`] constant for
//! applications that wish to reference it.
//!
//! The size of a single [`CacheAligned`] element will always be the same as
//! the cache alignment, so buffers based on [`CacheAligned`] will always be
//! a multiple of [`CACHE_ALIGNMENT`] in size.
//!
//! ## Memory Overhead
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
//! sized arrays allocated using one of the `allocate_array*()` or
//! `allocate_slice*()` marker methods).
//!
//! ## Limitations
//!
//! - Due to a lack of support in Rust for generically implementing traits for
//!   any size of a static array of a given type, traits that are implemented
//!   for static array types such as [`Buffer`] and [`SliceSource`] are only
//!   implemented for a limited number of array sizes. Mutable slice
//!   references and boxed slices do not have this restriction, so they can be
//!   used for unsupported array sizes if necessary.
//! - [`SliceSourceCollection`] and [`SliceMoveSourceCollection`] (used by the
//!   slice concatenation methods of [`Marker`]) only support tuples with up
//!   to 12 items (most `std` library traits implemented for tuples are also
//!   limited to 12 items as well).
//!
//! ## Mutability Notes
//!
//! [`Scratchpad`] uses internal mutability when allocating and freeing
//! memory, with allocation methods operating on immutable [`Scratchpad`] and
//! [`Marker`] references. This is necessary to cleanly allow for multiple
//! concurrent allocations, as Rust's mutable borrowing restrictions would
//! otherwise prevent such behavior. Allocation and free operations do not
//! have any side effect on other existing allocations, so there are no
//! special considerations necessary by the user.
//!
//! # Known Issues
//!
//! - This crate allows for working with uninitialized memory for allocation
//!   and tracking buffers, but predates the [`MaybeUninit`] type. To retain
//!   backwards-compatibility with earlier crate releases, array and slice
//!   types used for uninitialized memory are allowed to contain elements of
//!   any [`ByteData`] type and not just [`MaybeUninit`]-wrapped types.
//!   [`ByteData`] types are restricted to the built-in integer types and some
//!   aggregates of those types, and while such values can safely store any
//!   possible pattern of bits without causing undefined behavior, the rules
//!   in Rust for using integers whose memory is specifically uninitialized
//!   have not been finalized. Until such rules have been finalized, it is
//!   recommended to use [`MaybeUninit`] variants of these types if possible
//!   when working with uninitialized memory to avoid undefined behavior.
//!
//!   Affected functions include:
//!
//!   - [`Scratchpad::static_new()`]
//!   - [`Scratchpad::static_new_in_place()`]
//!   - [`uninitialized_boxed_slice()`]
//!   - [`uninitialized_boxed_slice_for_bytes()`]
//!   - [`uninitialized_boxed_slice_for_markers()`]
//!
//! - [`IntoMutSliceLikePtr::into_mut_slice_like_ptr()`] is not declared as
//!   `unsafe`, as the implementations included by this crate don't need to
//!   read the data pointed to by pointer given at this time (they can operate
//!   entirely using the pointer values themselves, so passing null pointers
//!   won't cause a segmentation fault). This safety cannot be guaranteed, as
//!   future changes to Rust or additional [`IntoMutSliceLikePtr`]
//!   implementations may need to read the data; in particular, [`CStr`] may
//!   be changed at some point to no longer compute and store the length as
//!   part of a fat pointer when created, so its
//!   [`into_mut_slice_like_ptr()`][`IntoMutSliceLikePtr::into_mut_slice_like_ptr()`]
//!   implementation would need to read the string itself in order to produce
//!   a valid slice).
//!
//!   Adding `unsafe` is potentially a breaking change for anyone currently
//!   using the [`IntoMutSliceLikePtr`] trait directly in their code, so this
//!   will not be addressed until the 2.0 release of this crate.
//!
//!   This is only a safety issue that does not affect crate functionality or
//!   any code that does not use the [`IntoMutSliceLikePtr`] trait directly,
//!   so it can largely be ignored.
//!
//! # Example - Temporary Thread-local Allocations
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
//! # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
//! # use std::mem::{size_of, uninitialized};
//! # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
//! use std::mem::{MaybeUninit, size_of};
//! use std::os::raw::c_void;
//!
//! /// Thread-local scratchpad size, in bytes (1 MB).
//! const THREAD_LOCAL_SCRATCHPAD_SIZE: usize = 1024 * 1024;
//! /// Maximum thread-local scratchpad allocation marker count.
//! const THREAD_LOCAL_SCRATCHPAD_MAX_MARKERS: usize = 8;
//!
//! /// Buffer type for thread-local scratchpad allocations. By using
//! /// `CacheAligned`, we avoid false sharing of cache lines between threads.
//! # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
//! # type ThreadLocalScratchBuffer = Box<[CacheAligned]>;
//! # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
//! type ThreadLocalScratchBuffer = Box<[MaybeUninit<CacheAligned>]>;
//!
//! /// Buffer type for tracking of thread-local scratchpad markers (each
//! /// marker requires a `usize` value).
//! # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
//! # type ThreadLocalScratchTracking = array_type_for_markers!(
//! #     CacheAligned,
//! #     THREAD_LOCAL_SCRATCHPAD_MAX_MARKERS,
//! # );
//! # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
//! type ThreadLocalScratchTracking = array_type_for_markers!(
//!     MaybeUninit<CacheAligned>,
//!     THREAD_LOCAL_SCRATCHPAD_MAX_MARKERS,
//! );
//!
//! thread_local! {
//!     /// Thread-local scratchpad. The initial contents of the allocation
//!     /// buffer and marker tracking buffer are ignored, so we can create
//!     /// them as uninitialized.
//! #     #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
//! #     pub static THREAD_LOCAL_SCRATCHPAD: Scratchpad<
//! #         ThreadLocalScratchBuffer,
//! #         ThreadLocalScratchTracking,
//! #     > = unsafe { Scratchpad::new(
//! #         uninitialized_boxed_slice_for_bytes(THREAD_LOCAL_SCRATCHPAD_SIZE),
//! #         uninitialized(),
//! #     ) };
//! #     #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
//!     pub static THREAD_LOCAL_SCRATCHPAD: Scratchpad<
//!         ThreadLocalScratchBuffer,
//!         ThreadLocalScratchTracking,
//!     > = unsafe { Scratchpad::new(
//!         uninitialized_boxed_slice_for_bytes(THREAD_LOCAL_SCRATCHPAD_SIZE),
//!         MaybeUninit::uninit().assume_init(),
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
//! [`Allocation`]: struct.Allocation.html
//! [`Allocation::concat()`]: struct.Allocation.html#method.concat
//! [`Allocation::concat_unchecked()`]: struct.Allocation.html#method.concat_unchecked
//! [`Allocation::into_slice_like_allocation()`]: struct.Allocation.html#method.into_slice_like_allocation
//! [`Allocation::unwrap()`]: struct.Allocation.html#method.unwrap
//! [`array_len_for_bytes!()`]: macro.array_len_for_bytes.html
//! [`array_len_for_markers!()`]: macro.array_len_for_markers.html
//! [`array_type_for_bytes!()`]: macro.array_type_for_bytes.html
//! [`array_type_for_markers!()`]: macro.array_type_for_markers.html
//! [`Box`]: https://doc.rust-lang.org/std/boxed/index.html
//! [`Buffer`]: trait.Buffer.html
//! [`ByteData`]: trait.ByteData.html
//! [`cache_aligned_zeroed!()`]: macro.cache_aligned_zeroed.html
//! [`cache_aligned_zeroed_for_bytes!()`]: macro.cache_aligned_zeroed_for_bytes.html
//! [`cache_aligned_zeroed_for_markers!()`]: macro.cache_aligned_zeroed_for_markers.html
//! [`CACHE_ALIGNMENT`]: constant.CACHE_ALIGNMENT.html
//! [`CacheAligned`]: struct.CacheAligned.html
//! [`CStr`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html
//! [`Deref`]: https://doc.rust-lang.org/std/ops/trait.Deref.html
//! [`DerefMut`]: https://doc.rust-lang.org/std/ops/trait.DerefMut.html
//! [`Drop`]: https://doc.rust-lang.org/std/ops/trait.Drop.html
//! [`extend()`]: trait.Marker.html#method.extend
//! [`extend_clone()`]: trait.Marker.html#method.extend_clone
//! [`extend_copy()`]: trait.Marker.html#method.extend_copy
//! [`IntoMutSliceLikePtr`]: trait.IntoMutSliceLikePtr.html
//! [`IntoMutSliceLikePtr::into_mut_slice_like_ptr()`]: trait.IntoMutSliceLikePtr.html#tymethod.into_mut_slice_like_ptr
//! [`Marker`]: trait.Marker.html
//! [`Marker::concat_slices()`]: trait.Marker.html#method.concat_slices
//! [`Marker::concat_slices_clone()`]: trait.Marker.html#method.concat_slices_clone
//! [`Marker::concat_slices_copy()`]: trait.Marker.html#method.concat_slices_copy
//! [`MarkerBack`]: struct.MarkerBack.html
//! [`MarkerBack::prepend()`]: struct.MarkerBack.html#method.prepend
//! [`MarkerBack::prepend_clone()`]: struct.MarkerBack.html#method.prepend_clone
//! [`MarkerBack::prepend_copy()`]: struct.MarkerBack.html#method.prepend_copy
//! [`MarkerFront`]: struct.MarkerFront.html
//! [`MarkerFront::append()`]: struct.MarkerFront.html#method.append
//! [`MarkerFront::append_clone()`]: struct.MarkerFront.html#method.append_clone
//! [`MarkerFront::append_copy()`]: struct.MarkerFront.html#method.append_copy
//! [`MaybeUninit`]: https://doc.rust-lang.org/std/mem/union.MaybeUninit.html
//! [`owning_ref`]: https://crates.io/crates/owning_ref
//! [`rental`]: https://crates.io/crates/rental
//! [`Scratchpad`]: struct.Scratchpad.html
//! [`Scratchpad::mark_back()`]: struct.Scratchpad.html#method.mark_back
//! [`Scratchpad::mark_front()`]: struct.Scratchpad.html#method.mark_front
//! [`Scratchpad::new()`]: struct.Scratchpad.html#method.new
//! [`Scratchpad::static_new()`]: struct.Scratchpad.html#method.static_new
//! [`Scratchpad::static_new_in_place()`]: struct.Scratchpad.html#method.static_new_in_place
//! [`SliceSource`]: trait.SliceSource.html
//! [`SliceSourceCollection`]: trait.SliceSourceCollection.html
//! [`SliceMoveSourceCollection`]: trait.SliceMoveSourceCollection.html
//! [`StableDeref`]: https://crates.io/crates/stable_deref_trait
//! [`Tracking`]: trait.Tracking.html
//! [`uninitialized_boxed_slice()`]: fn.uninitialized_boxed_slice.html
//! [`uninitialized_boxed_slice_for_bytes()`]: fn.uninitialized_boxed_slice_for_bytes.html
//! [`uninitialized_boxed_slice_for_markers()`]: fn.uninitialized_boxed_slice_for_markers.html
//! [`Vec`]: https://doc.rust-lang.org/std/vec/index.html

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(
    all(feature = "unstable", not(feature = "std"), not(feature = "alloc")),
    feature(alloc)
)]
#![cfg_attr(feature = "unstable", feature(const_fn))]
#![cfg_attr(
    all(not(stable_maybe_uninit), feature = "unstable"),
    feature(maybe_uninit)
)]
// LINT: Rust 1.25.0 does not support the `dyn` keyword for trait objects, so
//       we can't make that fix yet.
#![allow(unknown_lints, bare_trait_objects)]
// LINT: Disabling `ptr_offset_with_cast` warning since using the pointer
//       `add` method would break Rust 1.25 compatibility (`add` was not
//       introduced until 1.26). As of Rust 1.32.0, `add` simply calls
//       `offset` with a cast anyway, so they are functionally equivalent.
#![cfg_attr(feature = "cargo-clippy", allow(clippy::ptr_offset_with_cast))]

#[cfg(all(
    any(feature = "alloc", feature = "unstable"),
    not(feature = "std")
))]
extern crate alloc;
#[cfg(feature = "std")]
extern crate core;

extern crate stable_deref_trait;

#[cfg(test)]
extern crate arrayvec;

#[cfg(feature = "std")]
use std::boxed::Box;
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(all(
    any(feature = "alloc", feature = "unstable"),
    not(feature = "std")
))]
use alloc::boxed::Box;
#[cfg(all(
    any(feature = "alloc", feature = "unstable"),
    not(feature = "std")
))]
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

mod array_iter;
pub(crate) use array_iter::*;

#[cfg(test)]
mod tests;
