// Copyright 2018 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Support traits.

use core::slice;

use super::CacheAligned;
use core::mem::size_of;

#[cfg(any(feature = "std", feature = "unstable"))]
use super::Box;

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

/// Trait for converting mutable references to slices (used by [`Allocation`]
/// concatenation).
///
/// [`Allocation`]: struct.Allocation.html
pub trait AsMutSlice<T> {
    /// Converts the given mutable reference to a slice reference of the
    /// target type (if it is not already one).
    fn as_mut_slice(&mut self) -> &mut [T];
}

impl<T> AsMutSlice<T> for [T] {
    #[inline]
    fn as_mut_slice(&mut self) -> &mut [T] {
        self
    }
}

impl<T> AsMutSlice<T> for T {
    #[inline]
    fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self, 1) }
    }
}

/// Macro for generating trait implementations for static arrays.
macro_rules! generate_array_trait_impls {
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

        impl<T> AsMutSlice<T> for [T; $size] {
            #[inline]
            fn as_mut_slice(&mut self) -> &mut [T] {
                &mut self[..]
            }
        }
    };

    ($size:expr, $($other:tt)*) => {
        generate_array_trait_impls!($size);
        generate_array_trait_impls!($($other)*);
    };

    () => {};
}

generate_array_trait_impls!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
generate_array_trait_impls!(11, 12, 13, 14, 15, 16, 17, 18, 19, 20);
generate_array_trait_impls!(21, 22, 23, 24, 25, 26, 27, 28, 29, 30);
generate_array_trait_impls!(31, 32, 33, 34, 35, 36, 37, 38, 39, 40);
generate_array_trait_impls!(41, 42, 43, 44, 45, 46, 47, 48, 49, 50);
generate_array_trait_impls!(51, 52, 53, 54, 55, 56, 57, 58, 59, 60);
generate_array_trait_impls!(61, 62, 63, 64);
generate_array_trait_impls!(0x80, 0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000);
generate_array_trait_impls!(
    0x4000, 0x8000, 0x10000, 0x20000, 0x40000, 0x80000
);
generate_array_trait_impls!(
    0x100000, 0x200000, 0x400000, 0x800000, 0x1000000
);
generate_array_trait_impls!(0x2000000, 0x4000000, 0x8000000, 0x10000000);
generate_array_trait_impls!(0x20000000, 0x40000000);
