// Copyright 2018-2019 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Support traits.

use core::ptr;
use core::slice;
use core::str;

use super::CacheAligned;
#[cfg(any(stable_maybe_uninit, feature = "unstable"))]
use core::mem::MaybeUninit;
use core::mem::{forget, size_of};

#[cfg(feature = "std")]
use std::ffi::CStr;

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
use super::{Box, Vec};
#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
use core::mem::ManuallyDrop;

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
/// [`Drop`]: https://doc.rust-lang.org/std/ops/trait.Drop.html
pub unsafe trait ByteData: Sized {}

unsafe impl ByteData for u8 {}
unsafe impl ByteData for u16 {}
unsafe impl ByteData for u32 {}
unsafe impl ByteData for u64 {}
#[cfg(any(stable128, feature = "unstable"))]
unsafe impl ByteData for u128 {}
unsafe impl ByteData for usize {}
unsafe impl ByteData for i8 {}
unsafe impl ByteData for i16 {}
unsafe impl ByteData for i32 {}
unsafe impl ByteData for i64 {}
#[cfg(any(stable128, feature = "unstable"))]
unsafe impl ByteData for i128 {}
unsafe impl ByteData for isize {}
unsafe impl ByteData for CacheAligned {}

#[cfg(any(stable_maybe_uninit, feature = "unstable"))]
unsafe impl<T: ByteData> ByteData for MaybeUninit<T> {}

/// Trait for static arrays.
///
/// # Safety
///
/// This trait is declared as unsafe to signify that it should only be
/// implemented for static arrays (`[T; x]` for some size x). Implementing it
/// for other types can result in undefined behavior and instability.
pub unsafe trait Array {
    /// Array item type.
    type Item: Sized;

    /// Returns a slice of the entire array.
    fn as_slice(&self) -> &[Self::Item];

    /// Returns a mutable slice of the entire array.
    fn as_mut_slice(&mut self) -> &mut [Self::Item];
}

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

impl<T> Buffer for T
where
    T: Array,
    <T as Array>::Item: ByteData,
{
    #[inline]
    fn as_bytes(&self) -> &[u8] {
        let data = self.as_slice();
        unsafe {
            slice::from_raw_parts(
                data.as_ptr() as *const u8,
                data.len() * size_of::<<T as Array>::Item>(),
            )
        }
    }

    #[inline]
    fn as_bytes_mut(&mut self) -> &mut [u8] {
        let data = self.as_mut_slice();
        unsafe {
            slice::from_raw_parts_mut(
                data.as_mut_ptr() as *mut u8,
                data.len() * size_of::<<T as Array>::Item>(),
            )
        }
    }
}

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

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
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
pub unsafe trait StaticBuffer: Buffer + Array {}

unsafe impl<T> StaticBuffer for T
where
    T: Array,
    <T as Array>::Item: ByteData,
{
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

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cast_ptr_alignment))]
    #[inline]
    fn set(&mut self, index: usize, value: usize) {
        let bytes = self.as_bytes_mut();
        unsafe {
            let contents = slice::from_raw_parts_mut(
                bytes.as_mut_ptr() as *mut usize,
                bytes.len() / size_of::<usize>(),
            );
            ptr::write_unaligned(&mut contents[index], value);
        }
    }

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cast_ptr_alignment))]
    #[inline]
    fn get(&self, index: usize) -> usize {
        let bytes = self.as_bytes();
        unsafe {
            let contents = slice::from_raw_parts(
                bytes.as_ptr() as *const usize,
                bytes.len() / size_of::<usize>(),
            );
            ptr::read_unaligned(&contents[index])
        }
    }
}

/// Trait for dynamically sized types that wrap some slice type.
///
/// Some DSTs, such as [`str`], are mostly a wrapper for a basic slice type,
/// often providing some abstraction to ensure the data isn't used in an
/// unsafe manner. Implementing this trait for such DSTs exposes conversions
/// to and from the slice type, allowing us to use these types with allocation
/// operations.
///
/// [`str`]: https://doc.rust-lang.org/std/primitive.str.html
pub trait SliceLike {
    /// Slice element type.
    type Element: Sized;

    /// Returns a slice of `Self::Element` elements containing this slice's
    /// data.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceLike;
    ///
    /// let message = "foo";
    /// let bytes = message.as_element_slice();
    /// assert_eq!(bytes, &[b'f', b'o', b'o']);
    /// ```
    fn as_element_slice(&self) -> &[Self::Element];

    /// Returns a mutable slice of `Self::Element` elements containing this
    /// slice's data.
    ///
    /// # Safety
    ///
    /// Slices of this type may perform validity checks against the internal
    /// data (e.g. `str` slices must contain valid UTF-8 data). This function
    /// allows for modification of the slice contents outside such checks.
    /// Improper use of this function can result in undefined behavior.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceLike;
    ///
    /// let mut message = String::from("foo");
    ///
    /// unsafe {
    ///     let bytes = message.as_mut_str().as_element_slice_mut();
    ///     bytes[0] = b'b';
    ///     bytes[1] = b'a';
    ///     bytes[2] = b'r';
    /// }
    ///
    /// assert_eq!(message, "bar");
    /// ```
    unsafe fn as_element_slice_mut(&mut self) -> &mut [Self::Element];

    /// Reinterprets a slice of `Self::Inner` elements as a slice of this
    /// type.
    ///
    /// # Safety
    ///
    /// Slices of this type may perform validity checks against the internal
    /// data (e.g. `str` slices must contain valid UTF-8 data). This function
    /// bypasses any such checks, potentially returning data that is invalid.
    /// Improper use of this function can result in undefined behavior.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceLike;
    ///
    /// let bytes = [b'f', b'o', b'o'];
    /// let message = unsafe {
    ///     <str as SliceLike>::from_element_slice(&bytes[..])
    /// };
    /// assert_eq!(message, "foo");
    /// ```
    unsafe fn from_element_slice(slice: &[Self::Element]) -> &Self;

    /// Reinterprets a mutable slice of `Self::Inner` elements as a mutable
    /// slice of this type.
    ///
    /// # Safety
    ///
    /// Slices of this type may perform validity checks against the internal
    /// data (e.g. `str` slices must contain valid UTF-8 data). This function
    /// bypasses any such checks, potentially returning data that is invalid.
    /// Improper use of this function can result in undefined behavior.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceLike;
    ///
    /// let mut bytes = [b'f', b'o', b'o'];
    ///
    /// unsafe {
    ///     let message = <str as SliceLike>::from_element_slice_mut(
    ///         &mut bytes[..],
    ///     );
    ///     message.as_bytes_mut()[0] = b'b';
    ///     message.as_bytes_mut()[1] = b'a';
    ///     message.as_bytes_mut()[2] = b'r';
    /// }
    ///
    /// assert_eq!(bytes, [b'b', b'a', b'r']);
    /// ```
    unsafe fn from_element_slice_mut(
        slice: &mut [Self::Element],
    ) -> &mut Self;
}

impl<T> SliceLike for [T] {
    type Element = T;

    #[inline]
    fn as_element_slice(&self) -> &[Self::Element] {
        self
    }

    #[inline]
    unsafe fn as_element_slice_mut(&mut self) -> &mut [Self::Element] {
        self
    }

    #[inline]
    unsafe fn from_element_slice(slice: &[Self::Element]) -> &Self {
        slice
    }

    #[inline]
    unsafe fn from_element_slice_mut(
        slice: &mut [Self::Element],
    ) -> &mut Self {
        slice
    }
}

impl SliceLike for str {
    type Element = u8;

    #[inline]
    fn as_element_slice(&self) -> &[Self::Element] {
        self.as_bytes()
    }

    #[inline]
    unsafe fn as_element_slice_mut(&mut self) -> &mut [Self::Element] {
        self.as_bytes_mut()
    }

    #[inline]
    unsafe fn from_element_slice(slice: &[Self::Element]) -> &Self {
        str::from_utf8_unchecked(slice)
    }

    #[inline]
    unsafe fn from_element_slice_mut(
        slice: &mut [Self::Element],
    ) -> &mut Self {
        str::from_utf8_unchecked_mut(slice)
    }
}

#[cfg(feature = "std")]
impl SliceLike for CStr {
    /// Slice element type.
    ///
    /// `u8` is used as the element type instead of [`c_char`], as the
    /// [`CStr`] methods that handle conversions to and from slices work with
    /// `u8` slices (`c_char` is only used when working with raw pointers).
    ///
    /// [`c_char`]: https://doc.rust-lang.org/std/os/raw/type.c_char.html
    /// [`CStr`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html
    type Element = u8;

    /// Returns a slice of `Self::Element` elements containing this slice's
    /// data.
    ///
    /// This uses [`CStr::to_bytes_with_nul()`] to return the entire [`CStr`]
    /// contents, including the nul terminator.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceLike;
    /// use std::ffi::CString;
    ///
    /// let message = CString::new("foo").unwrap();
    /// let message_c_str = message.as_c_str();
    /// let bytes = message_c_str.as_element_slice();
    /// assert_eq!(bytes, &[b'f', b'o', b'o', b'\0']);
    /// ```
    ///
    /// [`CStr::to_bytes_with_nul()`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html#method.to_bytes_with_nul
    /// [`CStr`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html
    #[inline]
    fn as_element_slice(&self) -> &[Self::Element] {
        self.to_bytes_with_nul()
    }

    /// Returns a mutable slice of `Self::Element` elements containing this
    /// slice's data.
    ///
    /// This is roughly equivalent to [`CStr::to_bytes_with_nul()`], returning
    /// a mutable slice instead of an immutable slice.
    ///
    /// # Safety
    ///
    /// This function potentially allows for modification of the [`CStr`]
    /// contents outside of any validity checks, specifically checks for a nul
    /// terminator and no internal nul bytes. Improper use of this function
    /// can result in undefined behavior.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceLike;
    /// use std::ffi::{CStr, CString};
    ///
    /// let mut message = CString::new("foo").unwrap().into_boxed_c_str();
    /// let message_c_str = &mut *message;
    ///
    /// unsafe {
    ///     let bytes = message_c_str.as_element_slice_mut();
    ///     bytes[0] = b'b';
    ///     bytes[1] = b'a';
    ///     bytes[2] = b'r';
    /// }
    ///
    /// assert_eq!(
    ///     message_c_str,
    ///     CStr::from_bytes_with_nul(b"bar\0").unwrap(),
    /// );
    /// ```
    ///
    /// [`CStr::to_bytes_with_nul()`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html#method.to_bytes_with_nul
    /// [`CStr`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html
    #[inline]
    unsafe fn as_element_slice_mut(&mut self) -> &mut [Self::Element] {
        // TODO: Converting from a constant reference to a mutable reference
        //       is technically undefined behavior, but aliasing of the
        //       constant reference is prevented by the fact that we take a
        //       mutable reference to the `CStr` slice as an argument.
        //       Regardless, a better alternative should be found if possible.
        // LINT: Allowing mutable cast for the time being (see above TODO).
        #[cfg_attr(feature = "cargo-clippy", allow(clippy::cast_ref_to_mut))]
        &mut *(self.to_bytes_with_nul() as *const [u8] as *mut [u8])
    }

    /// Reinterprets a slice of `Self::Inner` elements as a slice of this
    /// type.
    ///
    /// This uses [`CStr::from_bytes_with_nul_unchecked()`] to create a
    /// [`CStr`] from a nul-terminated byte slice.
    ///
    /// # Safety
    ///
    /// No safety checking is performed on the provided byte slice. It
    /// **must** be nul-terminated and not contain any interior nul bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceLike;
    /// use std::ffi::CStr;
    ///
    /// let bytes = [b'f', b'o', b'o', b'\0'];
    /// let message = unsafe {
    ///     <CStr as SliceLike>::from_element_slice(&bytes[..])
    /// };
    /// assert_eq!(message, CStr::from_bytes_with_nul(b"foo\0").unwrap());
    /// ```
    ///
    /// [`CStr::from_bytes_with_nul_unchecked()`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html#method.from_bytes_with_nul_unchecked
    /// [`CStr`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html
    #[inline]
    unsafe fn from_element_slice(slice: &[Self::Element]) -> &Self {
        CStr::from_bytes_with_nul_unchecked(slice)
    }

    /// Reinterprets a mutable slice of `Self::Inner` elements as a mutable
    /// slice of this type.
    ///
    /// This is roughly equivalent to
    /// [`CStr::from_bytes_with_nul_unchecked()`], returning a mutable
    /// [`CStr`] reference instead of an immutable reference.
    ///
    /// # Safety
    ///
    /// No safety checking is performed on the provided byte slice. It
    /// **must** be nul-terminated and not contain any interior nul bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceLike;
    /// use std::ffi::CStr;
    ///
    /// let mut bytes = [b'f', b'o', b'o', b'\0'];
    /// let message = unsafe {
    ///     <CStr as SliceLike>::from_element_slice_mut(&mut bytes[..])
    /// };
    /// assert_eq!(message, CStr::from_bytes_with_nul(b"foo\0").unwrap());
    /// ```
    ///
    /// [`CStr::from_bytes_with_nul_unchecked()`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html#method.from_bytes_with_nul_unchecked
    /// [`CStr`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html
    #[inline]
    unsafe fn from_element_slice_mut(
        slice: &mut [Self::Element],
    ) -> &mut Self {
        // TODO: This makes the same assumption as `as_element_slice_mut()`
        //       above with regards to casting from a constant reference to a
        //       mutable reference. Seeing as it still falls under the blanket
        //       of undefined behavior, an alternative should be used if
        //       possible.
        // LINT: Allowing mutable cast for the time being (see above TODO).
        #[cfg_attr(feature = "cargo-clippy", allow(clippy::cast_ref_to_mut))]
        &mut *(CStr::from_bytes_with_nul_unchecked(slice) as *const CStr
            as *mut CStr)
    }
}

/// Extension of the [`SliceLike`] trait used to mark DSTs for which
/// concatenation can safely be performed by concatenating the underlying
/// slice type.
///
/// This is used as a constraint for functions such as
/// [`Allocation::concat()`] and [`MarkerFront::append()`].
///
/// [`Allocation::concat()`]: struct.Allocation.html#method.concat
/// [`MarkerFront::append()`]: struct.MarkerFront.html#method.append
/// [`SliceLike`]: trait.SliceLike.html
pub trait ConcatenateSlice: SliceLike {}

impl<T> ConcatenateSlice for [T] {}
impl ConcatenateSlice for str {}

/// Trait for reinterpreting a pointer to a given type as a compatible
/// [`SliceLike`] pointer that uses the exact same backing memory.
///
/// # Safety
///
/// This trait is marked as unsafe due to the potential for data loss if
/// implemented incorrectly. It is used within this crate to determine which
/// slice conversions are valid for a given [`Allocation`] without requiring
/// the allocated data to be modified in any way. The source and destination
/// types must use the same exact storage memory, and dropping the data stored
/// in the destination type must also perform any cleanup required by the
/// original source type.
///
/// [`Allocation`]: struct.Allocation.html
/// [`SliceLike`]: trait.SliceLike.html
pub unsafe trait IntoMutSliceLikePtr<T>
where
    T: SliceLike + ?Sized,
{
    /// Reinterprets a mutable pointer of this type as a [`SliceLike`]
    /// pointer.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::IntoMutSliceLikePtr;
    ///
    /// let mut value = 3.14159;
    /// let value_ptr: *mut f64 = &mut value;
    ///
    /// let slice_ptr = IntoMutSliceLikePtr::into_mut_slice_like_ptr(
    ///     value_ptr,
    /// );
    /// assert_eq!(unsafe { &*slice_ptr }, [3.14159]);
    /// ```
    ///
    /// [`SliceLike`]: trait.SliceLike.html
    fn into_mut_slice_like_ptr(ptr: *mut Self) -> *mut T;
}

unsafe impl<T> IntoMutSliceLikePtr<[T]> for T {
    // LINT: `ptr` doesn't get dereferenced by `slice::from_raw_parts_mut()`,
    //       so the lint error can be ignored.
    #[cfg_attr(
        feature = "cargo-clippy",
        allow(clippy::not_unsafe_ptr_arg_deref)
    )]
    #[inline]
    fn into_mut_slice_like_ptr(ptr: *mut T) -> *mut [T] {
        unsafe { slice::from_raw_parts_mut(ptr, 1) }
    }
}

unsafe impl<T> IntoMutSliceLikePtr<T> for T
where
    T: SliceLike + ?Sized,
{
    #[inline]
    fn into_mut_slice_like_ptr(ptr: *mut T) -> *mut T {
        ptr
    }
}

unsafe impl IntoMutSliceLikePtr<[u8]> for str {
    // LINT: Despite the notation, `ptr` doesn't actually get dereferenced by
    //       this function (only the pointer value itself is ever read), so
    //       the lint error can be ignored.
    #[cfg_attr(
        feature = "cargo-clippy",
        allow(clippy::not_unsafe_ptr_arg_deref)
    )]
    #[inline]
    fn into_mut_slice_like_ptr(ptr: *mut str) -> *mut [u8] {
        unsafe { (*ptr).as_bytes_mut() }
    }
}

#[cfg(feature = "std")]
unsafe impl IntoMutSliceLikePtr<[u8]> for CStr {
    // TODO: While the current implementation of `CStr` doesn't require
    //       dereferencing `ptr` in order to convert to a `*mut [u8]`, it may
    //       in the future, so the `not_unsafe_ptr_arg_deref` lint warning is
    //       valid in this context. Since `IntoMutSliceLikePtr` is a public
    //       trait, adding `unsafe` to this function would be a breaking
    //       change, so we can't do so until the 2.0 release.
    #[cfg_attr(
        feature = "cargo-clippy",
        allow(clippy::not_unsafe_ptr_arg_deref)
    )]
    #[inline]
    fn into_mut_slice_like_ptr(ptr: *mut CStr) -> *mut [u8] {
        unsafe { (*ptr).as_element_slice_mut() }
    }
}

/// Trait for sources of slice data provided to [`Marker`] trait methods.
///
/// `SliceSource` is implemented for static arrays and slice references. If
/// either the `std` or `unstable` features are enabled, [boxed slice] and
/// [`Vec`] instances can be used as slice sources as well.
///
/// `SliceSource` on its own is only usable for `Clone` and `Copy` data
/// sources. For move operations, the [`SliceMoveSource`] subtrait provides
/// additional functionality for moving slices out of supported types.
///
/// [`Marker`]: trait.Marker.html
/// [`SliceMoveSource`]: trait.SliceMoveSource.html
/// [`Vec`]: https://doc.rust-lang.org/std/vec/struct.Vec.html
/// [boxed slice]: https://doc.rust-lang.org/std/boxed/struct.Box.html
pub trait SliceSource<T>
where
    T: SliceLike + ?Sized,
{
    /// Returns a [`SliceLike`] reference to this object's data.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceSource;
    ///
    /// // `value` is an `[f64; 1]`...
    /// let value = [3.14159];
    ///
    /// // ...but `value_slice` is a `&[f64]`...
    /// let value_slice = value.as_slice_like();
    /// assert_eq!(value_slice.len(), 1);
    /// assert_eq!(value_slice[0], 3.14159);
    ///
    /// // ...that references the same memory as `value`.
    /// assert!(std::ptr::eq(&value[0], &value_slice[0]));
    /// ```
    ///
    /// [`SliceLike`]: trait.SliceLike.html
    fn as_slice_like(&self) -> &T;

    /// Returns a [`SliceLike`] pointer to this object's data.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceSource;
    ///
    /// // `value` is an `[f64; 1]`...
    /// let value = [3.14159];
    ///
    /// unsafe {
    ///     // ...but `value_slice_ptr` is a `*const [f64]`...
    ///     let value_slice_ptr = value.as_slice_like_ptr();
    ///     assert_eq!((*value_slice_ptr).len(), 1);
    ///     assert_eq!((*value_slice_ptr)[0], 3.14159);
    ///
    ///     // ...that references the same memory as `value`.
    ///     assert!(std::ptr::eq(&value[0], &(*value_slice_ptr)[0]));
    /// }
    /// ```
    ///
    /// [`SliceLike`]: trait.SliceLike.html
    #[inline(always)]
    fn as_slice_like_ptr(&self) -> *const T {
        self.as_slice_like()
    }
}

impl<T> SliceSource<[<T as Array>::Item]> for T
where
    T: Array,
{
    #[inline]
    fn as_slice_like(&self) -> &[<T as Array>::Item] {
        self.as_slice()
    }
}

impl<'a, T> SliceSource<T> for &'a T
where
    T: SliceLike + ?Sized,
{
    #[inline]
    fn as_slice_like(&self) -> &T {
        *self
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<T> SliceSource<T> for Box<T>
where
    T: SliceLike + ?Sized,
{
    #[inline]
    fn as_slice_like(&self) -> &T {
        &**self
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<T> SliceSource<[T]> for Vec<T> {
    #[inline]
    fn as_slice_like(&self) -> &[T] {
        self.as_slice()
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<'a, T> SliceSource<T> for &'a Box<T>
where
    T: SliceLike + ?Sized,
{
    #[inline]
    fn as_slice_like(&self) -> &T {
        &***self
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<'a, T> SliceSource<[T]> for &'a Vec<T> {
    #[inline]
    fn as_slice_like(&self) -> &[T] {
        self.as_slice()
    }
}

/// Subtrait of [`SliceSource`] for taking ownership of the contents of a
/// slice.
///
/// [`SliceSource`]: trait.SliceSource.html
pub trait SliceMoveSource<T>: SliceSource<T>
where
    T: SliceLike + ?Sized,
{
    /// Calls a closure for each item in this source, consuming the source.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceMoveSource;
    ///
    /// fn move_to_vec<T: SliceMoveSource<[i32]>>(source: T) -> Vec<i32> {
    ///     let mut out = Vec::new();
    ///     source.move_elements(|x| out.push(x));
    ///     out
    /// }
    ///
    /// let values = [5, 6, 7, 8];
    /// let out = move_to_vec([5, 6, 7, 8]);
    /// assert_eq!(*out, [5, 6, 7, 8]);
    /// ```
    fn move_elements<F>(self, f: F)
    where
        Self: Sized,
        F: FnMut(<T as SliceLike>::Element);
}

impl<T> SliceMoveSource<[<T as Array>::Item]> for T
where
    T: Array,
{
    fn move_elements<F>(self, mut f: F)
    where
        F: FnMut(<T as Array>::Item),
    {
        unsafe {
            for item in self.as_slice() {
                f(ptr::read(item))
            }

            forget(self);
        }
    }
}

impl<'a, T> SliceMoveSource<T> for &'a T
where
    T: SliceLike + ?Sized,
    <T as SliceLike>::Element: Copy,
{
    fn move_elements<F>(self, mut f: F)
    where
        F: FnMut(<T as SliceLike>::Element),
    {
        for item in self.as_element_slice() {
            f(*item)
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<T> SliceMoveSource<T> for Box<T>
where
    T: SliceLike + ?Sized,
{
    fn move_elements<F>(self, mut f: F)
    where
        F: FnMut(<T as SliceLike>::Element),
    {
        unsafe {
            for item in self.as_element_slice() {
                f(ptr::read(item));
            }

            Box::from_raw((*Box::into_raw(self)).as_element_slice_mut()
                as *mut [<T as SliceLike>::Element]
                as *mut [ManuallyDrop<<T as SliceLike>::Element>]);
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<T> SliceMoveSource<[T]> for Vec<T> {
    fn move_elements<F>(self, mut f: F)
    where
        F: FnMut(T),
    {
        for item in self {
            f(item);
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<'a, T> SliceMoveSource<T> for &'a Box<T>
where
    T: SliceLike + ?Sized,
    <T as SliceLike>::Element: Copy,
{
    fn move_elements<F>(self, mut f: F)
    where
        F: FnMut(<T as SliceLike>::Element),
    {
        for item in self.as_element_slice() {
            f(*item);
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<'a, T> SliceMoveSource<[T]> for &'a Vec<T>
where
    T: Copy,
{
    fn move_elements<F>(self, mut f: F)
    where
        F: FnMut(T),
    {
        for item in self {
            f(*item);
        }
    }
}

/// Trait for generic access to collections of [`SliceSource`] objects.
///
/// Collections can be either arrays, tuples, or slices of [`SliceSource`]
/// objects. Tuples can contain contain objects of different [`SliceSource`]
/// implementation types. If either the `std` or `unstable` features are
/// enabled, [`boxed slice`] and [`Vec`] instances can be used as slice
/// source collections as well.
///
/// `SliceSourceCollection` on its own is only usable for `Clone` and `Copy`
/// data sources. For move operations, the [`SliceMoveSourceCollection`]
/// subtrait provides additional functionality for moving slices out of
/// supported types.
///
/// [`boxed slice`]: https://doc.rust-lang.org/std/boxed/struct.Box.html
/// [`Marker`]: trait.Marker.html
/// [`SliceMoveSourceCollection`]: trait.SliceMoveSourceCollection.html
/// [`SliceSource`]: trait.SliceSource.html
/// [`Vec`]: https://doc.rust-lang.org/std/vec/struct.Vec.html
pub trait SliceSourceCollection<T>
where
    T: SliceLike + ?Sized,
{
    /// Calls a closure for each [`SliceSource`] in this collection.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{SliceLike, SliceSourceCollection};
    ///
    /// let collection = ([1, 2, 3], [4], [5, 6]);
    ///
    /// let mut out = Vec::new();
    /// collection.for_each(|source| {
    ///     for x in source.as_slice_like().as_element_slice() {
    ///         out.push(*x * 2);
    ///     }
    /// });
    /// assert_eq!(*out, [2, 4, 6, 8, 10, 12]);
    /// ```
    ///
    /// [`SliceSource`]: trait.SliceSource.html
    fn for_each<F>(&self, f: F)
    where
        F: for<'a> FnMut(&'a SliceSource<T>);
}

impl<T, U> SliceSourceCollection<T> for U
where
    T: SliceLike + ?Sized,
    U: Array,
    <U as Array>::Item: SliceSource<T>,
{
    fn for_each<F>(&self, mut f: F)
    where
        F: for<'a> FnMut(&'a SliceSource<T>),
    {
        for source in self.as_slice() {
            f(source);
        }
    }
}

impl<'b, T, U> SliceSourceCollection<T> for &'b [U]
where
    T: SliceLike + ?Sized,
    U: SliceSource<T>,
{
    fn for_each<F>(&self, mut f: F)
    where
        F: for<'a> FnMut(&'a SliceSource<T>),
    {
        for source in *self {
            f(source);
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<T, U> SliceSourceCollection<T> for Box<[U]>
where
    T: SliceLike + ?Sized,
    U: SliceSource<T>,
{
    fn for_each<F>(&self, mut f: F)
    where
        F: for<'a> FnMut(&'a SliceSource<T>),
    {
        for source in &**self {
            f(source);
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<T, U> SliceSourceCollection<T> for Vec<U>
where
    T: SliceLike + ?Sized,
    U: SliceSource<T>,
{
    fn for_each<F>(&self, mut f: F)
    where
        F: for<'a> FnMut(&'a SliceSource<T>),
    {
        for source in self {
            f(source);
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<'b, T, U> SliceSourceCollection<T> for &'b Box<[U]>
where
    T: SliceLike + ?Sized,
    U: SliceSource<T>,
{
    fn for_each<F>(&self, mut f: F)
    where
        F: for<'a> FnMut(&'a SliceSource<T>),
    {
        for source in &***self {
            f(source);
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<'b, T, U> SliceSourceCollection<T> for &'b Vec<U>
where
    T: SliceLike + ?Sized,
    U: SliceSource<T>,
{
    fn for_each<F>(&self, mut f: F)
    where
        F: for<'a> FnMut(&'a SliceSource<T>),
    {
        for source in *self {
            f(source);
        }
    }
}

/// Subtrait of [`SliceSourceCollection`] for taking ownership of the contents
/// of a collection of slice sources.
///
/// [`SliceSourceCollection`]: trait.SliceSourceCollection.html
pub trait SliceMoveSourceCollection<T>: SliceSourceCollection<T>
where
    T: SliceLike + ?Sized,
{
    /// Calls a closure for each item in all [`SliceSource`] objects of this
    /// collection in sequence, consuming the collection.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::SliceMoveSourceCollection;
    ///
    /// let collection = ([1, 2, 3], [4], [5, 6]);
    ///
    /// let mut out = Vec::new();
    /// collection.move_all_elements(|x| out.push(x * 2));
    /// assert_eq!(*out, [2, 4, 6, 8, 10, 12]);
    /// ```
    ///
    /// [`SliceSource`]: trait.SliceSource.html
    fn move_all_elements<F>(self, f: F)
    where
        Self: Sized,
        F: FnMut(<T as SliceLike>::Element);
}

impl<T, U> SliceMoveSourceCollection<T> for U
where
    T: SliceLike + ?Sized,
    U: Array,
    <U as Array>::Item: SliceMoveSource<T>,
{
    fn move_all_elements<F>(self, mut f: F)
    where
        F: FnMut(<T as SliceLike>::Element),
    {
        unsafe {
            for source in self.as_slice() {
                ptr::read(source).move_elements(&mut f);
            }

            forget(self);
        }
    }
}

impl<'b, T, U> SliceMoveSourceCollection<T> for &'b [U]
where
    T: SliceLike + ?Sized,
    <T as SliceLike>::Element: Copy,
    U: SliceSource<T>,
{
    fn move_all_elements<F>(self, mut f: F)
    where
        F: FnMut(<T as SliceLike>::Element),
    {
        for source in self {
            for item in source.as_slice_like().as_element_slice() {
                f(*item);
            }
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<T, U> SliceMoveSourceCollection<T> for Box<[U]>
where
    T: SliceLike + ?Sized,
    U: SliceMoveSource<T>,
{
    fn move_all_elements<F>(self, mut f: F)
    where
        F: FnMut(<T as SliceLike>::Element),
    {
        unsafe {
            for source in &*self {
                ptr::read(source).move_elements(&mut f);
            }

            Box::from_raw(Box::into_raw(self) as *mut [ManuallyDrop<U>]);
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<T, U> SliceMoveSourceCollection<T> for Vec<U>
where
    T: SliceLike + ?Sized,
    U: SliceMoveSource<T>,
{
    fn move_all_elements<F>(self, mut f: F)
    where
        F: FnMut(<T as SliceLike>::Element),
    {
        for source in self {
            source.move_elements(&mut f);
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<'b, T, U> SliceMoveSourceCollection<T> for &'b Box<[U]>
where
    T: SliceLike + ?Sized,
    <T as SliceLike>::Element: Copy,
    U: SliceSource<T>,
{
    fn move_all_elements<F>(self, mut f: F)
    where
        F: FnMut(<T as SliceLike>::Element),
    {
        for source in &**self {
            for item in source.as_slice_like().as_element_slice() {
                f(*item);
            }
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
impl<'b, T, U> SliceMoveSourceCollection<T> for &'b Vec<U>
where
    T: SliceLike + ?Sized,
    <T as SliceLike>::Element: Copy,
    U: SliceSource<T>,
{
    fn move_all_elements<F>(self, mut f: F)
    where
        F: FnMut(<T as SliceLike>::Element),
    {
        for source in self {
            for item in source.as_slice_like().as_element_slice() {
                f(*item);
            }
        }
    }
}

/// Macro for generating trait implementations for tuples.
macro_rules! generate_tuple_trait_impls {
    ($($name:ident,)+) => {
        impl<T, $($name,)*> SliceSourceCollection<T> for ($($name,)*)
        where
            T: SliceLike + ?Sized,
            $($name: SliceSource<T>,)*
        {
            #[allow(non_snake_case)]
            fn for_each<F>(&self, mut f: F)
            where
                F: for<'a> FnMut(&'a SliceSource<T>)
            {
                let ($(ref $name,)*) = *self;
                $(
                    f($name);
                )*
            }
        }

        #[allow(non_snake_case)]
        impl<'b, T, $($name,)*> SliceSourceCollection<T> for &'b ($($name,)*)
        where
            T: SliceLike + ?Sized,
            $($name: SliceSource<T>,)*
        {
            #[allow(non_snake_case)]
            fn for_each<F>(&self, mut f: F)
            where
                F: for<'a> FnMut(&'a SliceSource<T>)
            {
                let ($(ref $name,)*) = **self;
                $(
                    f($name);
                )*
            }
        }

        impl<T, $($name,)*> SliceMoveSourceCollection<T> for ($($name,)*)
        where
            T: SliceLike + ?Sized,
            $($name: SliceMoveSource<T>,)*
        {
            #[allow(non_snake_case)]
            fn move_all_elements<F>(self, mut f: F)
            where
                F: FnMut(<T as SliceLike>::Element),
            {
                let ($($name,)*) = self;
                $(
                    $name.move_elements(&mut f);
                )*
            }
        }

        #[allow(non_snake_case)]
        impl<'b, T, $($name,)*> SliceMoveSourceCollection<T>
            for &'b ($($name,)*)
        where
            T: SliceLike + ?Sized,
            <T as SliceLike>::Element: Copy,
            $($name: SliceSource<T>,)*
        {
            #[allow(non_snake_case)]
            fn move_all_elements<F>(self, mut f: F)
            where
                F: FnMut(<T as SliceLike>::Element),
            {
                let ($(ref $name,)*) = *self;
                $(
                    for item in $name.as_slice_like().as_element_slice() {
                        f(*item);
                    }
                )*
            }
        }

        generate_tuple_trait_impls!([REDUCE] $($name,)*);
    };

    () => {};

    ([REDUCE] $name:ident, $($remaining:ident,)*) => {
        generate_tuple_trait_impls!($($remaining,)*);
    }
}

generate_tuple_trait_impls!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11,);

/// Macro for generating trait implementations for static arrays.
macro_rules! generate_array_trait_impls {
    ($size:expr) => {
        unsafe impl<T> Array for [T; $size]
        where
            T: Sized,
        {
            type Item = T;

            #[inline]
            fn as_slice(&self) -> &[T] {
                &self[..]
            }

            #[inline]
            fn as_mut_slice(&mut self) -> &mut [T] {
                &mut self[..]
            }
        }

        unsafe impl<T> IntoMutSliceLikePtr<[T]> for [T; $size] {
            // LINT: Despite the notation, `ptr` doesn't actually get
            //       dereferenced by this function (only the pointer value
            //       itself is ever read), so the lint error can be ignored.
            #[cfg_attr(feature = "cargo-clippy", allow(clippy::not_unsafe_ptr_arg_deref))]
            #[inline]
            fn into_mut_slice_like_ptr(ptr: *mut [T; $size]) -> *mut [T] {
                unsafe { &mut (*ptr)[..] }
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
    0x10_0000, 0x20_0000, 0x40_0000, 0x80_0000, 0x100_0000
);
generate_array_trait_impls!(0x200_0000, 0x400_0000, 0x800_0000, 0x1000_0000);
generate_array_trait_impls!(0x2000_0000, 0x4000_0000);
