// Copyright 2018-2019 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! `Allocation` type implementation.

use core::fmt;
use core::ptr;
use core::slice;

use super::{
    ConcatenateSlice, Error, ErrorKind, IntoMutSliceLikePtr, SliceLike,
};
use core::marker::PhantomData;
use core::mem::forget;
use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;
use stable_deref_trait::StableDeref;

/// Scratchpad [`Marker`] allocation.
///
/// Allocations implement the [`Deref`] and [`DerefMut`] traits, allowing the
/// data to be dereferenced explicitly using the unary `*` operator (e.g.
/// `*allocation`) or implicitly by the compiler under various circumstances.
/// Allocations also implement [`StableDeref`], allowing them to be used with
/// crates that support the trait such as [`owning_ref`] and [`rental`].
///
/// An allocation is statically bound to the lifetime of the [`Marker`] from
/// which it is allocated, ensuring that no dangling references can be left
/// when the [`Marker`] is dropped.
///
/// [`Marker`]: trait.Marker.html
/// [`Deref`]: https://doc.rust-lang.org/std/ops/trait.Deref.html
/// [`DerefMut`]: https://doc.rust-lang.org/std/ops/trait.DerefMut.html
/// [`StableDeref`]: https://crates.io/crates/stable_deref_trait
/// [`owning_ref`]: https://crates.io/crates/owning_ref
/// [`rental`]: https://crates.io/crates/rental
pub struct Allocation<'marker, T>
where
    T: ?Sized,
{
    /// Allocation data.
    pub(crate) data: NonNull<T>,
    /// Dummy reference for ensuring the allocation does not outlive the
    /// `Marker` from which it was allocated.
    pub(crate) _phantom: PhantomData<&'marker ()>,
}

impl<'marker, T> Allocation<'marker, T>
where
    T: Sized,
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
    /// [`Sized`]: https://doc.rust-lang.org/std/marker/trait.Sized.html
    pub fn unwrap(self) -> T {
        unsafe {
            let value = ptr::read(self.data.as_ptr());
            forget(self);
            value
        }
    }
}

impl<'marker, T> Allocation<'marker, T>
where
    T: ?Sized,
{
    /// Converts this allocation into an allocation of a compatible
    /// [`SliceLike`] type without altering the allocation data.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Allocation, Scratchpad};
    ///
    /// let scratchpad = Scratchpad::<[i32; 6], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let scalar = marker.allocate(3).unwrap();
    /// assert_eq!(*scalar.into_slice_like_allocation(), [3]);
    ///
    /// let slice = marker.allocate_array_with(2, |index| index as i32)
    ///     .unwrap();
    /// assert_eq!(*slice.into_slice_like_allocation(), [0, 1]);
    ///
    /// // Automatic conversion of an array into a slice is ambiguous, as the
    /// // compiler can't tell whether we want a slice with the same length as
    /// // the array or a slice with only one element containing the entire
    /// // array. We must explicitly specify the slice type in this example.
    /// let array = marker.allocate([9, 8, 7]).unwrap();
    /// let array_slice: Allocation<[i32]> = array
    ///     .into_slice_like_allocation();
    /// assert_eq!(*array_slice, [9, 8, 7]);
    /// ```
    ///
    /// [`SliceLike`]: trait.SliceLike.html
    #[inline]
    pub fn into_slice_like_allocation<U>(self) -> Allocation<'marker, U>
    where
        T: IntoMutSliceLikePtr<U>,
        U: SliceLike + ?Sized,
    {
        let data = unsafe {
            NonNull::new_unchecked(T::into_mut_slice_like_ptr(
                self.data.as_ptr(),
            ))
        };
        forget(self);
        Allocation {
            data,
            _phantom: PhantomData,
        }
    }

    /// Combines this allocation with an allocation immediately following it
    /// in memory, returning a single slice allocation.
    ///
    /// Allocations must fulfill the following requirements to be able to be
    /// concatenated:
    ///
    /// - Each allocation must contain an instance, array, or slice of the
    ///   same type. Any combination of these can be used.
    /// - Allocations must come from markers with the *exact* same lifetime.
    ///   Concatenating allocations from different markers is possible, as the
    ///   matching lifetimes ensure that neither marker can be invalidated
    ///   before the combined allocation is dropped.
    /// - The first allocation must occupy the memory *immediately before* the
    ///   second allocation. For allocations made from a [`MarkerBack`], this
    ///   means that the allocations need to be specified in the reverse order
    ///   in which they were made since back markers allocate memory
    ///   downwards.
    /// - No gaps in memory can reside between the allocations, even if that
    ///   memory is no longer in use.
    ///
    /// The first two requirements are checked at compile time, while the last
    /// two are checked at runtime.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{ErrorKind, Scratchpad};
    ///
    /// let scratchpad = Scratchpad::<[i32; 8], [usize; 2]>::static_new();
    ///
    /// {
    ///     let marker = scratchpad.mark_front().unwrap();
    ///     let a = marker.allocate(1).unwrap();
    ///     let b = marker.allocate([2, 3]).unwrap();
    ///     let c = marker.allocate_array_with(3, |index| index as i32 + 4)
    ///         .unwrap();
    ///
    ///     // `a` and `c` cannot be concatenated since they are not adjacent.
    ///     let error = a.concat(c).unwrap_err();
    ///     assert_eq!(error.kind(), ErrorKind::NotAdjacent);
    ///     let (a, c) = error.unwrap_args();
    ///
    ///     // `a`, `b`, and `c` can be concatenated as long as adjacent
    ///     // allocations are concatenated first.
    ///     let abc = a.concat(b.concat(c).unwrap()).unwrap();
    ///     assert_eq!(*abc, [1, 2, 3, 4, 5, 6]);
    /// }
    ///
    /// {
    ///     let marker = scratchpad.mark_back().unwrap();
    ///     let a = marker.allocate([1]).unwrap();
    ///     let b = marker.allocate([2, 3]).unwrap();
    ///
    ///     // When using a back marker, allocations must be concatenated in
    ///     // the reverse order of creation.
    ///     let error = a.concat::<[i32], _>(b).unwrap_err();
    ///     assert_eq!(error.kind(), ErrorKind::OutOfOrder);
    ///     let (a, b) = error.unwrap_args();
    ///
    ///     let ba = b.concat::<[i32], _>(a).unwrap();
    ///     assert_eq!(*ba, [2, 3, 1]);
    /// }
    ///
    /// {
    ///     // Both of the markers created here exist until the end of the
    ///     // same scope, so their allocations can be concatenated.
    ///     let marker_a = scratchpad.mark_front().unwrap();
    ///     let a = marker_a.allocate(1).unwrap();
    ///     let marker_b = scratchpad.mark_front().unwrap();
    ///     let b = marker_b.allocate(2).unwrap();
    ///
    ///     let ab = a.concat(b).unwrap();
    ///     assert_eq!(*ab, [1, 2]);
    /// }
    /// ```
    ///
    /// [`MarkerBack`]: struct.MarkerBack.html
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::type_complexity))] // LINT: Result type lint warning.
    pub fn concat<U, V>(
        self,
        other: Allocation<'marker, V>,
    ) -> Result<
        Allocation<'marker, U>,
        Error<(Allocation<'marker, T>, Allocation<'marker, V>)>,
    >
    where
        T: IntoMutSliceLikePtr<U>,
        U: ConcatenateSlice + ?Sized,
        V: IntoMutSliceLikePtr<U> + ?Sized,
    {
        unsafe {
            let data0 = (*T::into_mut_slice_like_ptr(self.data.as_ptr()))
                .as_element_slice();
            let data1 = (*V::into_mut_slice_like_ptr(other.data.as_ptr()))
                .as_element_slice();
            let data0_len = data0.len();
            let data1_len = data1.len();
            assert!(data0_len <= ::core::isize::MAX as usize);
            assert!(data1_len <= ::core::isize::MAX as usize);

            let data0_start = data0.as_ptr();
            let data0_end = data0_start.offset(data0_len as isize);
            let data1_start = data1.as_ptr();
            if data0_end != data1_start {
                return Err(Error::new(
                    if data0_start < data1_start {
                        ErrorKind::NotAdjacent
                    } else {
                        ErrorKind::OutOfOrder
                    },
                    (self, other),
                ));
            }

            Ok(self.concat_unchecked(other))
        }
    }

    /// Combines two allocations without performing any runtime checks.
    ///
    /// See the safe version, [`concat()`], for requirements and additional
    /// information.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it does not check whether the
    /// allocations are adjacent in memory or whether they are specified in
    /// the correct order (with `self` residing immediately before `other` in
    /// memory). Calling this on allocations that do not fit these
    /// requirements can lead to memory corruption, undefined behavior, or
    /// crashes.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{ErrorKind, Scratchpad};
    ///
    /// let scratchpad = Scratchpad::<[i32; 3], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let a = marker.allocate(1).unwrap();
    /// let b = marker.allocate([2, 3]).unwrap();
    ///
    /// let ab = unsafe { a.concat_unchecked(b) };
    /// assert_eq!(*ab, [1, 2, 3]);
    /// ```
    ///
    /// [`concat()`]: #method.concat
    #[inline]
    pub unsafe fn concat_unchecked<U, V>(
        self,
        other: Allocation<'marker, V>,
    ) -> Allocation<'marker, U>
    where
        T: IntoMutSliceLikePtr<U>,
        U: ConcatenateSlice + ?Sized,
        V: IntoMutSliceLikePtr<U> + ?Sized,
    {
        let data0 = (*T::into_mut_slice_like_ptr(self.data.as_ptr()))
            .as_element_slice_mut();
        let data1 = (*V::into_mut_slice_like_ptr(other.data.as_ptr()))
            .as_element_slice_mut();

        forget(self);
        forget(other);

        Allocation {
            data: NonNull::new(<U as SliceLike>::from_element_slice_mut(
                slice::from_raw_parts_mut(
                    data0.as_mut_ptr(),
                    data0.len() + data1.len(),
                ),
            ))
            .unwrap(),
            _phantom: PhantomData,
        }
    }
}

impl<'marker, T> Deref for Allocation<'marker, T>
where
    T: ?Sized,
{
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        unsafe { self.data.as_ref() }
    }
}

impl<'marker, T> DerefMut for Allocation<'marker, T>
where
    T: ?Sized,
{
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.data.as_mut() }
    }
}

unsafe impl<'marker, T> StableDeref for Allocation<'marker, T> {}

impl<'marker, T> Drop for Allocation<'marker, T>
where
    T: ?Sized,
{
    #[inline]
    fn drop(&mut self) {
        unsafe { ptr::drop_in_place(self.data.as_ptr()) };
    }
}

impl<'marker, T> fmt::Debug for Allocation<'marker, T>
where
    T: ?Sized + fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            write!(f, "Allocation {{ data: {:?} }}", self.data.as_ref())
        }
    }
}
