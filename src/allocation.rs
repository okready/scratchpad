// Copyright 2018 Theodore Cipicchio
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

use super::{AsMutSlice, ConcatError};
use core::marker::PhantomData;
use core::mem::forget;
use core::ops::{Deref, DerefMut};

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
pub struct Allocation<'marker, T>
where
    T: ?Sized,
{
    /// Allocation data.
    pub(crate) data: *mut T,
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
    /// [`Sized`]: https://doc.rust-lang.org/core/marker/trait.Sized.html
    pub fn unwrap(self) -> T {
        unsafe {
            let value = ptr::read(self.data);
            forget(self);
            value
        }
    }
}

impl<'marker, T> Allocation<'marker, T>
where
    T: ?Sized,
{
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
    /// It should be noted that allocations do not necessarily have to be
    /// created from the same marker, but the markers *do* have to have the
    ///
    /// # Errors
    ///
    /// If concatenation fails, an [`Err`] is returned containing a tuple with
    /// the following data:
    ///
    /// - The `self` parameter.
    /// - The `other` parameter.
    /// - A [`ConcatError`] variant specifying why the allocations could not
    ///   be joined.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{ConcatError, Scratchpad};
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
    ///     let (a, c, error) = a.concat(c).unwrap_err();
    ///     assert_eq!(error, ConcatError::NotAdjacent);
    ///
    ///     // `a`, `b`, and `c` can be concatenated as long as adjacent
    ///     // allocations are concatenated first.
    ///     let abc = a.concat(b.concat(c).unwrap()).unwrap();
    ///     assert_eq!(*abc, [1, 2, 3, 4, 5, 6])
    /// }
    ///
    /// {
    ///     let marker = scratchpad.mark_back().unwrap();
    ///     let a = marker.allocate([1]).unwrap();
    ///     let b = marker.allocate([2, 3]).unwrap();
    ///
    ///     // When using a back marker, allocations must be concatenated in
    ///     // the reverse order of creation.
    ///     let (a, b, error) = a.concat::<i32, _>(b).unwrap_err();
    ///     assert_eq!(error, ConcatError::OutOfOrder);
    ///
    ///     let ba = b.concat::<i32, _>(a).unwrap();
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
    /// [`Err`]: https://doc.rust-lang.org/core/result/enum.Result.html#variant.Err
    /// [`ConcatError`]: enum.ConcatError.html
    pub fn concat<V, U>(
        self,
        other: Allocation<'marker, U>,
    ) -> Result<
        Allocation<'marker, [V]>,
        (
            Allocation<'marker, T>,
            Allocation<'marker, U>,
            ConcatError,
        ),
    >
    where
        T: AsMutSlice<V>,
        U: AsMutSlice<V> + ?Sized,
        V: Sized,
    {
        unsafe {
            let data0 = (&mut *self.data).as_mut_slice();
            let data1 = (&mut *other.data).as_mut_slice();
            let data0_len = data0.len();
            let data1_len = data1.len();
            assert!(data0_len <= ::core::isize::MAX as usize);
            assert!(data1_len <= ::core::isize::MAX as usize);

            let data0_start = data0.as_mut_ptr();
            let data0_end = data0_start.offset(data0_len as isize);
            let data1_start = data1.as_mut_ptr();
            if data0_end != data1_start {
                return Err((
                    self,
                    other,
                    if data0_start < data1_start {
                        ConcatError::NotAdjacent
                    } else {
                        ConcatError::OutOfOrder
                    },
                ));
            }

            forget(self);
            forget(other);

            Ok(Allocation {
                data: slice::from_raw_parts_mut(
                    data0_start,
                    data0_len + data1_len,
                ),
                _phantom: PhantomData,
            })
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
        unsafe { &*self.data }
    }
}

impl<'marker, T> DerefMut for Allocation<'marker, T>
where
    T: ?Sized,
{
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.data }
    }
}

impl<'marker, T> Drop for Allocation<'marker, T>
where
    T: ?Sized,
{
    #[inline]
    fn drop(&mut self) {
        unsafe { ptr::drop_in_place(self.data) };
    }
}

impl<'marker, T> fmt::Debug for Allocation<'marker, T>
where
    T: ?Sized + fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe { write!(f, "Allocation {{ data: {:?} }}", &*self.data) }
    }
}
