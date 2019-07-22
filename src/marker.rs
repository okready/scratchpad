// Copyright 2018-2019 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! `Marker` trait and marker implementations.

use core::ptr;
use core::slice;

use super::{
    Allocation, Buffer, ConcatenateSlice, Error, ErrorKind,
    IntoMutSliceLikePtr, Scratchpad, SliceLike, SliceMoveSource,
    SliceMoveSourceCollection, SliceSource, SliceSourceCollection, Tracking,
};
use core::marker::PhantomData;
use core::mem::{align_of, forget, size_of};
use core::ptr::NonNull;

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
    #[inline]
    fn allocate<'marker, T>(
        &'marker self,
        value: T,
    ) -> Result<Allocation<'marker, T>, Error<(T,)>> {
        unsafe {
            match self.allocate_uninitialized::<T>() {
                Ok(allocation) => {
                    ptr::write(allocation.data.as_ptr(), value);
                    Ok(allocation)
                }
                Err(e) => Err(e.map(|_| (value,))),
            }
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
    #[inline]
    fn allocate_default<'marker, T: Default>(
        &'marker self,
    ) -> Result<Allocation<'marker, T>, Error<()>> {
        self.allocate(Default::default()).map_err(|e| e.map(|_| ()))
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
    #[inline]
    unsafe fn allocate_uninitialized<'marker, T>(
        &'marker self,
    ) -> Result<Allocation<'marker, T>, Error<()>> {
        let data =
            self.allocate_memory(align_of::<T>(), size_of::<T>(), 1)?;

        Ok(Allocation {
            data: NonNull::new(data as *mut T).unwrap(),
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
    #[inline]
    fn allocate_array<'marker, T: Clone>(
        &'marker self,
        len: usize,
        value: T,
    ) -> Result<Allocation<'marker, [T]>, Error<(T,)>> {
        unsafe {
            self.allocate_array_uninitialized(len)
                .map(|allocation| {
                    let data = &mut *allocation.data.as_ptr();
                    debug_assert_eq!(data.len(), len);
                    for element in data.iter_mut() {
                        ptr::write(element, value.clone());
                    }
                    allocation
                })
                .map_err(|e| e.map(|_| (value,)))
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
    #[inline]
    fn allocate_array_default<'marker, T: Default>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, [T]>, Error<()>> {
        unsafe {
            self.allocate_array_uninitialized(len).map(|allocation| {
                let data = &mut *allocation.data.as_ptr();
                debug_assert_eq!(data.len(), len);
                for element in data.iter_mut() {
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
    #[inline]
    fn allocate_array_with<'marker, T, F: FnMut(usize) -> T>(
        &'marker self,
        len: usize,
        mut func: F,
    ) -> Result<Allocation<'marker, [T]>, Error<()>> {
        unsafe {
            self.allocate_array_uninitialized(len).map(|allocation| {
                let data = &mut *allocation.data.as_ptr();
                debug_assert_eq!(data.len(), len);
                for (index, element) in data.iter_mut().enumerate() {
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
    #[inline]
    unsafe fn allocate_array_uninitialized<'marker, T>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, [T]>, Error<()>> {
        let data =
            self.allocate_memory(align_of::<T>(), size_of::<T>(), len)?;

        Ok(Allocation {
            data: NonNull::new(slice::from_raw_parts_mut(
                data as *mut T,
                len,
            ))
            .unwrap(),
            _phantom: PhantomData,
        })
    }

    /// Allocates a slice, initializing its contents by moving the given
    /// values into the allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let values = [3.14159, 4.14159, 5.14159];
    /// let allocation = marker.allocate_slice(values).unwrap();
    /// let allocation_slice: &[f64] = &*allocation;
    /// assert_eq!(*allocation_slice, [3.14159, 4.14159, 5.14159]);
    /// ```
    fn allocate_slice<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<(U,)>>
    where
        T: SliceLike + ?Sized,
        U: SliceMoveSource<T>,
    {
        unsafe {
            let element_slice =
                (*values.as_slice_like_ptr()).as_element_slice();
            let element_count = element_slice.len();
            let alloc_result = self
                .allocate_array_uninitialized::<<T as SliceLike>::Element>(
                    element_count,
                );
            match alloc_result {
                Err(e) => Err(e.map(|()| (values,))),
                Ok(allocation) => {
                    let data = &mut *allocation.data.as_ptr();
                    debug_assert_eq!(data.len(), element_count);
                    forget(allocation);

                    let mut index = 0;
                    values.move_elements(|element| {
                        ptr::write(&mut data[index], element);
                        index += 1;
                    });

                    debug_assert_eq!(index, data.len());

                    Ok(Allocation {
                        data: NonNull::new_unchecked(
                            <T as SliceLike>::from_element_slice_mut(data),
                        ),
                        _phantom: PhantomData,
                    })
                }
            }
        }
    }

    /// Allocates a copy of a slice by cloning each individual element.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 32], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let message = "foo";
    /// let allocation = marker.allocate_slice_clone(message).unwrap();
    /// assert_eq!(&*allocation, "foo");
    /// ```
    fn allocate_slice_clone<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: SliceLike + ?Sized,
        <T as SliceLike>::Element: Clone,
        U: SliceSource<T>,
    {
        unsafe {
            let element_slice =
                <T as SliceLike>::as_element_slice(values.as_slice_like());
            let element_count = element_slice.len();
            let alloc_result = self
                .allocate_array_with::<<T as SliceLike>::Element, _>(
                    element_count,
                    |index| element_slice[index].clone(),
                );
            alloc_result.map(|allocation| {
                let data = &mut *allocation.data.as_ptr();
                forget(allocation);

                Allocation {
                    data: NonNull::new_unchecked(
                        <T as SliceLike>::from_element_slice_mut(data),
                    ),
                    _phantom: PhantomData,
                }
            })
        }
    }

    /// Allocates a copy of a slice by performing a fast copy (equivalent to
    /// C's `memcpy()` function) into the new allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 32], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let message = "foo";
    /// let allocation = marker.allocate_slice_copy(message).unwrap();
    /// assert_eq!(&*allocation, "foo");
    /// ```
    fn allocate_slice_copy<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: SliceLike + ?Sized,
        <T as SliceLike>::Element: Copy,
        U: SliceSource<T>,
    {
        unsafe {
            let element_slice =
                <T as SliceLike>::as_element_slice(values.as_slice_like());
            let alloc_result = self
                .allocate_array_uninitialized::<<T as SliceLike>::Element>(
                    element_slice.len(),
                );
            alloc_result.map(|allocation| {
                let data = &mut *allocation.data.as_ptr();
                forget(allocation);

                // Objects implementing `Copy` are not allowed to have `Drop`
                // implementations, so there should be no side-effects from
                // copying into uninitialized memory.
                data.copy_from_slice(&element_slice[..]);

                Allocation {
                    data: NonNull::new_unchecked(
                        <T as SliceLike>::from_element_slice_mut(data),
                    ),
                    _phantom: PhantomData,
                }
            })
        }
    }

    /// Extends an allocation by moving values onto one of its ends,
    /// converting the allocation to a slice if necessary.
    ///
    /// Whether values are added to the beginning or end depends on the marker
    /// type: [`MarkerFront`] objects will always append, while [`MarkerBack`]
    /// objects will always prepend.
    ///
    /// The following requirements must be met to allow allocations to be
    /// extended:
    ///
    /// - The marker must be the most recently created marker of its type
    ///   (front or back) from its scratchpad. Front and back markers can be
    ///   extended independently.
    /// - The allocation must contain a scalar, array, or slice of the type
    ///   being pushed.
    /// - The data being pushed must be a scalar, array, boxed slice, or
    ///   vector of the same type.
    /// - The allocation must be the most recent allocation made from this
    ///   marker, even if other, more recent allocations have been released.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Marker, Scratchpad};
    ///
    /// let scratchpad = Scratchpad::<[u32; 5], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let a = marker.allocate([3.14159f32, 2.71828f32]).unwrap();
    ///
    /// let ab = marker.extend(a, [0.70711f32]).unwrap();
    /// assert_eq!(*ab, [3.14159f32, 2.71828f32, 0.70711f32]);
    ///
    /// let abc = marker.extend(ab, vec![0.57722f32, 1.61803f32]).unwrap();
    /// assert_eq!(
    ///     *abc,
    ///     [3.14159f32, 2.71828f32, 0.70711f32, 0.57722f32, 1.61803f32],
    /// );
    /// ```
    ///
    /// [`MarkerBack`]: struct.MarkerBack.html
    /// [`MarkerFront`]: struct.MarkerFront.html
    fn extend<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, T>, Error<(Allocation<'marker, U>, V)>>
    where
        T: ConcatenateSlice + ?Sized,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: SliceMoveSource<T>,
    {
        // Verify that the allocation is at the end of the marker.
        if !self.is_allocation_at_end(&allocation) {
            return Err(Error::new(
                ErrorKind::NotAtEnd,
                (allocation, values),
            ));
        }

        // Create a new allocation for the value given and merge the two
        // allocations. This will also perform all remaining validity checks.
        match self.allocate_slice::<T, V>(values) {
            Err(e) => Err(e.map(|(vals,)| (allocation, vals))),
            Ok(val_alloc) => unsafe {
                Ok(Self::concat_allocations_unchecked::<T, U, T>(
                    allocation, val_alloc,
                ))
            },
        }
    }

    /// Extends an allocation by cloning values onto one of its ends,
    /// converting the allocation to a slice if necessary.
    ///
    /// Whether values are added to the beginning or end depends on the marker
    /// type: [`MarkerFront`] objects will always append, while [`MarkerBack`]
    /// objects will always prepend.
    ///
    /// The following requirements must be met to allow elements to be
    /// appended:
    ///
    /// - The marker must be the most recently created marker of its type
    ///   (front or back) from its scratchpad. Front and back markers can be
    ///   extended independently.
    /// - The allocation must contain a scalar, array, or slice of the type
    ///   being pushed.
    /// - The data being pushed must be a scalar, array, boxed slice, or
    ///   vector of the same type.
    /// - The allocation must be the most recent allocation made from this
    ///   marker, even if other, more recent allocations have been released.
    ///
    /// Note that the `values` parameter is consumed as part of the allocation
    /// process and cannot be returned on error.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Allocation, Marker, Scratchpad};
    ///
    /// let scratchpad = Scratchpad::<[u32; 5], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let a: Allocation<[f32]> = marker.allocate([3.14159f32, 2.71828f32])
    ///     .unwrap()
    ///     .into_slice_like_allocation();
    ///
    /// let ab = marker.extend_clone(a, &[0.57722f32, 1.61803f32][..])
    ///     .unwrap();
    /// assert_eq!(*ab, [3.14159f32, 2.71828f32, 0.57722f32, 1.61803f32]);
    /// ```
    ///
    /// [`MarkerBack`]: struct.MarkerBack.html
    /// [`MarkerFront`]: struct.MarkerFront.html
    fn extend_clone<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, T>, Error<(Allocation<'marker, U>,)>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Clone,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: SliceSource<T>,
    {
        // Verify that the allocation is at the end of the marker.
        if !self.is_allocation_at_end(&allocation) {
            return Err(Error::new(ErrorKind::NotAtEnd, (allocation,)));
        }

        // Create a new allocation for the value given and merge the two
        // allocations. This will also perform all remaining validity checks.
        match self.allocate_slice_clone::<T, V>(values) {
            Err(e) => Err(e.map(|()| (allocation,))),
            Ok(val_alloc) => unsafe {
                Ok(Self::concat_allocations_unchecked::<T, U, T>(
                    allocation, val_alloc,
                ))
            },
        }
    }

    /// Extends an allocation by copying values onto one of its ends,
    /// converting the allocation to a slice if necessary.
    ///
    /// Whether values are added to the beginning or end depends on the marker
    /// type: [`MarkerFront`] objects will always append, while [`MarkerBack`]
    /// objects will always prepend.
    ///
    /// The following requirements must be met to allow elements to be
    /// appended:
    ///
    /// - The marker must be the most recently created marker of its type
    ///   (front or back) from its scratchpad. Front and back markers can be
    ///   extended independently.
    /// - The allocation must contain a scalar, array, or slice of the type
    ///   being pushed.
    /// - The data being pushed must be a scalar, array, boxed slice, or
    ///   vector of the same type.
    /// - The allocation must be the most recent allocation made from this
    ///   marker, even if other, more recent allocations have been released.
    ///
    /// Note that the `values` parameter is consumed as part of the allocation
    /// process and cannot be returned on error.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Allocation, Marker, Scratchpad};
    ///
    /// let scratchpad = Scratchpad::<[u32; 5], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let a: Allocation<[f32]> = marker.allocate([3.14159f32, 2.71828f32])
    ///     .unwrap()
    ///     .into_slice_like_allocation();
    ///
    /// let ab = marker.extend_copy(a, &[0.57722f32, 1.61803f32][..])
    ///     .unwrap();
    /// assert_eq!(*ab, [3.14159f32, 2.71828f32, 0.57722f32, 1.61803f32]);
    /// ```
    ///
    /// [`MarkerBack`]: struct.MarkerBack.html
    /// [`MarkerFront`]: struct.MarkerFront.html
    fn extend_copy<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, T>, Error<(Allocation<'marker, U>,)>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Copy,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: SliceSource<T>,
    {
        // Verify that the allocation is at the end of the marker.
        if !self.is_allocation_at_end(&allocation) {
            return Err(Error::new(ErrorKind::NotAtEnd, (allocation,)));
        }

        // Create a new allocation for the value given and merge the two
        // allocations. This will also perform all remaining validity checks.
        match self.allocate_slice_copy::<T, V>(values) {
            Err(e) => Err(e.map(|()| (allocation,))),
            Ok(val_alloc) => unsafe {
                Ok(Self::concat_allocations_unchecked::<T, U, T>(
                    allocation, val_alloc,
                ))
            },
        }
    }

    /// Combines each of the provided slices into a single slice allocated
    /// from this marker, moving the slice values into the new allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let combined = marker.concat_slices(("Hello,", " world", "!"))
    ///     .unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    fn concat_slices<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<(U,)>>
    where
        T: ConcatenateSlice + ?Sized,
        U: SliceMoveSourceCollection<T>,
    {
        unsafe {
            let mut len = 0;
            values.for_each(|item| {
                len += item.as_slice_like().as_element_slice().len();
            });

            let alloc_result = self
                .allocate_array_uninitialized::<<T as SliceLike>::Element>(
                    len,
                );
            match alloc_result {
                Err(e) => Err(e.map(|()| (values,))),
                Ok(allocation) => {
                    let data = &mut *allocation.data.as_ptr();
                    debug_assert_eq!(data.len(), len);
                    forget(allocation);

                    let mut index = 0;
                    values.move_all_elements(|element| {
                        ptr::write(&mut data[index], element);
                        index += 1;
                    });

                    debug_assert_eq!(index, data.len());

                    Ok(Allocation {
                        data: NonNull::new_unchecked(
                            <T as SliceLike>::from_element_slice_mut(
                                slice::from_raw_parts_mut(
                                    data.as_mut_ptr(),
                                    len,
                                ),
                            ),
                        ),
                        _phantom: PhantomData,
                    })
                }
            }
        }
    }

    /// Combines cloned copies of each of the provided slices into a single
    /// slice allocated from this marker.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let combined = marker.concat_slices_clone(("Hello,", " world", "!"))
    ///     .unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    fn concat_slices_clone<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Clone,
        U: SliceSourceCollection<T>,
    {
        unsafe {
            let mut len = 0;
            values.for_each(|item| {
                len += item.as_slice_like().as_element_slice().len();
            });

            let mut allocation = self
                .allocate_array_uninitialized::<<T as SliceLike>::Element>(
                    len,
                )?;
            let mut index = 0;
            values.for_each(|item| {
                for element in item.as_slice_like().as_element_slice() {
                    ptr::write(&mut allocation[index], element.clone());
                    index += 1;
                }
            });

            debug_assert_eq!(index, len);

            let data_ptr = allocation.as_mut_ptr();
            forget(allocation);

            Ok(Allocation {
                data: NonNull::new_unchecked(
                    <T as SliceLike>::from_element_slice_mut(
                        slice::from_raw_parts_mut(data_ptr, len),
                    ),
                ),
                _phantom: PhantomData,
            })
        }
    }

    /// Combines copies of each of the provided slices into a single slice
    /// allocated from this marker.
    ///
    /// Slices are copied by performing a fast memory copy from each of the
    /// source slices (equivalent to C's `memcpy()` function).
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let combined = marker.concat_slices_copy(("Hello,", " world", "!"))
    ///     .unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    fn concat_slices_copy<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Copy,
        U: SliceSourceCollection<T>,
    {
        unsafe {
            let mut len = 0;
            values.for_each(|item| {
                len += item.as_slice_like().as_element_slice().len();
            });

            let mut allocation = self
                .allocate_array_uninitialized::<<T as SliceLike>::Element>(
                    len,
                )?;

            let mut start = 0;
            values.for_each(|item| {
                let elements = item.as_slice_like().as_element_slice();
                let end = start + elements.len();

                // Objects implementing `Copy` are not allowed to have `Drop`
                // implementations, so there should be no side-effects from
                // copying into uninitialized memory.
                allocation[start..end].copy_from_slice(elements);

                start = end;
            });

            debug_assert_eq!(start, len);

            let data_ptr = allocation.as_mut_ptr();
            forget(allocation);

            Ok(Allocation {
                data: NonNull::new_unchecked(
                    <T as SliceLike>::from_element_slice_mut(
                        slice::from_raw_parts_mut(data_ptr, len),
                    ),
                ),
                _phantom: PhantomData,
            })
        }
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
    ) -> Result<*mut u8, Error<()>>;

    /// Returns whether an allocation is the most recent allocation made from
    /// this marker.
    ///
    /// **_This is intended primarily for use by the internal implementation
    /// of this trait, and is not safe for use by external code. Its signature
    /// and behavior are not guaranteed to be consistent across versions of
    /// this crate._**
    #[doc(hidden)]
    fn is_allocation_at_end<'marker, T, U>(
        &'marker self,
        allocation: &Allocation<'marker, T>,
    ) -> bool
    where
        T: IntoMutSliceLikePtr<U> + ?Sized,
        U: SliceLike + ?Sized;

    /// Extends the first allocation with the second allocation by either
    /// appending or prepending it, without performing any runtime checks for
    /// validity.
    ///
    /// Whether the allocation is appended or prepending depends on the marker
    /// type: [`MarkerFront`] objects will always append, while [`MarkerBack`]
    /// objects will always prepend.
    ///
    /// **_This is intended primarily for use by the internal implementation
    /// of this trait, and is not safe for use by external code. Its signature
    /// and behavior are not guaranteed to be consistent across versions of
    /// this crate._**
    ///
    /// [`MarkerBack`]: struct.MarkerBack.html
    /// [`MarkerFront`]: struct.MarkerFront.html
    #[doc(hidden)]
    unsafe fn concat_allocations_unchecked<'marker, T, U, V>(
        base: Allocation<'marker, U>,
        extension: Allocation<'marker, V>,
    ) -> Allocation<'marker, T>
    where
        T: ConcatenateSlice + ?Sized,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: IntoMutSliceLikePtr<T> + ?Sized;
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
    pub(crate) scratchpad: &'scratchpad Scratchpad<BufferT, TrackingT>,
    /// Marker index.
    pub(crate) index: usize,
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
    ) -> Result<*mut u8, Error<()>> {
        // Make sure the marker is the top-most front marker (no allocations
        // are allowed if a more-recently created front marker is still
        // active).
        let mut markers = self.scratchpad.markers.borrow_mut();
        if markers.front != self.index + 1 {
            return Err(Error::new(ErrorKind::MarkerLocked, ()));
        }

        let alignment_mask = alignment - 1;
        debug_assert_eq!(alignment & alignment_mask, 0);

        // Pad the allocation size to match the requested alignment.
        let size = size
            .checked_add(alignment_mask)
            .ok_or_else(|| Error::new(ErrorKind::Overflow, ()))?
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

        let start = start
            .checked_add(alignment_mask)
            .ok_or_else(|| Error::new(ErrorKind::Overflow, ()))?
            & !alignment_mask;
        let end = start
            .checked_add(size)
            .ok_or_else(|| Error::new(ErrorKind::Overflow, ()))?;
        if end > buffer_end {
            return Err(Error::new(ErrorKind::InsufficientMemory, ()));
        }

        // Update this marker's offset and return the allocation.
        markers.data.set(self.index, end - buffer_start);

        Ok(start as *mut u8)
    }

    fn is_allocation_at_end<'marker, T, U>(
        &'marker self,
        allocation: &Allocation<'marker, T>,
    ) -> bool
    where
        T: IntoMutSliceLikePtr<U> + ?Sized,
        U: SliceLike + ?Sized,
    {
        let data = unsafe {
            (*T::into_mut_slice_like_ptr(allocation.data.as_ptr()))
                .as_element_slice()
        };
        let data_len = data.len();
        assert!(data_len <= ::core::isize::MAX as usize);

        let data_start = data.as_ptr();
        let data_end = unsafe { data_start.offset(data_len as isize) };

        let buffer_start =
            unsafe { (*self.scratchpad.buffer.get()).as_bytes().as_ptr() };
        let marker_offset =
            self.scratchpad.markers.borrow().data.get(self.index);
        let marker_end =
            unsafe { buffer_start.offset(marker_offset as isize) };

        data_end as usize == marker_end as usize
    }

    #[inline(always)]
    unsafe fn concat_allocations_unchecked<'marker, T, U, V>(
        base: Allocation<'marker, U>,
        extension: Allocation<'marker, V>,
    ) -> Allocation<'marker, T>
    where
        T: ConcatenateSlice + ?Sized,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: IntoMutSliceLikePtr<T> + ?Sized,
    {
        base.concat_unchecked(extension)
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
    pub fn allocate<'marker, T>(
        &'marker self,
        value: T,
    ) -> Result<Allocation<'marker, T>, Error<(T,)>> {
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
    pub fn allocate_default<'marker, T: Default>(
        &'marker self,
    ) -> Result<Allocation<'marker, T>, Error<()>> {
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
    pub unsafe fn allocate_uninitialized<'marker, T>(
        &'marker self,
    ) -> Result<Allocation<'marker, T>, Error<()>> {
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
    pub fn allocate_array<'marker, T: Clone>(
        &'marker self,
        len: usize,
        value: T,
    ) -> Result<Allocation<'marker, [T]>, Error<(T,)>> {
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
    pub fn allocate_array_default<'marker, T: Default>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, [T]>, Error<()>> {
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
    pub fn allocate_array_with<'marker, T, F: FnMut(usize) -> T>(
        &'marker self,
        len: usize,
        func: F,
    ) -> Result<Allocation<'marker, [T]>, Error<()>> {
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
    pub unsafe fn allocate_array_uninitialized<'marker, T>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, [T]>, Error<()>> {
        Marker::allocate_array_uninitialized(self, len)
    }

    /// Allocates a slice, initializing its contents by moving the given
    /// values into the allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let values = [3.14159, 4.14159, 5.14159];
    /// let allocation = marker.allocate_slice(values).unwrap();
    /// let allocation_slice: &[f64] = &*allocation;
    /// assert_eq!(*allocation_slice, [3.14159, 4.14159, 5.14159]);
    /// ```
    #[inline(always)]
    pub fn allocate_slice<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<(U,)>>
    where
        T: SliceLike + ?Sized,
        U: SliceMoveSource<T>,
    {
        Marker::allocate_slice(self, values)
    }

    /// Allocates a copy of a slice by cloning each individual element.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 32], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let message = "foo";
    /// let allocation = marker.allocate_slice_clone(message).unwrap();
    /// assert_eq!(&*allocation, "foo");
    /// ```
    #[inline(always)]
    pub fn allocate_slice_clone<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: SliceLike + ?Sized,
        <T as SliceLike>::Element: Clone,
        U: SliceSource<T>,
    {
        Marker::allocate_slice_clone(self, values)
    }

    /// Allocates a copy of a slice by performing a fast copy (equivalent to
    /// C's `memcpy()` function) into the new allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 32], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let message = "foo";
    /// let allocation = marker.allocate_slice_copy(message).unwrap();
    /// assert_eq!(&*allocation, "foo");
    /// ```
    #[inline(always)]
    pub fn allocate_slice_copy<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: SliceLike + ?Sized,
        <T as SliceLike>::Element: Copy,
        U: SliceSource<T>,
    {
        Marker::allocate_slice_copy(self, values)
    }

    /// Extends an allocation by moving values onto its end, converting the
    /// allocation to a slice if necessary.
    ///
    /// The following requirements must be met to allow elements to be
    /// appended:
    ///
    /// - The marker must be the most recently created front marker from its
    ///   scratchpad.
    /// - The allocation must contain a scalar, array, or slice of the type
    ///   being pushed.
    /// - The data being pushed must be a scalar, array, boxed slice, or
    ///   vector of the same type.
    /// - The allocation must be the most recent allocation made from this
    ///   marker, even if other, more recent allocations have been released.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u32; 5], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let a = marker.allocate([3.14159f32, 2.71828f32]).unwrap();
    ///
    /// let ab = marker.append(a, [0.70711f32]).unwrap();
    /// assert_eq!(*ab, [3.14159f32, 2.71828f32, 0.70711f32]);
    ///
    /// let abc = marker.append(ab, vec![0.57722f32, 1.61803f32]).unwrap();
    /// assert_eq!(
    ///     *abc,
    ///     [3.14159f32, 2.71828f32, 0.70711f32, 0.57722f32, 1.61803f32],
    /// );
    /// ```
    #[inline(always)]
    pub fn append<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, T>, Error<(Allocation<'marker, U>, V)>>
    where
        T: ConcatenateSlice + ?Sized,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: SliceMoveSource<T>,
    {
        Marker::extend(self, allocation, values)
    }

    /// Extends an allocation by cloning values onto its end, converting the
    /// allocation to a slice if necessary.
    ///
    /// The following requirements must be met to allow elements to be
    /// appended:
    ///
    /// - The marker must be the most recently created front marker from its
    ///   scratchpad.
    /// - The allocation must contain a scalar, array, or slice of the type
    ///   being pushed.
    /// - The data being pushed must be a scalar, array, boxed slice, or
    ///   vector of the same type.
    /// - The allocation must be the most recent allocation made from this
    ///   marker, even if other, more recent allocations have been released.
    ///
    /// Note that the `values` parameter is consumed as part of the allocation
    /// process and cannot be returned on error.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Allocation, Scratchpad};
    ///
    /// let scratchpad = Scratchpad::<[u32; 5], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let a: Allocation<[f32]> = marker.allocate([3.14159f32, 2.71828f32])
    ///     .unwrap()
    ///     .into_slice_like_allocation();
    ///
    /// let ab = marker.append_clone(a, &[0.57722f32, 1.61803f32][..])
    ///     .unwrap();
    /// assert_eq!(*ab, [3.14159f32, 2.71828f32, 0.57722f32, 1.61803f32]);
    /// ```
    #[inline(always)]
    pub fn append_clone<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, T>, Error<(Allocation<'marker, U>,)>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Clone,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: SliceSource<T>,
    {
        Marker::extend_clone(self, allocation, values)
    }

    /// Extends an allocation by copying values onto its end, converting the
    /// allocation to a slice if necessary.
    ///
    /// The following requirements must be met to allow elements to be
    /// appended:
    ///
    /// - The marker must be the most recently created front marker from its
    ///   scratchpad.
    /// - The allocation must contain a scalar, array, or slice of the type
    ///   being pushed.
    /// - The data being pushed must be a scalar, array, boxed slice, or
    ///   vector of the same type.
    /// - The allocation must be the most recent allocation made from this
    ///   marker, even if other, more recent allocations have been released.
    ///
    /// Note that the `values` parameter is consumed as part of the allocation
    /// process and cannot be returned on error.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Allocation, Scratchpad};
    ///
    /// let scratchpad = Scratchpad::<[u32; 5], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let a: Allocation<[f32]> = marker.allocate([3.14159f32, 2.71828f32])
    ///     .unwrap()
    ///     .into_slice_like_allocation();
    ///
    /// let ab = marker.append_copy(a, &[0.57722f32, 1.61803f32][..])
    ///     .unwrap();
    /// assert_eq!(*ab, [3.14159f32, 2.71828f32, 0.57722f32, 1.61803f32]);
    /// ```
    #[inline(always)]
    pub fn append_copy<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, T>, Error<(Allocation<'marker, U>,)>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Copy,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: SliceSource<T>,
    {
        Marker::extend_copy(self, allocation, values)
    }

    /// Combines each of the provided slices into a single slice allocated
    /// from this marker, moving the slice values into the new allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let combined = marker.concat_slices(("Hello,", " world", "!"))
    ///     .unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    #[inline(always)]
    pub fn concat_slices<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<(U,)>>
    where
        T: ConcatenateSlice + ?Sized,
        U: SliceMoveSourceCollection<T>,
    {
        Marker::concat_slices(self, values)
    }

    /// Combines cloned copies of each of the provided slices into a single
    /// slice allocated from this marker.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let combined = marker.concat_slices_clone(("Hello,", " world", "!"))
    ///     .unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    #[inline(always)]
    pub fn concat_slices_clone<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Clone,
        U: SliceSourceCollection<T>,
    {
        Marker::concat_slices_clone(self, values)
    }

    /// Combines copies of each of the provided slices into a single slice
    /// allocated from this marker.
    ///
    /// Slices are copied by performing a fast memory copy from each of the
    /// source slices (equivalent to C's `memcpy()` function).
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let combined = marker.concat_slices_copy(("Hello,", " world", "!"))
    ///     .unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    #[inline(always)]
    pub fn concat_slices_copy<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Copy,
        U: SliceSourceCollection<T>,
    {
        Marker::concat_slices_copy(self, values)
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
            markers.data.set(self.index, ::core::usize::MAX);
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
            if markers.data.get(last_index) != ::core::usize::MAX {
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
    pub(crate) scratchpad: &'scratchpad Scratchpad<BufferT, TrackingT>,
    /// Marker index.
    pub(crate) index: usize,
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
    ) -> Result<*mut u8, Error<()>> {
        // Make sure the marker is the bottom-most back marker (no allocations
        // are allowed if a more-recently created back marker is still
        // active).
        let mut markers = self.scratchpad.markers.borrow_mut();
        if markers.back != self.index {
            return Err(Error::new(ErrorKind::MarkerLocked, ()));
        }

        let alignment_mask = alignment - 1;
        debug_assert_eq!(alignment & alignment_mask, 0);

        // Pad the allocation size to match the requested alignment.
        let size = size
            .checked_add(alignment_mask)
            .ok_or_else(|| Error::new(ErrorKind::Overflow, ()))?
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

        let start = buffer_end
            .checked_sub(size)
            .ok_or_else(|| Error::new(ErrorKind::Overflow, ()))?
            & !alignment_mask;
        if start < start_min {
            return Err(Error::new(ErrorKind::InsufficientMemory, ()));
        }

        // Update this marker's offset and return the allocation.
        markers.data.set(self.index, start - buffer_start);

        Ok(start as *mut u8)
    }

    fn is_allocation_at_end<'marker, T, U>(
        &'marker self,
        allocation: &Allocation<'marker, T>,
    ) -> bool
    where
        T: IntoMutSliceLikePtr<U> + ?Sized,
        U: SliceLike + ?Sized,
    {
        let data = unsafe {
            (*T::into_mut_slice_like_ptr(allocation.data.as_ptr()))
                .as_element_slice()
        };

        let data_start = data.as_ptr();

        let buffer_start =
            unsafe { (*self.scratchpad.buffer.get()).as_bytes().as_ptr() };
        let marker_offset =
            self.scratchpad.markers.borrow().data.get(self.index);
        let marker_end =
            unsafe { buffer_start.offset(marker_offset as isize) };

        data_start as usize == marker_end as usize
    }

    #[inline(always)]
    unsafe fn concat_allocations_unchecked<'marker, T, U, V>(
        base: Allocation<'marker, U>,
        extension: Allocation<'marker, V>,
    ) -> Allocation<'marker, T>
    where
        T: ConcatenateSlice + ?Sized,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: IntoMutSliceLikePtr<T> + ?Sized,
    {
        extension.concat_unchecked(base)
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
    pub fn allocate<'marker, T>(
        &'marker self,
        value: T,
    ) -> Result<Allocation<'marker, T>, Error<(T,)>> {
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
    pub fn allocate_default<'marker, T: Default>(
        &'marker self,
    ) -> Result<Allocation<'marker, T>, Error<()>> {
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
    pub unsafe fn allocate_uninitialized<'marker, T>(
        &'marker self,
    ) -> Result<Allocation<'marker, T>, Error<()>> {
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
    pub fn allocate_array<'marker, T: Clone>(
        &'marker self,
        len: usize,
        value: T,
    ) -> Result<Allocation<'marker, [T]>, Error<(T,)>> {
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
    pub fn allocate_array_default<'marker, T: Default>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, [T]>, Error<()>> {
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
    pub fn allocate_array_with<'marker, T, F: FnMut(usize) -> T>(
        &'marker self,
        len: usize,
        func: F,
    ) -> Result<Allocation<'marker, [T]>, Error<()>> {
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
    pub unsafe fn allocate_array_uninitialized<'marker, T>(
        &'marker self,
        len: usize,
    ) -> Result<Allocation<'marker, [T]>, Error<()>> {
        Marker::allocate_array_uninitialized(self, len)
    }

    /// Allocates a slice, initializing its contents by moving the given
    /// values into the allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u64; 3], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let values = [3.14159, 4.14159, 5.14159];
    /// let allocation = marker.allocate_slice(values).unwrap();
    /// let allocation_slice: &[f64] = &*allocation;
    /// assert_eq!(*allocation_slice, [3.14159, 4.14159, 5.14159]);
    /// ```
    #[inline(always)]
    pub fn allocate_slice<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<(U,)>>
    where
        T: SliceLike + ?Sized,
        U: SliceMoveSource<T>,
    {
        Marker::allocate_slice(self, values)
    }

    /// Allocates a copy of a slice by cloning each individual element.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 32], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let message = "foo";
    /// let allocation = marker.allocate_slice_clone(message).unwrap();
    /// assert_eq!(&*allocation, "foo");
    /// ```
    #[inline(always)]
    pub fn allocate_slice_clone<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: SliceLike + ?Sized,
        <T as SliceLike>::Element: Clone,
        U: SliceSource<T>,
    {
        Marker::allocate_slice_clone(self, values)
    }

    /// Allocates a copy of a slice by performing a fast copy (equivalent to
    /// C's `memcpy()` function) into the new allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 32], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let message = "foo";
    /// let allocation = marker.allocate_slice_copy(message).unwrap();
    /// assert_eq!(&*allocation, "foo");
    /// ```
    #[inline(always)]
    pub fn allocate_slice_copy<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: SliceLike + ?Sized,
        <T as SliceLike>::Element: Copy,
        U: SliceSource<T>,
    {
        Marker::allocate_slice_copy(self, values)
    }

    /// Extends an allocation by moving values onto its start, converting the
    /// allocation to a slice if necessary.
    ///
    /// The following requirements must be met to allow elements to be
    /// prepended:
    ///
    /// - The marker must be the most recently created back marker from its
    ///   scratchpad.
    /// - The allocation must contain a scalar, array, or slice of the type
    ///   being pushed.
    /// - The data being pushed must be a scalar, array, boxed slice, or
    ///   vector of the same type.
    /// - The allocation must be the most recent allocation made from this
    ///   marker, even if other, more recent allocations have been released.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u32; 5], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let a = marker.allocate([3.14159f32, 2.71828f32]).unwrap();
    ///
    /// let ab = marker.prepend(a, [0.70711f32]).unwrap();
    /// assert_eq!(*ab, [0.70711f32, 3.14159f32, 2.71828f32]);
    ///
    /// let abc = marker.prepend(ab, vec![0.57722f32, 1.61803f32]).unwrap();
    /// assert_eq!(
    ///     *abc,
    ///     [0.57722f32, 1.61803f32, 0.70711f32, 3.14159f32, 2.71828f32],
    /// );
    /// ```
    #[inline(always)]
    pub fn prepend<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, T>, Error<(Allocation<'marker, U>, V)>>
    where
        T: ConcatenateSlice + ?Sized,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: SliceMoveSource<T>,
    {
        Marker::extend(self, allocation, values)
    }

    /// Extends an allocation by cloning values onto its start, converting the
    /// allocation to a slice if necessary.
    ///
    /// The following requirements must be met to allow elements to be
    /// prepended:
    ///
    /// - The marker must be the most recently created back marker from its
    ///   scratchpad.
    /// - The allocation must contain a scalar, array, or slice of the type
    ///   being pushed.
    /// - The data being pushed must be a scalar, array, boxed slice, or
    ///   vector of the same type.
    /// - The allocation must be the most recent allocation made from this
    ///   marker, even if other, more recent allocations have been released.
    ///
    /// Note that the `values` parameter is consumed as part of the allocation
    /// process and cannot be returned on error.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Allocation, Scratchpad};
    ///
    /// let scratchpad = Scratchpad::<[u32; 5], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let a: Allocation<[f32]> = marker.allocate([3.14159f32, 2.71828f32])
    ///     .unwrap()
    ///     .into_slice_like_allocation();
    ///
    /// let ab = marker.prepend_clone(a, &[0.57722f32, 1.61803f32][..])
    ///     .unwrap();
    /// assert_eq!(*ab, [0.57722f32, 1.61803f32, 3.14159f32, 2.71828f32]);
    /// ```
    #[inline(always)]
    pub fn prepend_clone<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, T>, Error<(Allocation<'marker, U>,)>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Clone,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: SliceSource<T>,
    {
        Marker::extend_clone(self, allocation, values)
    }

    /// Extends an allocation by copying values onto its start, converting the
    /// allocation to a slice if necessary.
    ///
    /// The following requirements must be met to allow elements to be
    /// prepended:
    ///
    /// - The marker must be the most recently created back marker from its
    ///   scratchpad.
    /// - The allocation must contain a scalar, array, or slice of the type
    ///   being pushed.
    /// - The data being pushed must be a scalar, array, boxed slice, or
    ///   vector of the same type.
    /// - The allocation must be the most recent allocation made from this
    ///   marker, even if other, more recent allocations have been released.
    ///
    /// Note that the `values` parameter is consumed as part of the allocation
    /// process and cannot be returned on error.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Allocation, Scratchpad};
    ///
    /// let scratchpad = Scratchpad::<[u32; 5], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let a: Allocation<[f32]> = marker.allocate([3.14159f32, 2.71828f32])
    ///     .unwrap()
    ///     .into_slice_like_allocation();
    ///
    /// let ab = marker.prepend_copy(a, &[0.57722f32, 1.61803f32][..])
    ///     .unwrap();
    /// assert_eq!(*ab, [0.57722f32, 1.61803f32, 3.14159f32, 2.71828f32]);
    /// ```
    #[inline(always)]
    pub fn prepend_copy<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, T>, Error<(Allocation<'marker, U>,)>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Copy,
        U: IntoMutSliceLikePtr<T> + ?Sized,
        V: SliceSource<T>,
    {
        Marker::extend_copy(self, allocation, values)
    }

    /// Combines each of the provided slices into a single slice allocated
    /// from this marker, moving the slice values into the new allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let combined = marker.concat_slices(("Hello,", " world", "!"))
    ///     .unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    #[inline(always)]
    pub fn concat_slices<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<(U,)>>
    where
        T: ConcatenateSlice + ?Sized,
        U: SliceMoveSourceCollection<T>,
    {
        Marker::concat_slices(self, values)
    }

    /// Combines cloned copies of each of the provided slices into a single
    /// slice allocated from this marker.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let combined = marker.concat_slices_clone(("Hello,", " world", "!"))
    ///     .unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    #[inline(always)]
    pub fn concat_slices_clone<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Clone,
        U: SliceSourceCollection<T>,
    {
        Marker::concat_slices_clone(self, values)
    }

    /// Combines copies of each of the provided slices into a single slice
    /// allocated from this marker.
    ///
    /// Slices are copied by performing a fast memory copy from each of the
    /// source slices (equivalent to C's `memcpy()` function).
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let combined = marker.concat_slices_copy(("Hello,", " world", "!"))
    ///     .unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    #[inline(always)]
    pub fn concat_slices_copy<'marker, T, U>(
        &'marker self,
        values: U,
    ) -> Result<Allocation<'marker, T>, Error<()>>
    where
        T: ConcatenateSlice + ?Sized,
        <T as SliceLike>::Element: Copy,
        U: SliceSourceCollection<T>,
    {
        Marker::concat_slices_copy(self, values)
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
            markers.data.set(self.index, ::core::usize::MAX);
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

            if markers.data.get(back) != ::core::usize::MAX {
                break;
            }
        }

        markers.back = back;
    }
}
