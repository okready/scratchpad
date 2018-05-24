// Copyright 2018 Theodore Cipicchio
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
    Allocation, Buffer, Error, ErrorKind, IntoSliceAllocation, OwnedSlice,
    Scratchpad, Tracking,
};
use core::marker::PhantomData;
use core::mem::{align_of, size_of, transmute};
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
        let data = self.allocate_memory(align_of::<T>(), size_of::<T>(), 1)?;

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
            )).unwrap(),
            _phantom: PhantomData,
        })
    }

    /// Combines each of the provided strings into a single string slice
    /// allocated from this marker.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let combined = marker.concat(&["Hello,", " world", "!"]).unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    fn concat<'marker, IntoIteratorT, IteratorT, StrT>(
        &'marker self,
        strings: IntoIteratorT,
    ) -> Result<Allocation<'marker, str>, Error<()>>
    where
        IntoIteratorT: IntoIterator<Item = StrT, IntoIter = IteratorT>,
        IteratorT: Iterator<Item = StrT> + Clone,
        StrT: AsRef<str>,
    {
        let iter = strings.into_iter();
        let size = iter
            .clone()
            .fold(0usize, |sum, item| sum + item.as_ref().as_bytes().len());
        let mut result =
            unsafe { self.allocate_array_uninitialized::<u8>(size)? };

        let mut start = 0usize;
        for item in iter {
            let bytes = item.as_ref().as_bytes();
            let end = start + bytes.len();
            result[start..end].copy_from_slice(&bytes[..]);
            start = end;
        }

        unsafe { Ok(transmute(result)) }
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

    /// Combines each of the provided strings into a single string slice
    /// allocated from this marker.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_front().unwrap();
    ///
    /// let combined = marker.concat(&["Hello,", " world", "!"]).unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    #[inline(always)]
    pub fn concat<'marker, IntoIteratorT, IteratorT, StrT>(
        &'marker self,
        strings: IntoIteratorT,
    ) -> Result<Allocation<'marker, str>, Error<()>>
    where
        IntoIteratorT: IntoIterator<Item = StrT, IntoIter = IteratorT>,
        IteratorT: Iterator<Item = StrT> + Clone,
        StrT: AsRef<str>,
    {
        Marker::concat(self, strings)
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
    /// let ab = marker.append(a, 0.70711f32).unwrap();
    /// assert_eq!(*ab, [3.14159f32, 2.71828f32, 0.70711f32]);
    ///
    /// let abc = marker.append(ab, vec![0.57722f32, 1.61803f32]).unwrap();
    /// assert_eq!(
    ///     *abc,
    ///     [3.14159f32, 2.71828f32, 0.70711f32, 0.57722f32, 1.61803f32],
    /// );
    /// ```
    pub fn append<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, [T]>, Error<(Allocation<'marker, U>, V)>>
    where
        U: ?Sized,
        Allocation<'marker, U>: IntoSliceAllocation<'marker, T>,
        V: OwnedSlice<T>,
    {
        // Verify that the allocation is at the end of the marker.
        let data = unsafe {
            &*IntoSliceAllocation::<'marker, T>::as_slice_ptr(&allocation)
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

        if data_end as usize != marker_end as usize {
            return Err(Error::new(ErrorKind::NotAtEnd, (allocation, values)));
        }

        // Create a new allocation for the value given and merge the two
        // allocations. This will also perform all remaining validity checks.
        let source = unsafe { &*values.as_slice_ptr() };
        match self.allocate_array_with::<T, _>(source.len(), |index| unsafe {
            ptr::read(&source[index])
        }) {
            Err(e) => Err(e.map(|()| (allocation, values))),
            Ok(val_alloc) => unsafe {
                OwnedSlice::<T>::drop_container(values);
                Ok(allocation.concat_unchecked::<T, [T]>(val_alloc))
            },
        }
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

    /// Combines each of the provided strings into a single string slice
    /// allocated from this marker.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::Scratchpad;
    ///
    /// let scratchpad = Scratchpad::<[u8; 16], [usize; 1]>::static_new();
    /// let marker = scratchpad.mark_back().unwrap();
    ///
    /// let combined = marker.concat(&["Hello,", " world", "!"]).unwrap();
    /// assert_eq!(&*combined, "Hello, world!");
    /// ```
    #[inline(always)]
    pub fn concat<'marker, IntoIteratorT, IteratorT, StrT>(
        &'marker self,
        strings: IntoIteratorT,
    ) -> Result<Allocation<'marker, str>, Error<()>>
    where
        IntoIteratorT: IntoIterator<Item = StrT, IntoIter = IteratorT>,
        IteratorT: Iterator<Item = StrT> + Clone,
        StrT: AsRef<str>,
    {
        Marker::concat(self, strings)
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
    /// let ab = marker.prepend(a, 0.70711f32).unwrap();
    /// assert_eq!(*ab, [0.70711f32, 3.14159f32, 2.71828f32]);
    ///
    /// let abc = marker.prepend(ab, vec![0.57722f32, 1.61803f32]).unwrap();
    /// assert_eq!(
    ///     *abc,
    ///     [0.57722f32, 1.61803f32, 0.70711f32, 3.14159f32, 2.71828f32],
    /// );
    /// ```
    pub fn prepend<'marker, T, U, V>(
        &'marker self,
        allocation: Allocation<'marker, U>,
        values: V,
    ) -> Result<Allocation<'marker, [T]>, Error<(Allocation<'marker, U>, V)>>
    where
        U: ?Sized,
        Allocation<'marker, U>: IntoSliceAllocation<'marker, T>,
        V: OwnedSlice<T>,
    {
        // Verify that the allocation is at the end of the marker.
        let data_start = unsafe {
            &*IntoSliceAllocation::<'marker, T>::as_slice_ptr(&allocation)
        }.as_ptr();

        let buffer_start =
            unsafe { (*self.scratchpad.buffer.get()).as_bytes().as_ptr() };
        let marker_offset =
            self.scratchpad.markers.borrow().data.get(self.index);
        let marker_end =
            unsafe { buffer_start.offset(marker_offset as isize) };

        if data_start as usize != marker_end as usize {
            return Err(Error::new(ErrorKind::NotAtEnd, (allocation, values)));
        }

        // Create a new allocation for the value given and merge the two
        // allocations. This will also perform all remaining validity checks.
        let source = unsafe { &*values.as_slice_ptr() };
        match self.allocate_array_with::<T, _>(source.len(), |index| unsafe {
            ptr::read(&source[index])
        }) {
            Err(e) => Err(e.map(|()| (allocation, values))),
            Ok(val_alloc) => unsafe {
                OwnedSlice::<T>::drop_container(values);
                Ok(val_alloc.concat_unchecked::<T, U>(allocation))
            },
        }
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
