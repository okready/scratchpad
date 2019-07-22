// Copyright 2018-2019 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! `Scratchpad` type implementation.

use core::fmt;
use core::ptr;

use super::{
    Buffer, Error, ErrorKind, MarkerBack, MarkerFront, StaticBuffer, Tracking,
};
use core::cell::{Cell, UnsafeCell};
use core::mem::size_of;
#[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
use core::mem::uninitialized;
#[cfg(any(stable_maybe_uninit, feature = "unstable"))]
use core::mem::MaybeUninit;
use core::ops::{Deref, DerefMut};

/// Front and back stacks for `Marker` tracking (used internally).
pub(crate) struct MarkerStacks<TrackingT>
where
    TrackingT: Tracking,
{
    /// Stack data.
    pub(crate) data: TrackingT,
    /// Front stack offset.
    pub(crate) front: usize,
    /// Back stack offset.
    pub(crate) back: usize,
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

/// `BorrowFlag` replacement for internal `RefCell` type.
type BorrowFlag = usize;
const UNUSED: BorrowFlag = 0usize;
const WRITING: BorrowFlag = !0usize;

/// `Ref` replacement for internal `RefCell` type.
///
/// This only implements the parts of `core::cell::Ref` used by this crate and
/// is not intended as a full replacement.
pub(crate) struct Ref<'a, T>
where
    T: 'a,
{
    cell: &'a RefCell<T>,
}

impl<'a, T> Drop for Ref<'a, T> {
    #[inline]
    fn drop(&mut self) {
        let borrow = self.cell.borrow.get();
        debug_assert_ne!(borrow, UNUSED);
        debug_assert_ne!(borrow, WRITING);
        self.cell.borrow.set(borrow - 1);
    }
}

impl<'a, T> Deref for Ref<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        unsafe { &*self.cell.value.get() }
    }
}

impl<'a, T> fmt::Debug for Ref<'a, T>
where
    T: fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

/// `RefMut` replacement for internal `RefCell` type.
///
/// This only implements the parts of `core::cell::RefMut` used by this crate
/// and is not intended as a full replacement.
pub(crate) struct RefMut<'a, T>
where
    T: 'a,
{
    cell: &'a RefCell<T>,
}

impl<'a, T> Drop for RefMut<'a, T> {
    #[inline]
    fn drop(&mut self) {
        debug_assert_eq!(self.cell.borrow.get(), WRITING);
        self.cell.borrow.set(UNUSED);
    }
}

impl<'a, T> Deref for RefMut<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        unsafe { &*self.cell.value.get() }
    }
}

impl<'a, T> DerefMut for RefMut<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.cell.value.get() }
    }
}

impl<'a, T> fmt::Debug for RefMut<'a, T>
where
    T: fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

/// `RefCell` replacement for internal use.
///
/// In order to support initialization of a `RefCell<MarkerStacks<_>>` using
/// `Scratchpad::static_new_in_place()`, we would need to be able to rely on
/// the internals of `RefCell` to have a specific layout. It is likely
/// dangerous to assume that its internals won't change over time, so we'll
/// instead use a custom type whose layout we can depend on over future
/// versions.
pub(crate) struct RefCell<T> {
    borrow: Cell<BorrowFlag>,
    value: UnsafeCell<T>,
}

impl<T> RefCell<T> {
    #[inline]
    #[cfg(feature = "unstable")]
    pub(crate) const fn new(value: T) -> Self {
        RefCell {
            borrow: Cell::new(UNUSED),
            value: UnsafeCell::new(value),
        }
    }

    #[inline]
    #[cfg(not(feature = "unstable"))]
    pub(crate) fn new(value: T) -> Self {
        RefCell {
            borrow: Cell::new(UNUSED),
            value: UnsafeCell::new(value),
        }
    }

    /// Creates a `RefCell` in uninitialized memory, leaving its value
    /// uninitialized.
    #[inline]
    pub(crate) unsafe fn new_uninitialized_value_in_place(dst: *mut Self) {
        // `UnsafeCell<T>` simply wraps a `T` value, so we don't need to do
        // any special initialization for the `value` field.
        ptr::write(&mut (*dst).borrow, Cell::new(UNUSED));
    }

    #[inline]
    pub(crate) fn borrow(&self) -> Ref<T> {
        let borrow = self.borrow.get();
        assert_ne!(borrow, WRITING);
        self.borrow.set(borrow + 1);
        Ref { cell: self }
    }

    #[inline]
    pub(crate) fn borrow_mut(&self) -> RefMut<T> {
        assert_eq!(self.borrow.get(), UNUSED);
        self.borrow.set(WRITING);
        RefMut { cell: self }
    }

    #[inline]
    pub(crate) fn get_mut(&mut self) -> &mut T {
        unsafe { &mut *self.value.get() }
    }
}

impl<T> fmt::Debug for RefCell<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `Debug` implementation for `core::cell::RefCell` won't show the
        // cell's value if it is mutably borrowed, but that seems unnecessary
        // since we won't be holding on to the immutably borrowed value
        // outside the scope of this function (and since `RefCell` isn't
        // `Sync`, we can expect it to not be modified while in this
        // function).
        f.debug_struct("RefCell")
            .field("value", unsafe { &*self.value.get() })
            .finish()
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
    pub(crate) buffer: UnsafeCell<BufferT>,
    /// Dual stack containing the offsets of each active marker. If a marker
    /// not at the end of one of the stacks is freed, its offset is set to
    /// `core::usize::MAX` to indicate it is no longer active until the
    /// allocations that came after it (in the same stack) have also been
    /// freed.
    pub(crate) markers: RefCell<MarkerStacks<TrackingT>>,
}

impl<BufferT, TrackingT> Scratchpad<BufferT, TrackingT>
where
    BufferT: Buffer,
    TrackingT: Tracking,
{
    /// Creates a new scratchpad instance.
    ///
    /// Note that using large static arrays for allocation storage or marker
    /// tracking can cause the program to run out of stack space while calling
    /// this function. It is recommended to either use borrowed slices or
    /// boxed slices if this occurs, or alternatively use the unsafe
    /// [`static_new_in_place()`] function to create the `Scratchpad`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate scratchpad;
    /// use scratchpad::Scratchpad;
    /// # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    /// # use std::mem::uninitialized;
    /// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    /// use std::mem::MaybeUninit;
    ///
    /// # fn main() {
    /// // Creates a scratchpad that can hold up to 256 bytes of data and up
    /// // to 4 allocation markers. The initial contents of each buffer are
    /// // ignored, so we can provide uninitialized data in order to reduce
    /// // the runtime overhead of creating a scratchpad.
    /// # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    /// # let scratchpad = unsafe { Scratchpad::new(
    /// #     uninitialized::<array_type_for_bytes!(u64, 256)>(),
    /// #     uninitialized::<array_type_for_markers!(usize, 4)>(),
    /// # ) };
    /// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    /// let scratchpad = unsafe { Scratchpad::new(
    ///     MaybeUninit::<array_type_for_bytes!(MaybeUninit<u64>, 256)>::uninit().assume_init(),
    ///     MaybeUninit::<array_type_for_markers!(MaybeUninit<usize>, 4)>::uninit().assume_init(),
    /// ) };
    /// # }
    /// ```
    ///
    /// [`static_new_in_place()`]: #method.static_new_in_place
    #[inline(always)]
    #[cfg(feature = "unstable")]
    pub const fn new(buffer: BufferT, tracking: TrackingT) -> Self {
        Scratchpad {
            buffer: UnsafeCell::new(buffer),
            markers: RefCell::new(MarkerStacks {
                data: tracking,
                front: 0,
                back: ::core::usize::MAX, // Lazy initialization.
            }),
        }
    }

    /// Creates a new scratchpad instance.
    ///
    /// Note that using large static arrays for allocation storage or marker
    /// tracking can cause the program to run out of stack space while calling
    /// this function. It is recommended to either use borrowed slices or
    /// boxed slices if this occurs, or alternatively use the unsafe
    /// [`static_new_in_place()`] function to create the `Scratchpad`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate scratchpad;
    /// use scratchpad::Scratchpad;
    /// # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    /// # use std::mem::uninitialized;
    /// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    /// use std::mem::MaybeUninit;
    ///
    /// # fn main() {
    /// // Creates a scratchpad that can hold up to 256 bytes of data and up
    /// // to 4 allocation markers. The initial contents of each buffer are
    /// // ignored, so we can provide uninitialized data in order to reduce
    /// // the runtime overhead of creating a scratchpad.
    /// # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    /// # let scratchpad = unsafe { Scratchpad::new(
    /// #     uninitialized::<array_type_for_bytes!(u64, 256)>(),
    /// #     uninitialized::<array_type_for_markers!(usize, 4)>(),
    /// # ) };
    /// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    /// let scratchpad = unsafe { Scratchpad::new(
    ///     MaybeUninit::<array_type_for_bytes!(MaybeUninit<u64>, 256)>::uninit().assume_init(),
    ///     MaybeUninit::<array_type_for_markers!(MaybeUninit<usize>, 4)>::uninit().assume_init(),
    /// ) };
    /// # }
    /// ```
    ///
    /// [`static_new_in_place()`]: #method.static_new_in_place
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
    /// Note that using large static arrays for allocation storage or marker
    /// tracking can cause the program to run out of stack space while calling
    /// this function. It is recommended to either use borrowed slices or
    /// boxed slices if this occurs, or alternatively use the unsafe
    /// [`static_new_in_place()`] function to create the `Scratchpad`.
    ///
    /// It is strongly recommended to use arrays of [`MaybeUninit`] elements
    /// for both allocation and tracking storage if possible when using this
    /// function. Even though [`ByteData`] types can safely store any pattern
    /// of bits without causing undefined behavior, the rules in Rust for
    /// using integers whose memory is specifically uninitialized have not
    /// been finalized.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate scratchpad;
    /// use scratchpad::Scratchpad;
    /// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    /// use std::mem::MaybeUninit;
    ///
    /// # fn main() {
    /// // Creates a scratchpad that can hold up to 256 bytes of data and up
    /// // to 4 allocation markers.
    /// # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    /// # let scratchpad = Scratchpad::<
    /// #     array_type_for_bytes!(u64, 256),
    /// #     array_type_for_markers!(usize, 4),
    /// # >::static_new();
    /// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    /// let scratchpad = Scratchpad::<
    ///     array_type_for_bytes!(MaybeUninit<u64>, 256),
    ///     array_type_for_markers!(MaybeUninit<usize>, 4),
    /// >::static_new();
    /// # }
    /// ```
    ///
    /// [`Buffer`]: trait.Buffer.html
    /// [`Tracking`]: trait.Tracking.html
    /// [`static_new_in_place()`]: #method.static_new_in_place
    /// [`MaybeUninit`]: https://doc.rust-lang.org/std/mem/union.MaybeUninit.html
    /// [`ByteData`]: trait.ByteData.html
    #[inline(always)]
    pub fn static_new() -> Self {
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        return Scratchpad {
            buffer: unsafe { MaybeUninit::uninit().assume_init() },
            markers: RefCell::new(MarkerStacks {
                data: unsafe { MaybeUninit::uninit().assume_init() },
                front: 0,
                back: size_of::<TrackingT>() / size_of::<usize>(),
            }),
        };

        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        return Scratchpad {
            buffer: unsafe { uninitialized() },
            markers: RefCell::new(MarkerStacks {
                data: unsafe { uninitialized() },
                front: 0,
                back: size_of::<TrackingT>() / size_of::<usize>(),
            }),
        };
    }

    /// Initializes a new instance in uninitialized memory for scratchpad
    /// types backed entirely by static arrays.
    ///
    /// This is provided to allow for creation of scratchpads backed by large
    /// static arrays while guaranteeing that both arrays and the created
    /// `Scratchpad` are never accidentally stored on the stack, avoiding
    /// possible stack overflow.
    ///
    /// It is strongly recommended to use arrays of [`MaybeUninit`] elements
    /// for both allocation and tracking storage if possible when using this
    /// function. Even though [`ByteData`] types can safely store any pattern
    /// of bits without causing undefined behavior, the rules in Rust for
    /// using integers whose memory is specifically uninitialized have not
    /// been finalized.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it operates on a raw pointer.
    ///
    /// It does not drop any existing contents of `dst` before writing, nor
    /// does it check for whether `dst` is a valid pointer.
    ///
    /// `dst` must be properly aligned for storage of an instance of
    /// `Scratchpad`.
    ///
    /// After returning, the contents of `dst` will need to be dropped when
    /// the scratchpad is no longer in use before the memory pointed to by
    /// `dst` is freed.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate scratchpad;
    /// use scratchpad::{CacheAligned, Scratchpad};
    /// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    /// use std::mem::MaybeUninit;
    ///
    /// // Scratchpad that can hold up to 1 MB of data and up to 16 allocation
    /// // markers.
    /// # #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    /// # type LargeScratchpad = Scratchpad<
    /// #     array_type_for_bytes!(CacheAligned, 1024 * 1024),
    /// #     array_type_for_markers!(usize, 16),
    /// # >;
    /// # #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    /// type LargeScratchpad = Scratchpad<
    ///     array_type_for_bytes!(MaybeUninit<CacheAligned>, 1024 * 1024),
    ///     array_type_for_markers!(MaybeUninit<usize>, 16),
    /// >;
    ///
    /// # fn main() {
    /// unsafe {
    ///     // The `Vec` here represents any region of memory in which a
    ///     // `Scratchpad` needs to be initialized at runtime, whether
    ///     // allocated from the heap or elsewhere.
    ///     let mut memory = Vec::with_capacity(1);
    ///     memory.set_len(1);
    ///
    ///     LargeScratchpad::static_new_in_place(memory.as_mut_ptr());
    ///
    ///     let scratchpad = &memory[0];
    ///     let marker = scratchpad.mark_front().unwrap();
    ///     let allocation = marker.allocate(12).unwrap();
    ///     assert_eq!(*allocation, 12);
    /// }
    /// # }
    /// ```
    ///
    /// [`MaybeUninit`]: https://doc.rust-lang.org/std/mem/union.MaybeUninit.html
    /// [`ByteData`]: trait.ByteData.html
    #[inline]
    pub unsafe fn static_new_in_place(dst: *mut Self) {
        // `UnsafeCell<T>` simply wraps a `T` value, so we don't need to do
        // any special initialization for the `buffer` or `MarkerStacks::data`
        // fields.
        RefCell::new_uninitialized_value_in_place(&mut (*dst).markers);
        let markers = (*dst).markers.get_mut();
        ptr::write(&mut markers.front, 0);
        ptr::write(&mut markers.back, markers.data.capacity());
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
    ) -> Result<MarkerFront<'scratchpad, BufferT, TrackingT>, Error<()>> {
        let mut markers = self.markers.borrow_mut();

        // `markers.back` is lazy-initialized when the "unstable" feature is
        // enabled so that `Scratchpad::new()` can be a `const` function.
        #[cfg(feature = "unstable")]
        {
            if markers.back == ::core::usize::MAX {
                markers.back = markers.data.capacity();
            }
        }

        let index = markers.front;
        if index == markers.back {
            return Err(Error::new(ErrorKind::MarkerLimit, ()));
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
    ) -> Result<MarkerBack<'scratchpad, BufferT, TrackingT>, Error<()>> {
        let mut markers = self.markers.borrow_mut();

        // `markers.back` is lazy-initialized when the "unstable" feature is
        // enabled so that `Scratchpad::new()` can be a `const` function.
        #[cfg(feature = "unstable")]
        {
            if markers.back == ::core::usize::MAX {
                markers.back = markers.data.capacity();
            }
        }

        let mut index = markers.back;
        if index == markers.front {
            return Err(Error::new(ErrorKind::MarkerLimit, ()));
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
