// Copyright 2018 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! `Scratchpad` type implementation.

use core::fmt;

use super::{AllocateError, Buffer, MarkerBack, MarkerFront, StaticBuffer,
            Tracking};
use core::cell::{RefCell, UnsafeCell};
use core::mem::{size_of, uninitialized};

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
                back: ::core::usize::MAX, // Lazy initialization.
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
    pub fn static_new() -> Self {
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
    ) -> Result<MarkerFront<'scratchpad, BufferT, TrackingT>, AllocateError>
    {
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
            return Err(AllocateError::MarkerLimit);
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
    ) -> Result<MarkerBack<'scratchpad, BufferT, TrackingT>, AllocateError>
    {
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
            return Err(AllocateError::MarkerLimit);
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
