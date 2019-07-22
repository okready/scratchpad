// Copyright 2018-2019 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::*;
use arrayvec::ArrayVec;
use core::cell::Cell;
use core::mem::align_of;
#[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
use core::mem::uninitialized;
#[cfg(any(stable_maybe_uninit, feature = "unstable"))]
use core::mem::MaybeUninit;

#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
use super::{Box, Vec};

// Struct that increments a counter each time it is dropped (used for testing
// for accidental drops due to incorrect moving of values in unsafe code).
#[derive(Clone, Debug)]
struct DropCounter<'a> {
    count: &'a Cell<usize>,
}

impl<'a> DropCounter<'a> {
    #[inline]
    fn new(count: &'a Cell<usize>) -> Self {
        Self { count }
    }
}

impl<'a> Drop for DropCounter<'a> {
    #[inline]
    fn drop(&mut self) {
        self.count.set(self.count.get() + 1);
    }
}

// Trait for accessing the internals of `Marker` structs.
trait MarkerInternal: Marker {
    fn index(&self) -> usize;
}

impl<'scratchpad, BufferT, TrackingT> MarkerInternal
    for MarkerFront<'scratchpad, BufferT, TrackingT>
where
    BufferT: Buffer + 'scratchpad,
    TrackingT: Tracking + 'scratchpad,
{
    fn index(&self) -> usize {
        self.index
    }
}

impl<'scratchpad, BufferT, TrackingT> MarkerInternal
    for MarkerBack<'scratchpad, BufferT, TrackingT>
where
    BufferT: Buffer + 'scratchpad,
    TrackingT: Tracking + 'scratchpad,
{
    fn index(&self) -> usize {
        self.scratchpad.markers.borrow().data.capacity() - self.index - 1
    }
}

// `PartialEq` implementation for comparing the `Marker` offsets in a
// `MarkerStacks` against a pair of slices of expected offsets for unit tests.
impl<T, U, TrackingT> PartialEq<(T, U)> for MarkerStacks<TrackingT>
where
    T: AsRef<[usize]>,
    U: AsRef<[usize]>,
    TrackingT: Tracking,
{
    fn eq(&self, other: &(T, U)) -> bool {
        let front_slice = other.0.as_ref();
        if self.front != front_slice.len() {
            return false;
        }

        let capacity = self.data.capacity();

        // `back` is lazy-initialized on the first call to either
        // `mark_front()` or `mark_back()` when the "unstable" crate feature
        // is enabled, so we need to account for it possibly being
        // uninitialized.
        let mut back = self.back;
        if back == core::usize::MAX {
            back = capacity;
        }

        let back_len = capacity - back;
        let back_slice = other.1.as_ref();
        if back_len != back_slice.len() {
            return false;
        }

        for (index, &other_element) in front_slice.iter().enumerate() {
            if self.data.get(index) != other_element {
                return false;
            }
        }

        let last_index = capacity - 1;
        for (index, &other_element) in back_slice.iter().enumerate() {
            if self.data.get(last_index - index) != other_element {
                return false;
            }
        }

        true
    }
}

/// Scratchpad allocation buffer size for basic tests, in bytes.
const BUFFER_SIZE: usize = 32;
/// Max number of scratchpad markers that can be set for basic tests.
const MAX_MARKERS: usize = 4;

/// Scratchpad allocation buffer type for `validate_basic_operations()` (32
/// bytes of allocated data).
#[cfg(any(stable_maybe_uninit, feature = "unstable"))]
type SimpleBuffer = array_type_for_bytes!(MaybeUninit<u64>, BUFFER_SIZE);
#[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
type SimpleBuffer = array_type_for_bytes!(u64, BUFFER_SIZE);

/// Scratchpad marker tracking buffer type for `validate_basic_operations()`
/// (4 markers).
#[cfg(any(stable_maybe_uninit, feature = "unstable"))]
type SimpleTracking =
    array_type_for_markers!(MaybeUninit<usize>, MAX_MARKERS);
#[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
type SimpleTracking = array_type_for_markers!(usize, MAX_MARKERS);

/// `Scratchpad` type for `validate_basic_operations()`.
type SimpleScratchpad = Scratchpad<SimpleBuffer, SimpleTracking>;

/// Unwraps the error returned from a scratchpad `mark` method, panicking if
/// the `mark` method returned a valid marker.
fn unwrap_mark_err<M>(result: Result<M, Error<()>>) -> Error<()>
where
    M: Marker,
{
    match result {
        Ok(_) => panic!("expected error, but received marker"),
        Err(err) => err,
    }
}

/// Shared implementation for `validate_front_operations()` and
/// `validate_back_operations()`.
fn validate_basic_operations<'scratchpad, MF, CF, M>(
    scratchpad: &'scratchpad SimpleScratchpad,
    mark: MF,
    conv: CF,
) where
    MF: Fn(&'scratchpad SimpleScratchpad) -> Result<M, Error<()>>,
    CF: for<'a> Fn(
        &'a [usize],
    ) -> (
        ArrayVec<[usize; MAX_MARKERS]>,
        ArrayVec<[usize; MAX_MARKERS]>,
    ),
    M: MarkerInternal,
{
    // Rust should guarantee a default alignment of at least 8 bytes (for
    // allocations of 8 bytes or more), so verify our buffer is aligned as
    // such.
    assert_eq!(*scratchpad.markers.borrow(), conv(&[][..]));
    unsafe {
        assert_eq!((*scratchpad.buffer.get()).as_ptr() as usize & 0x7, 0);
    }

    // Set an initial marker, `a`, for allocations.
    let a = mark(scratchpad).unwrap();
    assert_eq!(a.index(), 0);
    assert_eq!(*scratchpad.markers.borrow(), conv(&[0][..]));

    let a_padding = {
        // Allocate a 32-bit integer and 64-bit floating-point value. The
        // 32-bit integer requires 4-byte alignment, which should be satisfied
        // by the base alignment of the buffer. The 64-bit floating-point
        // value will require either 4- or 8-byte alignment depending on the
        // platform, so padding may be added after the integer (result should
        // either be 12 or 16 bytes).
        let a0 = a.allocate(5u32).unwrap();
        assert_eq!(*a0, 5u32);
        assert_eq!(*scratchpad.markers.borrow(), conv(&[4][..]));

        let a1_padding = align_of::<f64>() - 4;

        let a1 = a.allocate(12.5f64).unwrap();
        assert_eq!(*a0, 5u32);
        assert_eq!(*a1, 12.5f64);
        assert_eq!(
            *scratchpad.markers.borrow(),
            conv(&[12 + a1_padding][..]),
        );

        a1_padding
    };

    // Set another marker, `b`.
    let b = mark(scratchpad).unwrap();
    assert_eq!(b.index(), 1);
    assert_eq!(
        *scratchpad.markers.borrow(),
        conv(&[12 + a_padding, 12 + a_padding][..]),
    );

    // Attempt another allocation from marker `a`, which should fail since a
    // more recently created marker, `b`, is still active.
    assert_eq!(a.allocate(3u8).unwrap_err().kind(), ErrorKind::MarkerLocked);

    {
        // Allocate an 8-bit integer and 16-bit integer from marker `b`. The
        // 8-bit integer only requires 1-byte alignment, but the 16-bit
        // integer requires 2-byte alignment.
        let b0 = b.allocate(3u8).unwrap();
        assert_eq!(*b0, 3u8);
        assert_eq!(
            *scratchpad.markers.borrow(),
            conv(&[12 + a_padding, 13 + a_padding][..]),
        );
        let b1 = b.allocate(9u16).unwrap();
        assert_eq!(*b0, 3u8);
        assert_eq!(*b1, 9u16);
        assert_eq!(
            *scratchpad.markers.borrow(),
            conv(&[12 + a_padding, 16 + a_padding][..]),
        );
    }

    // Attempt to allocate an array of 20 bytes, which should fail due to
    // insufficient space.
    assert_eq!(
        b.allocate_array(20, 24u8).unwrap_err().kind(),
        ErrorKind::InsufficientMemory,
    );

    // Set a new marker, `c`.
    let c = mark(scratchpad).unwrap();
    assert_eq!(c.index(), 2);
    assert_eq!(
        *scratchpad.markers.borrow(),
        conv(&[12 + a_padding, 16 + a_padding, 16 + a_padding][..]),
    );

    {
        // Allocate an array of 12 bytes from `c`.
        let c0 = c.allocate_array(12, 17u8).unwrap();
        assert_eq!(*c0, [17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17]);
        assert_eq!(
            *scratchpad.markers.borrow(),
            conv(&[12 + a_padding, 16 + a_padding, 28 + a_padding][..]),
        );
    }

    // Set a new marker, `d`. Even though our buffer may now be full, we can
    // still set new markers (we simply won't be able to allocate from them).
    let d = mark(scratchpad).unwrap();
    assert_eq!(d.index(), 3);
    assert_eq!(
        *scratchpad.markers.borrow(),
        conv(
            &[
                12 + a_padding,
                16 + a_padding,
                28 + a_padding,
                28 + a_padding,
            ][..],
        ),
    );

    // Attempt to set another marker, which should fail due to reaching
    // our marker limit.
    assert_eq!(
        unwrap_mark_err(mark(scratchpad)).kind(),
        ErrorKind::MarkerLimit
    );

    // Release marker `a` and re-attempt creation of a new marker. This should
    // still fail since the reservation for marker `a` cannot be reclaimed
    // until all subsequent markers have been released.
    drop(a);
    assert_eq!(
        *scratchpad.markers.borrow(),
        conv(
            &[
                core::usize::MAX,
                16 + a_padding,
                28 + a_padding,
                28 + a_padding,
            ][..],
        ),
    );
    assert_eq!(
        unwrap_mark_err(mark(scratchpad)).kind(),
        ErrorKind::MarkerLimit
    );

    // Release marker `c`.
    drop(c);
    assert_eq!(
        *scratchpad.markers.borrow(),
        conv(
            &[
                core::usize::MAX,
                16 + a_padding,
                core::usize::MAX,
                28 + a_padding,
            ][..],
        ),
    );

    // Release marker `d`. This should free up the reservations for both `c`
    // and `d` now that there are no more active markers that follow them.
    drop(d);
    assert_eq!(
        *scratchpad.markers.borrow(),
        conv(&[core::usize::MAX, 16 + a_padding][..]),
    );

    // Create a new marker, `e`, that should succeed now that we have
    // available marker stack space.
    let e = mark(scratchpad).unwrap();
    assert_eq!(e.index(), 2);
    assert_eq!(
        *scratchpad.markers.borrow(),
        conv(&[core::usize::MAX, 16 + a_padding, 16 + a_padding][..]),
    );

    // Release markers `e` and `b`, ensuring the marker stack is cleaned up
    // properly in the process.
    drop(e);
    assert_eq!(
        *scratchpad.markers.borrow(),
        conv(&[core::usize::MAX, 16 + a_padding][..]),
    );
    drop(b);
    assert_eq!(*scratchpad.markers.borrow(), ([], []));
}

/// General test for validating that the internal state of a `Scratchpad` and
/// `Marker` match the expected state across various "front" operations.
#[test]
fn validate_front_operations() {
    #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    let scratchpad = unsafe {
        Scratchpad::new(
            MaybeUninit::<SimpleBuffer>::uninit().assume_init(),
            MaybeUninit::<SimpleTracking>::uninit().assume_init(),
        )
    };
    #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    let scratchpad = unsafe {
        Scratchpad::new(
            uninitialized::<SimpleBuffer>(),
            uninitialized::<SimpleTracking>(),
        )
    };
    validate_basic_operations(
        &scratchpad,
        |scratchpad| scratchpad.mark_front(),
        |stack| (stack.iter().map(|x| *x).collect(), ArrayVec::new()),
    );
}

/// General test for validating that the internal state of a `Scratchpad` and
/// `Marker` match the expected state across various "back" operations.
#[test]
fn validate_back_operations() {
    #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    let scratchpad = unsafe {
        Scratchpad::new(
            MaybeUninit::<SimpleBuffer>::uninit().assume_init(),
            MaybeUninit::<SimpleTracking>::uninit().assume_init(),
        )
    };
    #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    let scratchpad = unsafe {
        Scratchpad::new(
            uninitialized::<SimpleBuffer>(),
            uninitialized::<SimpleTracking>(),
        )
    };
    validate_basic_operations(
        &scratchpad,
        |scratchpad| scratchpad.mark_back(),
        |stack| {
            let flipped_stack = stack
                .iter()
                .map(|x| {
                    if *x == core::usize::MAX {
                        *x
                    } else {
                        BUFFER_SIZE - *x
                    }
                })
                .collect();
            (ArrayVec::new(), flipped_stack)
        },
    );
}

/// General test for validating that the internal state of a `Scratchpad` and
/// `Marker` match the expected state across various "front" operations when
/// using a `Scratchpad` created with `static_new_in_place()`.
#[test]
fn validate_front_operations_in_place() {
    unsafe {
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let mut scratchpad = MaybeUninit::uninit();
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        SimpleScratchpad::static_new_in_place(scratchpad.as_mut_ptr());
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let scratchpad_ref = &*scratchpad.as_ptr();

        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let mut scratchpad = uninitialized();
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        SimpleScratchpad::static_new_in_place(&mut scratchpad);
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let scratchpad_ref = &scratchpad;

        validate_basic_operations(
            scratchpad_ref,
            |scratchpad| scratchpad.mark_front(),
            |stack| (stack.iter().map(|x| *x).collect(), ArrayVec::new()),
        );
    }
}

/// General test for validating that the internal state of a `Scratchpad` and
/// `Marker` match the expected state across various "back" operations when
/// using a `Scratchpad` created with `static_new_in_place()`.
#[test]
fn validate_back_operations_in_place() {
    unsafe {
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let mut scratchpad = MaybeUninit::uninit();
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        SimpleScratchpad::static_new_in_place(scratchpad.as_mut_ptr());
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let scratchpad_ref = &*scratchpad.as_ptr();

        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let mut scratchpad = uninitialized();
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        SimpleScratchpad::static_new_in_place(&mut scratchpad);
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let scratchpad_ref = &scratchpad;

        validate_basic_operations(
            scratchpad_ref,
            |scratchpad| scratchpad.mark_back(),
            |stack| {
                let flipped_stack = stack
                    .iter()
                    .map(|x| {
                        if *x == core::usize::MAX {
                            *x
                        } else {
                            BUFFER_SIZE - *x
                        }
                    })
                    .collect();
                (ArrayVec::new(), flipped_stack)
            },
        );
    }
}

/// Shared implementation for `validate_front_memory_limits()` and
/// `validate_back_memory_limits()`
fn validate_memory_limits<'scratchpad, MF, OF, M, O>(
    scratchpad: &'scratchpad SimpleScratchpad,
    mark: MF,
    mark_opposite: OF,
) where
    MF: Fn(&'scratchpad SimpleScratchpad) -> Result<M, Error<()>>,
    OF: Fn(&'scratchpad SimpleScratchpad) -> Result<O, Error<()>>,
    M: Marker,
    O: Marker,
{
    let test_allocation = |bytes_available| {
        let marker = mark(scratchpad).unwrap();
        let result = marker.allocate_array(bytes_available + 1, 0u8);
        assert_eq!(result.unwrap_err().kind(), ErrorKind::InsufficientMemory);
        let result = marker.allocate_array(bytes_available, 0u8);
        assert!(result.is_ok());
    };

    let opposite_marker = mark_opposite(scratchpad).unwrap();
    for i in 0..BUFFER_SIZE {
        test_allocation(BUFFER_SIZE - i);
        opposite_marker.allocate(0u8).unwrap();
    }

    test_allocation(0);
}

/// Verifies that the available space for allocations at the front of a
/// scratchpad is correctly adjusted as allocations at the back increase.
#[test]
fn validate_front_memory_limits() {
    #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    let scratchpad = unsafe {
        Scratchpad::new(
            MaybeUninit::uninit().assume_init(),
            MaybeUninit::uninit().assume_init(),
        )
    };
    #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    let scratchpad =
        unsafe { Scratchpad::new(uninitialized(), uninitialized()) };
    validate_memory_limits(
        &scratchpad,
        |scratchpad| scratchpad.mark_front(),
        |scratchpad| scratchpad.mark_back(),
    );
}

/// Verifies that the available space for allocations at the back of a
/// scratchpad is correctly adjusted as allocations at the front increase.
#[test]
fn validate_back_memory_limits() {
    #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    let scratchpad = unsafe {
        Scratchpad::new(
            MaybeUninit::uninit().assume_init(),
            MaybeUninit::uninit().assume_init(),
        )
    };
    #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    let scratchpad =
        unsafe { Scratchpad::new(uninitialized(), uninitialized()) };
    validate_memory_limits(
        &scratchpad,
        |scratchpad| scratchpad.mark_back(),
        |scratchpad| scratchpad.mark_front(),
    );
}

/// Verifies that the available space for allocations at the front of a
/// scratchpad is correctly adjusted as allocations at the back increase when
/// using a `Scratchpad` created with `static_new_in_place()`.
#[test]
fn validate_front_memory_limits_in_place() {
    unsafe {
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let mut scratchpad = MaybeUninit::uninit();
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        SimpleScratchpad::static_new_in_place(scratchpad.as_mut_ptr());
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let scratchpad_ref = &*scratchpad.as_ptr();

        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let mut scratchpad = uninitialized();
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        SimpleScratchpad::static_new_in_place(&mut scratchpad);
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let scratchpad_ref = &scratchpad;

        validate_memory_limits(
            scratchpad_ref,
            |scratchpad| scratchpad.mark_front(),
            |scratchpad| scratchpad.mark_back(),
        );
    }
}

/// Verifies that the available space for allocations at the back of a
/// scratchpad is correctly adjusted as allocations at the front increase when
/// using a `Scratchpad` created with `static_new_in_place()`.
#[test]
fn validate_back_memory_limits_in_place() {
    unsafe {
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let mut scratchpad = MaybeUninit::uninit();
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        SimpleScratchpad::static_new_in_place(scratchpad.as_mut_ptr());
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let scratchpad_ref = &*scratchpad.as_ptr();

        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let mut scratchpad = uninitialized();
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        SimpleScratchpad::static_new_in_place(&mut scratchpad);
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let scratchpad_ref = &scratchpad;

        validate_memory_limits(
            scratchpad_ref,
            |scratchpad| scratchpad.mark_back(),
            |scratchpad| scratchpad.mark_front(),
        );
    }
}

/// Shared implementation for `validate_front_marker_limits()` and
/// `validate_back_marker_limits()`
fn validate_marker_limits<'scratchpad, MF, OF, M, O>(
    scratchpad: &'scratchpad SimpleScratchpad,
    mark: MF,
    mark_opposite: OF,
) where
    MF: Fn(&'scratchpad SimpleScratchpad) -> Result<M, Error<()>>,
    OF: Fn(&'scratchpad SimpleScratchpad) -> Result<O, Error<()>>,
    M: Marker,
    O: Marker,
{
    let test_marker_allocation = |markers_available| {
        let mut markers = ArrayVec::<[M; MAX_MARKERS]>::new();
        for _ in 0..markers_available {
            markers.push(mark(scratchpad).unwrap());
        }

        let result = mark(scratchpad);
        assert_eq!(unwrap_mark_err(result).kind(), ErrorKind::MarkerLimit);
    };

    let mut opposite_markers = ArrayVec::<[O; MAX_MARKERS]>::new();
    for i in 0..MAX_MARKERS {
        test_marker_allocation(MAX_MARKERS - i);
        opposite_markers.push(mark_opposite(scratchpad).unwrap());
    }

    test_marker_allocation(0);
}

/// Verifies that the available space for tracking markers at the front of a
/// scratchpad is correctly adjusted as markers set at the back increase.
#[test]
fn validate_front_marker_limits() {
    #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    let scratchpad = unsafe {
        Scratchpad::new(
            MaybeUninit::uninit().assume_init(),
            MaybeUninit::uninit().assume_init(),
        )
    };
    #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    let scratchpad =
        unsafe { Scratchpad::new(uninitialized(), uninitialized()) };
    validate_marker_limits(
        &scratchpad,
        |scratchpad| scratchpad.mark_front(),
        |scratchpad| scratchpad.mark_back(),
    );
}

/// Verifies that the available space for tracking markers at the back of a
/// scratchpad is correctly adjusted as markers set at the front increase.
#[test]
fn validate_back_marker_limits() {
    #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
    let scratchpad = unsafe {
        Scratchpad::new(
            MaybeUninit::uninit().assume_init(),
            MaybeUninit::uninit().assume_init(),
        )
    };
    #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
    let scratchpad =
        unsafe { Scratchpad::new(uninitialized(), uninitialized()) };
    validate_marker_limits(
        &scratchpad,
        |scratchpad| scratchpad.mark_back(),
        |scratchpad| scratchpad.mark_front(),
    );
}

/// Verifies that the available space for tracking markers at the front of a
/// scratchpad is correctly adjusted as markers set at the back increase when
/// using a `Scratchpad` created with `static_new_in_place()`.
#[test]
fn validate_front_marker_limits_in_place() {
    unsafe {
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let mut scratchpad = MaybeUninit::uninit();
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        SimpleScratchpad::static_new_in_place(scratchpad.as_mut_ptr());
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let scratchpad_ref = &*scratchpad.as_ptr();

        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let mut scratchpad = uninitialized();
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        SimpleScratchpad::static_new_in_place(&mut scratchpad);
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let scratchpad_ref = &scratchpad;

        validate_marker_limits(
            scratchpad_ref,
            |scratchpad| scratchpad.mark_front(),
            |scratchpad| scratchpad.mark_back(),
        );
    }
}

/// Verifies that the available space for tracking markers at the back of a
/// scratchpad is correctly adjusted as markers set at the front increase when
/// using a `Scratchpad` created with `static_new_in_place()`.
#[test]
fn validate_back_marker_limits_in_place() {
    unsafe {
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let mut scratchpad = MaybeUninit::uninit();
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        SimpleScratchpad::static_new_in_place(scratchpad.as_mut_ptr());
        #[cfg(any(stable_maybe_uninit, feature = "unstable"))]
        let scratchpad_ref = &*scratchpad.as_ptr();

        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let mut scratchpad = uninitialized();
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        SimpleScratchpad::static_new_in_place(&mut scratchpad);
        #[cfg(not(any(stable_maybe_uninit, feature = "unstable")))]
        let scratchpad_ref = &scratchpad;

        validate_marker_limits(
            scratchpad_ref,
            |scratchpad| scratchpad.mark_back(),
            |scratchpad| scratchpad.mark_front(),
        );
    }
}

/// Verifies `Allocation::unwrap()` doesn't accidentally drop or leak data.
#[test]
fn allocation_unwrap_test() {
    let drop_count = Cell::new(0);

    {
        let _data = {
            let scratchpad =
                Scratchpad::<[usize; 1], [usize; 1]>::static_new();
            let marker = scratchpad.mark_front().unwrap();

            // Create an instance of `DropCounter` within the scratchpad. The
            // `drop_count` should remain at zero.
            let allocation =
                marker.allocate(DropCounter::new(&drop_count)).unwrap();
            assert_eq!(drop_count.get(), 0);

            // Move the `DropCounter` instance out of the allocation, and
            // ensure the scratchpad is completely destroyed as it goes out of
            // scope.
            allocation.unwrap()
        };

        // `drop()` should not be called yet, so `drop_count` should still be
        // zero.
        assert_eq!(drop_count.get(), 0);
    }

    // The `DropCounter` instance is now out of scope, so it`s `drop()` method
    // should have incremented `drop_count`.
    assert_eq!(drop_count.get(), 1);
}

/// Verifies `Marker::allocate()` doesn't accidentally drop or leak data.
#[test]
fn marker_allocate_test() {
    let drop_count = Cell::new(0);

    {
        let scratchpad = Scratchpad::<[usize; 1], [usize; 1]>::static_new();
        let marker = scratchpad.mark_front().unwrap();

        let _allocation =
            marker.allocate(DropCounter::new(&drop_count)).unwrap();
        assert_eq!(drop_count.get(), 0);
    }

    assert_eq!(drop_count.get(), 1);
}

/// Verifies `Marker::allocate_array()` doesn't accidentally drop or leak
/// data.
#[test]
fn marker_allocate_array_test() {
    let drop_count = Cell::new(0);

    {
        let scratchpad = Scratchpad::<[usize; 10], [usize; 1]>::static_new();
        let marker = scratchpad.mark_front().unwrap();

        let _allocation = marker
            .allocate_array(10, DropCounter::new(&drop_count))
            .unwrap();

        // When writing this test, `Marker::allocate_array()` only ever stores
        // clones of the initial value and drops the original parameter given,
        // but it may be possible to optimize it at some point to use the
        // original value as well and save a clone operation, so we'll allow
        // for either zero or one drop during array creation.
        assert!(drop_count.get() <= 1);
        drop_count.set(0);
    }

    assert_eq!(drop_count.get(), 10);
}

/// Verifies `Marker::allocate_array_with()` doesn't accidentally drop or leak
/// data.
#[test]
fn marker_allocate_array_with_test() {
    let drop_count = Cell::new(0);

    {
        let scratchpad = Scratchpad::<[usize; 10], [usize; 1]>::static_new();
        let marker = scratchpad.mark_front().unwrap();

        let _allocation = marker
            .allocate_array_with(10, |_| DropCounter::new(&drop_count))
            .unwrap();
        assert_eq!(drop_count.get(), 0);
    }

    assert_eq!(drop_count.get(), 10);
}

/// Verifies `Marker::extend()` doesn't accidentally drop or leak data.
#[test]
fn marker_extend_test() {
    let drop_count = Cell::new(0);

    {
        let scratchpad = Scratchpad::<[usize; 8], [usize; 1]>::static_new();
        let marker = scratchpad.mark_back().unwrap();

        let allocation =
            marker.allocate(DropCounter::new(&drop_count)).unwrap();
        let allocation = marker
            .extend(allocation, [DropCounter::new(&drop_count)])
            .unwrap();

        #[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
        let allocation = {
            let mut v = Vec::with_capacity(2);
            v.push(DropCounter::new(&drop_count));
            v.push(DropCounter::new(&drop_count));

            let bs = v.clone().into_boxed_slice();

            let ba = Box::new([
                DropCounter::new(&drop_count),
                DropCounter::new(&drop_count),
            ]);

            let allocation = marker.extend(allocation, v).unwrap();
            let allocation = marker.extend(allocation, bs).unwrap();
            let allocation =
                marker.extend(allocation, ba as Box<[DropCounter]>).unwrap();

            allocation
        };

        assert_eq!(drop_count.get(), 0);

        let _ = allocation; // Silence unused variable warnings...
    }

    if cfg!(any(
        feature = "std",
        feature = "alloc",
        feature = "unstable"
    )) {
        assert_eq!(drop_count.get(), 8);
    } else {
        assert_eq!(drop_count.get(), 2);
    }
}

/// Verifies `Marker::extend_clone()` doesn't accidentally drop or leak data.
#[test]
fn marker_extend_clone_test() {
    let drop_count = Cell::new(0);

    {
        let scratchpad = Scratchpad::<[usize; 2], [usize; 1]>::static_new();
        let marker = scratchpad.mark_back().unwrap();

        let allocation =
            marker.allocate(DropCounter::new(&drop_count)).unwrap();
        let _allocation = marker
            .extend_clone(allocation, &[DropCounter::new(&drop_count)][..])
            .unwrap();
        assert_eq!(drop_count.get(), 1);
    }

    assert_eq!(drop_count.get(), 3);
}

/// Verifies `Allocation::concat()` works with strings.
#[test]
fn allocation_concat_string_test() {
    let scratchpad = Scratchpad::<[u8; 32], [usize; 1]>::static_new();
    let marker = scratchpad.mark_front().unwrap();

    let foo = marker.allocate_slice_copy("foo").unwrap();
    let bar = marker.allocate_slice_copy("bar").unwrap();
    let foobar: Allocation<str> = foo.concat(bar).unwrap();
    assert_eq!(&*foobar, "foobar");
}

/// Verifies `Marker::extend()` works with strings.
#[test]
fn marker_extend_string_test() {
    let scratchpad = Scratchpad::<[u8; 32], [usize; 1]>::static_new();
    let marker = scratchpad.mark_front().unwrap();

    let foo = marker.allocate_slice_copy("foo").unwrap();
    let foobar = marker.extend(foo, "bar").unwrap();
    assert_eq!(&*foobar, "foobar");
}

/// Tests tuple implementations of `SliceMoveSourceCollection` use for
/// possible leaks or unintentional dropping of elements.
#[test]
fn slice_move_source_collection_tuple_test() {
    let scratchpad = Scratchpad::<[usize; 6], [usize; 1]>::static_new();
    let drop_count = Cell::new(0);

    {
        let marker = scratchpad.mark_back().unwrap();

        let a = [DropCounter::new(&drop_count)];

        // `Box` and `Vec` may not be testable depending on the feature set,
        // so use different types if they're not available.
        #[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
        let (b, c) = {
            let mut c = Vec::new();
            c.push(DropCounter::new(&drop_count));
            c.push(DropCounter::new(&drop_count));
            c.push(DropCounter::new(&drop_count));

            (
                Box::new([
                    DropCounter::new(&drop_count),
                    DropCounter::new(&drop_count),
                ]) as Box<[_]>,
                c,
            )
        };

        #[cfg(not(any(
            feature = "std",
            feature = "alloc",
            feature = "unstable"
        )))]
        let (b, c) = (
            [DropCounter::new(&drop_count), DropCounter::new(&drop_count)],
            [
                DropCounter::new(&drop_count),
                DropCounter::new(&drop_count),
                DropCounter::new(&drop_count),
            ],
        );

        let _allocation = marker.concat_slices((a, b, c)).unwrap();
        assert_eq!(drop_count.get(), 0);
    }

    assert_eq!(drop_count.get(), 6);
}

/// Tests array implementations of `SliceMoveSourceCollection` use for
/// possible leaks or unintentional dropping of elements.
#[test]
fn slice_move_source_collection_array_test() {
    let scratchpad = Scratchpad::<[usize; 3], [usize; 1]>::static_new();
    let drop_count = Cell::new(0);

    {
        let marker = scratchpad.mark_back().unwrap();

        let a = [DropCounter::new(&drop_count)];
        let b = [DropCounter::new(&drop_count)];
        let c = [DropCounter::new(&drop_count)];

        let _allocation = marker.concat_slices([a, b, c]).unwrap();
        assert_eq!(drop_count.get(), 0);
    }

    assert_eq!(drop_count.get(), 3);
    drop_count.set(0);

    #[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
    {
        {
            let marker = scratchpad.mark_back().unwrap();

            let a = Box::new([DropCounter::new(&drop_count)]) as Box<[_]>;
            let b = Box::new([DropCounter::new(&drop_count)]) as Box<[_]>;
            let c = Box::new([DropCounter::new(&drop_count)]) as Box<[_]>;

            let _allocation = marker.concat_slices([a, b, c]).unwrap();
            assert_eq!(drop_count.get(), 0);
        }

        assert_eq!(drop_count.get(), 3);
        drop_count.set(0);

        {
            let marker = scratchpad.mark_back().unwrap();

            let mut a = Vec::new();
            a.push(DropCounter::new(&drop_count));
            let mut b = Vec::new();
            b.push(DropCounter::new(&drop_count));
            let mut c = Vec::new();
            c.push(DropCounter::new(&drop_count));

            let _allocation = marker.concat_slices([a, b, c]).unwrap();
            assert_eq!(drop_count.get(), 0);
        }

        assert_eq!(drop_count.get(), 3);
    }
}

/// Tests vector implementations of `SliceMoveSourceCollection` use for
/// possible leaks or unintentional dropping of elements.
#[test]
#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
fn slice_move_source_collection_vec_test() {
    let scratchpad = Scratchpad::<[usize; 3], [usize; 1]>::static_new();
    let drop_count = Cell::new(0);

    {
        let marker = scratchpad.mark_back().unwrap();

        let a = [DropCounter::new(&drop_count)];
        let b = [DropCounter::new(&drop_count)];
        let c = [DropCounter::new(&drop_count)];

        let mut sources = Vec::new();
        sources.push(a);
        sources.push(b);
        sources.push(c);

        let _allocation = marker.concat_slices(sources).unwrap();
        assert_eq!(drop_count.get(), 0);
    }

    assert_eq!(drop_count.get(), 3);
    drop_count.set(0);

    {
        let marker = scratchpad.mark_back().unwrap();

        let a = Box::new([DropCounter::new(&drop_count)]) as Box<[_]>;
        let b = Box::new([DropCounter::new(&drop_count)]) as Box<[_]>;
        let c = Box::new([DropCounter::new(&drop_count)]) as Box<[_]>;

        let mut sources = Vec::new();
        sources.push(a);
        sources.push(b);
        sources.push(c);

        let _allocation = marker.concat_slices(sources).unwrap();
        assert_eq!(drop_count.get(), 0);
    }

    assert_eq!(drop_count.get(), 3);
    drop_count.set(0);

    {
        let marker = scratchpad.mark_back().unwrap();

        let mut a = Vec::new();
        a.push(DropCounter::new(&drop_count));
        let mut b = Vec::new();
        b.push(DropCounter::new(&drop_count));
        let mut c = Vec::new();
        c.push(DropCounter::new(&drop_count));

        let mut sources = Vec::new();
        sources.push(a);
        sources.push(b);
        sources.push(c);

        let _allocation = marker.concat_slices(sources).unwrap();
        assert_eq!(drop_count.get(), 0);
    }

    assert_eq!(drop_count.get(), 3);
}

/// Tests boxed slice implementations of `SliceMoveSourceCollection` use for
/// possible leaks or unintentional dropping of elements.
#[test]
#[cfg(any(feature = "std", feature = "alloc", feature = "unstable"))]
fn slice_move_source_collection_boxed_slice_test() {
    let scratchpad = Scratchpad::<[usize; 3], [usize; 1]>::static_new();
    let drop_count = Cell::new(0);

    {
        let marker = scratchpad.mark_back().unwrap();

        let a = [DropCounter::new(&drop_count)];
        let b = [DropCounter::new(&drop_count)];
        let c = [DropCounter::new(&drop_count)];

        let _allocation = marker
            .concat_slices(Box::new([a, b, c]) as Box<[[_; 1]]>)
            .unwrap();
        assert_eq!(drop_count.get(), 0);
    }

    assert_eq!(drop_count.get(), 3);
    drop_count.set(0);

    {
        let marker = scratchpad.mark_back().unwrap();

        let a = Box::new([DropCounter::new(&drop_count)]) as Box<[_]>;
        let b = Box::new([DropCounter::new(&drop_count)]) as Box<[_]>;
        let c = Box::new([DropCounter::new(&drop_count)]) as Box<[_]>;

        let _allocation = marker
            .concat_slices(Box::new([a, b, c]) as Box<[Box<[_]>]>)
            .unwrap();
        assert_eq!(drop_count.get(), 0);
    }

    assert_eq!(drop_count.get(), 3);
    drop_count.set(0);

    {
        let marker = scratchpad.mark_back().unwrap();

        let mut a = Vec::new();
        a.push(DropCounter::new(&drop_count));
        let mut b = Vec::new();
        b.push(DropCounter::new(&drop_count));
        let mut c = Vec::new();
        c.push(DropCounter::new(&drop_count));

        let _allocation = marker
            .concat_slices(Box::new([a, b, c]) as Box<[Vec<_>]>)
            .unwrap();
        assert_eq!(drop_count.get(), 0);
    }

    assert_eq!(drop_count.get(), 3);
}

/// Verifies ZST allocations work properly.
#[test]
fn zst_test() {
    let scratchpad = Scratchpad::<[u8; 1], [usize; 2]>::static_new();
    let front_marker = scratchpad.mark_front().unwrap();

    // Fill the scratchpad buffer so there's no space left.
    let _byte = front_marker.allocate(0u8).unwrap();
    assert_eq!(*scratchpad.markers.borrow(), ([1usize], []));

    // Allocate a unit (zero-sized) from the front.
    let _unit = front_marker.allocate(()).unwrap();
    assert_eq!(*scratchpad.markers.borrow(), ([1usize], []));

    // Allocate a unit struct slice from the back.
    #[derive(Clone, Copy, Debug)]
    struct UnitStruct;

    let back_marker = scratchpad.mark_back().unwrap();
    assert_eq!(*scratchpad.markers.borrow(), ([1usize], [1usize]));
    let _unit = back_marker.allocate_array(12, UnitStruct).unwrap();
    assert_eq!(*scratchpad.markers.borrow(), ([1usize], [1usize]));
}
