// Copyright 2018 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::*;
use arrayvec::ArrayVec;
use core::mem::uninitialized;

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
type SimpleBuffer = array_type_for_bytes!(u64, BUFFER_SIZE);
/// Scratchpad marker tracking buffer type for `validate_basic_operations()`
/// (4 markers).
type SimpleTracking = array_type_for_markers!(usize, MAX_MARKERS);
/// `Scratchpad` type for `validate_basic_operations()`.
type SimpleScratchpad = Scratchpad<SimpleBuffer, SimpleTracking>;

/// Shared implementation for `validate_front_operations()` and
/// `validate_back_operations()`.
fn validate_basic_operations<'scratchpad, MF, CF, M>(
    scratchpad: &'scratchpad SimpleScratchpad,
    mark: MF,
    conv: CF,
) where
    MF: Fn(&'scratchpad SimpleScratchpad) -> Result<M, Error>,
    CF: for<'a> Fn(
        &'a [usize]
    ) -> (
        ArrayVec<SimpleTracking>,
        ArrayVec<SimpleTracking>,
    ),
    M: MarkerInternal + fmt::Debug,
{
    // Rust should guarantee a default alignment of at least 8 bytes (for
    // allocations of 8 bytes or more), so verify our buffer is aligned as
    // such.
    assert_eq!(*scratchpad.markers.borrow(), conv(&[][..]));
    unsafe {
        assert_eq!(
            (*scratchpad.buffer.get()).as_ptr() as usize & 0x7,
            0,
        );
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
    assert_eq!(
        a.allocate(3u8).unwrap_err(),
        Error::MarkerLocked,
    );

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
        b.allocate_array(20, 24u8).unwrap_err(),
        Error::InsufficientMemory,
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
        assert_eq!(
            *c0,
            [17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17],
        );
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
        mark(scratchpad).unwrap_err(),
        Error::MarkerLimit,
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
        mark(scratchpad).unwrap_err(),
        Error::MarkerLimit,
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
    let scratchpad = unsafe {
        Scratchpad::new(
            uninitialized::<SimpleBuffer>(),
            uninitialized::<SimpleTracking>(),
        )
    };
    validate_basic_operations(
        &scratchpad,
        |scratchpad| scratchpad.mark_front(),
        |stack| {
            (
                stack.iter().map(|x| *x).collect(),
                ArrayVec::new(),
            )
        },
    );
}

/// General test for validating that the internal state of a `Scratchpad` and
/// `Marker` match the expected state across various "back" operations.
#[test]
fn validate_back_operations() {
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

/// Shared implementation for `validate_front_memory_limits()` and
/// `validate_back_memory_limits()`
fn validate_memory_limits<'scratchpad, MF, OF, M, O>(
    scratchpad: &'scratchpad SimpleScratchpad,
    mark: MF,
    mark_opposite: OF,
) where
    MF: Fn(&'scratchpad SimpleScratchpad) -> Result<M, Error>,
    OF: Fn(&'scratchpad SimpleScratchpad) -> Result<O, Error>,
    M: Marker + fmt::Debug,
    O: Marker + fmt::Debug,
{
    let test_allocation = |bytes_available| {
        let marker = mark(scratchpad).unwrap();
        let result = marker.allocate_array(bytes_available + 1, 0u8);
        assert_eq!(result.unwrap_err(), Error::InsufficientMemory);
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
    let scratchpad =
        unsafe { Scratchpad::new(uninitialized(), uninitialized()) };
    validate_memory_limits(
        &scratchpad,
        |scratchpad| scratchpad.mark_back(),
        |scratchpad| scratchpad.mark_front(),
    );
}

/// Shared implementation for `validate_front_marker_limits()` and
/// `validate_back_marker_limits()`
fn validate_marker_limits<'scratchpad, MF, OF, M, O>(
    scratchpad: &'scratchpad SimpleScratchpad,
    mark: MF,
    mark_opposite: OF,
) where
    MF: Fn(&'scratchpad SimpleScratchpad) -> Result<M, Error>,
    OF: Fn(&'scratchpad SimpleScratchpad) -> Result<O, Error>,
    M: Marker + fmt::Debug,
    O: Marker + fmt::Debug,
{
    let test_marker_allocation = |markers_available| {
        let mut markers = ArrayVec::<[M; MAX_MARKERS]>::new();
        for _ in 0..markers_available {
            markers.push(mark(scratchpad).unwrap());
        }

        let result = mark(scratchpad);
        assert_eq!(result.unwrap_err(), Error::MarkerLimit);
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
    let scratchpad =
        unsafe { Scratchpad::new(uninitialized(), uninitialized()) };
    validate_marker_limits(
        &scratchpad,
        |scratchpad| scratchpad.mark_back(),
        |scratchpad| scratchpad.mark_front(),
    );
}

/// Verifies `Allocation::unwrap()` works properly.
#[test]
fn allocation_drop_test() {
    struct DropTest<'a> {
        is_active: &'a mut bool,
    }

    impl<'a> DropTest<'a> {
        fn check(&self) {
            assert!(*self.is_active);
        }
    }

    impl<'a> Drop for DropTest<'a> {
        fn drop(&mut self) {
            assert!(*self.is_active);
            *self.is_active = false;
        }
    }

    let mut is_active = true;

    {
        let data = {
            let scratchpad = Scratchpad::new([0usize; 1], [0usize; 1]);
            let marker = scratchpad.mark_front().unwrap();

            // Create an instance of `DropTest` within the scratchpad. The
            // `is_active` flag should remain `true`.
            let allocation = marker
                .allocate(DropTest {
                    is_active: &mut is_active,
                })
                .unwrap();
            allocation.check();

            // Move the `DropTest` instance out of the allocation, and ensure
            // the scratchpad is completely destroyed as it goes out of scope.
            allocation.unwrap()
        };

        // `drop()` should not be called yet, so the `is_active` flag should
        // still remain `true`.
        data.check();
    }

    // The `DropTest` instance is now out of scope, so it`s `drop()` method
    // should have set the `is_active` flag to `false`.
    assert!(!is_active);
}
