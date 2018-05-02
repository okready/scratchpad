// Copyright 2018 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Error support.

use core::fmt;

/// [`Scratchpad`] and [`Marker`] allocation errors.
///
/// [`Marker`]: trait.Marker.html
/// [`Scratchpad`]: struct.Scratchpad.html
#[derive(Debug, PartialEq)]
pub enum AllocateError {
    /// Maximum number of scratchpad markers are currently set.
    MarkerLimit,
    /// Allocation cannot be made because the marker is not the most-recently
    /// created active marker.
    MarkerLocked,
    /// Insufficient space in the scratchpad buffer for the allocation.
    InsufficientMemory,
    /// Integer overflow detected (typically due to a very large size or
    /// alignment).
    Overflow,
}

impl fmt::Display for AllocateError {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &AllocateError::MarkerLimit => {
                write!(f, "scratchpad marker limit reached")
            }
            &AllocateError::MarkerLocked => {
                write!(f, "marker is not the most recent active marker")
            }
            &AllocateError::InsufficientMemory => {
                write!(f, "insufficient allocation buffer space")
            }
            &AllocateError::Overflow => write!(f, "integer overflow"),
        }
    }
}

#[cfg(feature = "std")]
impl ::std::error::Error for AllocateError {
    fn description(&self) -> &str {
        match self {
            &AllocateError::MarkerLimit => "scratchpad marker limit reached",
            &AllocateError::MarkerLocked => {
                "marker is not the most recent active marker"
            }
            &AllocateError::InsufficientMemory => {
                "insufficient allocation buffer space"
            }
            &AllocateError::Overflow => "integer overflow",
        }
    }
}

/// [`Allocation`] concatenation errors.
///
/// [`Allocation`]: struct.Allocation.html
#[derive(Debug, PartialEq)]
pub enum ConcatError {
    /// Allocations are not in order.
    OutOfOrder,
    /// Allocations are not adjacent in memory.
    NotAdjacent,
}

impl fmt::Display for ConcatError {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ConcatError::OutOfOrder => {
                write!(f, "allocations specified out-of-order")
            }
            &ConcatError::NotAdjacent => {
                write!(f, "allocations are not adjacent in memory")
            }
        }
    }
}

#[cfg(feature = "std")]
impl ::std::error::Error for ConcatError {
    #[inline]
    fn description(&self) -> &str {
        match self {
            &ConcatError::OutOfOrder => "allocations specified out-of-order",
            &ConcatError::NotAdjacent => {
                "allocations are not adjacent in memory"
            }
        }
    }
}
