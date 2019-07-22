// Copyright 2018-2019 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Error support.

use core::fmt;
use core::ptr;

use core::mem::forget;

/// Categories of errors during scratchpad operations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ErrorKind {
    /// Maximum number of scratchpad markers are currently set.
    MarkerLimit,
    /// Allocation cannot be made because the marker is not the most-recently
    /// created active marker.
    MarkerLocked,
    /// Insufficient space in the scratchpad buffer for the allocation.
    InsufficientMemory,
    /// Allocation cannot be extended since it is not the most recent
    /// allocation in its marker.
    NotAtEnd,
    /// Allocations being merged are not in order.
    OutOfOrder,
    /// Allocations being merged are not adjacent in memory.
    NotAdjacent,
    /// Integer overflow detected (typically due to a very large size or
    /// alignment).
    Overflow,
}

/// The error type for scratchpad operations.
///
/// Various scratchpad operations require ownership of some or all of their
/// parameters to be transferred to the callee, such as for storage in a new
/// allocation. If such operations fail, the caller may still want to do
/// something with the data originally provided.
///
/// This type encapsulates both the kind of error that occurred and any
/// recoverable "owned" parameters that were passed so that the caller can
/// have them back.
#[derive(Debug)]
pub struct Error<T> {
    /// "Owned" arguments passed to the function.
    args: T,
    /// Type of error that occurred.
    kind: ErrorKind,
}

impl<T> Error<T> {
    /// Creates a new error with the specified category and argument values.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Error, ErrorKind};
    ///
    /// // Error containing multiple recycled arguments.
    /// let multi_error = Error::new(
    ///     ErrorKind::MarkerLocked,
    ///     (3.14159f32, 2.71828f32),
    /// );
    ///
    /// // Error containing a single recycled argument. A tuple is still used
    /// // to remain consistent with errors that recycle multiple argument
    /// // values.
    /// let single_error = Error::new(ErrorKind::MarkerLocked, (3.14159f32,));
    ///
    /// // Error containing no recycled argument values. A unit is used by the
    /// // crate to signify an empty set of arguments.
    /// let simple_error = Error::new(ErrorKind::MarkerLocked, ());
    /// ```
    #[inline]
    pub fn new(kind: ErrorKind, args: T) -> Self {
        Error { args, kind }
    }

    /// Returns the category of error that occurred.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Error, ErrorKind};
    ///
    /// let error = Error::new(
    ///     ErrorKind::MarkerLocked,
    ///     (3.14159f32, 2.71828f32),
    /// );
    ///
    /// let kind = error.kind();
    /// assert_eq!(kind, ErrorKind::MarkerLocked);
    /// ```
    #[inline]
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    /// Returns a tuple containing values that would have passed ownership to
    /// the callee if the operation was successful.
    ///
    /// While there's no constraint on the arguments type, this crate will
    /// only ever produce errors that have either a [unit] or [tuple] for its
    /// arguments, even if only a single value is returned to the caller.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Error, ErrorKind};
    ///
    /// let error = Error::new(
    ///     ErrorKind::MarkerLocked,
    ///     (3.14159f32, 2.71828f32),
    /// );
    ///
    /// let args = error.args();
    /// assert_eq!(args, &(3.14159f32, 2.71828f32));
    /// ```
    ///
    /// [unit]: https://doc.rust-lang.org/std/primitive.unit.html
    /// [tuple]: https://doc.rust-lang.org/std/primitive.tuple.html
    #[inline]
    pub fn args(&self) -> &T {
        &self.args
    }

    /// Unwraps the error, yielding a tuple containing values that would have
    /// passed ownership to the callee if the operation was successful.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Error, ErrorKind};
    ///
    /// let error = Error::new(
    ///     ErrorKind::MarkerLocked,
    ///     (3.14159f32, 2.71828f32),
    /// );
    ///
    /// let (x, y) = error.unwrap_args();
    /// assert_eq!(x, 3.14159f32);
    /// assert_eq!(y, 2.71828f32);
    /// ```
    #[inline]
    pub fn unwrap_args(self) -> T {
        unsafe {
            let args = ptr::read(&self.args);
            forget(self);
            args
        }
    }

    /// Maps an `Error<T>` to an `Error<U>` by applying a function to the
    /// contained arguments value, leaving the `ErrorKind` value untouched.
    ///
    /// # Examples
    ///
    /// ```
    /// use scratchpad::{Error, ErrorKind};
    ///
    /// let error = Error::new(ErrorKind::MarkerLocked, (-12, 23));
    ///
    /// let new_error = error.map(|args| (args.1 as i64 * -2,));
    /// assert_eq!(new_error.args().0, -46i64);
    /// ```
    #[inline]
    pub fn map<U, F>(self, op: F) -> Error<U>
    where
        F: FnOnce(T) -> U,
    {
        let kind = self.kind;
        let args = op(self.unwrap_args());
        Error { args, kind }
    }
}

impl<T> fmt::Display for Error<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ErrorKind::MarkerLimit => {
                write!(f, "scratchpad marker limit reached")
            }
            ErrorKind::MarkerLocked => {
                write!(f, "marker is not the most recent active marker")
            }
            ErrorKind::InsufficientMemory => {
                write!(f, "insufficient allocation buffer space")
            }
            ErrorKind::NotAtEnd => {
                write!(f, "allocation not most recent from its marker")
            }
            ErrorKind::OutOfOrder => {
                write!(f, "allocations specified out-of-order")
            }
            ErrorKind::NotAdjacent => {
                write!(f, "allocations are not adjacent in memory")
            }
            ErrorKind::Overflow => write!(f, "integer overflow"),
        }
    }
}

#[cfg(feature = "std")]
impl<T> ::std::error::Error for Error<T>
where
    T: fmt::Debug,
{
    #[inline]
    fn description(&self) -> &str {
        match self.kind {
            ErrorKind::MarkerLimit => "scratchpad marker limit reached",
            ErrorKind::MarkerLocked => {
                "marker is not the most recent active marker"
            }
            ErrorKind::InsufficientMemory => {
                "insufficient allocation buffer space"
            }
            ErrorKind::NotAtEnd => {
                "allocation not most recent from its marker"
            }
            ErrorKind::OutOfOrder => "allocations specified out-of-order",
            ErrorKind::NotAdjacent => {
                "allocations are not adjacent in memory"
            }
            ErrorKind::Overflow => "integer overflow",
        }
    }
}
