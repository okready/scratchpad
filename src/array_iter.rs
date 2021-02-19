// Copyright 2018-2021 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Consuming iterator for static arrays.

use super::Array;
use core::{mem::ManuallyDrop, ptr, slice};

/// Consuming iterator for static arrays.
pub struct ArrayIter<T>
where
    T: Array,
{
    /// Source array being consumed.
    source: ManuallyDrop<T>,
    /// Index of the next element to yield.
    index: usize,
}

impl<T> ArrayIter<T>
where
    T: Array,
{
    /// Creates a new iterator consuming all elements in the given array.
    pub fn new(source: T) -> Self {
        Self {
            source: ManuallyDrop::new(source),
            index: 0,
        }
    }
}

impl<T> Iterator for ArrayIter<T>
where
    T: Array,
{
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let source = self.source.as_slice();
        let index = self.index;
        if index < source.len() {
            self.index = index + 1;

            Some(unsafe { ptr::read(&source[index]) })
        } else {
            None
        }
    }
}

impl<T> Drop for ArrayIter<T>
where
    T: Array,
{
    fn drop(&mut self) {
        // Drop all unconsumed elements.
        let source = self.source.as_mut_slice();
        let remaining_len = source.len() - self.index;
        unsafe {
            // The Rust compiler and standard library try to prevent cases
            // where the length of an allocation is larger than `isize::MAX`
            // (see the `pointer::offset` safety documentation), so casting
            // the source array length to an `isize` should be okay.
            let start = source.as_mut_ptr().offset(self.index as isize);
            let remaining = slice::from_raw_parts_mut(start, remaining_len);
            ptr::drop_in_place(remaining);
        }
    }
}
