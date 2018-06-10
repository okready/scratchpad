scratchpad
==========

A Rust library providing a stack-like memory allocator with double-ended
allocation support.

[![Build Status](https://travis-ci.org/okready/scratchpad.svg?branch=master)](https://travis-ci.org/okready/scratchpad)

- [Documentation](https://docs.rs/scratchpad)
- [Release notes](https://github.com/okready/scratchpad/releases)

`Scratchpad` provides a method for quick and safe dynamic allocations of
arbitrary types without relying on the global heap (e.g. using `Box` or
`Vec`). Allocations are made from a fixed-size region of memory in a
stack-like fashion using two separate stacks (one for each end of the
allocation buffer) to allow different types of allocations with independent
lifecycles to be made from each end.

Such allocators are commonly used in game development, but are also useful in
general for short-lived allocations or groups of allocations that share a
common lifetime. While not quite as flexible as heap allocations, allocations
from a stack allocator are usually much faster and are isolated from the rest
of the heap, reducing memory fragmentation.

Features include:

- User-defined backing storage of data (static arrays, boxed slices, or
  mutable slice references).
- Allocation of any data type from any scratchpad instance.
- Ability to combine allocations that are adjacent in memory or add to the
  most recently created allocation.
- Double-ended allocation support (allocations from the "front" are separate
  from the "back", but share the same memory pool).
- Use of lifetimes to prevent dangling references to allocated data.
- Low runtime overhead.
- Support for `no_std` usage.

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
scratchpad = "2.0"
```

and this to your crate root:

```rust
#[macro_use]
extern crate scratchpad;
```

## Rust Version Support

The minimum supported Rust version is 1.25 due to use of `NonNull<T>` and the
`repr(align)` attribute.

## `no_std` Support

`scratchpad` doesn't require the Rust standard library, although it makes use
of it by default (via the `std` crate feature) to provide support for use of
`Box` and `Vec` in various places. For `no_std` support, the `std` feature
must be disabled in your `Cargo.toml`:

```toml
[dependencies]
scratchpad = { version = "2.0", default-features = false }
```

`Box` and `Vec` support is still available for `no_std` builds when using a
nightly toolchain by enabling the `unstable` crate feature.

## Unstable Features

The `unstable` crate feature provides some additional functionality when using
a nightly toolchain:

- Support for `Box` and `Vec` types as mentioned with the `std` feature,
  regardless of whether the `std` feature is enabled (if `std` is disabled,
  this will use the `alloc` library directly).
- Declaration of the function `Scratchpad::new()` as `const`.
- `ByteData` trait implementations for `u128`/`i128` for Rust versions prior
  to 1.26 (`u128`/`i128` support is enabled by default with both stable and
  unstable toolchains if the detected Rust version is 1.26 or greater).

Simply add the `unstable` feature to your `Cargo.toml` dependency:

```toml
[dependencies]
scratchpad = { version = "2.0", features = ["unstable"] }
```
