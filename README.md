scratchpad
==========

A Rust library providing a stack-like memory allocator with double-ended
allocation support.

[![Latest Version](https://img.shields.io/crates/v/scratchpad.svg)](https://crates.io/crates/scratchpad)
[![Released API docs](https://docs.rs/scratchpad/badge.svg)](https://docs.rs/scratchpad)
![MIT/Apache-2.0 licensed](https://img.shields.io/crates/l/scratchpad.svg)
[![Rustc Version 1.25+](https://img.shields.io/badge/rustc-1.25+-lightgray.svg)](https://blog.rust-lang.org/2018/03/29/Rust-1.25.html)
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
scratchpad = "1.3"
```

For Rust 2015 code, also add this to your crate root:

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
scratchpad = { version = "1.3", default-features = false }
```

`Box` and `Vec` support is still available for `no_std` builds by enabling the
`alloc` feature, which uses the `alloc` crate directly:

```toml
[dependencies]
scratchpad = { version = "1.3", default-features = false, features = ["alloc"] }
```

The `alloc` feature requires Rust 1.36.0 or later. Older versions of the
nightly toolchain can still use `Box` and `Vec` in `no_std` code via the
`unstable` feature.

## Unstable Features

The `unstable` crate feature provides some additional functionality when using
a nightly toolchain:

- Declaration of the function `Scratchpad::new()` as `const`.
- Support for various features that were still unstable with legacy Rust
  releases:
  - `Box` and `Vec` support for `no_std` code (enabled without the `unstable`
    feature when using Rust 1.36.0 or later with the `alloc` feature enabled).
  - `ByteData` trait implementations for `u128` and `i128` (enabled without
    the `unstable` feature when using Rust 1.26.0 or later).
  - `ByteData` trait implementation for all `std::mem::MaybeUninit` types
    wrapping other `ByteData` types (enabled without the `unstable` feature
    when using Rust 1.36.0 or later).

Simply add the `unstable` feature to your `Cargo.toml` dependency:

```toml
[dependencies]
scratchpad = { version = "1.3", features = ["unstable"] }
```

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
