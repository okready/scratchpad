# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- `CStr` allocation support, including conversion of `CStr` allocations to
  `[u8]` allocations.

## [1.1.0] - 2018-06-16
### Added
- `Scratchpad::static_new_in_place()` for initializing a `Scratchpad` within
  a block of uninitialized memory for `Scratchpad` types that use only static
  arrays for backing memory, bypassing potential call stack use for parameters
  and return values that may exceed the amount of space available on the
  stack.

## [1.0.1] - 2018-06-07
### Fixed
- Use unaligned reads and writes in `Tracking::get()` and `Tracking::set()`
  implementations for `Buffer` types to avoid potential issues with marker
  tracking buffers that are not aligned to at least the same alignment as
  `usize`, as `Buffer` doesn't guarantee any specific alignment, and some CPU
  architectures do not allow unaligned access. This can impact performance,
  but retains compatibility with code that may be using tracking buffers with
  lower alignment requirements.

## [1.0.0] - 2018-06-04
### Added
- `IntoMutSliceLikePtr` trait for reinterpreting pointer types to compatible
  `SliceLike` pointers. This replaces the `IntoSliceLikeAllocation` trait for
  determining which `Allocation` types can be converted to slices and
  slice-like types.
- Missing support for converting `str` allocations to `[u8]` allocations.

### Changed
- Moved `into_slice_like_allocation()` away from the `IntoSliceLikeAllocation`
  trait into a method of `Allocation` itself, allowing it to be called
  directly on any allocation without having to import any special traits.
- Rewrote much of the crate-level documentation to be more useful as well as
  better reflect the changes made since the initial release.

### Removed
- `IntoSliceLikeAllocation` trait (superseded by
  `Allocation::into_slice_like_allocation()` and `IntoMutSliceLikePtr` trait).

## [1.0.0-beta.2] - 2018-05-31
### Added
- `SliceSource` trait for providing slice data to `Marker` functions from
  various source data types.
- `SliceMoveSource` subtrait of `SliceSource` that allows for taking ownership
  of the contents of a slice.
- `SliceSourceCollection` trait for abstracting collections of `SliceSource`
  objects.
- `SliceMoveSourceCollection` subtrait of `SliceSourceCollection` that allows
  for taking ownership of the slices provided by all `SliceSource` objects in
  a collection.
- `Marker::concat_slices()` function for concatenating slices by moving their
  contents into the new allocation.

### Changed
- Overhauled `Marker` functions that work with slices to take slice input
  using the new `SliceSource`, `SliceMoveSource`, `SliceSourceCollection`, and
  `SliceMoveSourceCollection` traits, allowing for more flexible input for
  slice data. Additionally, the inputs for these functions are now more
  consistent with one another.
- Renamed `IntoSliceAllocation` to `IntoSliceLikeAllocation` and updated its
  methods accordingly for consistency with other traits that work with
  `SliceLike` types.
- Unconditionally enable `i128` and `u128` implementations of `ByteData` if
  the `rustc` version detected at build time is 1.26 or later.

### Removed
- `OwnedSlice` trait (superseded by `SliceSource` and its related traits).
- Automatic coercion of scalar values (including boxed scalars) and boxed
  arrays into slices when used as arguments for `Marker::allocate_slice()`,
  `Marker::extend()`, and the related `MarkerFront` and `MarkerBack` wrappers.
  Support for scalars previously resulted in ambiguity when trying to
  determine whether an array or slice reference should be interpreted as a
  slice of its contents or a single-element slice containing either the array
  or slice reference, requiring explicit type annotations in such cases. On
  the other hand, support for boxed arrays simply becomes difficult to
  implement when using the newly added `Array` trait, so they've simply been
  removed in response. Both can still be used as input via explicit conversion
  into a supported type.
- Miscellaneous unnecessary generic parameters.

### Fixed
- Check for the correct feature ("unstable" versus "nightly") in the `cfg`
  attributes used to control whether `ByteData` implementations are generated
  for `i128` and `u128`.

## [1.0.0-beta.1] - 2018-05-25
### Added
- `Marker::allocate_slice()`, `Marker::allocate_slice_clone()`, and
  `Marker::allocate_slice_copy()` for creating slice allocations.
- `Allocation::concat()` and `Allocation::concat_unchecked()` for
  concatenating two adjacent allocations into a single slice allocation.
- `MarkerFront::append{,_clone,_copy}()` and
  `MarkerBack::prepend{,_clone,_copy}()` for extending existing allocations at
  the end of their respective stacks with new data. This functionality is also
  available through the `Marker` trait using the `extend{,_clone,_copy}()`
  methods (appending is always performed by front markers, and prepending is
  always performed by back markers).
- `IntoSliceAllocation` trait for safely coercing `Allocation` instances into
  allocations of slices. This is also used to determine what types can be used
  for allocation concatenation (`concat()` and `concat_unchecked()` allocation
  methods) and extension (`append*()` and `prepend*()` marker methods).
- `SliceLike` trait for performing conversions between DSTs that essentially
  wrap some primitive slice type and the underlying slice type (e.g. between
  `str` and `[u8]`), and `ConcatenateSlice` trait for marking `SliceLike`
  types that can be safely concatenated without any additional verification
  needed. This is used to allow for general use of such DSTs in allocations.
- Various unit tests for edge cases (e.g. allocation extension safety, ZST
  allocation support).

### Changed
- Replaced the simple `Error` enum with an `Error` struct that provides the
  error category (an `ErrorKind` enum variant based on the old `Error` enum)
  and any values whose ownership was intended to be passed to the callee. This
  allows the caller to reuse such values if an operation fails instead of
  simply throwing them away.
- Replaced `Marker::concat()` with `Marker::concat_slices_clone()` and
  `Marker::concat_slices_copy()`, which both work on general slices.
- Use `NonNull<T>` for internal storage of `Allocation` pointers, allowing for
  compiler optimizations such as reducing `Option<Allocation<'marker, T>>`
  size to that of a single pointer.

## [0.3.0] - 2018-04-30
### Added
- `Marker::concat()` for concatenating a series of strings into a single
  string slice allocated from the marker on which it is called.
- `#[inline]` attribute to most `Marker` allocation functions and the
  `uninitialized_boxed_slice*()` utility functions.

### Changed
- Replaced data reference in `Allocation` with a raw pointer, allowing the
  removal of the nebulous `'t` lifetime in the `Allocation` type and `Marker`
  methods (conceptually, an `Allocation` instance owns its data, and this
  helps clarify that ownership).

### Removed
- Reference to creating static instances of `Scratchpad` from README
  (overlooked when correcting the crate documentation for version 0.2.0).

## [0.2.0] - 2018-04-29
### Added
- `Scratchpad::static_new()` function for creating scratchpads backed by
  static arrays without having to pass array instances as function parameters.
- `StaticBuffer` trait for constraining `Scratchpad::static_new()` to
  scratchpads that use only static arrays for backing storage.
- `cache_aligned_zeroed!()`, `cache_aligned_zeroed_for_bytes!()`, and
  `cache_aligned_zeroed_for_markers!()` macros for shorthand creation of
  zeroed-out `CacheAligned` data.
- References to `MarkerFront` and `MarkerBack` in documentation for `Marker`.
- Run tests with both the `std` and `unstable` crate features enabled in
  Travis CI configuration.
- "data-structures" and "embedded" categories in `Cargo.toml` manifest.
- Documentation link in `Cargo.toml` manifest.
- Documentation and release nodes links in `README.md`.
- Changelog.

### Changed
- Clarified how the front and back allocation stacks operate independently in
  documentation.
- Use clearer wording in "Memory Overhead" documentation section.
- Minor documentation tweaks.

### Removed
- References to creating static instances of `Scratchpad` from documentation
  (static variables must implement `Sync`, but `Scratchpad` is not thread-safe
  by design).

### Fixed
- Incorrect "deque" term use internally.

## 0.1.0 - 2018-04-27
### Added
- Initial release.

[Unreleased]: https://github.com/okready/scratchpad/compare/v1.1.0...HEAD
[1.1.0]: https://github.com/okready/scratchpad/compare/v1.0.1...v1.1.0
[1.0.1]: https://github.com/okready/scratchpad/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/okready/scratchpad/compare/v1.0.0-beta.2...v1.0.0
[1.0.0-beta.2]: https://github.com/okready/scratchpad/compare/v1.0.0-beta.1...v1.0.0-beta.2
[1.0.0-beta.1]: https://github.com/okready/scratchpad/compare/0.3.0...v1.0.0-beta.1
[0.3.0]: https://github.com/okready/scratchpad/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/okready/scratchpad/compare/0.1.0...0.2.0
