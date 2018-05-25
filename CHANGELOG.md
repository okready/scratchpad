# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Removed
- Extraneous lifetime parameter left in the `Marker::concat_slices_clone()`
  signature.

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

[Unreleased] https://github.com/okready/scratchpad/compare/v1.0.0-beta.1...HEAD
[1.0.0-beta.1]: https://github.com/okready/scratchpad/compare/0.3.0...v1.0.0-beta.1
[0.3.0]: https://github.com/okready/scratchpad/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/okready/scratchpad/compare/0.1.0...0.2.0
