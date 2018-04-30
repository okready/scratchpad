# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Resolve link to `Scratchpad::static_new()` in crate documentation.
- Resolve links to `CacheAligned` in documentation for
  `cache_aligned_zeroed!()`, `cache_aligned_zeroed_for_bytes!()`, and
  `cache_aligned_zeroed_for_markers!()` macros.

## [0.1.1] - 2018-04-29
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

[Unreleased]: https://github.com/okready/scratchpad/compare/0.1.1...HEAD
[0.1.1]: https://github.com/okready/scratchpad/compare/0.1.0...0.1.1
