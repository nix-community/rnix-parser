# Changelog

All notable changes between releases will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

* `rnix::types::ParsedTypeError` now implements the `Debug`, `Copy`, `Clone`, `fmt::Display` traits (from [@NerdyPepper](https://github.com/NerdyPepper))

### Changed

* A few more examples of invalid `inherit`-expressions are correctly marked as such (from [@oberblastmeister](https://github.com/oberblastmeister)).

* `rnix::types::BinOp::operator()` now returns `Option<BinOpKind>` rather than unwrapping the
  value (from [@NerdyPepper](https://github.com/NerdyPepper))

## [v0.9.1] - 2021-09-23

### Changed

* Fixed a memory leak while parsing `inherit`-expressions with invalid syntax (from [@Ma27](https://github.com/Ma27/)).

[Unreleased]: https://github.com/nix-community/rnix-parser/compare/v0.9.1...master
[v0.9.1]: https://github.com/nix-community/rnix-parser/compare/v0.9.0...v0.9.1
