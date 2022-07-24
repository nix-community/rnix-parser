# Changelog

All notable changes between releases will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

## [v0.10.2] - 2022-06-14

* Correctly parse every possible case of path-interpolation in Nix code ([#85](https://github.com/nix-community/rnix-parser/issues/85)).

  (from [@ncfavier](https://github.com/ncfavier)).

## [v0.10.1] - 2021-12-06

### Fixed

* Trivia tokens (i.e. ` `, `\r`, `\n` etc.) are no longer misplaced around path-expressions.
  This is a regression from `v0.10.0` which would've broken `nixpkgs-fmt`.

  (from [@Ma27](https://github.com/Ma27)).

## [v0.10.0] - 2021-11-30

### Added

* Support for the new path-interpolation syntax from Nix 2.4 was added. The parser
  now correctly detects

  ```nix
  ./foo/${bar}
  ```

  as valid code and parses it down to

  ```
  NODE_ROOT 0..13 {
    NODE_PATH_WITH_INTERPOL 0..12 {
      TOKEN_PATH("./foo/") 0..6
      NODE_STRING_INTERPOL 6..12 {
        TOKEN_INTERPOL_START("${") 6..8
        NODE_IDENT 8..11 {
          TOKEN_IDENT("bar") 8..11
        }
        TOKEN_INTERPOL_END("}") 11..12
      }
    }
    TOKEN_WHITESPACE("\n") 12..13
  }
  ```

  Paths without interpolation are still within `NODE_LITERAL` for backwards-compatibility.

  (from [@Ma27](https://github.com/Ma27)).

* `rnix::types::ParsedTypeError` now implements the `Debug`, `Copy`, `Clone`, `fmt::Display` traits (from [@NerdyPepper](https://github.com/NerdyPepper)).

* During development, the parser can be tested like this:

  ```
  $ cargo run --example from-stdin <<< 'builtins.map (x: x * x) [ 1 2 3 ]'
  ```

  (from [@efx](https://github.com/efx)).

### Changed

* A few more examples of invalid `inherit`-expressions are correctly marked as such (from [@oberblastmeister](https://github.com/oberblastmeister)).

* `rnix::types::BinOp::operator()` now returns `Option<BinOpKind>` rather than unwrapping the
  value (from [@NerdyPepper](https://github.com/NerdyPepper)).

  If your code worked fine before, it will be sufficient to add a `.unwrap()` since this was
  what happened before within `::operator()`.

* Duplicated arguments in the argument-pattern syntax are now marked as parser error. E.g.

  ```nix
  { a, a }: a
  ```

  now produces an error (from [@Ma27](https://github.com/Ma27)).

* Floats without trailing zeroes (e.g. `.5`) are no longer marked as syntax error (from [@Ma27](https://github.com/Ma27)).

## [v0.9.1] - 2021-09-23

### Changed

* Fixed a memory leak while parsing `inherit`-expressions with invalid syntax (from [@Ma27](https://github.com/Ma27/)).

[Unreleased]: https://github.com/nix-community/rnix-parser/compare/v0.10.2...master
[v0.10.2]: https://github.com/nix-community/rnix-parser/compare/v0.10.1...v0.10.2
[v0.10.1]: https://github.com/nix-community/rnix-parser/compare/v0.10.0...v0.10.1
[v0.10.0]: https://github.com/nix-community/rnix-parser/compare/v0.9.1...v0.10.0
[v0.9.1]: https://github.com/nix-community/rnix-parser/compare/v0.9.0...v0.9.1
