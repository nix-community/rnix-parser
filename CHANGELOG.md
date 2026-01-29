# Changelog

All notable changes between releases will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.13.0] - 2026-01-29

### Breaking Changes

* **Path types are now distinguished at the type level.** `TOKEN_PATH` and `NODE_PATH` have been
  replaced with specific variants for each path type:
  - `TOKEN_PATH_ABS` / `NODE_PATH_ABS` - absolute paths (`/foo/bar`)
  - `TOKEN_PATH_REL` / `NODE_PATH_REL` - relative paths (`./foo`, `../bar`, `foo/bar`)
  - `TOKEN_PATH_HOME` / `NODE_PATH_HOME` - home-relative paths (`~/foo`)
  - `TOKEN_PATH_SEARCH` / `NODE_PATH_SEARCH` - search paths (`<nixpkgs>`)

  The `ast::Expr` enum now has `PathAbs`, `PathRel`, `PathHome`, and `PathSearch` variants
  instead of a single `Path` variant. See `MIGRATING.md` for upgrade guidance.

* **Double slashes in paths are now syntax errors.** Paths like `foo//bar` now produce
  `TOKEN_ERROR` instead of being parsed.

* **The `parser` and `tokenizer` modules are now private.** Use `rnix::Root::parse()` and
  `rnix::tokenize()` instead.

### Added

* Implement `FromStr` for `SyntaxKind` (from [@abhillman](https://github.com/abhillman)).

* Add `MIGRATING.md` with upgrade guidance for breaking changes.

* Add `nix develop .#fuzz` devShell for running cargo-fuzz with nightly Rust.

### Fixed

* Fix leading zero number parsing to match Nix behavior (from [@hsjobeki](https://github.com/hsjobeki)).

### Changed

* Update `rowan` to 0.16.
* Update `criterion` to 0.8.
* Update `expect-test` to 1.5.1.
* Add Clippy to CI and resolve all warnings.

## [v0.12.0] - 2025-01-09

* Add support for pipe operators

## [v0.11.0] - 2022-11-11

* removes the `types` module and replaces it with `ast`

* the `ast` modules has better types and sum types

* removes `ParsedType` in favor of `Expr`

* removes the `value` module. methods on `Value` go on the individual types. For example, string related methods go on `ast::Str`

* `parse` changed to `Root::parse`

* `NODE_OR_DEFAULT` is removed

* `NODE_KEY` -> `NODE_ATTRPATH`, `NODE_KEY_VALUE` -> `NODE_ATTRPATH_VALUE`

* `BinOp::IsSet` is removed since it's actually not a normal binary operator.

* `NODE_SELECT` is flattened in favor of `NODE_ATTRPATH` which consists of multiple Attrs, including `a.b.c` and `a.b.c or expr`

* `a or b` is Apply, not "OrDefault", which matches the result of Nix. Fixes #23

* `${` is considered invalid at Expr places now, which matches the result of Nix.

* remove `TOKEN_DYNAMIC_START` and `TOKEN_DYNAMIC_END`

* rowan 15.0

* various other parsing fixes

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

[Unreleased]: https://github.com/nix-community/rnix-parser/compare/v0.13.0...master
[v0.13.0]: https://github.com/nix-community/rnix-parser/compare/v0.12.0...v0.13.0
[v0.12.0]: https://github.com/nix-community/rnix-parser/compare/v0.11.0...v0.12.0
[v0.11.0]: https://github.com/nix-community/rnix-parser/compare/v0.10.2...v0.11.0
[v0.10.2]: https://github.com/nix-community/rnix-parser/compare/v0.10.1...v0.10.2
[v0.10.1]: https://github.com/nix-community/rnix-parser/compare/v0.10.0...v0.10.1
[v0.10.0]: https://github.com/nix-community/rnix-parser/compare/v0.9.1...v0.10.0
[v0.9.1]: https://github.com/nix-community/rnix-parser/compare/v0.9.0...v0.9.1
