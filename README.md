# rnix-parser [![Crates.io](https://img.shields.io/crates/v/rnix.svg)](http://crates.io/crates/rnix) [![Chat on Matrix](https://matrix.to/img/matrix-badge.svg)](https://matrix.to/#/#rnix-lsp:matrix.org)

rnix is a parser for the [Nix language](https://nixos.org/nix/) written in Rust.

This can be used to manipulate the Nix AST and can for example be used for:

- Interactively render Nix on a GUI
- Formatting Nix code
- Rename identifiers

and a lot more!

rnix nowadays uses [@matklad](https://github.com/matklad)'s
[rowan](https://crates.io/crates/rowan) crate to ensure:

- all span information is preserved, meaning you can use the AST to for
  example apply highlighting
- printing out the AST prints out 100% the original code. This is not an
  over-exaggeration, even completely invalid nix code such as this README can
  be intact after a parsing session (though the AST will mark errnous nodes)
- easy ways to walk the tree without resorting to recursion

## Demo

Examples can be found in the `examples/` directory. 

You may also want to see
[nix-explorer](https://gitlab.com/jD91mZM2/nix-explorer): An example
that highlights AST nodes in Nix code. This demonstrates how
whitespaces and commands are preserved.

## Hacking

Tests can be run with `cargo test`.

In order to update all `.expect`-files to the currently expected results,
you may run `UPDATE_TESTS=1 cargo test`.

You can parse Nix expressions from standard input using the `from-stdin` example.
To try that, run the following in your shell:

```sh
echo "[hello nix]" | cargo run --quiet --example from-stdin
```

## Release Checklist

* Ensure that all PRs that were scheduled for the release are merged (or optionally move
  them to another milestone).
* Close the milestone for the release (if any).
* Run `cargo test` on `master` (or the branch to release from) with all changes being pulled in.
* Apply the following patch to [nixpkgs-fmt](https://github.com/nix-community/nixpkgs-fmt):
  ```diff
  diff --git a/Cargo.toml b/Cargo.toml
  index 0891350..edad471 100644
  --- a/Cargo.toml
  +++ b/Cargo.toml
  @@ -13,6 +13,9 @@ repository = "https://github.com/nix-community/nixpkgs-fmt"
   [workspace]
   members = [ "./wasm" ]
   
  +[patch.crates-io]
  +rnix = { path = "/home/ma27/Projects/rnix-parser" }
  +
   [dependencies]
   rnix = "0.9.0"
   smol_str = "0.1.17"
  ```

  and run `cargo test`.

  While it's planned to add [fuzzing to `rnix-parser` as well](https://github.com/nix-community/rnix-parser/issues/32),
  `nixpkgs-fmt` has a decent test-suite that would've helped to catch regressions in the past.

  __Note:__ API changes are OK (and fixes should be contributed to `nixpkgs-fmt`), behavioral changes
  are not unless explicitly discussed before.
* Update the [CHANGELOG.md](https://github.com/nix-community/rnix-parser/blob/master/CHANGELOG.md).
* Bump the version number in [Cargo.toml](https://github.com/nix-community/rnix-parser/blob/master/Cargo.toml) & re-run `cargo build` to refresh the lockfile.
* Commit & run `nix build`.
* Tag the release and push everything.
* As soon as the CI has completed, run `cargo publish`.

# RIP jd91mzm2

Sadly, the original author of this project, [@jD91mZM2 has passed
away](https://www.redox-os.org/news/open-source-mental-health/). His online
presence was anonymous and what we have left is his code. This is but one of
his many repos that he contributed to.
