# rnix-parser [![Crates.io](https://img.shields.io/crates/v/rnix.svg)](http://crates.io/crates/rnix)

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

# RIP jd91mzm2

Sadly, the original author of this project, [@jD91mZM2 has passed
away](https://www.redox-os.org/news/open-source-mental-health/). His online
presence was anonymous and what we have left is his code. This is but one of
his many repos that he contributed to.
