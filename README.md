# rnix [![Crates.io](https://img.shields.io/crates/v/rnix.svg)](http://crates.io/crates/rnix)

rnix is a Nix parser written in Rust.

This can be used to manipulate the Nix AST and can for example be used for:

 - Interactively render Nix on a GUI
 - Formatting Nix code
 - Rename identifiers

and a lot more!

rnix ...

 - will always maintain span information (line number and column) so the error
   messages can be as useful as possible and any syntax highlighting plugin
   could use the tokenizer to highlight.
 - works on top of Rust iterators, which are lazy by design. You get cool
   functions like `map` and `filter` at your fingertips!

# Unimplemented

I have implemented everything I can think of. Most, if not all of these, are
tested inside the `tests/` directory. If some kind of syntax isn't tested
there, you should report it as unimplemented.
