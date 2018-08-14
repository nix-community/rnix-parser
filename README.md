# rnix

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

Not everything is implemented yet. I don't know all of nix inside my head, but what I do know:

**TODO**:

 - `or` keyword
