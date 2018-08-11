# rust-nix

rust-nix is a very early prototype of a Nix parser written in Rust.

This can be used to manipulate the Nix AST and can for example be used for:

 - Interactively render Nix on a GUI
 - Formatting Nix code
 - Rename identifiers

and a lot more!

rust-nix ...

 - will always maintain span information (line number and column) so the error
   messages can be as useful as possible and any syntax highlighting plugin
   could use the tokenizer to highlight.
 - works on top of Rust iterators, which are lazy by design. Less heap
   allocations, yay!

## Currently implemented

You can check the `tests/` directory to see what is currently implemented.
