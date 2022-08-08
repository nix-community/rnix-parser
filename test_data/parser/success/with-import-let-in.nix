with import ./simple-set.nix;

let
  a = 4 + 2;
in {
  b = a + 2;

  legacy = let {
    this_syntax_sucks = true;
    body = this_syntax_sucks;
  };
}
