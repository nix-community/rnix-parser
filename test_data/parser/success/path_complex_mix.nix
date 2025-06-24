{
  # Various path types in one file
  abs = /nix/store/hash-name-1.0.0;
  rel = ./foo/bar/../baz;
  home = ~/.config/nixpkgs/overlays;
  search = <nixpkgs/lib/systems>;
  
  # Paths with special characters
  withNumbers = /usr/lib64;
  withDots = ./foo.bar.baz;
  withPlus = ./foo+bar;
  withDash = /nix-store/package;
  
  # Path operations
  combined = /foo + ./bar;
  inList = [ /a ./b ~/c <d> ];
}