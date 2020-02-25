let
  pkgs = import ./nix;
in
pkgs.mkShell {
  buildInputs = [
    pkgs.rustfmt
    pkgs.cargo
    pkgs.cargo.passthru.rustc
  ];
}
