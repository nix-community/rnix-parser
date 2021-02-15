{
  description = "Rust-based parser for nix files";

  inputs = {
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
      in
      rec {
        # `nix develop`
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ rustfmt rustc cargo ];
        };
      }
    );
}
