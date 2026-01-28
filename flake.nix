{
  description = "Rust-based parser for nix files";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    import-cargo.url = "github:edolstra/import-cargo";
  };

  outputs = { self, nixpkgs, utils, import-cargo }:
    {
      overlay = final: prev: let
        target = final.stdenv.hostPlatform.rust.rustcTarget;
        flags = "--release --offline --target ${target}";
        inherit (import-cargo.builders) importCargo;
      in {
        rnix-parser = final.stdenv.mkDerivation {
          pname = "rnix-parser";
          version = (builtins.fromTOML (builtins.readFile ./Cargo.toml)).package.version;
          src = final.lib.cleanSource ./.;
          nativeBuildInputs = with final; [
            rustc cargo
            (importCargo { lockFile = ./Cargo.lock; inherit (final) pkgs; }).cargoHome
          ];

          outputs = [ "out" "doc" ];
          doCheck = true;

          buildPhase = ''
            cargo build ${flags}
            cargo doc ${flags}
          '';

          checkPhase = ''
            cargo test ${flags}
            cargo bench
          '';

          installPhase = ''
            mkdir -p $out/lib
            mkdir -p $doc/share/doc/rnix

            cp -r ./target/${target}/doc $doc/share/doc/rnix
            cp ./target/${target}/release/librnix.rlib $out/lib/
          '';
        };
      };
    }
    // utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
      in
      rec {
        # `nix develop`
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ rustfmt rustc cargo clippy ];
        };

        packages.rnix-parser = pkgs.rnix-parser;
        defaultPackage = packages.rnix-parser;
      }
    );
}
