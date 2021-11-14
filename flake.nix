{
  description = "Rust-based parser for nix files";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, fenix, nixpkgs, utils }: {
    overlay = final: prev:
      let
        fenix' = (fenix.overlay final prev).fenix;

        rustToolchain = fenix'.fromToolchainFile {
          file = ./rust-toolchain.toml;
          sha256 = "sha256-MJyH6FPVI7diJql9d+pifu5aoqejvvXyJ+6WSJDWaIA=";
        };

        rustPlatform = final.makeRustPlatform {
          cargo = rustToolchain;
          rustc = rustToolchain;
        };

        target = final.rust.toRustTarget final.stdenv.hostPlatform;
      in
      {
        rnix-parser = rustPlatform.buildRustPackage {
          pname = "rnix-parser";
          version = "0.9.1";

          src = final.lib.cleanSource ./.;
          cargoLock.lockFile = ./Cargo.lock;

          postBuild = "cargo doc";

          doCheck = true;
          postCheck = "cargo bench";

          outputs = [ "out" "doc" ];
          installPhase = ''
            runHook preInstall
            mkdir -p $out/lib
            mkdir -p $doc/share/doc/rnix

            cp ./target/${target}/$cargoBuildType/librnix.rlib $out/lib
            cp -r ./target/doc/. $doc/share/doc/rnix
            runHook postInstall
          '';

          passthru.rustPlatform = rustPlatform;
        };
      };
  }
  // utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
    in
    {
      devShell = pkgs.mkShell {
        name = "rnix-parser";

        inputsFrom = pkgs.lib.attrValues self.packages.${system};

        nativeBuildInputs = with pkgs; [
          cargo-edit
        ];
      };

      packages.rnix-parser = pkgs.rnix-parser;

      defaultPackage = self.packages.${system}.rnix-parser;
    }
  );
}
