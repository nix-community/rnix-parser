{
  description = "Rust-based parser for nix files";

  # Stolen from https://gist.github.com/573/885a062ca49d2db355c22004cc395066
  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };
    rust-overlay = { url = "github:oxalica/rust-overlay"; };
  };

  outputs = { nixpkgs, rust-overlay, ... }:
    let system = "x86_64-linux";
    in
    {
      devShell.${system} =
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ rust-overlay.overlay ];
          };
        in
        (({ pkgs, ... }:
          pkgs.mkShell {
            buildInputs = with pkgs; [
              cargo
              cargo-watch
              nodejs
              wasm-pack

              # Note: to use stable, just replace `nightly` with `stable`
              (rust-bin.stable.latest.default.override {
                extensions = [ "rust-src" ];
                targets = [ "wasm32-unknown-unknown" ];
              })

              # Add some extra dependencies from `pkgs`
              pkg-config
              openssl
            ];

            shellHook = "";
          }) {
          pkgs = pkgs;

          # Set Environment Variables
          RUST_BACKTRACE = 1;
          PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
          RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
        });
    };
}
