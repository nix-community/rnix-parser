{ pkgs_fn ? import <nixpkgs> }:

let
  mozilla_overlay = import (fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  pkgs = pkgs_fn { overlays = [ mozilla_overlay ]; };

  inherit (pkgs) stdenv;
in stdenv.mkDerivation {
  name = "rnix";
  buildInputs = [ pkgs.latest.rustChannels.stable.rust ];
}
