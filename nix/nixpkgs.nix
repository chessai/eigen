{ compiler ? "ghc843" }:

with rec {
  fetchFromGitHub = (
    (import <nixpkgs> { config = {}; overlays = []; }).fetchFromGitHub);
  _nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "49dc36c9a8576b69fe50a7789dc6ef9d6c3ca584";
    sha256 = "1015g6q0jp2g7wx6zgf60q9fjaismxv3lajd1gv8jc8cjmwdbkmb";
  };
};

import _nixpkgs {
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskellPackages = super.haskell.packages.${compiler}.override {
        overrides = import ./overrides.nix { pkgs = self; };
      };

    };
  };
  overlays = [];
}
