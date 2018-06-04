{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  refined = doJailbreak (self.callPackage ./refined.nix {});

  # primitive = dontCheck (self.callPackage ./primitive.nix {});

  singletons = dontCheck super.singletons;

  eigen = (
    with rec {
      eigenSource = pkgs.lib.cleanSource ../.;
      eigenBasic = self.callCabal2nix "eigen" eigenSource {};
    };
    overrideCabal eigenBasic (old: {
    })
  );
}
