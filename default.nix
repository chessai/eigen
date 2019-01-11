{ compiler ? "ghc843"
, nixpkgs ? (import ./nix/nixpkgs.nix { inherit compiler; })
}:

with rec {
#  nixpkgs = (import ./nix/nixpkgs.nix {
#    inherit compiler; 
#  });
  drv = nixpkgs.haskellPackages.eigen;
};

drv
