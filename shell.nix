{ package ? "eigen", compiler ? "ghc841" }:

(import ./default.nix {
  inherit package compiler;
}).eigen
