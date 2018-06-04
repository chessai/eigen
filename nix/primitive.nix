{ mkDerivation, base, fetchgit, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "primitive";
  version = "0.6.4.0";
  src = fetchgit {
    url = "https://github.com/haskell/primitive.git";
    sha256 = "0f3fqx1khl9wnbkn3g98xnnw1iy7yp9hr4l1a3cprznw6rvid343";
    rev = "c6e022424b7f7099e2c8bf8ccd4361bde84b0192";
  };
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = stdenv.lib.licenses.bsd3;
}
