{ mkDerivation, base, containers, exceptions, fetchgit, mtl
, prettyprinter, stdenv, template-haskell, these, transformers
}:
mkDerivation {
  pname = "refined";
  version = "0.2.3.0";
  src = fetchgit {
    url = "https://github.com/nikita-volkov/refined.git";
    sha256 = "0wwjymh96y0vxr0nkq6rnkblw5h9znbk6b0c7v5yylk4l45vijv8";
    rev = "d3a4159fb33a4f6dfc37e99b1c99827b6d042d8f";
  };
  libraryHaskellDepends = [
    base containers exceptions mtl prettyprinter template-haskell these
    transformers
  ];
  homepage = "https://github.com/nikita-volkov/refined";
  description = "Refinement types with static and runtime checking";
  license = stdenv.lib.licenses.mit;
}
