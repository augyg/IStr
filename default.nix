{ mkDerivation, base, haskell-src-meta, lib, template-haskell }:
mkDerivation {
  pname = "IStr";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base haskell-src-meta template-haskell ];
  license = lib.licenses.mit;
}
