{ mkDerivation, base, reactive-banana, stdenv }:
mkDerivation {
  pname = "talk";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base reactive-banana ];
  license = stdenv.lib.licenses.bsd3;
}
