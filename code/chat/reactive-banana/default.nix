{ mkDerivation, base, containers, reactive-banana, stdenv }:
mkDerivation {
  pname = "chat-reactive-banana";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers reactive-banana ];
  license = stdenv.lib.licenses.bsd3;
}
