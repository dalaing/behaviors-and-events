{ mkDerivation, async, base, containers, haskeline, lens, mtl
, network, profunctors, reactive-banana, safe, stdenv, stm
}:
mkDerivation {
  pname = "talk";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base containers haskeline lens mtl network profunctors
    reactive-banana safe stm
  ];
  license = stdenv.lib.licenses.bsd3;
}
