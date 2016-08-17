{ mkDerivation, base, containers, profunctors, reactive-banana
, stdenv
}:
mkDerivation {
  pname = "talk";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers profunctors reactive-banana
  ];
  license = stdenv.lib.licenses.bsd3;
}
