{ mkDerivation, async, base, containers, haskeline, network
, reactive-banana, servant, servant-snap, snap-core, snap-server
, stdenv, stm, text
}:
mkDerivation {
  pname = "chat-reactive-banana";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base containers haskeline network reactive-banana servant
    servant-snap snap-core snap-server stm text
  ];
  license = stdenv.lib.licenses.bsd3;
}
