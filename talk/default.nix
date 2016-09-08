{stdenv, pandoc, texlive}:

stdenv.mkDerivation {
  name = "slides";
  src = ./slides.md;
  builder = ./builder.sh;
  inherit pandoc;
  inherit texlive;
}
