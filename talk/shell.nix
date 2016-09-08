{ nixpkgs ? import <nixpkgs> {}}:
let
  inherit (nixpkgs) pkgs;
  slides = pkgs.callPackage ./. {
    texlive = (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-basic collection-latexrecommended collection-fontsrecommended; });
  };
in
  slides

