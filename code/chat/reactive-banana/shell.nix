{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", withProfiling ? false, withHoogle ? true }:

let

  inherit (nixpkgs) pkgs;
  lib = import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix" { pkgs = nixpkgs; };

  haskellPackagesWithCompiler = 
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackagesWithProfiling = 
    if withProfiling
    then haskellPackagesWithCompiler.override {
           overrides = self: super: {
             mkDerivation = args: super.mkDerivation (args // { enableLibraryProfiling = true; });
           };
         }
    else haskellPackagesWithCompiler;
                 
  haskellPackagesWithHoogle =
    if withHoogle
    then haskellPackagesWithProfiling.override {
           overrides = self: super: {
             ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
             ghcWithPackages = self.ghc.withPackages;
           };
         }
    else haskellPackagesWithProfiling;

  modifiedHaskellPackages = haskellPackagesWithHoogle.override {
    overrides = self: super: {
      # Add various dependencies here
    };
  }; 

  drv = modifiedHaskellPackages.callPackage ./. {};

in

  if pkgs.lib.inNixShell then drv.env else drv
