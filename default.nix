{ pkgs ? (import <nixpkgs> {}) }:

let haskellPackages = pkgs.haskellPackages_ghc784_profiling; in

haskellPackages.cabal.mkDerivation (self: {
  pname = "playlists";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;

  buildDepends = with pkgs; [
    # Basic Haskell tools needed to build this project:
    haskellPackages.ghc haskellPackages.cabalInstall
    haskellPackages.alex haskellPackages.happy
    haskellPackages.hlint
  ];

  meta = {
    homepage = https://github.com/pjones/playlists;
    description = "Library and executable for working with playlist files";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
