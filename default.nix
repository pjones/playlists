{ pkgs ? import <nixpkgs> { }
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/open/nix-hs.git";
    rev = "a2b666faf8cb3c6f769655dfb36f4695f78bc3c3";
  };

  nix-hs = import "${nix-hs-src}/default.nix" { inherit pkgs; };

in nix-hs {
  cabal = ./playlists.cabal;
  flags = [ "maintainer" "test-doctest" ];

  overrides = lib: self: super: with lib; {
    optparse-applicative =
      if super ? optparse-applicative_0_15_0_0
        then super.optparse-applicative_0_15_0_0
        else optparse-applicative;
  };
}
