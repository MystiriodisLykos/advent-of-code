let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
  pkg = pkgs.haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      aoc-common = ../common;
    };
  };
in
pkg
