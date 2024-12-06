let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
  pkg = pkgs.haskellPackages.developPackage {
    root = ./.;
  };
in
pkg
