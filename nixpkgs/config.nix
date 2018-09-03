{
  allowUnfree = true;
  
  # packageOverrides = pkgs :
  #   let
  #     fetchNixPkgs = { rev, sha256 }:
  #       pkgs.fetchFromGitHub {
  #         inherit sha256 rev;
  #         owner = "NixOS";
  #         repo = "nixpkgs-channels";
  #       };
  #     pinnedPkgs = import (fetchNixPkgs {
  #       rev = "2180d2c1180b04b14877cccad841fdb06941255a";
  #       sha256 = "0kf5babd9y7r5wz4982m637x65lh8m4qma6gpc9mix1bxp2bvh8q";
  #     }) {};
  #   in {
  #     pinned = pinnedPkgs;
  #   };

  packageOverrides = pkgs : {
                          dwm = pkgs.dwm.override {
                          patches = [ ./dwm-config.diff ];
                     };

  };

}
