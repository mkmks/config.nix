{ config, pkgs, ... }:

{
  nix = {
    settings = {
      max-jobs = 8;
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
      trusted-substituters = [
        "https://nix-community.cachix.org"
        "https://cache.iog.io"
      ];
    };
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true 
      experimental-features = nix-command flakes
    '';
  };
}
