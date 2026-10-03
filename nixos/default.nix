{ config, pkgs, ... }:

{
  nix = {
    settings = {
      auto-optimise-store = true;
      max-jobs = 8;
      substituters = [
        "https://cache.iog.io"
        "https://cache.nixos-cuda.org"
        "https://devenv.cachix.org"
        "https://niri.cachix.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "niri.cachix.org-1:Wv0OmO7PsuocRKzfDoJ3mulSl7Z6oezYhGhR+3W2964="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true 
      experimental-features = nix-command flakes
      allow-import-from-derivation = true
    '';
  };

  programs = {
    fish.enable = true;
    git.enable = true;
  };
  
  users.users.viv = {
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    shell = pkgs.fish;
    uid = 1000;
  };
}
