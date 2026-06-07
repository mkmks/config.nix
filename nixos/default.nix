{ config, pkgs, ... }:

{
  nix = {
    settings = {
      max-jobs = 8;
      substituters = [
        "https://cuda-maintainers.cachix.org"
        "https://devenv.cachix.org"
        "https://niri.cachix.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "niri.cachix.org-1:Wv0OmO7PsuocRKzfDoJ3mulSl7Z6oezYhGhR+3W2964="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
      ];
      trusted-substituters = [
        "https://cache.iog.io"
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
