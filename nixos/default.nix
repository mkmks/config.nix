{ config, pkgs, ... }:

{
  nix = {
    settings = {
      auto-optimise-store = true;
      max-jobs = 8;
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
