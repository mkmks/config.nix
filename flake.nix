{
  description = "A Home Manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
#    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    emacs.url = "github:nix-community/emacs-overlay/34e8dff7d9ee40cc67bc821c4947f2879b182396";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";      
    };
  };

  outputs = inputs: {
    homeConfigurations = {
      viv = inputs.home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        stateVersion = "21.11";
        homeDirectory = "/home/viv";
        username = "viv";
        configuration = {
          imports = [
            ./home.nix
            ./devel.nix
            ./emacs.nix
            ./mail.nix
            ./term.nix 
            ./wayland.nix
          ];
          nixpkgs = {
            config = import ./config.nix;
            overlays = [ inputs.emacs.overlay ]; 
          };
        };
      };
    };
  };
}
