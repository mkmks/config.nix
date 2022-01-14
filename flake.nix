{
  description = "A Home Manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    emacs.url = "github:nix-community/emacs-overlay/34e8dff7d9ee40cc67bc821c4947f2879b182396";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";      
    };
  };

  outputs = inputs: {
    homeConfigurations = {
      viv = inputs.home-manager.lib.homeManagerConfiguration {
        homeDirectory = "/home/viv";
        username = "viv";
        configuration.imports = [ ./home.nix ];
      };
    };
  };
}
