{
  description = "My machines and home directories";

  inputs = {
    nixos.url = "github:nixos/nixpkgs/nixos-21.11";
    #    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    netkit.url = "github:icebox-nix/netkit.nix";
    std.url = "github:icebox-nix/std";    
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixos";      
    };
    emacs.url = "github:nix-community/emacs-overlay/34e8dff7d9ee40cc67bc821c4947f2879b182396";
  };

  outputs = { self, nixos, netkit, std, home-manager, emacs, ... }@inputs: {
    nixosConfigurations = {
      schildpad = nixos.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./configuration.nix
          netkit.nixosModule
          std.nixosModule
        ];
      };
    };
    
    homeConfigurations = {
      viv = home-manager.lib.homeManagerConfiguration {
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
            overlays = [ emacs.overlay ]; 
          };
        };
      };
    };
  };
}
