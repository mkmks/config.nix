{
  description = "My machines and home directories";

  inputs = {
    nixos.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/master";
    netkit.url = "github:icebox-nix/netkit.nix";
    std.url = "github:icebox-nix/std";    
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixos";      
    };
    nur.url = "github:nix-community/NUR";    
    emacs.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixos, netkit, std, home-manager, nur, emacs, ... }@inputs: {
    nixosConfigurations = {
      schildpad = nixos.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nixos/desktop.nix
          ./nixos/laptop.nix
          ./nixos/schildpad.nix
          netkit.nixosModule
          std.nixosModule
        ];
      };
    };
    
    homeConfigurations = {
      viv = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        stateVersion = "22.05";
        homeDirectory = "/home/viv";
        username = "viv";
        configuration = {
          imports = [
            ./home/desktop.nix
            ./home/devel.nix
            ./home/mail.nix
            ./home/research.nix
            ./home/term.nix
            ./home/term-emacs.nix
            ./home/term-fish.nix
            ./home/wayland.nix
          ];
          nixpkgs = {
            config = import ./config.nix;
            overlays = [
              emacs.overlay
              nur.overlay
              (final: prev: {
                unstable = inputs.nixpkgs-unstable.legacyPackages.${prev.system};
              })
            ]; 
          };
        };
      };
    };
  };
}
