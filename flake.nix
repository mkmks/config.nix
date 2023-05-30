{
  description = "My machines and home directories";

  inputs = {
    nixos.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/master";
    netkit.url = "github:icebox-nix/netkit.nix";
    std.url = "github:icebox-nix/std";    
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixos";      
    };
    nur.url = "github:nix-community/NUR";    
    emacs.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixos, netkit, std, home-manager, nur, emacs, ... }@inputs:
    let
      system = "x86_64-linux";
#      pkgs = inputs.nixos.legacyPackages.${system};
    in {
    nixosConfigurations = {
      schildpad = nixos.lib.nixosSystem {
#        inherit system;

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
        pkgs = nixos.legacyPackages.${system};
        modules = [
          ./home/desktop.nix
          ./home/devel.nix
          ./home/mail.nix
          ./home/research.nix
          ./home/term.nix
          ./home/term-emacs.nix
          ./home/term-fish.nix
          ./home/wayland.nix
          {
            home = {
              username = "viv";
              homeDirectory = "/home/viv";
              stateVersion = "23.05";
            };
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
          }
        ];
      };
    };
  };
}
