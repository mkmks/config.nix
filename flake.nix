{
  description = "My machines and home directories";

  inputs = {
    nixos.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixos";      
    };
#    nur.url = "github:nix-community/NUR";    
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixos";
    };
    # blockchains
    cardano-node.url = "github:IntersectMBO/cardano-node";
    cardano-wallet.url = "github:cardano-foundation/cardano-wallet";
    daedalus.url = "github:input-output-hk/daedalus";
    ethereum-nix = {
      url = "github:nix-community/ethereum.nix";
      inputs.nixpkgs.follows = "nixos";
    };
    # applications
    emacs.url = "github:nix-community/emacs-overlay";
    niri.url = "github:sodiboo/niri-flake";
  };

  outputs = { self, nixos, home-manager, ... }@inputs: let
    unstable-overlay = (final: prev: {
      unstable = inputs.nixpkgs-unstable.legacyPackages.${prev.system};
    });
  in {
      nixosConfigurations = {
        schildpad = nixos.lib.nixosSystem {
          modules = [
            ./nixos/workstation/laptop/schildpad
          ];
        };
        loderunner = nixos.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            inputs.disko.nixosModules.disko
            inputs.cardano-node.nixosModules.cardano-node
            inputs.cardano-node.nixosModules.cardano-submit-api
            inputs.cardano-wallet.nixosModule
            ./nixos/blockchain.nix
            ./nixos/server/loderunner
           {
             environment.systemPackages = [
               inputs.cardano-node.packages."x86_64-linux".cardano-cli
             ];
           }
          ];
        };
        hivemind = nixos.lib.nixosSystem {
          modules = [
            inputs.cardano-node.nixosModules.cardano-node
            inputs.cardano-node.nixosModules.cardano-submit-api
            inputs.cardano-wallet.nixosModule
            inputs.ethereum-nix.nixosModules.default
            ./nixos/blockchain.nix
            ./nixos/hosts/hivemind.nix
            {
              environment.systemPackages = [
                inputs.cardano-node.packages."x86_64-linux".cardano-cli
              ];
              nixpkgs = {
                config = {
                  cudaSupport = true;
                };
                overlays = [
                  inputs.cardano-node.overlay
                  inputs.ethereum-nix.overlays.default
                ];
              };
            }
          ];
        };
      };
      
      homeConfigurations = {
        viv = let
          system = "x86_64-linux";
          pkgs = nixos.legacyPackages.${system};
        in home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          
          modules = [
            inputs.niri.homeModules.niri
            ./home
            {
              home.packages = [
                # inputs.daedalus.defaultPackage.${system}
              ];
              nixpkgs = {
                config = {
                  allowUnfree = true;
                };
                overlays = [
                  inputs.emacs.overlays.package
#                  inputs.nur.overlays.default
                  inputs.niri.overlays.niri
                  unstable-overlay
                ];
              };
            }
          ];
        };
      };
    };
}
