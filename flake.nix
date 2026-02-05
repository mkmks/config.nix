{
  description = "My machines and home directories";

  inputs = {
    nixos.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixos";      
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixos";
    };
    # blockchains
    cardano-db-sync.url = "github:IntersectMBO/cardano-db-sync";
    cardano-node.url = "github:IntersectMBO/cardano-node";
    cardano-wallet.url = "github:cardano-foundation/cardano-wallet";
    ethereum-nix = {
      url = "github:nix-community/ethereum.nix?rev=8f01580481e88e169b7ada56f1500dccd6cefe61";
      inputs.nixpkgs.follows = "nixos";
    };
    nix-bitcoin.url = "github:fort-nix/nix-bitcoin/nixos-25.11";
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
            ./nixos/hosts/schildpad.nix
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
            ./nixos/hosts/loderunner.nix
           {
             environment.systemPackages = [
               inputs.cardano-node.packages."x86_64-linux".cardano-cli
             ];
           }
          ];
        };
        hivemind = nixos.lib.nixosSystem {
          modules = [
            inputs.cardano-db-sync.nixosModules.cardano-db-sync
            inputs.cardano-node.nixosModules.cardano-node
            inputs.cardano-node.nixosModules.cardano-submit-api
            inputs.cardano-node.nixosModules.cardano-tracer
            inputs.cardano-wallet.nixosModules.cardano-wallet
            inputs.ethereum-nix.nixosModules.geth
            inputs.ethereum-nix.nixosModules.lighthouse-beacon
            inputs.nix-bitcoin.nixosModules.default
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
                  inputs.cardano-wallet.overlay
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
              nixpkgs = {
                config = {
                  allowUnfree = true;
                  cudaSupport = true;
                };
                overlays = [
                  inputs.emacs.overlays.package
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
