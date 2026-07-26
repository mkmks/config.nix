{
  description = "My machines and home directories";

  inputs = {
    nixos.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixos";      
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixos";
    };
    # blockchains
    nix-bitcoin.url = "github:fort-nix/nix-bitcoin/nixos-25.11";
    ethereum-nix = {
      url = "github:nix-community/ethereum.nix";
      inputs.nixpkgs.follows = "nixos";
    };
    cardano-node.url = "github:IntersectMBO/cardano-node";
    cardano-db-sync = {
      url = "github:IntersectMBO/cardano-db-sync/release/13.6.0.5";
      inputs = {
        CHaP.follows = "cardano-node/CHaP";
        haskellNix.follows = "cardano-node/haskellNix";
        iohkNix.follows = "cardano-node/iohkNix";
      };
    };
    cardano-wallet = {
      url = "github:cardano-foundation/cardano-wallet";
      inputs = {
        cardano-node-runtime.follows = "cardano-node";
#        CHaP.follows = "cardano-node/CHaP";
#        haskellNix.follows = "cardano-node/haskellNix";
#        hackage.follows = "cardano-node/hackageNix";
#        iohkNix.follows = "cardano-node/iohkNix";        
      };
    };
    blockfrost-backend.url = "github:blockfrost/blockfrost-backend-ryo";
    # applications
    emacs.url = "github:nix-community/emacs-overlay";
    niri.url = "github:sodiboo/niri-flake";
#    llama-cpp.url = "github:ggml-org/llama.cpp";
  };

  outputs = { self, nixos, home-manager, ... }@inputs: let
    unstable-overlay = (final: prev: {
      unstable = import inputs.nixpkgs-unstable {
        system = prev.system;
        config.allowUnfree = true;
        config.cudaSupport = true;
      };
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
            inputs.nix-bitcoin.nixosModules.default
            inputs.ethereum-nix.nixosModules.erigon
            inputs.cardano-node.nixosModules.cardano-node
            inputs.cardano-node.nixosModules.cardano-submit-api
            inputs.cardano-node.nixosModules.cardano-tracer
            inputs.cardano-db-sync.nixosModules.cardano-db-sync
            inputs.cardano-wallet.nixosModules.cardano-wallet
            inputs.blockfrost-backend.nixosModules.default
            ./nixos/blockchains
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
                  inputs.ethereum-nix.overlays.default
                  inputs.cardano-node.overlay
                  inputs.cardano-wallet.overlay
#                  inputs.llama-cpp.overlays.default
                  unstable-overlay
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
