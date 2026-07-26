rec {
  description = "My machines and home directories";

  nixConfig = {
    extra-substituters = [
      "https://cuda-maintainers.cachix.org"
      "https://devenv.cachix.org"
      "https://niri.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "niri.cachix.org-1:Wv0OmO7PsuocRKzfDoJ3mulSl7Z6oezYhGhR+3W2964="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

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
    cardano-node.url = "github:IntersectMBO/cardano-node/11.0.1";
    cardano-db-sync = {
      url = "github:IntersectMBO/cardano-db-sync/13.7.2.1";
      inputs = {
        CHaP.follows = "cardano-db-sync-CHaP";
        hackageNix.follows = "cardano-db-sync-hackageNix";
        haskellNix.follows = "cardano-db-sync-haskellNix";
        iohkNix.follows = "cardano-db-sync-iohkNix";
        nixpkgs.follows = "cardano-db-sync-nixpkgs";
      };
    };
    # Preserve the release's tested graph so cache.iog.io can substitute it.
    cardano-db-sync-CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages/28bb010c42a79526697f429e4ef536e3aa131dd7";
      flake = false;
    };
    cardano-db-sync-hackageNix = {
      url = "github:input-output-hk/hackage.nix/f4cb0e97083bee8ad5fdc83d555ef7c17770d81e";
      flake = false;
    };
    cardano-db-sync-haskellNix = {
      url = "github:input-output-hk/haskell.nix/545ccfeef07d1100ad0eae9009139ef4706943ee";
      inputs = {
        hackage.follows = "cardano-db-sync-hackageNix";
        nixpkgs.follows = "cardano-db-sync-nixpkgs";
      };
    };
    cardano-db-sync-iohkNix = {
      url = "github:input-output-hk/iohk-nix/64ca6f4c0c6db283e2ec457c775bce75173fb319";
      inputs.nixpkgs.follows = "cardano-db-sync-nixpkgs";
    };
    cardano-db-sync-nixpkgs.url =
      "github:NixOS/nixpkgs/c1cb7d097cb250f6e1904aacd5f2ba5ffd8a49ce";
    cardano-wallet = {
      url = "github:cardano-foundation/cardano-wallet/v2026-07-23";
      inputs = {
        cardano-node-runtime.follows = "cardano-node";
      };
    };
    blockfrost-backend.url = "github:blockfrost/blockfrost-backend-ryo/v6.7.1";
    # applications
    emacs.url = "github:nix-community/emacs-overlay";
    niri.url = "github:sodiboo/niri-flake";
#    llama-cpp.url = "github:ggml-org/llama.cpp";
  };

  outputs = { self, nixos, home-manager, ... }@inputs: let
    extraSubstituters = nixConfig.extra-substituters;
    extraTrustedPublicKeys = nixConfig.extra-trusted-public-keys;
    unstable-overlay = (final: prev: {
      unstable = import inputs.nixpkgs-unstable {
        system = prev.system;
        config.allowUnfree = true;
        config.cudaSupport = true;
      };
    });
    cardanoModules = [
      inputs.cardano-node.nixosModules.cardano-node
      inputs.cardano-node.nixosModules.cardano-submit-api
      inputs.cardano-node.nixosModules.cardano-tracer
      inputs.cardano-db-sync.nixosModules.cardano-db-sync
      inputs.cardano-wallet.nixosModules.cardano-wallet
      inputs.blockfrost-backend.nixosModules.default
      ({ pkgs, ... }:
        let
          system = pkgs.stdenv.hostPlatform.system;
          nodePkgs = inputs.cardano-node.legacyPackages.${system}.cardanoNodePackages;
        in {
          services.cardano-node = {
            cardanoNodePackages = nodePkgs;
            package = nodePkgs.cardano-node;
          };
          services.cardano-submit-api.cardanoNodePackages = nodePkgs;
          services.cardano-tracer.cardanoNodePackages = nodePkgs;
          services.cardano-wallet.package =
            inputs.cardano-wallet.legacyPackages.${system}
              .hsPkgs.cardano-wallet-application.components.exes.cardano-wallet;

          environment.systemPackages = [
            nodePkgs.cardano-cli
          ];
        })
    ];
    nixCacheModule = {
      nix.settings = {
        extra-substituters = extraSubstituters;
        extra-trusted-public-keys = extraTrustedPublicKeys;
      };
    };
  in {
      nixosConfigurations = {
        schildpad = nixos.lib.nixosSystem {
          modules = [
            nixCacheModule
            ./nixos/hosts/schildpad.nix
          ];
        };
        loderunner = nixos.lib.nixosSystem {
          system = "x86_64-linux";
          modules = cardanoModules ++ [
            nixCacheModule
            inputs.disko.nixosModules.disko
            ./nixos/blockchains/cardano.nix
            ./nixos/hosts/loderunner.nix
          ];
        };
        hivemind = nixos.lib.nixosSystem {
          modules = cardanoModules ++ [
            nixCacheModule
            inputs.nix-bitcoin.nixosModules.default
            inputs.ethereum-nix.nixosModules.erigon
            ./nixos/blockchains
            ./nixos/hosts/hivemind.nix
            {
              nixpkgs = {
                config = {
                  cudaSupport = true;
                };
                overlays = [
                  inputs.ethereum-nix.overlays.default
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
