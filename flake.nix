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
    daedalus.url = "github:input-output-hk/daedalus";
    cardano-node.url = "github:IntersectMBO/cardano-node";
    cardano-wallet.url = "github:cardano-foundation/cardano-wallet";
    niri.url = "github:sodiboo/niri-flake";
  };

  outputs = { self, nixos, home-manager, ... }@inputs:
    {
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
            ./nixos/workstation/desktop/hivemind
            {
              environment.systemPackages = [
                inputs.cardano-node.packages."x86_64-linux".cardano-cli
              ];
              nixpkgs = {
                config = {
                  cudaSupport = true;
                };
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
#                  inputs.nur.overlays.default
                  inputs.niri.overlays.niri
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
