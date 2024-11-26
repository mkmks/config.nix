{
  description = "My machines and home directories";

  inputs = {
    nixos.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/master";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixos";      
    };
    nur.url = "github:nix-community/NUR";    
    #    emacs.url = "github:nix-community/emacs-overlay";
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixos";
    };
    daedalus.url = "github:input-output-hk/daedalus";
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
            ./nixos/server/loderunner
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
            ./home
            {
              home.packages = [
                # inputs.daedalus.defaultPackage.${system}
              ];
              nixpkgs = {
                config.allowUnfree = true;
                overlays = [
                  # emacs.overlay
                  inputs.nur.overlay
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
