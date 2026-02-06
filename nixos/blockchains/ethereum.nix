{ pkgs, ... }:

{
  services = {
    ethereum = {
      geth.mainnet = {
        enable = true;
        openFirewall = true;
        package = pkgs.ethereum-nix.geth;
        args = {
          authrpc.jwtsecret = "/etc/geth_jwtsecret";
          http = {
            enable = true;
            addr = "0.0.0.0";
            corsdomain = [ "localhost" ];
          };
        };
      };
      lighthouse-beacon.mainnet = {
        enable = true;
        openFirewall = true;
        package = pkgs.ethereum-nix.lighthouse;
        args = {
          execution-jwt = "/etc/geth_jwtsecret";
          checkpoint-sync-url = "https://mainnet-checkpoint-sync.stakely.io";
        };
      };
    };
  };
}
