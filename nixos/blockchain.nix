{ pkgs, ...}:

{
#  environment.systemPackages = [
#    pkgs.cardanoNodePackages.cardano-cli
#  ];
  
  networking.firewall = {
    allowedTCPPorts = [
      3001 # cardano
      8333 # bitcoin
    ];
  };
  
  services = {
    bitcoind.mainnet = {
      enable = true;
      prune = 100000;
    };
    cardano-node = {
      enable = false;
      environment = "mainnet";
      hostAddr = "0.0.0.0";
    };
    cardano-submit-api = {
      enable = false;
      network = "mainnet";
      socketPath = "/run/cardano-node/node.socket";
    };
    #    cardano-wallet = {
    #      enable = true;
    #      port = 8100;
    #    };
    ethereum = {
      geth.mainnet = {
        enable = false;
        openFirewall = true;
        args = {
          authrpc.jwtsecret = "/etc/geth_jwtsecret";
        };
      };
      lighthouse-beacon.mainnet = {
        enable = false;
        openFirewall = true;
        args = {
          execution-jwt = "/etc/geth_jwtsecret";
          checkpoint-sync-url = "https://mainnet-checkpoint-sync.stakely.io";
        };
      };
    };

  };
  users.users.viv = {
    extraGroups = [
      "bitcoind-mainnet"
      "cardano-node"
    ];
  };
}
