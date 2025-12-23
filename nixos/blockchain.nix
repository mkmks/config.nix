{
  services = {
    bitcoind.mainnet = {
      enable = true;
      prune = 100000;
    };
    cardano-node = {
      enable = true;
      environment = "mainnet";
      hostAddr = "0.0.0.0";
    };
    cardano-submit-api = {
      enable = true;
      network = "mainnet";
      socketPath = "/run/cardano-node/node.socket";
    };
    #    cardano-wallet = {
    #      enable = true;
    #      port = 8100;
    #    };

  };
  users.users.viv = {
    extraGroups = [ "cardano-node" ];
  };
}
