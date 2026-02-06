{ ... }:

{
  networking.firewall = {
    allowedTCPPorts = [
      50001 # electrum/fulcrum
      60845 # mempool
    ];
  };
  
  nix-bitcoin = {
    generateSecrets = true;
    onionServices = {
      bitcoind = {
        enable = true;
        public = true;
      };
    };
  };

  services = {
    bitcoind = {
      enable = true;
      listen = true;
      tor = {
        proxy = true;
        enforce = true;
      };
      extraConfig = ''
        onlynet=onion
      '';
    };
    fulcrum = {
      enable = true;
      address = "0.0.0.0";
    };
    mempool = {
      enable = true;
      electrumServer = "fulcrum";
      frontend.address = "0.0.0.0";
      tor = {
        proxy = true;
        enforce = true;        
      };
    };
  };

  # dependencies
  services = {
    # needed for mempool
    mysql.settings.mysqld.bind-address = "127.0.0.1";
  };

  users.users.viv = {
    extraGroups = [
      "bitcoin"
    ];
  };
}
