{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # cardano-cli
  ];
  
  networking.firewall = {
    allowedTCPPorts = [
      3001 # node p2p
    ];
  };

  services = {
    cardano-node = {
      enable = true;
      environment = "mainnet";
      hostAddr = "0.0.0.0";
      useNewTopology = true;
      peerSnapshotFile = null;
      tracerSocketPathConnect = "/run/cardano-tracer/tracer.socket";
    };
    cardano-submit-api = {
      enable = true;
      config = config.services.cardano-submit-api.cardanoNodePackages.cardanoLib.defaultExplorerLogConfig // {
        TraceOptions = {};
      };
      network = "mainnet";
      socketPath = "/run/cardano-node/node.socket";
    };
    cardano-tracer = {
      enable = true;
      environment = "mainnet";
    };
    cardano-wallet = {
      enable = true;
      package = pkgs.cardano-wallet;
      port = 8100;
    };
    cardano-db-sync = {
      enable = false;
    };
  };

  # dependencies
  services = {
    # needed for eternl
    nginx = {
      enable = true;
      virtualHosts.cardano-submit-api-cors = {
        listen = [
          {
            addr = "127.0.0.1";
            port = 8091;
          }
        ];
        locations = {
          "/" = {
            proxyPass = "http://127.0.0.1:8090";
            extraConfig = ''
              add_header "Access-Control-Allow-Origin" "https://eternl.io";
              add_header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS";
              add_header "Access-Control-Allow-Headers" "Content-Type, Authorization";
              add_header "Access-Control-Allow-Credentials" "true";
            '';
          };
        };
      };
    };
    # needed for cardano-db-sync
    postgresql = {
      enable = true;
      ensureDatabases = [
        "cdbsync"
      ];
    };
  };

  systemd.services = {
    cardano-node.serviceConfig = {
      UMask = "0002";
    };
    cardano-submit-api.serviceConfig = {
      Group = "cardano-node";
    };
    cardano-wallet.serviceConfig = {
      Group = "cardano-node";
    };
  };
  users.users.viv = {
    extraGroups = [
      "cardano-node"
    ];
  };
}
