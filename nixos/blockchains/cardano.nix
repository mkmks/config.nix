{ config, pkgs, ... }:

let
  cardanoNetwork = "mainnet";
in
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
      environment = cardanoNetwork;
      hostAddr = "0.0.0.0";
#      useNewTopology = true;
#      peerSnapshotFile = null;
      withUtxoHdLsmt = true;
      tracerSocketPathConnect = config.services.cardano-tracer.acceptAt;
    };
    cardano-submit-api = {
      enable = true;
      network = cardanoNetwork;
      socketPath = config.services.cardano-node.socketPath 0;
      config = config.services.cardano-submit-api.cardanoNodePackages.cardanoLib.defaultExplorerLogConfig
      // {
        TraceOptions = {};
      };
    };
    cardano-tracer = {
      enable = true;
      environment = cardanoNetwork;
    };
    cardano-db-sync = {
      enable = false;
      cluster = cardanoNetwork;
      socketPath = config.services.cardano-node.socketPath 0;
      postgres.database = config.services.cardano-db-sync.postgres.user;
      explorerConfig = {
        insert_options = {
          tx_out.value = "consumed";
          pool_stat = "enable";
        };
      } // config.services.cardano-db-sync.environment.dbSyncConfig;
    };
    cardano-wallet = {
      enable = false;
      package = pkgs.cardano-wallet;
      port = 8100;
    };
    blockfrost = {
      enable = false;
      settings.dbSync.database = config.services.cardano-db-sync.postgres.user;
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
      enable = false;
      ensureDatabases = [
        config.services.cardano-db-sync.postgres.user
      ];
      ensureUsers = [
        {
          name = config.services.cardano-db-sync.postgres.user;
          ensureDBOwnership = true;
        }
      ];
      authentication = ''
        local ${config.services.cardano-db-sync.postgres.user} ${config.services.cardano-db-sync.postgres.user} peer map=cdbsync
        host ${config.services.cardano-db-sync.postgres.user} ${config.services.cardano-db-sync.postgres.user} samehost trust
      '';
      identMap = ''
        cdbsync cardano-db-sync ${config.services.cardano-db-sync.postgres.user}
      '';
    };
  };

  # needed for cexplorer-mini
  # virtualisation.oci-containers.containers = {
  #   graphql-engine = {
  #     image = "hasura/graphql-engine:v2.25.1";
  #     extraOptions = [
  #       "--network=host"
  #     ];
  #     environment = {
  #       HASURA_GRAPHQL_DATABASE_URL = "postgres://${config.services.cardano-db-sync.postgres.user}:password@localhost:5432/${config.services.cardano-db-sync.postgres.user}";
  #       HASURA_GRAPHQL_SERVER_PORT = "3100";
  #       HASURA_GRAPHQL_ENABLE_CONSOLE = "true";
  #       HASURA_GRAPHQL_DEV_MODE = "false";
  #     };
  #   };
  # };
  
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
    # podman-graphql-engine = {
    #   after = [
    #     "postgresql.target"
    #   ];
    #   requires = [
    #     "postgresql.target"
    #   ];
    # };
  };
  users.users.viv = {
    extraGroups = [
      "cardano-node"
    ];
  };
}
