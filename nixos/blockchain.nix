{ config, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
#    cardano-cli
    monero-cli
    xmrig-cuda
  ];
  
  networking.firewall = {
    allowedTCPPorts = [
      3001  # cardano
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
    # Bitcoin
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
    fulcrum.enable = true;
    mempool = {
      enable = true;
      electrumServer = "fulcrum";
      tor = {
        proxy = true;
        enforce = true;        
      };
    };
    # Cardano
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
    # Ethereum
    ethereum = {
      geth.mainnet = {
        enable = true;
        openFirewall = true;
        package = pkgs.ethereum-nix.geth;
        args = {
          authrpc.jwtsecret = "/etc/geth_jwtsecret";
          http = {
            enable = true;
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
    # Monero
    monero = {
      enable = true;
      # define ANONYMOUS_INBOUND and MINING_ADDRESS here
      environmentFile = "${config.services.monero.dataDir}/monerod.env";
      extraConfig = ''
        anonymous-inbound=$ANONYMOUS_INBOUND:18084,127.0.0.1:18084,12
        tx-proxy=tor,127.0.0.1:9050,12,disable_noise
        zmq-pub=tcp://127.0.0.1:18083
      '';
      mining = {
        enable = false;
        address = "$MINING_ADDRESS";
      };
    };
    # module not merged to nixpkgs yet
    # p2pool = {
    #   enable = false;
    #   environmentFile = "${config.services.p2pool.dataDir}/p2pool.env";
    # };
    xmrig = {
      enable = false;
      settings = {
        autosave = true;
        cuda = {
          enabled = true;
          loader = "${pkgs.xmrig-cuda}/lib/libxmrig-cuda.so";
        };
        pools = [
          {
            url = "127.0.0.1:3333";
            keepalive = true;
          }
        ];
      };
    };
  };

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
    # needed for bitcoind/monerod
    tor = {
      enable = true;
      client.enable = true;
      relay.onionServices = {
        monero = config.nix-bitcoin.lib.mkOnionService {
          port = 18084;
          target.port = 18084;
          target.addr = "127.0.0.1";
        };
      };
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
      "bitcoin"
      "cardano-node"
    ];
  };
}
