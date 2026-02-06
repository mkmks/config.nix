{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    monero-cli
    xmrig-cuda
  ];

  networking.firewall = {
    allowedTCPPorts = [
      18081 # RPC
    ];
  };

  services = {
    monero = {
      enable = true;
      rpc.address = "0.0.0.0";
      # define ANONYMOUS_INBOUND and MINING_ADDRESS here
      environmentFile = "${config.services.monero.dataDir}/monerod.env";
      extraConfig = ''
        anonymous-inbound=$ANONYMOUS_INBOUND:18084,127.0.0.1:18084,12
        tx-proxy=tor,127.0.0.1:9050,12,disable_noise
        zmq-pub=tcp://127.0.0.1:18083
        confirm-external-bind=1
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

  # dependencies
  services = {
    tor.relay.onionServices = {
      monero = config.nix-bitcoin.lib.mkOnionService {
        port = 18084;
        target.port = 18084;
        target.addr = "127.0.0.1";
      };
    };
  };
}
