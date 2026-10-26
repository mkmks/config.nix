{ pkgs, ... }:

{
  # Erigon asked for this
  boot.kernel.sysctl."vm.max_map_count" = 16777216;
  networking.firewall = {
    allowedTCPPorts = [
      40010 # Caplin TCP
    ];
    allowedUDPPorts = [
      40000 # Caplin discovery
      40010 # Caplin QUIC
    ];
  };
  
  services = {
    ethereum = {
      erigon.mainnet = {
        enable = true;
        openFirewall = true;
        package = pkgs.ethereum-nix.erigon;
        settings = {
          "prune.mode" = "archive";

          http = true;
          "http.addr" = "0.0.0.0";
          "http.api" = [
            "eth"
            "net"
            "web3"
            "erigon"
            "trace"
            "ots"
            "debug"
            "txpool"
          ];
          "http.corsdomain" = "*";
          
          "caplin.discovery.port" = "40000";
          "caplin.discovery.tcpport" = "40010";
          "caplin.discovery.quicport" = "40010";
          "caplin.nat" = "upnp";
          "nat" = "any";
          "rpc.returndata.limit" = "1000000";
        };
      };
    };
  };

  virtualisation.oci-containers.containers = {
    otterscan = {
      image = "otterscan/otterscan:v2.11.0";
      ports = [
        "5100:80"
      ];
    };
  };

  systemd.services.podman-otterscan = {
    after = [
      "erigon-mainnet.service"
    ];
    requires = [
      "erigon-mainnet.service"
    ];
  };
}
