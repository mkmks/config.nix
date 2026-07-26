{ pkgs, ... }:

{
  services = {
    ethereum = {
      erigon.mainnet = {
        enable = false;
        openFirewall = true;
        package = pkgs.ethereum-nix.erigon;
        # args = {
        #   http = {
        #     enable = true;
        #     addr = "0.0.0.0";
        #     api = [
        #       "eth"
        #       "net"
        #       "web3"
        #       "erigon"
        #       "trace"
        #       "ots"
        #       "debug"
        #       "txpool"
        #     ];
        #     corsdomain = [
        #       "*"
        #     ];
        #   };
        #   prune.mode = "archive";
        #   ws.enable = true;
        # };
        extraArgs = [
          "--caplin.discovery.port=40000"
          "--caplin.discovery.tcpport=40010"
          "--rpc.returndata.limit=1000000"
        ];
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
