{pkgs, ...}:

{
  imports = [
    ../.
    ../../blockchain.nix
  ];

  networking.hostName = "loderunner";
  time.timeZone = "Europe/Paris";

  users.users.viv = {
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK0tapR+Dagn7C6mE/w7qcbyoGTQxKgG6kGAxWoFrmf0 nf@mkmks.org"
    ];
  };
  
  boot.loader.grub.enable = true;

  disko.devices = {
    disk = {
      disk1 = {
        type = "disk";
        device = "/dev/sda";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              type = "EF02";
              size = "1M";
            };
            root = {
              size = "100%";
              name = "root";
            };
          };
        };
      };
      disk2 = {
        type = "disk";
        device = "/dev/sdb";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              type = "EF02";
              size = "1M";
            };
            root = {
              size = "100%";
              content = {
                type = "btrfs";
                extraArgs = [
                  "-d raid1"
                  "/dev/disk/by-partlabel/disk-disk1-root"
                ];
                subvolumes = {
                  "/rootfs" = {
                    mountpoint = "/";
                  };
                  "/nix" = {
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                    mountpoint = "/nix";
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  networking.firewall = {
    allowedTCPPorts = [
      3001
      8333
      30303
    ];
    allowedUDPPorts = [
      30303
    ];
  };

  security.sudo.wheelNeedsPassword = false;

  services = {
    geth.mainnet = {
      enable = false;
      authrpc = {
        enable = true;
        jwtsecret = "/etc/geth_jwtsecret";
      };
    };
    lighthouse = {
      beacon = {
        enable = false;
        execution.jwtPath = "/etc/geth_jwtsecret";
        extraArgs = "--checkpoint-sync-url https://mainnet-checkpoint-sync.stakely.io";
      };
    };
    
    fail2ban.enable = true;
    openssh.enable = true;
  };

  system.stateVersion = "24.05";
}
