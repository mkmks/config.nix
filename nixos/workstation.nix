{ config, pkgs, ... }:

{
  imports = [
    ./.
 ];
  
  users.users.viv = {
    description = "Nikita Frolov";
    extraGroups = [
      "adbusers"
      "dialout"
      "ipfs"
      "pipewire"
      "syncthing"
      "transmission"
      "video"
    ];
    linger = true;
  };

  boot = {
    loader = {
      limine = {
        enable = true;
        maxGenerations = 5;
        panicOnChecksumMismatch = true;
	      secureBoot.enable = true;
      };
      efi.canTouchEfiVariables = true;
    };
    tmp.cleanOnBoot = true;
  };
  
  console.keyMap = "colemak";
  time.timeZone = "Europe/Paris";
  nixpkgs.config.allowUnfree = true;
  
  environment = {
    homeBinInPath = true;
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };
    systemPackages = with pkgs; [
      android-tools
      pciutils
      pcsc-tools
      usbutils
      v4l-utils
      swaylock
      adwaita-icon-theme
      sbctl
      nvtopPackages.full
    ];
  };

  hardware = {
    bluetooth.enable = true;
    graphics.enable = true;
    ledger.enable = true;
  };

  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [
        3000
        8082 # mitmproxy
        57621 # spotify connect
      ];
      allowedUDPPorts = [
        57621 # spotify connect
      ];
      extraCommands = ''
        iptables -A INPUT -p udp --sport 1900 --dport 1025:65535 -j ACCEPT -m comment --comment spotify
        iptables -A INPUT -p udp --sport 5353 --dport 1025:65535 -j ACCEPT -m comment --comment spotify
      '';
    };
    hosts = {
      "127.0.0.1" = [
        "local-mpc-node-1"
        "local-mpc-node-2"
        "local-mpc-node-3"
        "local-mpc-node-4"
      ];
      "127.0.0.11" = [
        "abcd.local-mpc-node-1"
        "abcd.local-mpc-node-2"
        "abcd.local-mpc-node-3"
        "abcd.local-mpc-node-4"
      ];
    };
  };
  
  programs = {
    dconf.enable = true;
    steam = {
      enable = true;
      gamescopeSession.enable = true;
      protontricks.enable = true;
      remotePlay.openFirewall = true;
    };
  };

  security = {
    pam.services = {
      login.enableGnomeKeyring = true;
      swaylock = {};
    };
    rtkit.enable = true;
  };
  
  services = {
    blueman.enable = true;
    dbus.packages = [ pkgs.dconf pkgs.gcr ];
    earlyoom.enable = true;
    fstrim.enable = true;
    fwupd.enable = true;
    gpm.enable = true;

    greetd = {
      enable = true;
      useTextGreeter = true;
      settings = {
        default_session = {
          command = "${pkgs.tuigreet}/bin/tuigreet --cmd niri-session";
        };
      };
    };

    k3s = {
      enable = false;
      extraFlags = [
        "--docker"
        "--write-kubeconfig-mode=640"
        "--write-kubeconfig-group=docker"
      ];
      extraKubeletConfig = {
        evictionHard = {
          "memory.available" = "100Mi";
          "nodefs.available" = "2%";
          "imagefs.available" = "2%";
        };
      };
    };

    kubo = {
      enable = true;
      settings = {
        Addresses.API = [
          "/ip4/127.0.0.1/tcp/5001"
        ];
        Swarm.DisableNatPortMap = false;
      };
    };
    mullvad-vpn.enable = true;

    pcscd = {
      enable = true;
      plugins = [
        pkgs.pcsc-cyberjack
      ];
    };

    mpd = {
      enable = true;
      settings = {
        audio_output = [
          {
            type = "pipewire";
            name = "System-wide PipeWire";
          }
        ];
        music_directory = "${config.services.syncthing.dataDir}/Music";        
      };
    };
    
    pipewire = {
      enable = true;
      audio.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      systemWide = true;

      extraConfig.pipewire = {
        "10-clock-rate" = {
          "context.properties" = {
            "default.clock.rate" = 44100;
          };
        };
      };

      wireplumber = {
        extraConfig = {
          "headset-autoconnect" = {
            "monitor.bluez.rules" = [
              {
                matches = [
                  {
                    "device.name" = "~bluez_card.*";
                  }
                ];
                actions = {
                  update-props = {
                    "bluez5.auto-connect" = "[ hfp_hf hsp_hs a2dp_sink ]";
                  };
                };
              }
            ];
          };
        };
      };

    };
    
    printing = {
      enable = true;
      drivers = [ pkgs.cups-bjnp pkgs.gutenprint ];
    };
        
    syncthing = {
      enable = true;
      openDefaultPorts = true;
    };
    
    tailscale = {
      enable = true;
      extraDaemonFlags = [
        "--encrypt-state=false"
      ];
    };
    transmission = {
      enable = true;
      package = pkgs.transmission_4;
      openPeerPorts = true;
    };
    udisks2.enable = true;    
  };

  # systemd.user.services.nvidia-apply-settings = {
  #   after = [ "graphical-session.target" ];
  #   partOf = [ "graphical-session.target" ];
  #   wantedBy = [ "graphical-session.target" ];
  #   description = "Apply nvidia-settings";
  #   serviceConfig = {
  #     Type = "simple";
  #     Restart = "on-failure";
  #     ExecStart = ''${config.hardware.nvidia.package.settings}/bin/nvidia-settings --load-config-only'';
  #   };
  # };

  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };

  users.users = {
    mpd.extraGroups = [
      "pipewire"
      "syncthing"
    ];
    syncthing.homeMode = "750";
  };
  
  xdg.portal = {
    enable = true;
    config.common.default = "*";
    wlr.enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  };
}
