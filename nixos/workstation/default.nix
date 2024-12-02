{ pkgs, ... }:

{
  imports = [
    ../.
  ];
  
  users.users.viv = {
    description = "Nikita Frolov";
    extraGroups = [ "wheel" "transmission" "adbusers" "dialout" "docker" "video" ];
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.fish;
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
      pciutils
      pcsctools
      usbutils
      v4l-utils
      swaylock
    ];
  };

  hardware = {
    bluetooth.enable = true;
    ledger.enable = true;
  };

  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22000 57621 ];
      allowedUDPPorts = [ 21027 57621 ];
      extraCommands = ''
        iptables -A INPUT -p udp --sport 1900 --dport 1025:65535 -j ACCEPT -m comment --comment spotify
        iptables -A INPUT -p udp --sport 5353 --dport 1025:65535 -j ACCEPT -m comment --comment spotify
      '';
    };
    
    extraHosts = ''
      127.0.0.1 googlesyndication.com
      127.0.0.1 tpc.googlesyndication.com
      127.0.0.1 doubleclick.net
      127.0.0.1 g.doubleclick.net
      127.0.0.1 googleads.g.doubleclick.net
      127.0.0.1 www.google-analytics.com
      127.0.0.1 ssl.google-analytics.com
      127.0.0.1 google-analytics.com
      # 127.0.0.1 www.onclickmax.com
    '';
  };
  
  programs = {
    adb.enable = true;
    dconf.enable = true;
    fish.enable = true;
    light.enable = true;
    steam.enable = true;
  };

  security = {
    pam.services = {
      login.enableGnomeKeyring = true;
      swaylock = {};
    };
    rtkit.enable = true;
  };
  
  services = {

    dbus.packages = [ pkgs.dconf pkgs.gcr ];
    gpm.enable = true;

    greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet --cmd sway";
        };
      };
    };

    mullvad-vpn.enable = true;

    pcscd = {
      enable = true;
      plugins = [
        pkgs.pcsc-cyberjack
      ];
    };
    
    pipewire = {
      enable = true;
      audio.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;

      extraConfig.pipewire = {
        "10-clock-rate" = {
          "context.properties" = {
            "default.clock.rate" = 44100;
          };
        };
      };

      wireplumber = {
        configPackages = [
          # (pkgs.writeTextDir
          #   "share/wireplumber/wireplumber.conf.d/10-bluez.conf" ''
          #     monitor.bluez.properties = {
          #       bluez5.roles = [ a2dp_sink a2dp_source bap_sink bap_source hsp_hs hsp_ag hfp_hf hfp_ag ]
          #       bluez5.codecs = [ sbc sbc_xq aac ]
          #       bluez5.enable-sbc-xq = true
          #       bluez5.enable-msbc = true
          #       bluez5.enable-hw-volume = true
          #       bluez5.hfphsp-backend = "native"
          #     }
          # ''
          # )
        ];
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

    transmission.enable = true;    
    udisks2.enable = true;    
  };

  virtualisation.docker.enable = true;
  
  xdg.portal = {
    enable = true;
    config.common.default = "*";
    wlr.enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  };
}
