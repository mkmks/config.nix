{ pkgs, ... }:

{
  console.keyMap = "colemak";
  time.timeZone = "Europe/Paris";
  nixpkgs.config.allowUnfree = true;
  
  environment = {
    homeBinInPath = true;
    systemPackages = with pkgs; [
      pciutils
      usbutils      
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

    nameservers = [ "1.1.1.1" ];

    # proxy.default = "http://127.0.0.1:8118";
    # proxy.noProxy = "localhost, 127.0.0.0/8, ::1, rutracker.org, libgen.io";
  };
  
  programs = {
    adb.enable = true;
    dconf.enable = true;
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

    dbus.packages = [ pkgs.gnome3.dconf pkgs.gcr ];
    gpm.enable = true;

    greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.greetd}/bin/agreety --cmd sway";
        };
      };
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;

      config.pipewire = {
        "context.properties" = {
          "default.clock.allowed-rates" = [ 44100 48000 ];
        };
      };
        
      media-session.config.bluez-monitor.rules = [
        {
          # Matches all cards
          matches = [ { "device.name" = "~bluez_card.*"; } ];
          actions = {
            "update-props" = {
              "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
              # mSBC is not expected to work on all headset + adapter combinations.
              "bluez5.msbc-support" = true;
              # SBC-XQ is not expected to work on all headset + adapter combinations.
              "bluez5.sbc-xq-support" = true;
              # automatically switch between A2DP and headset
              bluez5.autoswitch-profile = true;
            };
          };
        }
        {
          matches = [
            # Matches all sources
            { "node.name" = "~bluez_input.*"; }
            # Matches all outputs
            { "node.name" = "~bluez_output.*"; }
          ];
          actions = {
            "node.pause-on-idle" = false;
          };
        }
      ];      
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
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk xdg-desktop-portal-wlr ];
    gtkUsePortal = true;
  };
}
