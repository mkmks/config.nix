{ pkgs, ... }:

{
  console.keyMap = "colemak";
  time.timeZone = "Europe/Paris";
  nixpkgs.config.allowUnfree = true;
  
  environment = {
    etc = {
      "pipewire/pipewire.conf.d/99-custom.conf".text = ''
        context.properties = {
          default.clock.allowed-rates = [ 44100 48000 ]
        }
      '';
            
	    "wireplumber/bluetooth.lua.d/51-bluez-config.lua".text = ''
    		bluez_monitor.properties = {
    			["bluez5.enable-sbc-xq"] = true,
    			["bluez5.enable-msbc"] = true,
		    	["bluez5.enable-hw-volume"] = true,
    			["bluez5.headset-roles"] = "[ hsp_hs hsp_ag hfp_hf hfp_ag ]",
        }

        bluez_monitor.rules = {
          {
            matches = {
              {
                { "device.name", "matches", "bluez_card.*" },
              },
            },
            apply_properties = {
              ["bluez5.auto-connect"]  = "[ hfp_hf hsp_hs a2dp_sink ]",
            },
         },
        }
    	'';
    };

    homeBinInPath = true;
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };
    systemPackages = with pkgs; [
      pciutils
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

    nameservers = [ "1.1.1.1" ];

    # proxy.default = "http://127.0.0.1:8118";
    # proxy.noProxy = "localhost, 127.0.0.0/8, ::1, rutracker.org, libgen.io";
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
          command = "${pkgs.greetd.greetd}/bin/agreety --cmd sway";
        };
      };
    };

    pipewire = {
      enable = true;
      audio.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
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
    wlr.enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  };
}
