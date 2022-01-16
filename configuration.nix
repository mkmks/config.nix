{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot = {
    cleanTmpDir = true;

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  console.keyMap = "colemak";

  hardware = {
    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
    ledger.enable = true;
    
    opengl = {
      enable = true;
      driSupport = true;
      package = (pkgs.mesa.override {
        galliumDrivers = [ "nouveau" "virgl" "swrast" "iris" ];
      }).drivers;
      extraPackages = with pkgs; [ vaapiIntel vaapiVdpau libvdpau-va-gl ];  
    };
    
    video.hidpi.enable = true;
  };

  netkit.xmm7360 = {
    enable = false;
    autoStart = true;
    config = {
      apn = "orange";
      nodefaultroute = false;
      noresolv = true;
    };
    package = pkgs.netkit.xmm7360-pci_latest;
  };
  
  powerManagement.powertop.enable = true;
  
  networking = {
    hostName = "schildpad";

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

    wireless.iwd.enable = true;

    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
  };

  time.timeZone = "Europe/Paris";

  nix = {
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    binaryCaches = [
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
    ];
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  environment.homeBinInPath = true;
  environment.systemPackages = with pkgs; [
      acpi
      iw
      lm_sensors
      ntfs3g
      pciutils
      powertop
      swaylock
      usbutils
  ];
  environment.variables = {
    MESA_LOADER_DRIVER_OVERRIDE = "iris";
  };
    
  nixpkgs.config.allowUnfree = true;
  
  programs = {
    adb.enable = true;
    dconf.enable = true;
    java = {
      enable = true;
      package = pkgs.jdk11;
    };
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

  virtualisation.docker.enable = true;
  
  services = {

    # hardware
    
    fstrim.enable = true;
    fwupd.enable = true;
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

    throttled.enable = true;
    tlp = {
      enable = pkgs.lib.mkDefault true;
      settings = {
        START_CHARGE_THRESH_BAT0 = 75;
        STOP_CHARGE_THRESH_BAT0 = 80;
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
        ENERGY_PERF_POLICY_ON_BAT = "powersave";
      };
    };

    # network

    transmission.enable = true;
    
    # desktop

    dbus.packages = [ pkgs.gnome3.dconf pkgs.gcr ];
    udisks2.enable = true;
  };

  users.users.viv = {
    description = "Nikita Frolov";
    extraGroups = [ "wheel" "transmission" "adbusers" "dialout" "docker" "video" ];
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.fish;
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk xdg-desktop-portal-wlr ];
    gtkUsePortal = true;
  };
  
  system.stateVersion = "21.11";
}
