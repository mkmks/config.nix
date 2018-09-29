# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

with pkgs.lib;
with pkgs.haskell.lib;

let
  unstableTarball = fetchTarball https://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz;
  unstable = import unstableTarball {
           config = config.nixpkgs.config;
  };
in

{
  imports =
    [ # Include the results of the hardware scan.
      <nixos-hardware/lenovo/thinkpad/x1/6th-gen>
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [
    { name = "nixos";
      device = "/dev/nvme0n1p4";
      preLVM = true; }
  ];

  boot.cleanTmpDir = true;

  hardware.bluetooth.enable = true;
  hardware.opengl.driSupport32Bit = true;
  
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking = {
    firewall.enable = true;
    firewall.allowedTCPPorts = [ 22000 ];
    firewall.allowedUDPPorts = [ 21027 ];
    networkmanager.enable = true;
    extraHosts = ''
    127.0.0.1 googlesyndication.com
    127.0.0.1 tpc.googlesyndication.com
    127.0.0.1 doubleclick.net
    127.0.0.1 g.doubleclick.net
    127.0.0.1 googleads.g.doubleclick.net
    127.0.0.1 www.google-analytics.com
    127.0.0.1 ssl.google-analytics.com
    127.0.0.1 google-analytics.com
#    127.0.0.1 www.onclickmax.com
    '';
    hostName = "schildpad";
  };
  #networking.proxy.default = "http://127.0.0.1:8118";
  #networking.proxy.noProxy = "localhost, 127.0.0.0/8, ::1, rutracker.org, libgen.io";

  # Select internationalisation properties.
  i18n = {
  #   consoleFont = "Lat2-Terminus16";
     consoleUseXkbConfig = true;
     defaultLocale = "fr_FR.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # List packages installed in system profile.

  nixpkgs.config = {

    allowUnfree = true;

    packageOverrides = pkgs: rec {
      isabelle = pkgs.isabelle.override {
        java = pkgs.openjdk10;
      };
    };
  };
  
  environment = {
  
    sessionVariables = {
      _JAVA_AWT_WM_NONREPARENTING = "1";
      GTK_THEME = "Adwaita";
#      GTK_DATA_PREFIX = "${config.system.path}";
      GTK_PATH = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";
#      LIBGL_DISABLE_DRI3 = "1";
#      GDK_SCALE = "2";
    };
  
    systemPackages = with pkgs; with haskellPackages; [
      # desktop
      calibre      
      pkgs.dmenu
      evince
      feh
      firefox-bin
      goldendict
      mpv
      networkmanager_dmenu
      skypeforlinux
      spotify
      steam
      tdesktop
	    
      # development
      clang
      gdb
      gitAndTools.git
      gnumake
      haskellPackages.hlint
      manpages
      mercurial
      linuxPackages.perf
      sloccount
      haskellPackages.stylish-haskell
      haskellPackages.threadscope
      valgrind

      ((ghcWithPackages (self: with self;
      	 [
	   (dontCheck operational-alacarte)
#	   (dontCheck syntactic)
#	   (dontCheck imperative-edsl)

#	   ghc-mod
 	   alex
 	   cabal-install
 	   happy
	 ]
      )).override { withLLVM = true; })

      # music
      cuetools
      pkgs.flac
      id3v2
      pkgs.lame
      monkeysAudio
      mpc_cli
      ncmpcpp
      shntool
      wavpack
      
      # networking
      cadaver
      dnsutils
      gnupg
      inetutils
      lftp
      nmap      
      tor

      # provers
      haskellPackages.Agda
      AgdaStdlib
      coq
      isabelle

      # publishing
      briss
      djvu2pdf
      djvulibre
      dot2tex
      pkgs.exif
      ghostscript
      pkgs.gnuplot
      pkgs.graphviz
      haskellPackages.hakyll
      pkgs.imagemagick
      pdf2djvu
      pdftk
      python3Packages.pygments
      texlive.combined.scheme-full
      xfig
      	    	    
      # system
      acpi
      alsaUtils
      bc
      coreutils
      dos2unix
      dvtm
      exiftool
      fdupes
      file
      findutils
      lm_sensors
      mc
      nox
      oathToolkit
      pciutils
      powertop
      psmisc
      p7zip
      silver-searcher
      st
      tpacpi-bat
      udiskie
      unzip
      usbutils
      which
      xorg.xev
      xorg.xkill
    ];
  };
    
  fonts = {
#    fontconfig.antialias = false;

    fonts = with pkgs; [
      cm_unicode
      source-code-pro
      kochi-substitute
      wqy_zenhei
    ];
  };

  programs = {
    adb.enable = true;
    bash.enableCompletion = true;
    fish.enable = true;
    gnupg.agent.enable = true;
    gnupg.agent.enableSSHSupport = true;
    ssh.startAgent = false;
    slock.enable = true;
    tmux = {
         enable = true;         
#         newSession = true;
         terminal = "screen-256color";
         extraTmuxConf = ''
         set -g mouse on
         set -g prefix C-x
         bind-key C-x send-prefix
         unbind-key x
         bind-key k confirm-before -p "kill-pane #P? (y/n)" kill-pane

         set -g set-titles on
         set -g set-titles-string "[#I] #T"
         set -g status on
         set -g status-position top
         set -g status-left ""
         set -g status-right ""

         set -g destroy-unattached on
         new-session -At default
         '';
    };   
  };

  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
        if (subject.isInGroup('wheel') && subject.local) {
          return polkit.Result.YES;
        }
      });
  '';  

  # List services that you want to enable:

  services = {

    emacs = {
      enable = true;
      defaultEditor = true;
      #  package = unstable.emacs26.override { withGTK2 = false; withGTK3 = true; };
	    };

    illum.enable = true;

    mpd = {
      enable = true;
      group = "users";
      musicDirectory = "/home/viv/Music";
    };

    openssh.enable = false;

    printing = {
      enable = true;
      drivers = [ pkgs.cups-bjnp pkgs.gutenprint ];
    };

#    privoxy = {
#      enable = false;
#      enableEditActions = true;
#      actionsFiles = [ "match-all.action" "default.action" "/etc/privoxy/user.action" ];
#    };

    redshift = {
      enable = true;
      brightness = {
        day = "1.0";
        night = "0.7";
      };
      latitude = "57";
      longitude = "11";      
    };

    syncthing = {
      enable = true;
      dataDir = "/home/viv/.syncthing";
      user = "viv";
    };

    tor = {
      enable = true;
      client.enable = true;
    };

    transmission.enable = true;
    udisks2.enable = true;
        
    
    # Enable the X11 windowing system.
    xserver = {
      enable = true;

      dpi = 160;

      layout = "us(colemak),ru";
      xkbOptions = "grp:rctrl_toggle,compose:prsc,caps:ctrl_modifier";

      libinput = {
        accelSpeed = "0.4";
        clickMethod = "clickfinger";
        disableWhileTyping = true;
        naturalScrolling = true;
        scrollMethod = "twofinger";
        tapping = false;
      };

      desktopManager.default = "none";
      windowManager = {
        default = "dwm";
        session = singleton {
          name = "dwm";
          start = ''
            dwm &
            waitPID=$!
          '';
        };
      };
    };
  };

  systemd.services.powertop = {
      description = ''
        enables powertop's reccomended settings on boot
      '';
      wantedBy = [ "multi-user.target" ];

      path = with pkgs; [ powertop ];

      environment = {
        TERM = "dumb";
      };

      serviceConfig = {
        Type = "idle";
        User = "root";
        ExecStart = ''
          ${pkgs.powertop}/bin/powertop --auto-tune
        '';
      };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.viv = {
    description = "Nikita Frolov";
    extraGroups = [ "wheel" "transmission" "adbusers" ];
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.fish;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.09";

}
