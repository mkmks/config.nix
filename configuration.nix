# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

with pkgs.lib;
with pkgs.haskell.lib;

let
  unstableTarball =
    fetchTarball
      https://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz;
  unstable = import unstableTarball {
           config = config.nixpkgs.config;
  };
in

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [
    { name = "nixos";
      device = "/dev/sda2";
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

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking = {
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

    zathura.useMupdf = false;
  };
  
  environment = {
  
    sessionVariables = {
      GTK_THEME = "Adwaita";
#      GTK_DATA_PREFIX = "${config.system.path}";
      GTK_PATH = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";
#      LIBGL_DISABLE_DRI3 = "1";
    };
  
    systemPackages = with pkgs; with haskellPackages; [
      # desktop
      calibre      
      pkgs.dmenu
      feh
      firefox-bin
      goldendict
      mpv
#      steam
      unstable.tdesktop
      zathura
	    
      # development
      clang
      gdb
      gitAndTools.git
      gnumake
      linuxPackages.perf
      manpages
      sloccount
      valgrind

      ((ghcWithPackages (self: with self;
      	 [
	   (dontCheck operational-alacarte)
#	   (dontCheck syntactic)
#	   (dontCheck imperative-edsl)

#      	   (dontHaddock Agda)
	   Agda
#	   ghc-mod
	   hakyll
 	   alex
 	   cabal-install
 	   happy
 	   hlint
 	   stylish-haskell	
 	   threadscope	   
	 ]
      )).override { withLLVM = true; })
      AgdaStdlib

      (python36.withPackages (ps: with ps; [pip pygments setuptools]))

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
      pkgs.imagemagick
      pdf2djvu
      pdftk
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
      mc      
      nix-repl
      nox
      oathToolkit
      powertop
      psmisc
      p7zip
      sdcv
      silver-searcher
      udiskie
      unzip
      usbutils
      which
      # xcape
      # xorg.xev
      # xorg.xkill
      # xorg.xmodmap
    ];
  };
    
  fonts = {
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
  
  # List services that you want to enable:

  services = {

    emacs = {
      enable = true;
      defaultEditor = true;
        package = unstable.emacs26.override { withGTK2 = false; withGTK3 = true; };
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

    syncthing = {
      enable = true;
      dataDir = "/home/viv/.syncthing";
      user = "viv";
    };

    thermald.enable = true;

    tor = {
      enable = true;
      client.enable = true;
    };

    transmission.enable = true;
    udisks2.enable = true;
        
    
    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us(colemak),ru";

      xkbOptions = "grp:shifts_toggle,compose:rwin,caps:ctrl_modifier";

      synaptics = {
        enable = true;
        dev = "/dev/input/event*";
        twoFingerScroll = true;
        buttonsMap = [ 1 3 2 ];
        maxSpeed = "0.8";
        minSpeed = "0.3";
        accelFactor = "0.075";
      };

      multitouch = {
        enable = true;
        invertScroll = true;
        tapButtons = false;
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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.viv = {
    description = "Nikita Frolov";
    extraGroups = [ "wheel" "transmission" "adbusers" ];
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.fish;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";

}
