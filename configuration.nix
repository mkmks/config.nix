# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

with pkgs.lib;
with pkgs.haskell.lib;

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
  };
  #networking.proxy.default = "http://127.0.0.1:8118";
  #networking.proxy.noProxy = "localhost, 127.0.0.0/8, ::1, rutracker.org, libgen.io";

  # Select internationalisation properties.
  i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
     defaultLocale = "en_GB.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # List packages installed in system profile.

  nixpkgs.config = {

    allowUnfree = true;

    packageOverrides = pkgs: {

    };

    zathura.useMupdf = false;
  };
  
  environment = {
  
    sessionVariables = {
      GTK_THEME = "Adwaita";
#      GTK_DATA_PREFIX = "${config.system.path}";
      GTK_PATH = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";
      LIBGL_DISABLE_DRI3 = "1";
    };
  
    systemPackages = with pkgs; with haskellPackages; [
      # desktop
      chromium
      pkgs.dmenu
      mpv
      steam
      tdesktop
      zathura
	    
      # development
#      androidenv.platformTools
      clang
      coq
      gdb
      gitAndTools.git
      gnumake
#      linuxPackages.perf
      manpages
      sloccount
      valgrind

      ((ghcWithPackages (self: with self;
      	 [
	   (dontCheck operational-alacarte)
#	   (dontCheck syntactic)
#	   (dontCheck imperative-edsl)

#      	   (dontHaddock Agda)
	   ghc-mod
	   hakyll
 	   alex
 	   cabal-install
 	   happy
 	   hlint
 	   stylish-haskell	
 	   threadscope	   
	 ]
      )).override { withLLVM = true; })
#      AgdaStdlib

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
      isync
      lftp
      mu
      nmap      
      tor
      
      # publishing
      briss      
      djvu2pdf
      djvulibre
      dot2tex
      pkgs.exif
      ghostscript
      pkgs.gnuplot
      graphviz
      pkgs.imagemagick
      pdftk
      texlive.combined.scheme-full
      xfig
      	    	    
      # system
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
      pass
      powertop
      psmisc
      p7zip
      sdcv
      silver-searcher
      st
      tmux
      udiskie
      usbutils
      which
      xorg.xbacklight
      xorg.xkill
    ];
  };
    
  fonts = {
    fonts = with pkgs; [
      kochi-substitute
      wqy_zenhei
    ];
  };

  programs = {
    adb.enable = true;
    bash.enableCompletion = true;
    browserpass.enable = true;
    gnupg.agent.enable = true;
    gnupg.agent.enableSSHSupport = true;
    ssh.startAgent = false;
    slock.enable = true;  
  };
  
  # List services that you want to enable:

  services = {

    emacs = {
      enable = true;
      defaultEditor = true;
      package = pkgs.emacs25.override { withGTK2 = false; withGTK3 = true; };
    };
  
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
    
    telepathy.enable = false;

    tor = {
      enable = true;
      client.enable = true;
    };

    transmission.enable = true;
        
    udisks2.enable = true;
        
    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us,ru";

      xkbOptions = "ctrl:nocaps,grp:shifts_toggle,compose:rwin";

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

      displayManager.gdm.enable = false;
      displayManager.slim.enable = true;
      
      desktopManager = {
        default = "none";
        gnome3.enable = false;
	xterm.enable  = false;
      };

      windowManager = {
        xmonad.enable = true;
        xmonad.enableContribAndExtras = true;
        default = "xmonad";
      };

      xautolock = {
        enable = true;
	locker = "slock";
	time = 5;
      };
    };
  };

  systemd.user =  {
  
    services = {
    
      mbsync = {
        description = "Mailbox syncronization";

	serviceConfig = {
	  Type      = "oneshot";
	  ExecStart = "${pkgs.isync}/bin/mbsync -aq";
	};

	path = [ pkgs.gawk pkgs.gnupg ];

	after       = [ "network-online.target" "gpg-agent.service" ];
        wantedBy    = [ "default.target" ];
      };
    };

    timers = {
      mbsync = {
        description = "Mailbox syncronization";
      
        timerConfig = {
          OnCalendar = "*:0/5";
          Persistent = "true";
        };
        wantedBy = [ "timers.target" ];
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.viv = {
    description = "Nikita Frolov";
    extraGroups = [ "wheel" "transmission" "adbusers" ];
    isNormalUser = true;
    uid = 1000;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

}
