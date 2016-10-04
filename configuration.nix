# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

with pkgs.lib;

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
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # List packages installed in system profile.

  nixpkgs.config = {
  
    allowUnfree = true;

    packageOverrides = pkgs: {
    
        # emacs = pkgs.lib.overrideDerivation (pkgs.emacs25.override {
	#     withX = true;
        #     withGTK3 = true;
	#     withGTK2 = false;
	#     # let Emacs view images
	#     imagemagick = pkgs.imagemagickBig;
	    
        #   }) (attrs: {
	#     nativeBuildInputs = attrs.nativeBuildInputs ++ [ pkgs.webkitgtk24x ];	  
	#     configureFlags = attrs.configureFlags ++ [ "--with-xwidgets=yes" ];
	#     # Emacs daemon is part of user session, use emacsclient
	#     postInstall = attrs.postInstall + ''
	#       sed -i 's/Exec=emacs/Exec=emacsclient -c -n/' $out/share/applications/emacs.desktop
	#     '';
	#   });

	yi = pkgs.yi.override {
      	  haskellPackages = pkgs.haskell.packages.ghc7101;
          extraPackages = p: with p; [  ];
        };
    };

    zathura.useMupdf = false;
  };
  
  environment = {
    sessionVariables = {
      GTK_THEME = "Adwaita";
      GTK_DATA_PREFIX = "${config.system.path}";
      GTK_PATH = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";
    };
  
    systemPackages = with pkgs; with haskellPackages; [
      # desktop
      chromium
      dmenu
      mpv
      slock
      steam
      tdesktop
      zathura
	    
      # development
      androidenv.platformTools
      clang
      coq
      gdb
      gitAndTools.git
      gnumake
      linuxPackages.perf
      sloccount
      valgrind

      # haskell
      ((ghcWithPackages (self: with self;
      	 [
	   #syntactic imperative-edsl
	 ]
      )).override { withLLVM = true; })
      (Agda // { doHaddock = false; })
      AgdaStdlib
      cabal-install
      ghc-mod
      hlint
      threadscope

      # music
      cuetools
      flac
      lame
      monkeysAudio
      mpc_cli
      ncmpcpp
      shntool
      wavpack
      
      # networking
      cadaver
      gnupg
      inetutils
      isync
      mu
      nmap
      pass
      tor
      
      # publishing
      briss
      djvulibre
      pkgs.exif
      ghostscript
      gnuplot
      graphviz
      imagemagick
      texlive.combined.scheme-full
      xfig
      	    	    
      # system
      bashmount
      coreutils
      dos2unix
      dvtm
      fdupes
      file
      findutils
      mc
      nix-repl
      nox
      powertop
      psmisc
      p7zip
      silver-searcher
      st
      tmux
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

  programs.bash.enableCompletion = true;
  programs.ssh.startAgent = false;

  security.setuidPrograms = [ "slock" ];
    
  # List services that you want to enable:

  services = {

    emacs = {
      enable = true;
      defaultEditor = true;
      package = pkgs.emacs25;
    };
  
    mpd = {
      enable = true;
      group = "users";
      musicDirectory = "/home/viv/Music";
    };

    openssh.enable = false;
    printing.enable = true;

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
    };
  };

  systemd.user =  {
  
    services = {
    
      gpg-agent = {
        description = "GnuPG agent";

	serviceConfig = {
	  Type      = "forking";
	  ExecStart = "${pkgs.gnupg}/bin/gpg-agent -q  --daemon --enable-ssh-support";
	  Restart   = "on-abort";
	};

	wantedBy = [ "default.target" ];
      };

      mbsync = {
        description = "Mailbox syncronization";

	serviceConfig = {
	  Type      = "oneshot";
	  ExecStart = "${pkgs.isync}/bin/mbsync -aq";
	};

	path = [ pkgs.gawk pkgs.gnupg ];

	# environment = {
	#   DISPLAY = ":0.0";
	# };

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
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
