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

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
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
    
        emacs = pkgs.lib.overrideDerivation (pkgs.emacs25pre.override {
	    withX = true;
            withGTK3 = true;
	    withGTK2 = false;
	    # let Emacs view images
	    imagemagick = pkgs.imagemagickBig;
	    
          }) (attrs: {
	    nativeBuildInputs = attrs.nativeBuildInputs ++ [ pkgs.webkitgtk24x ];	  
	    configureFlags = attrs.configureFlags ++ [ "--with-xwidgets=yes" ];
	    # Emacs daemon is part of user session, use emacsclient
	    postInstall = attrs.postInstall + ''
	      sed -i 's/Exec=emacs/Exec=emacsclient -c -n/' $out/share/applications/emacs.desktop
	    '';
	  });

	firefox = (pkgs.wrapFirefox (pkgs.firefox-unwrapped.override {
	    enableGTK3 = true;
	    enableOfficialBranding = true;
	  }) {});
	    
	yi = pkgs.yi.override {
      	  haskellPackages = pkgs.haskell.packages.ghc7101;
          extraPackages = p: with p; [  ];
        };

	# bittorrentSync20 = pkgs.lib.overrideDerivation pkgs.bittorrentSync20 (attrs: {
	#   version = "2.3.6";
	#   src = pkgs.fetchurl {
	#     url = "https://download-cdn.getsync.com/stable/linux-x64/BitTorrent-Sync_x64.tar.gz";
	#     sha256 = "01yrligi61gxcixh7z6gi427ga0sx97wnmkv08p9ykd4b90hvj7s";
	#   };
	# });
    };

    zathura.useMupdf = false;
  };
  
  environment.sessionVariables = {
    GTK_THEME = "Adwaita";
    GTK_DATA_PREFIX = "${config.system.path}";
    GTK_PATH = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";
  };
  
  environment.systemPackages = with pkgs; [
    androidenv.platformTools
    coreutils
    coq
    djvulibre
    dvtm
    emacs
    file
    findutils
    firefox
    ghostscript
    git
    gnupg
    gnuplot
    graphviz
    imagemagick
    inetutils
    isync
    mpc_cli
    mpv
    mu
    nix-repl
    nmap
    nox
    powertop
    psmisc
    pwgen
    p7zip
    silver-searcher
    sloccount
    slock
    st
    steam
    texlive.combined.scheme-full
    tmux
    tor
    transmission
    xorg.xbacklight
    xfig
    zathura
  ];

  environment.gnome3.excludePackages = with pkgs.gnome3; [
    accerciser
    bijiben
    empathy
    epiphany
    evolution
    gedit
    gnome-calculator
    gnome-calendar
    gnome-characters
    gnome-clocks
    gnome-contacts
    gnome-documents
    gnome-logs
    gnome-maps
    gnome-music
    gnome-nettool
    gnome-photos
    gnome-system-log
    gnome-system-monitor
    gnome-weather
    gucharmap
    totem
  ];
  
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

    bitlbee = {
      enable = false;
      plugins = [ pkgs.bitlbee-facebook ];
    };
  
    btsync = {
      enable = false;
      enableWebUI = true;
      httpListenAddr = "127.0.0.1";
      storagePath = "/home/btsync";
      deviceName = "affair";
      package = pkgs.bittorrentSync20;
      checkForUpdates = false;
    };
  
    gnome3 = {
      evolution-data-server.enable = mkForce false;
      gnome-documents.enable = false;
      gnome-keyring.enable = mkForce false;
      gnome-online-accounts.enable = false;
      gnome-online-miners.enable = false;
      tracker.enable = false;
    };

    mpd = {
      enable = true;
      group = "users";
      musicDirectory = "/home/viv/Music";
    };

    openssh.enable = false;
    printing.enable = true;

    privoxy = {
      enable = false;
      enableEditActions = true;
      actionsFiles = [ "match-all.action" "default.action" "/etc/privoxy/user.action" ];
    };

    syncthing = {
      enable = true;
      dataDir = "/home/viv/.syncthing";
      user = "viv";
    };
    
    telepathy.enable = false;

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
    
      emacs = {
        description = "Emacs: the extensible, self-documenting text editor";

        serviceConfig = {
          Type      = "forking";
          ExecStart = "${pkgs.emacs}/bin/emacs --daemon";
          ExecStop  = "${pkgs.emacs}/bin/emacsclient --eval (kill-emacs)";
          Restart   = "always";
        };

        path = [ config.system.path ];

	after    = [ "gpg-agent.service" ];
        wantedBy = [ "default.target" ];
      };


      gpg-agent = {
        description = "GnuPG agent";

	serviceConfig = {
	  Type      = "forking";
	  ExecStart = "${pkgs.bash}/bin/bash -c 'eval `${pkgs.gnupg}/bin/gpg-agent -q  --daemon --enable-ssh-support --write-env-file` ; systemctl --user import-environment GPG_AGENT_INFO SSH_AUTH_SOCK'";
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
  system.stateVersion = "16.03";

}
