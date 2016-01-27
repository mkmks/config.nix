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

  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;
  networking.proxy.default = "http://127.0.0.1:8118";
  networking.proxy.noProxy = "localhost, 127.0.0.0/8, ::1, rutracker.org";

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
        # Define my own Emacs
        emacs = pkgs.lib.overrideDerivation (pkgs.emacs.override {
            # Use gtk3 instead of the default gtk2
	    withX = true;
            withGTK3 = true;
	    withGTK2 = false;
          }) (attrs: {
	    # Emacs daemon is part of user session, use emacsclient
	    postInstall = attrs.postInstall + ''
	      sed -i 's/Exec=emacs/Exec=emacsclient -c -n/' $out/share/applications/emacs.desktop
	    '';
	  });
    };
  };

  environment.systemPackages = with pkgs; [
    androidenv.platformTools
    cabal-install
    cabal2nix
    coreutils
    coq
    chromium
    djvulibre
    emacs
    file
    findutils
    ghc
    git
    gnupg
    gnuplot
    graphviz
    htop
    imagemagick
    isync
    mpc_cli
    mpv
    nix-repl
    nmap
    nox
    powertop
    psmisc
    pwgen
    p7zip
    silver-searcher
    sloccount
    steam
    tmux
    tor
    transmission

    #haskellPackages.Agda
    #haskellPackages.ghc-mod
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
      inconsolata
      kochi-substitute
      wqy_zenhei
    ];
  };

  programs.bash.enableCompletion = true;
  
  # List services that you want to enable:

  services = {

    btsync = {
      enable = true;
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
      gnome-online-accounts.enable = false;
      gnome-online-miners.enable = false;
      tracker.enable = false;
    };

    mpd = {
      enable = true;
      group = "btsync";
      musicDirectory = "/home/btsync/BitTorrent Sync/Music";
    };
          
    # Enable the OpenSSH daemon.
    #openssh.enable = true;
    
    # Enable CUPS to print documents.
    printing.enable = true;

    privoxy.enable = true;
    privoxy.enableEditActions = true;

    telepathy.enable = false;
    
    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us";

      # xkbOptions = "eurosign:e";

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

      displayManager.gdm.enable = true;
      desktopManager.gnome3.enable = true;
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

	path = [ pkgs.chromium pkgs.gnupg pkgs.xdg_utils ];

        # I want the emacs service to be started with the rest of the user services
        wantedBy = [ "default.target" ];

        environment = {
          SSH_AUTH_SOCK  = "%t/keyring/ssh";
	  GPG_AGENT_INFO = "%t/keyring/gpg:0:1";
          GTK_DATA_PREFIX = config.system.path;
          GTK_PATH = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";
        };
      };

      mbsync = {
        description = "Mailbox syncronization";

	serviceConfig = {
	  Type      = "oneshot";
	  ExecStart = "${pkgs.isync}/bin/mbsync -a";
	};

	path = [ pkgs.gawk pkgs.gnupg ];

	environment = {
	  GPG_AGENT_INFO = "%t/keyring/gpg:0:1";
	};
	
	after       = [ "network-online.target" ];
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
  system.stateVersion = "15.09";

}
