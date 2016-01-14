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
      device = "/dev/sda5";
      preLVM = true; }
  ];

  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;
  networking.proxy.default = "http://127.0.0.1:8118";

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
            # Make sure imagemgick is a dependency because I regularly
            # look at pictures from Emasc
            imagemagick = pkgs.imagemagickBig;
          }) (attrs: {});
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
    nix-repl
    nmap
    nox
    powertop
    psmisc
    pwgen
    silver-searcher
    sloccount
    steam
    tmux
    tor
    xz

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
    gnome-contacts
    gnome-documents
    gnome-logs
    gnome-maps
    gnome-nettool
    gnome-photos
    gnome-system-log
    gnome-system-monitor
    gnome-weather
    gucharmap
  ];
  
  fonts = {
    fonts = with pkgs; [
      inconsolata
    ];
  };

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
    };

    telepathy.enable = false;
          
    # Enable the OpenSSH daemon.
    #openssh.enable = true;
    
    # Enable CUPS to print documents.
    printing.enable = true;

    privoxy.enable = true;

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

  systemd.user.services.emacs = {
    description                 = "Emacs: the extensible, self-documenting text editor";


    serviceConfig               = {
      Type      = "forking";
      ExecStart = "${pkgs.emacs}/bin/emacs --daemon";
      ExecStop  = "${pkgs.emacs}/bin/emacsclient --eval (kill-emacs)";
      Restart   = "always";
    };

    # I want the emacs service to be started with the rest of the user services
    wantedBy = [ "default.target" ];

    # Annoyingly, systemd doesn't pass any environment variable to its
    # services. Below, I set some variables that I missed.
    # environment = {
      # Give Emacs a chance to use gnome keyring for the ssh-agent
      # SSH_AUTH_SOCK   = "%t/keyring/ssh";

      # Some variables for GTK applications I will launch from Emacs
      # (typically evince and the gnome-terminal)
      # GTK_DATA_PREFIX = config.system.path;
      # GTK_PATH        = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";

      # Make sure aspell will find its dictionaries
      # ASPELL_CONF     = "dict-dir /run/current-system/sw/lib/aspell";

      # Make sure locate will find its database
      # LOCATE_PATH     = "/var/cache/locatedb";
    # };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

}
