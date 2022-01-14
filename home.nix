{config, pkgs, nixpkgs-unstable, ...}:

let
#  unstable = import <nixpkgs-unstable> {};
  term-font = "DejaVu Sans Mono for Powerline 9";
in

{
  imports = [
    ./devel.nix
    ./emacs.nix
    ./mail.nix
    ./term.nix    
    ./wayland.nix
  ];

  home = {
    username = "viv";
    homeDirectory = "/home/viv";

    file."bin/chrome" = {
      executable = true;
      text = ''
#!/bin/sh
chromium \
    --enable-features=UseOzonePlatform --ozone-platform=wayland \
    --disable-gpu-memory-buffer-video-frames
             '';
    };

    file.".mg" = {
      text = ''
set-default-mode indent
make-backup-files 0
column-number-mode
             '';
    };
    
    packages = let
      inherit (nixpkgs-unstable) fetchurl;
      concordium-desktop-wallet = pkgs.appimageTools.wrapType2 { # or wrapType1
        name = "concordium-desktop-wallet";
        src = fetchurl {
          url = "https://distribution.mainnet.concordium.software/tools/linux/concordium-desktop-wallet-1.3.0.AppImage";
          sha256 = "6660da7cbc16772ab8cf2986f4775ce62c4d60140043fca124e5cc93f4d98fab";
        };
        extraPkgs = pkgs: with pkgs; [ ];
      };
      concordium-desktop-wallet-testnet = pkgs.appimageTools.wrapType2 { # or wrapType1
        name = "concordium-desktop-wallet";
        src = fetchurl {
          url = "https://s3.eu-west-1.amazonaws.com/desktopwallet.concordium.com/1.3.1/testnet/concordium-desktop-wallet-testnet-1.3.1.AppImage";
          sha256 = "80da238ee1ef92894ed53ad21690275afb9dd46c20a5c31465ccbcfce093f038";
        };
        extraPkgs = pkgs: with pkgs; [ ];
      };

    in with pkgs; [
      android-file-transfer
      libreoffice
      skypeforlinux
      nixpkgs-unstable.tdesktop
      nixpkgs-unstable.zoom-us
      
      gnome3.baobab
      gnome3.dconf-editor
      gnome3.seahorse
      pavucontrol
      
      gnome3.eog
      gnome3.evince
      gthumb
      
      gnuplot
      xfig

#      concordium-desktop-wallet
      concordium-desktop-wallet-testnet
      nixpkgs-unstable.ledger-live-desktop      
    ];

    sessionPath = [ "${config.home.homeDirectory}/bin" ];
    
    stateVersion = "21.11";
  };  
  
  programs = {
    home-manager.enable = true;    
           
    chromium = {
      enable = true;
      extensions = [
        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
        { id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp"; } # privacy badger
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # bitwarden
        { id = "dbepggeogbaibhgnhhndojpepiihcmeb"; } # vimium
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # dark reader
      ];
    };
    
    gpg.enable = true;
    
    mpv = {
      enable = true;
      config = {
        gpu-context = "wayland";
      };
    };
    
    zathura = {
      enable = true;
      options = {
        font = "${term-font}";
        window-title-basename = true;
        window-title-page = true;
        guioptions = "";
      };
    };

  };

  services = {
    gnome-keyring.enable = true;
    
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "gnome3";
      defaultCacheTtl = 86400;
      defaultCacheTtlSsh = 86400;
      maxCacheTtl = 604800;
      maxCacheTtlSsh = 604800;
    };

    mpd = {
      enable = true;
      musicDirectory = "${config.home.homeDirectory}/Music";
    };

    spotifyd = {
      enable = true;
      settings = {
        global = {
          username = "mkmks";
          password_cmd = "${pkgs.gnome3.libsecret}/bin/secret-tool lookup service spotifyd username mkmks";
          device_name = "schildpad";
          bitrate = 320;
        };
      };
    };
    
    syncthing.enable = true;
    udiskie = {
      enable = true;
      tray = "never";
    };
  };

  systemd.user.startServices = true;

  xdg = {
    enable = true;
    mime.enable = true;
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = [ "org.pwmt.zathura.desktop" ];
        "application/epub+zip" = [ "emacsclient.desktop" ];
        "image/vnd.djvu" = [ "org.pwmt.zathura.desktop" ];
        "text/plain" = [ "emacsclient.desktop" ];
        "text/html" = [ "chromium.desktop" ];
        "x-scheme-handler/http" = [ "chromium.desktop" ];
        "x-scheme-handler/https" = [ "chromium.desktop" ];
        "x-scheme-handler/chrome" = [ "chromium.desktop" ];
        "x-scheme-handler/webcal" = [ "chromium.desktop" ];        
      };
    };
    userDirs.enable = true;
  };
  
}
