{config, pkgs, ...}:

let
  unstable = import <nixpkgs-unstable> {};
  term-font = "DejaVu Sans Mono for Powerline 9";
in

{
  imports = [
    ./devel.nix
    ./emacs.nix
    ./mail.nix
    ./shell.nix    
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
    
    packages = let
      inherit (unstable) fetchurl;
      concordium-desktop-wallet = pkgs.appimageTools.wrapType2 { # or wrapType1
        name = "concordium-desktop-wallet";
        src = fetchurl {
          url = "https://distribution.mainnet.concordium.software/tools/linux/concordium-desktop-wallet-1.2.0.AppImage";
          sha256 = "526fc9f3d894eeb2bc49451d5d68dbd092fb3b40699158f3bb8e7693914d89c1";
        };
        extraPkgs = pkgs: with pkgs; [ ];
      };
    in with pkgs; [
      android-file-transfer
      libreoffice
      skypeforlinux
      unstable.tdesktop
      unstable.zoom-us
      
      gnome3.baobab
      gnome3.dconf-editor
      gnome3.seahorse
      pavucontrol
      
      gnome3.eog
      gnome3.evince
      gthumb
      
      gnuplot
      xfig

      concordium-desktop-wallet
      unstable.ledger-live-desktop      
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
