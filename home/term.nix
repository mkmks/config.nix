{config, pkgs, ...}:

{
  home = {
    file.".mg" = {
      text = ''
set-default-mode indent
make-backup-files 0
column-number-mode
             '';
    };
    
    packages = with pkgs; [
      bc
      bitwarden-cli
      dtach
      fdupes
      file
      hledger
      hledger-ui
      iotop
      jq
      mc
      mg
      p7zip
      psmisc
      sdcv
      silver-searcher
      unrar
      unzip
      xdg-utils

      # net
      dnsutils
      inetutils
      lftp
      nmap
      picocom

      # img
      pkgs.exif
      exiftool
      pkgs.imagemagick
      pdftk
      poppler_utils
      
      # snd
      pamixer
      spotify-tui
    ];

    sessionPath = [ "${config.home.homeDirectory}/bin" ];    
    sessionVariables = {
      ALTERNATIVE_EDITOR = "mg -n";
      SDCV_PAGER = "less -R";      
    };
  };
  
  programs = {
    gpg.enable = true;
    home-manager.enable = true;
    ncmpcpp.enable = true;
  };

  services = {
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
          password_cmd = "${pkgs.libsecret}/bin/secret-tool lookup service spotifyd username mkmks";
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
}
