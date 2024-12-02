{config, pkgs, ...}:

{
  imports = [
    ./emacs.nix
    ./fish.nix
  ];
  
  home = {
    file.".mg" = {
      text = ''
set-default-mode indent
make-backup-files 0
column-number-mode
             '';
    };
    
    packages = with pkgs; [
      bat
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
      procs
      psmisc
      sdcv
      silver-searcher
      unrar
      unzip
      xdg-utils
      yq-go

      # net
      dnsutils
      inetutils
      lftp
      nethogs
      nmap
      picocom
      socat
      tcpdump

      # img
      pkgs.exif
      djvulibre
      exiftool
      ffmpeg
      ghostscript
      pkgs.imagemagick
      pdftk
      poppler_utils
      
      # snd
      playerctl
    ];

    sessionPath = [ "${config.home.homeDirectory}/bin" ];    
    sessionVariables = {
      ALTERNATIVE_EDITOR = "mg -n";
      SDCV_PAGER = "less -R";      
    };
  };
  
  programs = {
    command-not-found.enable = true;
    gpg.enable = true;
    home-manager.enable = true;
    ncmpcpp.enable = true;
    starship.enable = true;
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryPackage = pkgs.pinentry-gnome3;
      defaultCacheTtl = 86400;
      defaultCacheTtlSsh = 86400;
      maxCacheTtl = 604800;
      maxCacheTtlSsh = 604800;
    };

    mpd = {
      enable = true;
      musicDirectory = "${config.home.homeDirectory}/Music";
    };

    playerctld.enable = true;
    
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
