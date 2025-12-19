{config, lib, pkgs, ...}:

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
      bc
      bitwarden-cli
      dtach
      fd
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
      smem
      unrar
      unzip
      xdg-utils
      yq-go

      # net
      dnsutils
      inetutils
      lftp
      nethogs
      ngrep
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
      poppler-utils
      
      # snd
      playerctl

      # text
      oterm
      pandoc
      mermaid-filter
    ];

    sessionPath = [ "${config.home.homeDirectory}/bin" ];    
    sessionVariables = {
      ALTERNATIVE_EDITOR = "mg -n";
      SDCV_PAGER = "less -R";      
    };

    shell.enableFishIntegration = true;
  };
  
  programs = {
    bat.enable = true;
    btop = {
      enable = true;
      settings = {
        graph_symbol = "block";
      };
    };
    command-not-found.enable = false;
    gpg.enable = true;
    home-manager.enable = true;
    nix-index.enable = true;
    ncmpcpp.enable = true;
    starship = {
      enable = true;
      enableFishIntegration = false;
      settings = {
        format = lib.concatStrings [
          "$username"
          "$hostname"
          "$localip"
          "$shlvl"
          "$docker_context"
          "$kubernetes"
          "$directory"
          "$git_branch"
          "$git_commit"
          "$git_state"
          "$git_status"
          "$line_break"
          "$package"
          "$c"
          "$cmake"
          "$golang"
          "$haskell"
          "$helm"
          "$nodejs"
          "$python"
          "$rust"
          "$solidity"
          "$terraform"
          "$zig"
          "$direnv"
          "$nix_shell"
          "$cmd_duration"
          "$line_break"
          "$jobs"
          "$status"
          "$battery"
          "$memory_usage"
          "$character"
        ];
        status.disabled = false;
      };
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentry.package = pkgs.pinentry-gnome3;
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

  systemd.user.sessionVariables = {
    SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";
  };
}
