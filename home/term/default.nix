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
      jq
      mc
      mg
      ncdu
      p7zip
      sdcv
      silver-searcher
      ticker
      unrar
      unzip
      xdg-utils
      yq-go

      # sys
      iotop
      lsof
      procs
      psmisc
      smem

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
      unixtools.netstat

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
      wiremix

      # text
      oterm
      pandoc
      mermaid-filter
      prettier
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
    ncmpcpp = {
      enable = true;
      mpdMusicDir = "/var/lib/syncthing/Music";
    };
    ripgrep.enable = true;
    spotify-player.enable = true;
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
    tmux = {
      enable = true;
      baseIndex = 1;
      extraConfig = ''
      set -g clock-mode-style 24-with-seconds
      set -g status-position top
      set -g status-right ""
      '';
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
    
    playerctld.enable = true;
    
    udiskie = {
      enable = true;
#      tray = "never";
    };    
  };

  systemd.user.sessionVariables = {
    SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";
  };
}
