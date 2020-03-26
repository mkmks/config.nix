{pkgs, ...}:

with pkgs;

#let
#  unstableTarball = fetchTarball #https://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz;
#  unstable = import unstableTarball {
#               config = config.nixpkgs.config;
#             };
#in

{
  
  accounts.email = {
    certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
    maildirBasePath = "Mail";
    
    accounts.fastmail = {
      primary = true;

      address = "nf@mkmks.org";
      realName = "Nikita Frolov";
      userName = "nf@mkmks.org";
      passwordCommand = "secret-tool lookup email nf@mkmks.org";
      
      imap.host = "imap.fastmail.com";      
      smtp.host = "smtp.fastmail.com";

      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
      };
      msmtp.enable = true;
    };  
  };

  gtk = {
    enable = true;
    font = {
      name = "Dejavu Sans 9";
      package = dejavu_fonts;
    };
    iconTheme = {
      name = "Adwaita";
      package = gnome3.adwaita-icon-theme;
    };
    theme = {
      name = "Adwaita";
      package = gnome3.adwaita-icon-theme;
    };
    gtk2.extraConfig = "gtk-key-theme-name = \"Emacs\"";
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-key-theme-name = "Emacs";
    };
  };

  home = {
    packages = [
      # desktop-brands
      libreoffice
      skypeforlinux
      spotify
      steam
      tdesktop
      
      # desktop-cli
      ncmpcpp
      sdcv

      # base
      mc
      mg
      silver-searcher

      # net
      lftp
      mu
      nmap      

      # img
      pkgs.exif
      exiftool
      fdupes      
      pkgs.imagemagick
      pdftk
      xfig       
    ];
    
    sessionVariables = {
      EDITOR = "emacsclient -ct";
      ALTERNATIVE_EDITOR = "mg -n";
      SDCV_PAGER = "less -R";
      MOZ_ENABLE_WAYLAND = 1;
    };    
  };  
  
  programs = {

    termite = {
      enable = true;

      cursorBlink = "off";
      scrollbar = "off";
      
      clickableUrl = true;
      dynamicTitle = true;

      browser = "firefox";

      foregroundColor = "white";
      backgroundColor = "black";
      cursorColor = "#cccccc";
    };

    
    fish = {
      enable = true;

      interactiveShellInit = ''
set fish_greeting

function fish_prompt
  set -l nix_shell_info (
    if test -n "$IN_NIX_SHELL"
      echo -n "<nix-shell> "
    end
  )
  echo -n -s $nix_shell_info '$ '
end

function fish_title
    echo $USER'@'(hostname)':'$PWD'$' $argv[1]
end

gpg-connect-agent -q updatestartuptty /bye > /dev/null
      '';
      
      promptInit = ''

          '';

      shellAliases = {
        ec = "emacsclient -n";
        v = "ebook-viewer";
        u = "udiskie-umount";
        feh = "feh -.d";
      };
    };
    
    git = {
      enable = true;
      userName = "Nikita Frolov";
      userEmail = "nf@mkmks.org";
    };

    mbsync.enable = true;
    msmtp.enable = true;
    
#    firefox = {
#      enable = true;
#      package = firefox-beta-bin;
#    };
    
    feh.enable = true;
    gpg.enable = true;
    
    mpv = {
      enable = true;
      config = {
        gpu-context = "wayland";
      };
    };

    tmux = {
      enable = true;

      extraConfig = ''
        set -g mouse on
        set -g prefix C-x
        bind-key C-x send-prefix
        unbind-key x
        bind-key k confirm-before -p "kill-pane #P? (y/n)" kill-pane

        set -g renumber-windows on
        set -g set-titles on
        set -g set-titles-string "[#I] #T"
        set -g status on
        set -g status-position top
        set -g status-left ""
        set -g status-right ""
      '';
    };
    
    zathura = {
      enable = true;
      options = {
        font = "Monospace 9";
        window-title-basename = true;
        window-title-page = true;
        guioptions = "";
      };
    };

#    vscode.enable = true;
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
    
    mbsync.enable = true;
    udiskie.enable = true;
  };

  systemd.user.startServices = true;
  
}
