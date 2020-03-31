{config, pkgs, ...}:

with pkgs;

let
  metals = import ./metals;
in

{
  
  accounts.email = {
    certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
    maildirBasePath = "Mail";
    
    accounts.fastmail = {
      primary = true;

      address = "nf@mkmks.org";
      realName = "Nikita Frolov";
      userName = "nf@mkmks.org";
      passwordCommand = "${pkgs.gnome3.libsecret}/bin/secret-tool lookup email nf@mkmks.org";
      
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
      # desktop-gui
      libreoffice
      steam
      skypeforlinux
      tdesktop

      gnome3.baobab
      gnome3.dconf-editor
      gnome3.evince
      gnome3.libsecret

      # base
      bc
      dtach
      fdupes
      file
      mc
      mg
      p7zip
      psmisc
      sdcv
      silver-searcher
      unzip

      # net
      dnsutils
      inetutils
      lftp
      mu
      nmap
      picocom

      # img
      pkgs.exif
      exiftool
      gnuplot
      gthumb
      pkgs.imagemagick
      pdftk
      poppler_utils
      xfig

      # snd
      ncmpcpp
      pamixer
      pavucontrol
      spotify

      # dev
      haskellPackages.Agda
      metals
      sbt
    ];
    
    sessionVariables = {
      EDITOR = "emacsclient -ct";
      ALTERNATIVE_EDITOR = "mg -n";
      SDCV_PAGER = "less -R";
      MOZ_ENABLE_WAYLAND = 1;
    };    
  };  
  
  programs = {
    home-manager.enable = true;

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

    emacs = {
      enable = true;
      extraPackages = e: [
	      e.base16-theme
        e.use-package
	      e.diminish
	      e.bind-key
	      e.pretty-mode
        # apps
	      e.nov
        # project management
	      e.helm
        e.helm-ghc
	      e.helm-projectile
	      e.projectile
	      e.magit
        # programming languages
	      e.haskell-mode
        e.nix-mode
	      e.scala-mode
	      e.sbt-mode
        # language server protocol
        e.lsp-mode
	      e.lsp-ui
	      e.lsp-treemacs
      ];
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
    emacs.enable = true;
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
    
    mbsync.enable = true;
    syncthing.enable = true;
    udiskie.enable = true;
  };

  systemd.user.startServices = true;
  
}
