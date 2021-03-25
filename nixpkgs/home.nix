{config, pkgs, ...}:

let
  unstable = import <nixpkgs-unstable> {};
  term-font = "Monospace 9";
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

  fonts.fontconfig.enable = true;
  
  gtk = {
    enable = true;
    font = {
      name = "Dejavu Sans 9";
      package = pkgs.dejavu_fonts;
    };
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
    theme = {
      name = "Adwaita";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
    gtk2.extraConfig = "gtk-key-theme-name = \"Emacs\"";
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-key-theme-name = "Emacs";
    };
  };

  home = {
    packages = with pkgs; [
      # desktop-gui
      android-file-transfer
      libreoffice
      skypeforlinux
      unstable.tdesktop

      gnome3.baobab
      gnome3.dconf-editor
      gnome3.evince
      gnome3.libsecret
      gnome3.seahorse

      # wayland
      bemenu
      mako
      i3status-rust
      swayidle
      swaylock

      # fonts
      cm_unicode
      font-awesome_4
      source-code-pro
      kochi-substitute
      wqy_zenhei
      
      # base
      bc
      dtach
      fdupes
      file
      jq
      mc
      mg
      p7zip
      psmisc
      sdcv
      silver-searcher
      unrar
      unzip

      # net
      dnsutils
      inetutils
      lftp
      mu
      nmap
      picocom

      # cloud
      aws
      aws-google-auth
      aws-iam-authenticator
      helm
      kubectl

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
      rnix-lsp
#      haskell.packages.ghc8102.ghcWithPackages (pkgs: with pkgs; [ cabal-install haskell-language-server ])
      haskellPackages.Agda
      haskellPackages.cabal-install
      unstable.haskell-language-server
      lua53Packages.digestif
      python37Packages.python-language-server
      nodePackages.bash-language-server
      nodePackages.typescript
      nodePackages.typescript-language-server
      unstable.metals
      maven
      sbt
      scalafmt
    ];
  
    sessionVariables = {
      EDITOR = "emacsclient -ct";
      ALTERNATIVE_EDITOR = "mg -n";
      SDCV_PAGER = "less -R";
      MOZ_ENABLE_WAYLAND = 1;
      DIGESTIFDATA = "${pkgs.lua53Packages.digestif}/digestif-${pkgs.lua53Packages.digestif.version}-rocks/digestif/${pkgs.lua53Packages.digestif.version}/data";
    };

    stateVersion = "20.03";
  };  

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];
  
  programs = {
    home-manager.enable = true;
    
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
    hostname
    echo ":"
    pwd
end

function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end 
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

function vterm_prompt_end;
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end
functions --copy fish_prompt vterm_old_fish_prompt
function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
    # Remove the trailing newline from the original prompt. This is done
    # using the string builtin from fish, but to make sure any escape codes
    # are correctly interpreted, use %b for printf.
    printf "%b" (string join "\n" (vterm_old_fish_prompt))
    vterm_prompt_end
end

if [ "$INSIDE_EMACS" = 'vterm' ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end
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
      lfs.enable = true;
      userName = "Nikita Frolov";
      userEmail = "nf@mkmks.org";
    };

    emacs = {
      enable = true;
      package = pkgs.emacsPgtkGcc;
      extraPackages = e: [
	      e.base16-theme
        e.use-package
	      e.diminish
	      e.bind-key
	      e.pretty-mode
        e.ag
        # apps
        e.mu4e-conversation
        e.mu4e-maildirs-extension
	      e.nov
        e.vterm
        e.multi-vterm
        # IDE
        e.company
        e.company-lsp
        e.flycheck
	      e.helm
        e.helm-ag
        e.helm-ghc
        e.helm-lsp
	      e.helm-projectile
	      e.magit
	      e.projectile
        e.restclient
        e.which-key
        # programming languages
	      e.haskell-mode
        e.nix-mode
	      e.scala-mode
	      e.sbt-mode
        e.sql-clickhouse
        e.typescript-mode
        # language server protocol
        e.dap-mode
        e.lsp-haskell
        e.lsp-java
        e.lsp-mode
        e.lsp-metals
	      e.lsp-ui
	      e.lsp-treemacs
        e.posframe
      ];
    };
    
    mbsync.enable = true;
    msmtp.enable = true;
    
    firefox = {
      enable = false;
      package = pkgs.firefox-wayland;
    };

    chromium.enable = true;
    
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
        font = "${term-font}";
        window-title-basename = true;
        window-title-page = true;
        guioptions = "";
      };
    };

    mako = {
      enable = true;
      font = "${term-font}";
      defaultTimeout = 5000;
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

    redshift = {
      enable = true;
      package = pkgs.redshift-wlr;
      brightness = {
        day = "1.0";
        night = "0.7";
      };
      latitude = "48.8566";
      longitude = "2.3522";
      provider = "manual";
    };
    
    mbsync.enable = true;
    syncthing.enable = true;
    udiskie.enable = true;
  };

  systemd.user.startServices = true;

  wayland.windowManager.sway = let
    cfg = config.wayland.windowManager.sway.config;
    mod = cfg.modifier;
    lockscreen-fg = "ff0000";    
  in {
    enable = true;
    extraSessionCommands = ''
        export SDL_VIDEODRIVER=wayland
        # needs qt5.qtwayland in systemPackages
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
    config = {
      fonts = [ "${term-font}" ];

      startup = [
        { command = "dbus-update-activation-environment --systemd --all"; }
        { command = ''
                      swayidle -w \
                                timeout 300  'swaylock -f -c ${lockscreen-fg}' \
                                timeout 600  'swaymsg \"output * dpms off\"' \
                                resume       'swaymsg \"output * dpms on\"' \
                                before-sleep 'swaylock -f -c ${lockscreen-fg}'
                    ''; }
        { command = "mako"; }
        { command = "telegram-desktop"; }
      ];

      assigns = {
        "mmm" = [
          { app_id = "^telegramdesktop$"; }
          { title = "^Spotify.*$"; }
        ];
      };

      terminal = "emacsclient -cn";
      menu = "bemenu-run --fn '${term-font}'";
      
      modifier = "Mod4";

      left = "a";
      down = "r";
      up = "s";
      right = "t";
      
      keybindings = {
        "${mod}+q" = "exec ${cfg.terminal}";
        "${mod}+x" = "exec ${cfg.menu}";

        "XF86AudioMute"        = "exec \"pamixer -t\"";
        "XF86AudioLowerVolume" = "exec \"pamixer -d 3 -u\"";
        "XF86AudioRaiseVolume" = "exec \"pamixer -i 3 -u\"";
        "XF86AudioMicMute"     = "exec \"pamixer --source alsa_input.pci-0000_00_1f.3.analog-stereo -t\"";
        
        "${mod}+z" = "exec \"swaylock -c ${lockscreen-fg}\"";
        "${mod}+Shift+z" = "exec \"swaynag -t warning -m 'Exit?' -b 'Yes, exit sway' 'swaymsg exit'\"";
        "${mod}+Shift+c" = "kill";
        "${mod}+Shift+x" = "restart";
        "${mod}+c" = "reload";

        # windows
        
        "${mod}+${cfg.left}" = "focus left";
        "${mod}+${cfg.down}" = "focus down";
        "${mod}+${cfg.up}" = "focus up";
        "${mod}+${cfg.right}" = "focus right";

        "${mod}+Shift+${cfg.left}" = "move left";
        "${mod}+Shift+${cfg.down}" = "move down";
        "${mod}+Shift+${cfg.up}" = "move up";
        "${mod}+Shift+${cfg.right}" = "move right";

        # workspaces
        
        "${mod}+1" = "workspace 1";
        "${mod}+2" = "workspace 2";
        "${mod}+3" = "workspace 3";
        "${mod}+4" = "workspace mmm";

        "${mod}+Shift+1" = "move container to workspace 1";
        "${mod}+Shift+2" = "move container to workspace 2";
        "${mod}+Shift+3" = "move container to workspace 3";
        "${mod}+Shift+4" = "move container to workspace mmm";

        "${mod}+Shift+Mod1+a" = "move workspace to output left";
        "${mod}+Shift+Mod1+r" = "move workspace to output down";
        "${mod}+Shift+Mod1+s" = "move workspace to output up";
        "${mod}+Shift+Mod1+t" = "move workspace to output right";

        # layouts
        
        "${mod}+space" = "layout toggle all";
        "${mod}+Shift+space" = "split toggle";
        "${mod}+Control+a" = "floating toggle";
        "${mod}+Control+r" = "focus child";
        "${mod}+Control+s" = "focus parent";
        "${mod}+Control+t" = "fullscreen toggle";       
      };

      workspaceLayout = "tabbed";
      window.hideEdgeBorders = "both";
      
      bars = [
        {
          position = "top";
          statusCommand = "i3status-rs";
        
          colors = {
            background = "#222222";
            statusline = "#dddddd";

            activeWorkspace = {
              border = "#333333";
              background = "#333333";
              text = "#ffffff";
            };
            inactiveWorkspace = {
              border = "#333333";
              background = "#333333";
              text = "#888888";
            };
          };

          fonts = [ "${term-font}" ];
        }
      ];
      
      input = {
        "*" = {
          xkb_layout = "us,ru";
          xkb_variant = "colemak,";
          xkb_options = "grp:rctrl_toggle,compose:prsc,caps:ctrl_modifier";

          accel_profile = "adaptive";
          click_method = "clickfinger";
          natural_scroll = "enabled";
          tap = "disabled";
        };
        
        "Synaptics TM3288-011" = {
          dwt = "enabled";
          scroll_method = "two_finger";
        };
      };

      output = {
        "*" = {
          background = "#000000 solid_color";
        };

        "Dell Inc. DELL U2415 7MT0188M11YU" = {
          scale = "1.2";
          pos = "0 0";
        };
          
        "eDP-1" = {
          scale = "2";
          pos = "0 1000";
        };        
      };
    };
  };

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
