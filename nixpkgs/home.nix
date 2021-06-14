{config, pkgs, ...}:

let
  unstable = import <nixpkgs-unstable> {};
  term-font = "DejaVu Sans Mono for Powerline 9";
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
    file."bin/chrome" = {
      executable = true;
      text = ''
#!/bin/sh
chromium \
    --enable-features=UseOzonePlatform --ozone-platform=wayland \
    --disable-gpu-memory-buffer-video-frames
             '';
    };

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
      swayidle

      # fonts
      cm_unicode
      font-awesome_4
      powerline-fonts
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
      unstable.bloop
      maven
      sbt
      scalafmt
    ];
  
    sessionVariables = {
      EDITOR = "emacsclient -c";
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

      functions = {
        d = ''
set -l dict
# 35/53/64
switch $argv[1]
	# ENGLISH (7/8)
	case ee
	    set dict 'OxfordDictionary (En-En)'
	case eg
	    set dict 'Collins (En-De)'
	case ed
	    set dict 'Van Dale (En-Nl)'
	case ef
	    set dict 'Universal (En-Fr)'
	case ei
	    set dict 'Universal (En-It)'
	case es
	    set dict 'Collins (En-Es)'
	case er
	    set dict 'LingvoUniversal (En-Ru)'
	# GERMAN (6/8)
	case ge
	    set dict 'Collins (De-En)'
	case gg
	    set dict 'Duden (De-De)'
	case gf
	    set dict 'CompactVerlag (De-Fr)'
	case gi
	    set dict 'CompactVerlag (De-It)'
	case gs
	    set dict 'CompactVerlag (De-Es)'
	case gr
	    set dict 'Universal (De-Ru)'
	# DUTCH (2/8)
	case de
	    set dict 'Van Dale (Nl-En)'	    
	case dr
	    set dict 'Part Ne-Ru'
	# FRENCH (4/8)
	case fe
	    set dict 'Universal (Fr-En)'    
	case fg
	    set dict 'CompactVerlag (Fr-De)'	    	    
	case ff
	    set dict 'Le Grand Robert (Fr-Fr)'
	case fr
	    set dict 'Universal (Fr-Ru)'	    
	# ITALIAN (3/8)
	case ie
	    set dict 'Universal (It-En)'    
	case ig
	    set dict 'CompactVerlag (It-De)'	    
	case ir
	    set dict 'Universal (It-Ru)'	    	    
	# SPANISH (4/8)
	case se
	    set dict 'Collins (Es-En)'
	case sg
	    set dict 'CompactVerlag (Es-De)'
	case ss
	    set dict 'VOX (Spa-Spa)'
	case sr
	    set dict 'Universal (Es-Ru)'	    
	# PORTUGUESE (1/8)
	case pr
	    set dict 'Universal (Pt-Ru)'
	# RUSSIAN (8/8)
	case re
	    set dict 'LingvoUniversal (Ru-En)'
	case rg
	    set dict 'Universal (Ru-De)'
	case rd
	    set dict 'Part Ru-Ne'
	case rf
	    set dict 'Universal (Ru-Fr)'
	case ri
	    set dict 'Universal (Ru-It)'
	case rs
	    set dict 'Universal (Ru-Es)'
	case rp
	    set dict 'Universal (Ru-Pt)'
	case rr
	    set dict 'ExplanatoryBTS (Ru-Ru)'
end
sdcv -0c -u $dict $argv[2]          
        '';

        my_fish_prompt = ''
set -l nix_shell_info (
  if test -n "$IN_NIX_SHELL"
    echo -n "<nix-shell> "
  end
)
echo -n -s $nix_shell_info '$ '
        '';

        fish_prompt = ''
# Remove the trailing newline from the original prompt. This is done
# using the string builtin from fish, but to make sure any escape codes
# are correctly interpreted, use %b for printf.
printf "%b" (string join "\n" (my_fish_prompt))
vterm_prompt_end
'';

        fish_title = ''
hostname
echo ":"
pwd
        '';

        vterm_printf = ''
if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end 
  # tell tmux to pass the escape sequences through
  printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
else if string match -q -- "screen*" "$TERM"
       # GNU screen (screen, screen-256color, screen-256color-bce)
       printf "\eP\e]%s\007\e\\" "$argv"
     else
       printf "\e]%s\e\\" "$argv"
     end
        '';

        vterm_prompt_end = "vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)";
      };
      
      interactiveShellInit = ''
set fish_greeting

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
        e.vs-dark-theme
        e.vs-light-theme
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

    i3status-rust = {
      enable = true;
      bars.default = {
        icons = "awesome";
        theme = "solarized-dark";
        blocks = [
          {
            block = "memory";
            display_type = "memory";
            clickable = false;
            format_mem = "{mem_avail;G}";
          }          
          {
            block = "disk_space";
            path = "/";
            alias = "/";
            unit = "GB";
            interval = 20;
            warning = 20.0;
            alert = 10.0;
          }
          {
            block = "maildir";
            interval = 60;
            inboxes = ["/home/viv/Mail/fastmail/Inbox"];
            threshold_warning = 1;
            threshold_critical = 10;
          }
          {
            block = "net";
            device = "wlan0";
            interval = 5;
          }
          {
            block = "net";
            device = "wwp0s20f0u6";
            interval = 5;
          }
          {
            block = "net";
            device = "enp0s31f6";
            interval = 5;
          }
          {
            block = "battery";
            interval = 10;
            format = "{percentage}% {time}";            
          }
          {
            block = "sound";            
          }
          {
            block = "time";
            interval = 60;
            format = "%a %b %d %R";            
          }
        ];
      };
    };
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
    lockcmd = "swaylock -f -c ff0000";
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
                                timeout 300  '${lockcmd}' \
                                timeout 600  'swaymsg \"output * dpms off\"' \
                                resume       'swaymsg \"output * dpms on\"' \
                                before-sleep '${lockcmd}'
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

      left  = "a";
      down  = "r";
      up    = "s";
      right = "t";
      
      keybindings = {
        "XF86AudioMute"        = "exec 'pamixer -t'";
        "XF86AudioLowerVolume" = "exec 'pamixer -d 3 -u'";
        "XF86AudioRaiseVolume" = "exec 'pamixer -i 3 -u'";
        "XF86AudioMicMute"     = "exec 'pamixer --source alsa_input.pci-0000_00_1f.3.analog-stereo -t'";
        "XF86MonBrightnessDown" = "exec 'light -U 5'";
        "XF86MonBrightnessUp"   = "exec 'light -A 5'";

        "${mod}+z" = "exec '${lockcmd}'";
        "${mod}+x" = "exec ${cfg.menu}";
        "${mod}+c" = "exec ${cfg.terminal}";

        "${mod}+Shift+z" = "exec \"swaynag -t warning -m 'Exit?' -b 'Yes, exit sway' 'swaymsg exit'\"";
        "${mod}+Shift+x" = "reload";
        "${mod}+Shift+c" = "kill";

        # windows
        
        "${mod}+${cfg.left}"  = "focus left";
        "${mod}+${cfg.down}"  = "focus down";
        "${mod}+${cfg.up}"    = "focus up";
        "${mod}+${cfg.right}" = "focus right";
        "${mod}+Control+${cfg.down}" = "focus child";
        "${mod}+Control+${cfg.up}"   = "focus parent";
        
        "${mod}+Shift+${cfg.left}"  = "move left";
        "${mod}+Shift+${cfg.down}"  = "move down";
        "${mod}+Shift+${cfg.up}"    = "move up";
        "${mod}+Shift+${cfg.right}" = "move right";

        "${mod}+Mod1+${cfg.left}"  = "focus output left";
        "${mod}+Mod1+${cfg.down}"  = "focus output down";
        "${mod}+Mod1+${cfg.up}"    = "focus output up";
        "${mod}+Mod1+${cfg.right}" = "focus output right";        

        # workspaces
        
        "${mod}+1" = "workspace 1";
        "${mod}+2" = "workspace 2";
        "${mod}+3" = "workspace 3";
        "${mod}+4" = "workspace mmm";

        "${mod}+Shift+1" = "move container to workspace 1";
        "${mod}+Shift+2" = "move container to workspace 2";
        "${mod}+Shift+3" = "move container to workspace 3";
        "${mod}+Shift+4" = "move container to workspace mmm";

        "${mod}+Shift+Mod1+${cfg.left}"  = "move workspace to output left";
        "${mod}+Shift+Mod1+${cfg.down}"  = "move workspace to output down";
        "${mod}+Shift+Mod1+${cfg.up}"    = "move workspace to output up";
        "${mod}+Shift+Mod1+${cfg.right}" = "move workspace to output right";

        # layouts
        
        "${mod}+space" = "layout toggle all";
        "${mod}+Shift+space" = "split toggle";
        "${mod}+Control+a" = "floating toggle";
        "${mod}+Control+t" = "fullscreen toggle";       
      };

      workspaceLayout = "tabbed";
      window.hideEdgeBorders = "both";
      
      bars = [
        {
          position = "top";
          statusCommand = "i3status-rs ~/.config/i3status-rust/config-default.toml";
        
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

        "TPPS/2 Elan TrackPoint" = {
          pointer_accel = "0.5";
        };
      };

      output = {
        "*" = {
          background = "#000000 solid_color";
        };

        "Dell Inc. DELL U2415 7MT0188M11YU" = {
          pos = "0 0";
        };

        "Samsung Electric Company S27C450 HTPFC00841" = {
          pos = "1920 0";
        };

        "Samsung Electric Company S27C450 HTPFC00855" = {
          pos = "0 0";
        };

        "Unknown 0x403D 0x00000000" = {
          scale = "1.2";
          pos = "0 1200";
        };

        "Unknown 0x2036 0x00000000" = {
          pos = "0 1200";
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
