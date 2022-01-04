{config, pkgs, ...}:

let
  term-font = "DejaVu Sans Mono 9";
  term-fonts-set = {
    names = [ "DejaVu Sans Mono for Powerline" ];
    size = 9.0;
  };  
in
{
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

  home.packages = with pkgs; [
    bemenu
    swayidle

    # fonts
    cm_unicode
    font-awesome_4
    powerline-fonts
    source-code-pro
    kochi-substitute
    wqy_zenhei    
  ];
  
  programs = {
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
            inboxes = ["${config.home.homeDirectory}/Mail/fastmail/Inbox"];
            threshold_warning = 1;
            threshold_critical = 10;
          }
          {
            block = "networkmanager";
            interface_name_exclude = ["br\\-[0-9a-f]{12}" "docker\\d+"];
          }
          {
            block = "battery";
            interval = 10;
            format = "{percentage} {time}";            
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

  services.gammastep = {
      enable = true;
      latitude = "48.8566";
      longitude = "2.3522";
      provider = "manual";
    };
  
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
        export BEMENU_BACKEND=wayland
      '';
    wrapperFeatures.gtk = true;
    config = {
      fonts = term-fonts-set;

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

          fonts = term-fonts-set;
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
}
