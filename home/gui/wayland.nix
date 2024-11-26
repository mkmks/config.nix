{config, pkgs, ...}:

let
  term-font = "DejaVu Sans Mono 9";
  term-fonts-set = {
    names = [ "DejaVu Sans Mono for Powerline" "FontAwesome6Free" ];
    size = 9.0;
  };
  cmd_lock = "${pkgs.swaylock}/bin/swaylock -f -c ff0000";
in
{
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
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
    file.".config/swayr/config.toml" = {
      text = ''
[menu]
executable = 'bemenu'
args = [
     '--ignorecase',
     '--list',
     '10'
     ]

[format]
output_format = '{indent}Output {name}'
workspace_format = '{indent}Workspace {name} [{layout}]'
container_format = '{indent}Container [{layout}] [{marks}]'
window_format = '{indent}{name}'
html_escape = false

[layout]
auto_tile = false
             '';
    };
  
    packages = with pkgs; [
      bemenu
      swayr
      wlr-randr
      sway-contrib.grimshot

      # fonts
      cm_unicode
      corefonts
      font-awesome_6
      #      google-fonts
      lmodern
      powerline-fonts
      source-code-pro
      kochi-substitute
      wqy_zenhei    
    ];
  };
  
  programs = {
    gnome-terminal = {
      enable = true;
      showMenubar = false;
      themeVariant = "system";
      profile."87d8197f-a5cf-43df-869e-93a69c44b87c" = {
        allowBold = true;
        cursorBlinkMode = "off";
        default = true;
        font = "${term-font}";
        visibleName = "default";
      };
    };
    
    i3status-rust = {
      enable = true;
      bars.default = {
        icons = "awesome6";
        theme = "solarized-dark";
        blocks = [
          # {
          #   block = "focused_window";
          #   format = "$title.str(width:90)";
          #   theme_overrides = {
          #     idle_bg = "#285577";
          #     idle_fg = "#ffffff";
          #   };
          # }
          
          {
            block = "memory";
            format = " $icon $mem_avail.eng(prefix:M) ";
          }          
          {
            block = "disk_space";
          }
          {
            block = "net";
#            interface_name_exclude = ["br\\-[0-9a-f]{12}" "docker\\d+"];
          }
          {
            block = "vpn";
            driver = "mullvad";
            format_connected = " $icon ";
            format_disconnected = " $icon ";
          }
          {
            block = "battery";
            interval = 10;
            format = " $icon $percentage $time ";
          }
          {
            block = "maildir"; 
           interval = 60;
            inboxes = [
              "${config.home.homeDirectory}/Mail/fastmail/Inbox"
              "${config.home.homeDirectory}/Mail/concordium/Inbox"
            ];
            threshold_warning = 1;
            threshold_critical = 10;
          }
          {
            block = "sound";            
          }
          {
            block = "sound";
            driver = "pulseaudio";
            device_kind = "source";
          }
          {
            block = "time";
            interval = 60;
          }
        ];
      };
    };

    waybar = {
      enable = false;
      systemd.enable = true;
      systemd.target = "sway-session.target";
      settings = {
        mainBar = {
          height = 20;
          spacing = 4;
          modules-left = [
            "sway/workspaces"
            "sway/window"
          ];
          modules-right = [
            "memory"
            "disk"
            "network"
            "battery"
            "wireplumber"
            "clock"
            "tray"
          ];
        };
      };
      style = ''
        * {
          font-family: DejaVu Sans Mono for Powerline, FontAwesome6Free;
          font-size: 10px;
        }
      '';
    };
  };

  services = {
    gammastep = {
      enable = true;
      latitude = "48.8566";
      longitude = "2.3522";
      provider = "manual";
    };

    kanshi = {
      enable = true;
      profiles = {
        on-the-go = {
          outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
            }
          ];  
        };
        home-office = {
          outputs = [
            {
              criteria = "Lenovo Group Limited T32p-30 V30AKM70";
              position = "0,0";
              scale = 2.0;
            }
            {
              criteria = "eDP-1";
              position = "0,1440";
              status = "disable";
            }
          ];
        };
        living-room = {
          outputs = [
            {
              criteria = "Unknown HP E233 CNC91301HT";
              position = "0,0";
              scale = 1.1;
            }
            {
              criteria = "eDP-1";
              position = "0,1080";
            }
          ];
        };
        zama = {
          outputs = [
            {
              criteria = "Unknown U3277WB 0x00000003";
              position = "0,0";
              scale = 2.0;
            }
            {
              criteria = "eDP-1";
              position = "0,1440";
            }
          ];
        };
      };
    };

    mako = {
      enable = true;
      font = "${term-font}";
      defaultTimeout = 5000;
    };

    swayidle = {
      enable = true;
      events = [
#        { event = "after-resume"; command = "swaymsg \"output * dpms on\""; }
        { event = "before-sleep"; command = "${cmd_lock}"; }
      ];
      timeouts = [
        { timeout = 300; command = "${cmd_lock}"; }
#        { timeout = 600; command = "swaymsg \"output * dpms off\""; }
      ];
    };
  };
  
  wayland.windowManager.sway = let
    cfg = config.wayland.windowManager.sway.config;
    mod = cfg.modifier;
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
    systemd = {
      enable = true;
      variables = [ "--all" ];
    };    
    wrapperFeatures.gtk = true;
    config = {
      fonts = term-fonts-set;

      startup = [
        { command = "mako"; }
        { command = "swayrd"; }
        { command = "telegram-desktop"; }
        { command = "slack"; }
        { command = "spotify"; }
      ];

      assigns = {
        "2" = [
          { app_id = "^org.telegram.desktop$"; }
          { app_id = "^Slack$"; }
          { app_id = "^Spotify$"; }
        ];
      };

      terminal = "emacsclient -cn";
      menu = "bemenu-run --fn '${term-font}'";
      
      modifier = "Mod4";

      left  = "a";
      down  = "r";
      up    = "s";
      right = "t";
      
      keybindings = let
        cmd_done = "exec \"swaynag -t warning -m 'Done?' -b 'Exit sway' 'swaymsg exit' -z 'Suspend' 'systemctl suspend'\"";
        cmd_switch_window = "exec swayr switch-window";
        cmd_swap_focus = "exec swayr swap-focused-with";
      in {
        "XF86AudioMute"        = "exec 'wpctl set-mute @DEFAULT_SINK@ toggle'";
        "XF86AudioLowerVolume" = "exec 'wpctl set-volume @DEFAULT_SINK@ 3%-'";
        "XF86AudioRaiseVolume" = "exec 'wpctl set-volume @DEFAULT_SINK@ 3%+'";
        "XF86AudioMicMute"     = "exec 'wpctl set-mute @DEFAULT_SOURCE@ toggle'";
        "XF86AudioRecord"      = "exec 'wpctl set-mute @DEFAULT_SOURCE@ toggle'";
        "XF86AudioPlay"        = "exec 'playerctl play-pause'";
        "XF86AudioNext"        = "exec 'playerctl next'";
        "XF86AudioPrev"        = "exec 'playerctl previous'";
        "XF86MonBrightnessDown" = "exec 'light -U 5'";
        "XF86MonBrightnessUp"   = "exec 'light -A 5'";

        "${mod}+z" = "exec '${cmd_lock}'";
        "${mod}+x" = "exec ${cfg.menu}";
        "${mod}+c" = "exec ${cfg.terminal}";
        "${mod}+v" = "${cmd_switch_window}";

        "${mod}+slash"  = "exec '${cmd_lock}'";
        "${mod}+period" = "exec ${cfg.menu}";
        "${mod}+comma"  = "exec ${cfg.terminal}";
        "${mod}+m"      = "${cmd_switch_window}";
        
        "${mod}+Control+z" = "${cmd_done}";
        "${mod}+Control+x" = "reload";
        "${mod}+Control+c" = "kill";
        "${mod}+Control+v" = "${cmd_swap_focus}";
        
        "${mod}+Control+slash"  = "${cmd_done}";
        "${mod}+Control+period" = "reload";
        "${mod}+Control+comma"  = "kill";
        "${mod}+Control+m"      = "${cmd_swap_focus}";

        "${mod}+Mod1+z" = "fullscreen toggle";
        "${mod}+Mod1+x" = "floating toggle";
        "${mod}+Mod1+c" = "split toggle";
        "${mod}+Mod1+v" = "layout toggle all";

        "${mod}+Mod1+slash"  = "fullscreen toggle";
        "${mod}+Mod1+period" = "split toggle";
        "${mod}+Mod1+comma"  = "floating toggle";
        "${mod}+Mod1+m"      = "layout toggle all";

        # windows
        
        "${mod}+${cfg.left}"  = "focus left";
        "${mod}+${cfg.down}"  = "focus down";
        "${mod}+${cfg.up}"    = "focus up";
        "${mod}+${cfg.right}" = "focus right";

        "${mod}+n" = "focus left";
        "${mod}+e" = "focus down";
        "${mod}+i" = "focus up";
        "${mod}+o" = "focus right";
                
        "${mod}+Control+${cfg.left}"  = "move left";
        "${mod}+Control+${cfg.down}"  = "move down";
        "${mod}+Control+${cfg.up}"    = "move up";
        "${mod}+Control+${cfg.right}" = "move right";

        "${mod}+Control+n" = "move left";
        "${mod}+Control+e" = "move down";
        "${mod}+Control+i" = "move up";
        "${mod}+Control+o" = "move right";

        "${mod}+Mod1+${cfg.left}"  = "focus prev sibling";
        "${mod}+Mod1+${cfg.down}"  = "focus child";
        "${mod}+Mod1+${cfg.up}"    = "focus parent";
        "${mod}+Mod1+${cfg.right}" = "focus next sibling";

        "${mod}+Mod1+n" = "focus prev sibling";
        "${mod}+Mod1+e" = "focus child";
        "${mod}+Mod1+i" = "focus parent";
        "${mod}+Mod1+o" = "focus next sibling";
        
        # workspaces
        
        "${mod}+q" = "workspace 1";
        "${mod}+w" = "workspace 2";
        "${mod}+f" = "workspace 3";
        "${mod}+p" = "workspace 4";

        "${mod}+l"         = "workspace 1";
        "${mod}+u"         = "workspace 2";
        "${mod}+y"         = "workspace 3";
        "${mod}+semicolon" = "workspace 4";
        
        "${mod}+Control+q" = "move container to workspace 1";
        "${mod}+Control+w" = "move container to workspace 2";
        "${mod}+Control+f" = "move container to workspace 3";
        "${mod}+Control+p" = "move container to workspace 4";

        "${mod}+Control+l"         = "move container to workspace 1";
        "${mod}+Control+u"         = "move container to workspace 2";
        "${mod}+Control+y"         = "move container to workspace 3";
        "${mod}+Control+semicolon" = "move container to workspace 4";

        "${mod}+Mod1+q" = "move workspace to output left";
        "${mod}+Mod1+w" = "move workspace to output down";
        "${mod}+Mod1+f" = "move workspace to output up";
        "${mod}+Mod1+p" = "move workspace to output right";

        "${mod}+Mod1+l"         = "move workspace to output left";
        "${mod}+Mod1+u"         = "move workspace to output down";
        "${mod}+Mod1+y"         = "move workspace to output up";
        "${mod}+Mod1+semicolon" = "move workspace to output right";
      };

      workspaceLayout = "tabbed";

      window = {
        hideEdgeBorders = "both";
        titlebar = true;
      };
      
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
          xkb_options = "grp:shifts_toggle,grp_led:caps,lv3:ralt_switch_multikey,compose:prsc,caps:ctrl_modifier";

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
      };

      seat = {
        "*" = {
          xcursor_theme = "Adwaita 24";
        };
      };
    };
  };
}
