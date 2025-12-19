{config, lib, pkgs, ...}:

let
  term-font-family = "JetBrains Mono";
  term-font-size = 9;
  term-font = "${term-font-family} ${toString term-font-size}";
  cmd_lock = "${pkgs.swaylock}/bin/swaylock -f -c ff0000";
in
{
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      gtk-key-theme = "Emacs";
    };
  };
  
  fonts.fontconfig.enable = true;
  
  gtk = {
    enable = true;
    font = {
      name = "Dejavu Sans";
      size = 9;
      package = pkgs.dejavu_fonts;
    };
    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
    theme = {
      name = "Adwaita";
      package = pkgs.gnome-themes-extra;
    };
    gtk2.extraConfig = "gtk-key-theme-name = \"Emacs\"";
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-key-theme-name = "Emacs";
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-key-theme-name = "Emacs";
    };
  };

  home = {
    packages = with pkgs; [
      bemenu
      wlr-randr
      unstable.xwayland-satellite

      # fonts
      cm_unicode
      corefonts
      font-awesome
      jetbrains-mono
      lmodern
      kochi-substitute
      wqy_zenhei
    ];
  };
  
  programs = {
    ghostty = {
      enable = true;
      enableFishIntegration = true;
      settings = {
        font-family = "${term-font-family}";
        font-size = term-font-size;
        cursor-style = "block";
        cursor-style-blink = false;
        shell-integration-features = "no-cursor";
        theme = "Builtin Dark";
        link-url = true;
        gtk-single-instance = true;
        linux-cgroup = "single-instance";
      };
    };
    
    niri = {
      enable = true;
      settings = {
        animations.enable = false;
        gestures.hot-corners.enable = false;
        hotkey-overlay.skip-at-startup = true;
        prefer-no-csd = true;
        cursor.theme = "Adwaita";
        environment = {
#          SDL_VIDEODRIVER = "wayland";
          # needs qt5.qtwayland in systemPackages
          QT_QPA_PLATFORM = "wayland";
          QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
          _JAVA_AWT_WM_NONREPARENTING = "1";
        };
        spawn-at-startup = [
          { command = [ "slack" ]; }
          { command = [ "Telegram" ]; }
          { command = [ "spotify" ]; }
        ];
        input = {
          keyboard.xkb = {
            layout = "us,ru";
            variant = "colemak,";
            options = "grp:shifts_toggle,lv3:ralt_switch_multikey,compose:prsc,caps:ctrl_modifier";
          };
          mouse = {
            accel-profile = "adaptive";
            accel-speed = 0.5;
          };
          touchpad = {
            dwt = true;
            dwtp = true;
            natural-scroll = true;
            accel-profile = "adaptive";
            scroll-method = "two-finger";
          };
          trackball = {
            natural-scroll = true;
          };
          trackpoint = {
            natural-scroll = true;
            accel-profile = "flat";
          };
          focus-follows-mouse = {
            enable = true;
            max-scroll-amount = "0%";
          };
        };
        layout = {
          gaps = 4;
          tab-indicator.place-within-column = true;
          default-column-width.fixed = 640;
          preset-column-widths = [
            { proportion = 1. / 3.; }
            { proportion = 1. / 2.; }
          ];
        };
        binds = with config.lib.niri.actions; let
          left  = "a";
          down  = "r";
          up    = "s";
          right = "t";
          home  = "n";
          pgdn  = "e";
          pgup  = "i";
          end   = "o";
          input-volume = spawn "swayosd-client" "--input-volume";
          output-volume = spawn "swayosd-client" "--output-volume";
          pctl = spawn "swayosd-client" "--playerctl";
        in {
          "XF86MonBrightnessDown".action = spawn "light" "-U" "5";
          "XF86MonBrightnessUp".action = spawn "light" "-A" "5";
          "Mod+multi_key".action.switch-layout = "next";
                    
          # media
          "XF86AudioMute".action                   = output-volume "mute-toggle";
          "XF86AudioMute".allow-when-locked        = true;
          "Mod+d".action                           = output-volume "mute-toggle";
          "Mod+d".allow-when-locked                = true;
          "XF86AudioMicMute".action                = input-volume "mute-toggle";
          "XF86AudioMicMute".allow-when-locked     = true;
          "XF86AudioRecord".action                 = input-volume "mute-toggle";
          "XF86AudioRecord".allow-when-locked      = true;
          "Mod+b".action                           = input-volume "mute-toggle";
          "Mod+b".allow-when-locked                = true;
          "XF86AudioPlay".action                   = pctl "play-pause";
          "XF86AudioPlay".allow-when-locked        = true;
          "Mod+j".action                           = pctl "play-pause";
          "Mod+j".allow-when-locked                = true;
          "XF86AudioRaiseVolume".action            = output-volume "raise";
          "XF86AudioRaiseVolume".allow-when-locked = true;
          "Mod+h".action                           = output-volume "raise";
          "Mod+h".allow-when-locked                = true;
          "XF86AudioLowerVolume".action            = output-volume "lower";
          "XF86AudioLowerVolume".allow-when-locked = true;
          "Mod+k".action                           = output-volume "lower";
          "Mod+k".allow-when-locked                = true;
          "XF86AudioPrev".action                   = pctl "previous";
          "XF86AudioPrev".allow-when-locked        = true;
          "Mod+Ctrl+h".action                      = pctl "previous";
          "Mod+Ctrl+h".allow-when-locked           = true;
          "XF86AudioNext".action                   = pctl "next";
          "XF86AudioNext".allow-when-locked        = true;
          "Mod+Ctrl+k".action                      = pctl "next";
          "Mod+Ctrl+k".allow-when-locked           = true;
          # launchers
          "Mod+z".action = spawn (lib.strings.splitString " " cmd_lock);
          "Mod+x".action = spawn "bemenu-run";
          "Mod+c".action = spawn "emacsclient" "-cn";
          "Mod+v".action.screenshot = { show-pointer =  false; };
          "Mod+Ctrl+z".action = spawn "systemctl" "suspend";
          "Mod+Ctrl+z".allow-when-locked = true;
          "Mod+Ctrl+c".action = close-window;
          
          # focus workspaces
          "Mod+q".action.focus-workspace = 1;
          "Mod+w".action.focus-workspace = 2;
          "Mod+f".action.focus-workspace = 3;
          "Mod+p".action.focus-workspace = 4;
          "Mod+g".action.focus-workspace = 5;
          # focus windows/columns
          "Mod+${left}".action  = focus-column-or-monitor-left;
          "Mod+${down}".action  = focus-workspace-down;
          "Mod+${up}".action    = focus-workspace-up;
          "Mod+${right}".action = focus-column-or-monitor-right;
          "Mod+${home}".action  = focus-column-first;
          "Mod+${pgdn}".action  = focus-window-down-or-top;
          "Mod+${pgup}".action  = focus-window-up-or-bottom;
          "Mod+${end}".action   = focus-column-last;
          # resize windows/columns
          "Mod+l".action.set-column-width         = "-2%";
          "Mod+u".action.set-window-height        = "-2%";
          "Mod+y".action.set-window-height        = "+2%";
          "Mod+semicolon".action.set-column-width = "+2%";
          "Mod+m".action      = switch-preset-column-width;
          "Mod+comma".action  = toggle-column-tabbed-display;
          "Mod+period".action = toggle-window-floating;
          "Mod+slash".action  = maximize-column;
          
          # move columns to workspaces
          "Mod+Ctrl+q".action.move-column-to-workspace = 1;
          "Mod+Ctrl+w".action.move-column-to-workspace = 2;
          "Mod+Ctrl+f".action.move-column-to-workspace = 3;
          "Mod+Ctrl+p".action.move-column-to-workspace = 4;
          "Mod+Ctrl+g".action.move-column-to-workspace = 5;
          # move workspaces
          "Mod+Ctrl+l".action         = move-workspace-to-monitor-left;
          "Mod+Ctrl+u".action         = move-workspace-down;
          "Mod+Ctrl+y".action         = move-workspace-up;
          "Mod+Ctrl+semicolon".action = move-workspace-to-monitor-right;
          # move columns
          "Mod+Ctrl+${left}".action  = move-column-left-or-to-monitor-left;
          "Mod+Ctrl+${down}".action  = move-column-to-workspace-down;
          "Mod+Ctrl+${up}".action    = move-column-to-workspace-up;
          "Mod+Ctrl+${right}".action = move-column-right-or-to-monitor-right;
          # move windows
          "Mod+Ctrl+${home}".action = consume-or-expel-window-left;
          "Mod+Ctrl+${pgdn}".action = move-window-down;
          "Mod+Ctrl+${pgup}".action = move-window-up;
          "Mod+Ctrl+${end}".action  = consume-or-expel-window-right;
        };
        layer-rules = [
          {
            matches = [ { namespace = "^notifications$"; } ];
            block-out-from = "screencast";
          }
        ];
        window-rules = [
          {
            matches = [
              { app-id = "^Slack$"; }
              { app-id = "^org.telegram.desktop$"; }
              { app-id = "^spotify$"; }
            ];
            block-out-from = "screencast";
          }
        ];
      };
    };

    waybar = {
      enable = true;
      systemd.enable = true;
      settings = {
        mainBar = {
          height = 10;
          spacing = 0;
          layer = "top";
          modules-left = [
            "niri/workspaces"
          ];
          modules-center = [
            "niri/window"
          ];
          modules-right = [
            "tray"
            "niri/language"
            "clock"
          ];
          "niri/workspaces" = {
            current-only = true;
          };
          "niri/language" = {
            format = "{short}";
          };
          "clock" = {
            format = "{:L%a %b %d %R}";
          };
        };
      };
      style = ''
* {
  border: none;
  border-radius: 0;
  font-family: Jetbrains Mono, FontAwesome;
  font-size: 12;
  min-height: 0;
  padding: 0;
}

#window, #workspaces {
    background: #285577;
    border: 1px solid #4c7899;
    color: #ffffff;
    padding: 1px;
}

#clock, #language {
    background: #222222;
    border: 1px solid #555555;
    padding: 0 2px;
}
      '';
    };
  };

  services = {
    batsignal.enable = true;
    
    gammastep = {
      enable = true;
      latitude = "48.8566";
      longitude = "2.3522";
      provider = "manual";
    };

    kanshi = {
      enable = true;
#      systemdTarget = "graphical-session.target";
      settings = [
        {
          profile.name = "on-the-go";
          profile.outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
              scale = 2.0;              
            }
          ];
        }
        {
          profile.name = "home-office";
          profile.outputs = [
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
        }
        {
          profile.name = "desktop";
          profile.outputs = [
            {
              criteria = "Lenovo Group Limited T32p-30 V30AKM70";
              position = "0,0";
              scale = 2.0;
            }
          ];
        }        
      ];
    };

    mako = {
      enable = true;
      settings = {
        font = "${term-font}";
        default-timeout = 5000;       
      };
    };
    
    swayidle = {
      enable = true;
      events = [
        { event = "before-sleep"; command = "${cmd_lock}"; }
      ];
      timeouts = [
        { timeout = 300; command = "${cmd_lock}"; }
      ];
    };

    swayosd.enable = true;
  };
}
