{config, lib, pkgs, ...}:

let
  term-font-family = "JetBrains Mono";
  term-font-size = 9;
  term-font = "${term-font-family} ${toString term-font-size}";
  cmd_lock = "${pkgs.swaylock}/bin/swaylock -f -c ff0000";
  cmd_display = status: "${pkgs.niri}/bin/niri msg action power-${status}-monitors";
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
        shell-integration-features = "no-cursor,title";
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
            { proportion = 2. / 3.; }
            { proportion = 1. ; }            
          ];
        };
        binds = with config.lib.niri.actions; let
          left  = "a";
          down  = "r";
          up    = "s";
          right = "t";
          home  = "q";
          pgdn  = "w";
          pgup  = "f";
          end   = "p";
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
          "XF86AudioMicMute".action                = input-volume "mute-toggle";
          "XF86AudioMicMute".allow-when-locked     = true;
          "XF86AudioRecord".action                 = input-volume "mute-toggle";
          "XF86AudioRecord".allow-when-locked      = true;
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
          "Mod+x".action = spawn "rofi" "-show" "run";
          "Mod+c".action = spawn "emacsclient" "-cn";
          "Mod+v".action.screenshot = { show-pointer =  false; };
          "Mod+b".action = spawn "rofi" "-show" "window";
          "Mod+Ctrl+z".action = spawn "systemctl" "suspend";
          "Mod+Ctrl+z".allow-when-locked = true;
          "Mod+Ctrl+c".action = close-window;
          # layouts
          "Mod+escape".action = open-overview;
          "Mod+space".action     = switch-preset-column-width;
          "Mod+tab".action       = toggle-column-tabbed-display;
          "Mod+backspace".action = toggle-window-floating;
          "Mod+return".action    = fullscreen-window;          

          # windows/columns
          ## focus
          "Mod+${left}".action  = focus-column-left-or-last;
          "Mod+${down}".action  = focus-window-down-or-top;
          "Mod+${up}".action    = focus-window-up-or-bottom;
          "Mod+${right}".action = focus-column-right-or-first;
          "Mod+${home}".action = focus-column-first;
          "Mod+${end}".action  = focus-column-last;
          ## move
          "Mod+Ctrl+${left}".action  = move-column-left;
          "Mod+Ctrl+${down}".action  = move-window-down;
          "Mod+Ctrl+${up}".action    = move-window-up;
          "Mod+Ctrl+${right}".action = move-column-right;
          "Mod+Ctrl+${home}".action = consume-or-expel-window-left;
          "Mod+Ctrl+${end}".action  = consume-or-expel-window-right;
          ## resize
          "Mod+home".action.set-column-width       = "-2%";
          "Mod+page_down".action.set-window-height = "-2%";
          "Mod+page_up".action.set-window-height   = "+2%";
          "Mod+end".action.set-column-width        = "+2%";

          # workspaces
          ## focus
          "Mod+1".action.focus-workspace = 1;
          "Mod+2".action.focus-workspace = 2;
          "Mod+3".action.focus-workspace = 3;
          "Mod+4".action.focus-workspace = 4;
          "Mod+5".action.focus-workspace = 5;
          "Mod+6".action.focus-workspace = 6;
          "Mod+7".action.focus-workspace = 7;
          "Mod+8".action.focus-workspace = 8;
          "Mod+9".action.focus-workspace = 9;
          "Mod+0".action.focus-workspace = 10;
          "Mod+${pgdn}".action = focus-workspace-down;
          "Mod+${pgup}".action = focus-workspace-up;
          ## move columns
          "Mod+Ctrl+1".action.move-column-to-workspace = 1;
          "Mod+Ctrl+2".action.move-column-to-workspace = 2;
          "Mod+Ctrl+3".action.move-column-to-workspace = 3;
          "Mod+Ctrl+4".action.move-column-to-workspace = 4;
          "Mod+Ctrl+5".action.move-column-to-workspace = 5;
          "Mod+Ctrl+6".action.move-column-to-workspace = 6;
          "Mod+Ctrl+7".action.move-column-to-workspace = 7;
          "Mod+Ctrl+8".action.move-column-to-workspace = 8;
          "Mod+Ctrl+9".action.move-column-to-workspace = 9;
          "Mod+Ctrl+0".action.move-column-to-workspace = 10;
          "Mod+Ctrl+${pgdn}".action = move-column-to-workspace-down;
          "Mod+Ctrl+${pgup}".action = move-column-to-workspace-up;          

          # monitors
          ## focus
          "Mod+left".action  = focus-monitor-left;
          "Mod+down".action  = focus-monitor-down;
          "Mod+up".action    = focus-monitor-up;
          "Mod+right".action = focus-monitor-right;          
          ## move columns
          "Mod+Ctrl+left".action  = move-column-to-monitor-left;
          "Mod+Ctrl+down".action  = move-column-to-monitor-down;
          "Mod+Ctrl+up".action    = move-column-to-monitor-up;
          "Mod+Ctrl+right".action = move-column-to-monitor-right;
          ## move workspaces
          "Mod+Ctrl+home".action      = move-workspace-to-monitor-left;
          "Mod+Ctrl+page_down".action = move-workspace-to-monitor-down;
          "Mod+Ctrl+page_up".action   = move-workspace-to-monitor-up;
          "Mod+Ctrl+end".action       = move-workspace-to-monitor-right;
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
          {
            matches = [
              { app-id = "^frame$"; }
            ];
            open-floating = false;
          }
        ];
      };
    };

    rofi = {
      enable = true;
      font = term-font;
      location = "top";
      terminal = "${pkgs.ghostty}/bin/ghostty";
      theme = "android_notification";
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
      systemdTarget = "graphical-session.target";
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
        { event = "before-sleep"; command = cmd_lock; }
      ];
      timeouts = [
        {
          timeout = 300;
          command = cmd_lock;
        }
        {
          timeout = 600;
          command = cmd_display "off";
          resumeCommand = cmd_display "on";
        }
      ];
    };

    swayosd.enable = true;
  };
}
