{config, pkgs, ...}:

{
  imports = [
    ./wayland.nix
  ];
  
  home = {  
    packages = with pkgs; [
      android-file-transfer
      libreoffice
      slack
      unstable.spotify
      unstable.tdesktop
      zoom-us
      
      gnome.baobab
      gnome.dconf-editor
      gnome.seahorse
      pavucontrol
      
      gnome.eog
      gnome.evince
      gnome.gnome-maps
      gthumb
      krop
      
      unstable.ledger-live-desktop
      sparrow
      unstable.tradingview
    ];
  };  
  
  programs = {
    chromium = {
      enable = true;
      package = pkgs.brave;
      extensions = [
#        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
#        { id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp"; } # privacy badger
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # bitwarden
        { id = "fnaicdffflnofjppbagibeoednhnbjhg"; } # floccus
        { id = "dbepggeogbaibhgnhhndojpepiihcmeb"; } # vimium
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # dark reader
        { id = "kmhcihpebfmpgmihbkipmjlmmioameka"; } # eternl
        { id = "mcohilncbfahbmgdjkbpemcciiolgcge"; } # okx wallet
      ];
    };    

    firefox = {
      enable = true;
      package = pkgs.firefox-wayland;
      profiles.default = {
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          ublock-origin
          privacy-badger
          bitwarden
          floccus
          vimium
          darkreader
          adsum-notabs
        ];
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        };
        userChrome = ''
#TabsToolbar { visibility: collapse !important;  }
'';
      };
    };
    
    mpv = {
      enable = true;
      config = {
        gpu-context = "wayland";
        hwdec = "vaapi";
        vo = "gpu-next";
      };
    };
    
    zathura = {
      enable = true;
      options = {
        font = "DejaVu Sans Mono 9";
        window-title-basename = true;
        window-title-page = true;
        guioptions = "";
      };
    };
  };

  services.gnome-keyring.enable = true;

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
        "text/html" = [ "brave.desktop" ];
        "x-scheme-handler/http" = [ "brave.desktop" ];
        "x-scheme-handler/https" = [ "brave.desktop" ];
        "x-scheme-handler/chrome" = [ "brave.desktop" ];
        "x-scheme-handler/webcal" = [ "brave.desktop" ];        
      };
    };
    userDirs.enable = true;
  };
  
}
