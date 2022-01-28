{config, pkgs, ...}:

{
  home = {  
    packages = let
      concordium-desktop-wallet = pkgs.callPackage ../pkgs/concordium-desktop-wallet-testnet {};
    in with pkgs; [
      android-file-transfer
      libreoffice
      unstable.tdesktop
      zoom-us
      
      gnome3.baobab
      gnome3.dconf-editor
      gnome3.seahorse
      pavucontrol
      
      gnome3.eog
      gnome3.evince
      gthumb
      
      concordium-desktop-wallet
      unstable.ledger-live-desktop
    ];
  };  
  
  programs = {
    chromium = {
      enable = true;
      package = pkgs.ungoogled-chromium;
      extensions = [
        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
        { id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp"; } # privacy badger
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # bitwarden
        { id = "fnaicdffflnofjppbagibeoednhnbjhg"; } # floccus
        { id = "dbepggeogbaibhgnhhndojpepiihcmeb"; } # vimium
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # dark reader
      ];
    };    

    firefox = {
      enable = true;
      package = pkgs.firefox-wayland;
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        ublock-origin
        privacy-badger
        bitwarden
        floccus
        vimium
        darkreader
        adsum-notabs
      ];
      profiles.default = {
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
        "text/html" = [ "firefox.desktop" ];
        "x-scheme-handler/http" = [ "firefox.desktop" ];
        "x-scheme-handler/https" = [ "firefox.desktop" ];
        "x-scheme-handler/chrome" = [ "firefox.desktop" ];
        "x-scheme-handler/webcal" = [ "firefox.desktop" ];        
      };
    };
    userDirs.enable = true;
  };
  
}
