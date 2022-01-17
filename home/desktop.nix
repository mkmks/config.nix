{config, pkgs, ...}:

{
  home = {  
    packages = let
      concordium-desktop-wallet = pkgs.callPackage ../pkgs/concordium-desktop-wallet-testnet {};

    in with pkgs; [
      android-file-transfer
      libreoffice
      skypeforlinux
      unstable.tdesktop
      zoom-us
      
      gnome3.baobab
      gnome3.dconf-editor
      gnome3.seahorse
      pavucontrol
      
      gnome3.eog
      gnome3.evince
      gthumb
      
      gnuplot
      xfig

      concordium-desktop-wallet
      unstable.ledger-live-desktop
    ];

  };  
  
  programs = {
    chromium = {
      enable = true;
      extensions = [
        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
        { id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp"; } # privacy badger
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # bitwarden
        { id = "fnaicdffflnofjppbagibeoednhnbjhg"; } # floccus
        { id = "dbepggeogbaibhgnhhndojpepiihcmeb"; } # vimium
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # dark reader
      ];
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
