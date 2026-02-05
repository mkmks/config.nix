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
      spotify
      telegram-desktop
      
      baobab
      dconf-editor
      pwvucontrol
      seahorse
      
      papers
      gnome-maps
      gthumb
      krop
      loupe
      nautilus
      
      tradingview
      ledger-live-desktop
      sparrow # btc
      framesh # eth
      feather # xmr
    ];
  };  
  
  programs = {
    brave = {
      enable = true;
      extensions = [
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # bitwarden
        { id = "dbepggeogbaibhgnhhndojpepiihcmeb"; } # vimium
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # dark reader
        { id = "ldcoohedfbjoobcadoglnnmmfbdlmmhf"; } # frame.sh
        { id = "gafhhkghbfjjkeiendhlofajokpaflmk"; } # lace
        { id = "kmhcihpebfmpgmihbkipmjlmmioameka"; } # eternl
      ];
    };    

    foliate.enable = true;
    lutris.enable = true;
    
    mpv = {
      enable = true;
      config = {
        gpu-context = "wayland";
        hwdec = "vaapi";
        vo = "gpu-next";
      };
    };    
  };

  services = {
    blueman-applet.enable = true;
    gnome-keyring.enable = true;
    network-manager-applet.enable = true;
  };

  xdg = {
    enable = true;
    mime.enable = true;
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = [ "papers.desktop" ];
        "application/epub+zip" = [ "foliate.desktop" ];
        "image/vnd.djvu" = [ "papers.desktop" ];
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
