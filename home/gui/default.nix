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
      unstable.telegram-desktop
      
      baobab
      dconf-editor
      pwvucontrol
      seahorse
      
      papers
      gnome-maps
      gthumb
#      krop
      loupe
      nautilus
      
      ledger-live-desktop
      unstable.sparrow # btc
      framesh # eth
      feather # xmr
      unstable.eigenwallet
    ];
  };  
  
  programs = {
    brave = {
      enable = true;
      package = (pkgs.brave.override {
        commandLineArgs = [
          "--enable-features=AcceleratedVideoEncoder"
          "--ignore-gpu-blocklist"
          "--enable-zero-copy"
        ];
      });
      extensions = [
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # bitwarden
        { id = "dbepggeogbaibhgnhhndojpepiihcmeb"; } # vimium
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # dark reader
        { id = "nibjojkomfdiaoajekhjakgkdhaomnch"; } # ipfs companion
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
        "application/pdf" = [ "org.gnome.Papers.desktop" ];
        "application/epub+zip" = [ "com.github.johnfactotum.Foliate.desktop" ];
        "image/gif" = [ "org.gnome.Loupe.desktop" ];
        "image/jpeg" = [ "org.gnome.Loupe.desktop" ];
        "image/png" = [ "org.gnome.Loupe.desktop" ];
        "image/vnd.djvu" = [ "org.gnome.Papers.desktop" ];
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
