{pkgs, ...}:

#with import <nixpkgs> {};

{
  accounts.email = {
    certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
    maildirBasePath = "Mail";
    
    accounts.fastmail = {
      primary = true;

      address = "nf@mkmks.org";
      realName = "Nikita Frolov";
      userName = "nf@mkmks.org";
      passwordCommand = "secret-tool lookup email nf@mkmks.org";
      
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
  
  programs = {

    fish = {
      enable = true;

      interactiveShellInit = ''
set fish_greeting

function fish_prompt
  set -l nix_shell_info (
    if test -n "$IN_NIX_SHELL"
      echo -n "<nix-shell> "
    end
  )
  echo -n -s $nix_shell_info '$ '
end

function fish_title
    echo $USER'@'(hostname)':'$PWD'$' $argv[1]
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
      userName = "Nikita Frolov";
      userEmail = "nf@mkmks.org";
    };

    mbsync.enable = true;
    msmtp.enable = true;
    
    feh.enable = true;
    
    mpv = {
      enable = true;
      config = {
        gpu-context = "wayland";
      };
    };
    
    termite = {
      enable = true;

      cursorBlink = "off";
      scrollbar = "off";
      
      clickableUrl = true;
      dynamicTitle = true;

      browser = "${pkgs.firefox}/firefox";

      foregroundColor = "white";
      backgroundColor = "black";
      cursorColor = "#cccccc";
    };

    zathura = {
      enable = true;
      options = {
        font = "Monospace 8";
        window-title-basename = true;
        window-title-page = true;
        guioptions = "";
      };
    };
  };

  services = {
    udiskie.enable = true;
  };

  systemd.user.sessionVariables = {
    EDITOR = "emacsclient -cn";
    ALTERNATIVE_EDITOR = "mg -n";
    SDCV_PAGER = "less -R";
  };
  
}
