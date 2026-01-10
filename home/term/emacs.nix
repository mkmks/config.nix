{pkgs, ...}:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = e: with e; [
      catppuccin-theme
      vs-dark-theme
      vs-light-theme
	    diminish
	    bind-key
      which-key      
	    pretty-mode
      ag
      ellama
      evil
      ivy
      ivy-posframe
      ivy-xref
      counsel
      counsel-at-point
      counsel-fd
      counsel-projectile
      swiper
      posframe
      exec-path-from-shell
      frames-only-mode
      treesit-grammars.with-all-grammars
      treesit-auto
      # apps
      eat
      flycheck-hledger
      hledger-mode
	    nov
      pdf-tools
#      slack
      smudge
      melpaStablePackages.telega
      restclient
      vterm
      multi-vterm
    ];
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    startWithUserSession = "graphical";
  };
}
