{pkgs, ...}:

{
  home.sessionVariables = {
    EDITOR = "emacsclient -c";    
  };
    
  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtkNativeComp;
    extraPackages = e: with e; [
      vs-dark-theme
      vs-light-theme
      use-package
	    diminish
	    bind-key
      which-key      
	    pretty-mode
      ag
	    helm
      helm-ag
      posframe
      exec-path-from-shell
      sway
      shackle
      frames-only-mode
      # apps
      flycheck-hledger
      hledger-mode
	    nov
      slack
      smudge
      melpaStablePackages.telega
      restclient
      vterm
      multi-vterm
    ];
  };

  services.emacs.enable = true;
}
