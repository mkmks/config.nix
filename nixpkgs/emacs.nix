{pkgs, ...}:

{
  home.sessionVariables = {
    EDITOR = "emacsclient -c";    
  };
  
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];
  
  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtkGcc;
    extraPackages = e: [
      e.vs-dark-theme
      e.vs-light-theme
      e.use-package
	    e.diminish
	    e.bind-key
	    e.pretty-mode
      e.ag
      e.exec-path-from-shell
      # apps
      e.mu4e-conversation
      e.mu4e-maildirs-extension
	    e.nov
      e.vterm
      e.multi-vterm
      # IDE
      e.company
      e.flycheck
	    e.helm
      e.helm-ag
      e.helm-lsp
	    e.helm-projectile
	    e.magit
	    e.projectile
      e.restclient
      e.which-key
      # programming languages
	    e.haskell-mode
      e.merlin
      e.nix-mode
	    e.scala-mode
	    e.sbt-mode
      e.sql-clickhouse
      e.tuareg
      e.typescript-mode
      # language server protocol
      e.dap-mode
      e.lsp-haskell
      e.lsp-java
      e.lsp-mode
      e.lsp-metals
	    e.lsp-ui
	    e.lsp-treemacs
      e.posframe
    ];
  };

  services.emacs.enable = true;
}
