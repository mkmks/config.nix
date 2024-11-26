{pkgs, ...}:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = e: with e; [
      vs-dark-theme
      vs-light-theme
	    diminish
	    bind-key
      which-key      
	    pretty-mode
      ag
      ivy
      ivy-posframe
      ivy-xref
      counsel
      counsel-at-point
      counsel-projectile
      swiper
      posframe
      exec-path-from-shell
      sway
      shackle
      frames-only-mode
      (treesit-grammars.with-grammars (g: with g; [
        tree-sitter-bash
        tree-sitter-bibtex
        tree-sitter-c
        tree-sitter-cmake
        tree-sitter-cpp
        tree-sitter-dockerfile
        tree-sitter-elisp
        tree-sitter-elm
        tree-sitter-fish
        tree-sitter-go
        tree-sitter-haskell
        tree-sitter-java
        tree-sitter-javascript
        tree-sitter-json
        tree-sitter-latex
        tree-sitter-llvm
        tree-sitter-make
        tree-sitter-markdown
        tree-sitter-nix
        tree-sitter-ocaml
        tree-sitter-python
        tree-sitter-rust
        tree-sitter-scala
        tree-sitter-solidity
        tree-sitter-sql
        tree-sitter-toml
        tree-sitter-typescript
        tree-sitter-yaml
        tree-sitter-zig
      ]))
      treesit-auto
      # apps
      eat
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

  services.emacs = {
    enable = true;
    defaultEditor = true;
    startWithUserSession = "graphical";
  };
}
