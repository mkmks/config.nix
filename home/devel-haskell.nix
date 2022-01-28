{pkgs, ...}:

{
  home.packages = with pkgs; [
    cabal-install
    cabal2nix
  ];

  programs = {    
    emacs.extraPackages = e: with e; [
      flycheck-haskell
      haskell-mode
      lsp-haskell
    ];

    vscode.extensions = with pkgs.vscode-extensions; [
      haskell.haskell
      justusadam.language-haskell            
    ];
  };
}
