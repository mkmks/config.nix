{pkgs, ...}:

{
  home.packages = with pkgs; [
    rnix-lsp
    python-language-server
    nodePackages.bash-language-server
    # ops
    aws
    aws-google-auth
    aws-iam-authenticator
    helm
    kubectl
  ];

  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    emacs.extraPackages = e: with e; [
      company
      direnv
      flycheck
      helm-lsp
	    helm-projectile      
      magit
	    projectile      
      # LSP
      dap-mode
      lsp-mode
	    lsp-ui
	    lsp-treemacs
      # programming languages
      elm-mode
      nix-mode
      protobuf-mode
      sql-clickhouse
      toml-mode
      typescript-mode
      ## java
      lsp-java
      ## haskell
      flycheck-haskell
      haskell-mode
      lsp-haskell
      ## ocaml
      merlin
      tuareg
      ## rust
      cargo-mode
      flycheck-rust
      rustic
      ## scala
      lsp-metals
	    sbt-mode
	    scala-mode
    ];
    
    git = {
      enable = true;
      lfs.enable = true;
      userName = "Nikita Frolov";
      userEmail = "nf@mkmks.org";
    };

    vscode = {
      enable = true;
      extensions = with pkgs.vscode-extensions; [
        haskell.haskell
        justusadam.language-haskell
        ms-vsliveshare.vsliveshare
        ocamllabs.ocaml-platform
        scala-lang.scala
        scalameta.metals
      ];
    };
  };

  services.lorri.enable = true;
}
