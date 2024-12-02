{pkgs, ...}:

{
  home.packages = with pkgs; [
    gcc
    cmake-language-server
    ltrace
    nil
    perf-tools
    python311Packages.python-lsp-server
    nodePackages.bash-language-server
    # ops
    argocd
    awscli2
    aws-iam-authenticator
    eksctl
    k9s
    kubectl
    kubectx
    kubernetes-helm
    terraform
    # ups
    stack
    rustup
  ];

  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    emacs.extraPackages = e: with e; [
      company
      direnv
      ein
      envrc
      flycheck
      magit
	    projectile
      nix-buffer
      # programming languages
      capnp-mode
      cmake-mode
      dockerfile-mode
      elm-mode
      nix-mode
      protobuf-mode
      sql-clickhouse
      terraform-mode
      toml-mode
      typescript-mode
      yaml-mode
      zig-mode
      ## haskell
      flycheck-haskell
      haskell-mode
      ## ocaml
      merlin
      tuareg
      ## rust
      cargo-mode
      flycheck-rust
      rustic
      ## scala
	    sbt-mode
	    scala-mode
      ## solidity
      solidity-flycheck
      solidity-mode
    ];
    
    git = {
      enable = true;
      lfs.enable = true;
      userName = "Nikita Frolov";
      userEmail = "nf@mkmks.org";
    };

    helix = {
      enable = true;
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
