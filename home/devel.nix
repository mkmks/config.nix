{pkgs, ...}:

{
  home.packages = with pkgs; [
#    gcc
    bash-language-server
    cmake-language-server
    copilot-language-server
    ltrace
    nil
    perf-tools
#    python311Packages.python-lsp-server
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
      copilot
      copilot-chat
      direnv
      ein
      envrc
      flycheck
      flycheck-eglot
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
      settings.user = {
        email = "nf@mkmks.org";
        name = "Nikita Frolov";
      };
    };

    helix = {
      enable = true;
    };
    
    vscode = {
      enable = true;
      profiles.default.extensions = with pkgs.vscode-extensions; [
        justusadam.language-haskell
        mkhl.direnv
        ms-vsliveshare.vsliveshare
        ocamllabs.ocaml-platform
        rust-lang.rust-analyzer
        scala-lang.scala
        scalameta.metals
      ];
    };
  };

  services.lorri.enable = true;
}
