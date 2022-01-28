{pkgs, ...}:

{
  home.packages = with pkgs; [
    # language servers
    rnix-lsp
    #     python37Packages.python-language-server
    nodePackages.bash-language-server
    nodePackages.typescript
    nodePackages.typescript-language-server

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
      # programming languages
      nix-mode
      protobuf-mode
      sql-clickhouse
      toml-mode
      typescript-mode
      # language server protocol
      dap-mode
      lsp-java
      lsp-mode
	    lsp-ui
	    lsp-treemacs      
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
        ms-vsliveshare.vsliveshare
      ];
    };
  };

  services.lorri.enable = true;
}
