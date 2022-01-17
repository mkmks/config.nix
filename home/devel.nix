{pkgs, ...}:

{
  home = {    
    packages = with pkgs; [
      vscode-with-extensions

      (agda.withPackages (p: with p; [ standard-library ]))
      
      # language servers
      rnix-lsp
      lua53Packages.digestif
      #     python37Packages.python-language-server
      nodePackages.bash-language-server
      nodePackages.typescript
      nodePackages.typescript-language-server
      metals
      rls

      # build
      cabal2nix
      bloop
      maven
      sbt
      scalafmt

      # ops
      aws
      aws-google-auth
      aws-iam-authenticator
      helm
      kubectl
    ];

    sessionVariables = {
      DIGESTIFDATA = "${pkgs.lua53Packages.digestif}/digestif-${pkgs.lua53Packages.digestif.version}-rocks/digestif/${pkgs.lua53Packages.digestif.version}/data";
    };
  };

  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    
    git = {
      enable = true;
      lfs.enable = true;
      userName = "Nikita Frolov";
      userEmail = "nf@mkmks.org";
    };    
  };

  services.lorri.enable = true;
}
