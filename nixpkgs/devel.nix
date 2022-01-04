{pkgs, ...}:

let
  unstable = import <nixpkgs-unstable> {};
in
{
  home = {    
    packages = with pkgs; [
      vscode-with-extensions

      #haskell.packages.ghc8102.ghcWithPackages (pkgs: with pkgs; [ cabal-install haskell-language-server ])
      haskellPackages.Agda
      
      # language servers
      rnix-lsp
      unstable.haskell-language-server
      lua53Packages.digestif
      #     python37Packages.python-language-server
      nodePackages.bash-language-server
      nodePackages.typescript
      nodePackages.typescript-language-server
      unstable.metals

      # build
      haskellPackages.cabal-install
      maven
      unstable.bloop
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

  programs.git = {
    enable = true;
    lfs.enable = true;
    userName = "Nikita Frolov";
    userEmail = "nf@mkmks.org";
  };
}
