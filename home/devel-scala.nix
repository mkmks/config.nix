{pkgs, ...}:

{
  home.packages = with pkgs; [
    metals
    bloop
    maven
    sbt
    scalafmt    
  ];

  programs = {    
    emacs.extraPackages = e: with e; [
	    scala-mode
	    sbt-mode
      lsp-metals    
    ];

    vscode.extensions = with pkgs.vscode-extensions; [
      scala-lang.scala
      scalameta.metals
    ];
  };
}
