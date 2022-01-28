{pkgs, ...}:

{
  programs = {    
    emacs.extraPackages = e: with e; [
      merlin
      tuareg
    ];
    
    vscode.extensions = with pkgs.vscode-extensions; [
      ocamllabs.ocaml-platform
    ];
  };
}
