{
  allowUnfree = true;

  packageOverrides = pkgs : {
                          idea-community = pkgs.jetbrains.idea-community.override {
                            jdk = pkgs.openjdk11;
                          };
                          vscode-with-extensions = pkgs.vscode-with-extensions.override {
                            vscodeExtensions = with pkgs.vscode-extensions; [
                              ms-vsliveshare.vsliveshare
                              ocamllabs.ocaml-platform
                            ]; };
  };

}
