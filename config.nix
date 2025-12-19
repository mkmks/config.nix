{
  allowUnfree = true;

  packageOverrides = pkgs : {
                          idea-community = pkgs.jetbrains.idea-community.override {
                            jdk = pkgs.openjdk11;
                          };
  };
}
