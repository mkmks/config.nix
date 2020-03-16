{
  allowUnfree = true;

  packageOverrides = pkgs : {
                          dwm = pkgs.dwm.override {
                            patches = [ ./dwm-config.diff ];
                     	    };
                          st = pkgs.st.override {
                            patches = [ ./st-config.diff ];
                          };
                          idea-community = pkgs.jetbrains.idea-community.override {
                            jdk = pkgs.openjdk11;
                          };
  };

}
