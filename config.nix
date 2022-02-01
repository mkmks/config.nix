{
  allowUnfree = true;

  packageOverrides = pkgs : {
                          idea-community = pkgs.jetbrains.idea-community.override {
                            jdk = pkgs.openjdk11;
                          };
  };

  chromium.commandLineArgs = "--enable-features=UseOzonePlatform --ozone-platform=wayland --disable-gpu-memory-buffer-video-frames";
}
