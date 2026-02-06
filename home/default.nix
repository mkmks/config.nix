{ ... }:

{
  imports = [
    ./devel.nix
    ./gui
    ./mail.nix
    ./research.nix
    ./term
  ];

  home = {
    username = "viv";
    homeDirectory = "/home/viv";
    stateVersion = "24.05";
  };
}
