{ lib, pkgs, ... }:

{
  imports = [
    ./workstation.nix
];

  services.openssh.enable = true;
}
