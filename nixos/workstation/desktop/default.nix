{ lib, pkgs, ... }:

{
  imports = [
    ../. 
];

  networking.useDHCP = lib.mkDefault true;

  services.openssh.enable = true;
}
