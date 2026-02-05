{ config, lib, pkgs, ... }:

{
  imports = [
    ./workstation.nix
];
  
  services = {
    minidlna = {
      enable = true;
      openFirewall = true;
      settings = {
        inotify = "yes";
        media_dir = [
          "${config.services.syncthing.dataDir}/Music"
          "${config.services.transmission.settings.download-dir}"
        ];
      };
    };
    openssh.enable = true;
  };
  
  users.users.minidlna.extraGroups = [
    "syncthing"
    "transmission"
  ];
}
