{ config, lib, pkgs, ... }:

{
  imports = [
    ../.
    ../../../blockchain.nix
  ];

  networking.hostName = "hivemind";
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  nixpkgs.config.cudaSupport = true;
  system.stateVersion = "25.11";


  boot = {
    initrd = {
      availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
      luks.devices."nixos".device = "/dev/disk/by-uuid/27eace65-e9e4-4e26-bd67-1666079bfbe3";
    };
  };
  
  fileSystems = {
    "/" =
      { device = "/dev/disk/by-uuid/a85e4148-655f-451e-a6df-a91585f11cde";
        fsType = "ext4";
      };

    "/boot" =
      { device = "/dev/disk/by-uuid/5323-7111";
        fsType = "vfat";
        options = [ "fmask=0077" "dmask=0077" ];
      };
  };


  hardware = {
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    graphics.extraPackages = with pkgs; [
      nvidia-vaapi-driver
      libva-vdpau-driver
      libvdpau-va-gl
    ];
    nvidia = {
      modesetting.enable = true;
      powerManagement = {
        enable = true;
        finegrained = false;
      };
      open = true;
      nvidiaSettings = true;
    };
  };

  services = {
    ollama = {
      enable = true;
      loadModels = [
        "deepseek-r1:14b"
      ];
    };
    xserver.videoDrivers = [ "nvidia" ];
  }; 
}
