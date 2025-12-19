{ config, pkgs, ... }:

{
  imports = [
    ../.
  ];
  
  networking.hostName = "schildpad";
  nixpkgs.hostPlatform = "x86_64-linux";
  system.stateVersion = "24.05";
 
  boot = {
    kernelModules = [ "kvm-intel" "acpi_call" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
    initrd = {
      availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "i915"];
      luks.devices."nixos".device = "/dev/disk/by-uuid/eb231255-b95c-4e57-88ea-eb00b661d434";
    };    
  };

  fileSystems = {
    "/" =
      { device = "/dev/disk/by-uuid/881475f4-e5a4-4c55-8366-4fe394ecbd1e";
        fsType = "ext4";
      };

    "/boot" =
      { device = "/dev/disk/by-uuid/9E26-9A52";
        fsType = "vfat";
      };
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;    
    graphics.extraPackages = with pkgs; [
      intel-vaapi-driver
      libva-vdpau-driver
      libvdpau-va-gl ];
  };
  
  environment.variables = {
    MESA_LOADER_DRIVER_OVERRIDE = "iris";
  };

  services.swapspace.enable = true;  
}
