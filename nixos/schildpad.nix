{ config, pkgs, ... }:

{
  networking.hostName = "schildpad";
  nixpkgs.hostPlatform = "x86_64-linux";
  system.stateVersion = "23.05";

  nix = {
    settings = {
      max-jobs = 8;
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
      trusted-substituters = [
        "https://nix-community.cachix.org"
        "https://cache.iog.io"
      ];
    };
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true 
      experimental-features = nix-command flakes
    '';
  };
 
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    tmp.cleanOnBoot = true;

    kernelPackages = pkgs.linuxPackages_latest;
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

  swapDevices = [ { device = "/var/swapfile"; } ];

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
    
    opengl = {
      enable = true;
      driSupport = true;
      extraPackages = with pkgs; [ vaapiIntel vaapiVdpau libvdpau-va-gl ];  
    };
  };

  netkit.xmm7360 = {
    enable = false;
    autoStart = true;
    config = {
      apn = "orange";
      nodefaultroute = false;
      noresolv = true;
    };
    package = pkgs.netkit.xmm7360-pci_latest;
  };  
  
  environment.variables = {
    MESA_LOADER_DRIVER_OVERRIDE = "iris";
  };    
    
  services.fwupd.enable = true;

  users.users.viv = {
    description = "Nikita Frolov";
    extraGroups = [ "wheel" "transmission" "adbusers" "dialout" "docker" "video" ];
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.fish;
  };
  
}
