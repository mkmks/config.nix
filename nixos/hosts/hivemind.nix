{ config, lib, pkgs, ... }:

{
  imports = [
    ../desktop.nix
  ];

  networking.hostName = "hivemind";
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  nixpkgs.config.cudaSupport = true;
  system.stateVersion = "25.11";


  boot = {
    initrd = {
      availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
      luks.devices = {
        "nixos-rootfs".device = "/dev/disk/by-uuid/27eace65-e9e4-4e26-bd67-1666079bfbe3";
        "nixos-varlib".device = "/dev/disk/by-uuid/35b77669-dd1c-425d-a6cc-2f7b477c1b03";
      };
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
    "/var/lib" = {
      device = "/dev/disk/by-uuid/49d57d03-13ce-4d7e-bb54-132bae6c499d";
      fsType = "ext4";
    };
  };


  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
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

  networking.interfaces."enp128s31f6".useDHCP = true;
  networking.firewall.allowedTCPPorts = [
    11435 # llama.cpp
  ];
  services = {
    llama-cpp = {
      enable = true;
      openFirewall = true;
      port = 11435;
      modelsPreset = {
        "gpt-oss-20b" = {
          alias = "gpt-oss-20b";
          hf-repo = "unsloth/gpt-oss-20b-GGUF:UD-Q4_K_XL";
          ctx-size = "131072";
          temp = "1.0";
          top-p = "1.0";
          top-k = "0";
        };
        "gpt-oss-120b" = {
          alias = "gpt-oss-120b";
          hf-repo = "unsloth/gpt-oss-120b-GGUF:UD-Q4_K_XL";
          ctx-size = "131072";
          temp = "1.0";
          top-p = "1.0";
          top-k = "0";
          chat-template-kwargs = "{\"reasoning_effort\":\"high\"}";
        };
        "glm-4.7-flash" = {
          alias = "glm-4.7-flash";
          hf-repo = "unsloth/GLM-4.7-Flash-GGUF:UD-Q4_K_XL";
          ctx-size = "202752";
          temp = "0.7";
          top-p = "1.0";
          min-p = "0.01";
          repeat-penalty = "1.0";
        };
        "qwen3-coder-next" = {
          alias = "qwen3-coder-next";
          hf-repo = "unsloth/Qwen3-Coder-Next-GGUF:Q4_K_XL";
          ctx-size = "262144";
          temp = "1.0";
          top-p = "0.95";
          min-p = "0.01";
          top-k = "40";
          repeat-penalty = "1.0";
          presence-penalty = "0.0";
        };
        "qwen3.6-35b-a3b" = {
          alias = "qwen3.6-35b-a3b";
          hf-repo = "unsloth/Qwen3.6-35B-A3B-GGUF:UD-Q8_K_XL";
          no-mmap = "true";
          ctx-size = "262144";
          temp = "0.6";
          top-p = "0.95";
          min-p = "0.0";
          top-k = "20";
          repeat-penalty = "1.0";
          presence-penalty = "0.0";          
        };
        "qwen3.8-27b" = {
          alias = "qwen3.8-27b";
          hf-repo = "unsloth/Qwen3.8-27B-GGUF:Q4_K_XL";
          ctx-size = "262144";
          temp = "1.0";
          top-p = "0.95";
          min-p = "0.0";
          top-k = "20";
          repeat-penalty = "1.0";
          presence-penalty = "0.0";
          chat-template-kwargs = "{\"reasoning_effort\":\"medium\"}";
        };
        "qwen3.8-27b-uncensored" = {
          alias = "qwen3.8-27b-uncensored";
          hf-repo = "JonathanColetti/Qwen3.8-27B-Uncensored-GGUF:IQ2_M";
          ctx-size = "262144";
          temp = "1.0";
          top-p = "0.95";
          min-p = "0.0";
          top-k = "20";
          repeat-penalty = "1.0";
          presence-penalty = "0.0";
          chat-template-kwargs = "{\"reasoning_effort\":\"medium\"}";
        };
        "qwen3.8-flash-next" = {
          alias = "qwen3.8-flash-next";
          hf-repo = "unsloth/Qwen3.8-Flash-Next-GGUF:UD-Q2_K_XL";
          no-mmap = "true";
          ctx-size = "262144";
          temp = "1.0";
          top-p = "0.95";
          min-p = "0.0";
          top-k = "20";
          repeat-penalty = "1.0";
          presence-penalty = "0.0";
        };
      };
    };
    xserver.videoDrivers = [ "nvidia" ];
  };

  systemd.services.llama-cpp.serviceConfig = {
    StateDirectory = lib.mkForce [ "llama-cpp" "models" ];

    Environment = lib.mkForce [
      "LLAMA_CACHE=/var/lib/models"
    ];
  };
}
