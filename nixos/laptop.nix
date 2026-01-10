{ pkgs, ... }:

{
  imports = [
    ./workstation.nix
  ];
  
  environment.systemPackages = with pkgs; [
    acpi
    iw
    lm_sensors
    powertop
  ];
  
  networking = {
    wireless.iwd.enable = true;
    
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
  };
  
  powerManagement.powertop.enable = true;

  services = {
    throttled.enable = true;
    tlp = {
      enable = pkgs.lib.mkDefault true;
      settings = {
        START_CHARGE_THRESH_BAT0 = 75;
        STOP_CHARGE_THRESH_BAT0 = 80;
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
        ENERGY_PERF_POLICY_ON_BAT = "powersave";
      };
    };
  };
}
