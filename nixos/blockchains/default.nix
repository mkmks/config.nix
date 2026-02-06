{ ... }:

{
  imports = [
    ./bitcoin.nix
    ./ethereum.nix
    ./cardano.nix
    ./monero.nix
  ];

  # common dependencies
  services = {
    tor = {
      enable = true;
      client.enable = true;
    };
  };
}
