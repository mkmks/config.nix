{appimageTools, fetchurl, ...}:

appimageTools.wrapType2 { # or wrapType1
  name = "concordium-desktop-wallet-testnet";
  src = fetchurl {
    url = "https://s3.eu-west-1.amazonaws.com/desktopwallet.concordium.com/1.3.1/testnet/concordium-desktop-wallet-testnet-1.3.1.AppImage";
    sha256 = "80da238ee1ef92894ed53ad21690275afb9dd46c20a5c31465ccbcfce093f038";
  };
  extraPkgs = pkgs: with pkgs; [ ];
}
