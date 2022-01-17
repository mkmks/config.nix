{appimageTools, fetchurl, ...}:

appimageTools.wrapType2 { # or wrapType1
  name = "concordium-desktop-wallet-testnet";
  src = fetchurl {
    url = "https://s3.eu-west-1.amazonaws.com/desktopwallet.concordium.com/1.3.1/testnet/concordium-desktop-wallet-testnet-1.3.1.AppImage";
    sha256 = "59dc17c2cf47dfff8c60c0c662b7145cd08a10fcb44d1fa367ff62afa57e35c4";
  };
  extraPkgs = pkgs: with pkgs; [ ];
}
