{appimageTools, fetchurl, ...}:

appimageTools.wrapType2 { # or wrapType1
  name = "concordium-desktop-wallet";
  src = fetchurl {
    url = "https://distribution.mainnet.concordium.software/tools/linux/concordium-desktop-wallet-1.3.0.AppImage";
    sha256 = "6660da7cbc16772ab8cf2986f4775ce62c4d60140043fca124e5cc93f4d98fab";
  };
  extraPkgs = pkgs: with pkgs; [ ];
}

