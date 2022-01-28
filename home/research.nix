{pkgs, ...}:

{
  home = {
    packages = with pkgs; [
      (agda.withPackages (p: with p; [ standard-library ]))
      lua53Packages.digestif
      gnuplot
      xfig
    ];

    sessionVariables = {
      DIGESTIFDATA = "${pkgs.lua53Packages.digestif}/digestif-${pkgs.lua53Packages.digestif.version}-rocks/digestif/${pkgs.lua53Packages.digestif.version}/data";
    };
  };

  programs.emacs.extraPackages = e: with e; [
    proof-general
  ];
}
