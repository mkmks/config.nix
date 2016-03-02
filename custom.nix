with import <nixpkgs> {};
  rec {
    texliveEnv = pkgs.texlive.combine {
      inherit (pkgs.texlive) scheme-medium xifthen ifmtarg polytable lazylist filehook Asana-Math tikz-cd minted ifplatform xstring ucs bbm textgreek cbfonts cbfonts-fd greek-fontenc xits gnu-freefont;
    };

    haskellEnv =
         pkgs.haskellPackages.ghcWithPackages
    	     (haskellPackages: with haskellPackages; [
               Agda ghc-mod ghc-paths threadscope hakyll
	       cabal-install cabal2nix
               ]);
	       
    musicPackages = [ shntool cuetools flac lame monkeysAudio wavpack ncmpcpp ];

  }
