with import <nixpkgs> {};
  rec {
    haskellEnv =
         pkgs.haskellPackages.ghcWithPackages
    	     (haskellPackages: with haskellPackages; [
               Agda ghc-mod ghc-paths threadscope hakyll
	       cabal-install cabal2nix
               ]);
	       
    musicPackages = [ shntool cuetools flac lame monkeysAudio wavpack ncmpcpp ];

    surf = pkgs.lib.overrideDerivation (pkgs.surf.override {
        gtk = gtk3;
        webkit = webkitgtk24x;
      }) (attrs: {
        patches =
          let
          smoothscrolling = 
          pkgs.fetchurl {
            url = "http://surf.suckless.org/patches/surf-0.7-smoothscrolling.diff";
          	sha256 = "a8f79358d0e61fe379219fe31af3637162e46d05f03111af9204874ea132397f";
          };
          searchengines =
            pkgs.fetchurl {
                  url = "http://surf.suckless.org/patches/surf-0.7-searchengines.diff";
           	sha256 = "5e41bb9675ec11f42a4562e7c60f9a206e275e2ae8d23cab148b1bd4aa6811ba";
            };
          in [ searchengines smoothscrolling ];

        preConfigure = let configdefh = ./surf-config.def.h;
                       in attrs.preConfigure + ''
		       
           cp ${configdefh} config.def.h
        '';
      });
    
  }
