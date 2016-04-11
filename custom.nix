with import <nixpkgs> {};
  rec {
    haskellEnv =
         pkgs.haskellPackages.ghcWithPackages
    	     (haskellPackages: with haskellPackages; [
               Agda ghc-mod ghc-paths threadscope hakyll
	       cabal-install cabal2nix
               ]);
	       
    musicPackages = [ shntool cuetools flac lame monkeysAudio wavpack ncmpcpp ];

    st = pkgs.lib.overrideDerivation (pkgs.st.override {}) (attrs: {
      patches =
        let
        solarized-light = pkgs.fetchurl {
	  url = "http://st.suckless.org/patches/st-0.6-solarized-light.diff";
	  sha256 = "0fjny2qddhajsn64rgf2xr2q0jaa1j4b94y6ki0ni5lb62nc2i7l";
	};
        solarized-dark = pkgs.fetchurl {
	  url = "http://st.suckless.org/patches/st-0.6-solarized-dark.diff";
	  sha256 = "134xw42ayl9pc7qdrismi3hjw0zgxp2pzhfy4ayf20lk63lzwn8g";
	};

	in [ ];
    });
    
    surf = pkgs.lib.overrideDerivation (pkgs.surf.override {
        gtk = gtk3;
        webkit = webkitgtk24x;
      }) (attrs: {
        patches =
          let
          smoothscrolling = pkgs.fetchurl {
            url = "http://surf.suckless.org/patches/surf-0.7-smoothscrolling.diff";
            sha256 = "a8f79358d0e61fe379219fe31af3637162e46d05f03111af9204874ea132397f";
          };
          searchengines = pkgs.fetchurl {
              url = "http://surf.suckless.org/patches/surf-0.7-searchengines.diff";
              sha256 = "5e41bb9675ec11f42a4562e7c60f9a206e275e2ae8d23cab148b1bd4aa6811ba";
            };
	    
          in [ smoothscrolling searchengines ];

        preConfigure = let configdefh = ./surf-config.def.h;
                       in attrs.preConfigure + ''
		       
           echo '

                static SearchEngine searchengines[] = {
                    { "d", "https://duckduckgo.com/%s" },     
                    { "g", "https://www.google.com/search?q=%s" },
                    { "w", "https://en.wikipedia.org/wiki/%s" },
                    { "y", "https://www.youtube.com/results?search_query=%s&aq=f" },
                };' >> config.def.h
        '';
      });
      
  }
