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
	scrollback = pkgs.fetchurl {
	  url = "http://st.suckless.org/patches/st-git-20151217-scrollback.diff";
	  sha256 = "1hwkb9lqyvmnzvq9pvjgy71w6bspl22hj5nfvvxrhhhxjlj2r4sp";
	};
	scrollback-mouse = pkgs.fetchurl {
	  url = "http://st.suckless.org/patches/st-git-20151106-scrollback-mouse.diff";
	  sha256 = "1py5cfchqmhymjyjwj1sar81kg7zl7cy9mvw3idvbgr0x6ffx6b9";
	};
	in [ scrollback scrollback-mouse ];

	src = pkgs.fetchgit {
	  url = "http://git.suckless.org/st";
	  rev = "39964614b742c4ec98a326762d98470cb987a45b";
	  sha256 = "119xv863qmwp288rs0nyv3mqhjrs1f4iddgy915jgzc97qhcw81m";
	};
    });
    
    surf = pkgs.lib.overrideDerivation (pkgs.surf.override {
        gtk = gtk3;
        # webkit = webkitgtk24x;
	webkit = pkgs.lib.overrideDerivation (pkgs.webkitgtk.override {
	}) (attrs: {
	  buildInputs = attrs.buildInputs
	    ++ [ pkgs.gst_plugins_good pkgs.gst_plugins_bad pkgs.gst_plugins_ugly];
	});
      }) (attrs: {
        patches =
          let
          smoothscrolling = pkgs.fetchurl {
            url = "http://surf.suckless.org/patches/surf-0.7-smoothscrolling.diff";
            sha256 = "a8f79358d0e61fe379219fe31af3637162e46d05f03111af9204874ea132397f";
          };
         # searchengines = pkgs.fetchurl {
         #      url = "http://surf.suckless.org/patches/surf-0.7-searchengines.diff";
         #      sha256 = "5e41bb9675ec11f42a4562e7c60f9a206e275e2ae8d23cab148b1bd4aa6811ba";
         #  };
         searchengines = pkgs.fetchurl {
              url = "http://surf.suckless.org/patches/surf-0.7-webkit2-searchengines.diff";
              sha256 = "1rkfa1fnb492fd4m2gnly86cb2s5aaxy6j8xgdc4nrdh77fc531v";
          };
	  	    
          in [ searchengines ];
	  
	src = pkgs.fetchgit {
	  url = "http://git.suckless.org/surf";
	  rev = "d04fb9bff3796025eeedf0f32ae1c0a40366f717";
	  sha256 = "12v1xwv2jh7rsvms5v55pmyv4dvsz2fvafcg057f93a2fc6i8ask";
	};
	  
	# buildInputs = attrs.buildInputs
	#   ++ [ pkgs.gst_plugins_good pkgs.gst_plugins_bad pkgs.gst_plugins_ugly ];
	  
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
