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
	  url = "http://st.suckless.org/patches/scrollback/st-scrollback-0.7.diff";
	  sha256 = "1dng2hfda3hlrfiw0sq00k57yppmqlqk2wa4dd5pmmx8b9db28gp";
	};
	scrollback-mouse = pkgs.fetchurl {
	  url = "http://st.suckless.org/patches/scrollback/st-scrollback-mouse-20170427-5a10aca.diff";
	  sha256 = "08bnain9vl2xi5vhiv4v99cn3yqfr2jjgmgalsml7m8xhmz9b7kv";
	};
	in [ ];

  name = "st-0.8.1";

	src = pkgs.fetchgit {
	  url = "http://git.suckless.org/st";
	  rev = "6f0f2b7ec3713351de274707672fbadb6cc727a2";
	  sha256 = "1sd59hia2grqjg71jp613kq416lnaw5zkyykr4q7y4zgbr3dry1c";
	};
    });
  }
