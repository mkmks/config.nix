with import <nixpkgs> { };
	
stdenv.mkDerivation rec {
  name = "makedict-33";

  meta = {
    homepage = https://github.com/soshial/xdxf_makedict;
    description = "A multiformat dictionary converter";
    license = stdenv.lib.licenses.gpl2;
    platforms = stdenv.lib.platforms.linux;
  };
  
  src = fetchurl {
    url = "https://github.com/soshial/xdxf_makedict/archive/rev33.tar.gz";
    sha256 = "0qa13w42cm4vn8ccc1bcgw8p3xlmczr0n0q5b576qzdbh1x05w7r";
  };

  buildInputs = [ cmake pkgconfig glib expat ];
}
