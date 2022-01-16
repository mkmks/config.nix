with import <nixpkgs> {};

stdenv.mkDerivation rec {

  name = "goldendict-1.5.0.rc2";
  src = fetchurl {
    url = "https://github.com/goldendict/goldendict/archive/1.5.0-RC2.tar.gz";
    sha256 = "1pizz39l61rbps0wby75fkvzyrah805257j33siqybwhsfiy1kmw";
  };

  nativeBuildInputs = with qt5; [ git pkgconfig qmake ];
  buildInputs = with qt5; [
    qtbase qtsvg qtwayland qtwebkit qtx11extras qttools
    libvorbis hunspell libao ffmpeg libeb lzo xz libtiff
  ];

  qmakeFlags = [ "CONFIG+=zim_support" ];

  meta = {
    homepage = http://goldendict.org/;
    description = "A feature-rich dictionary lookup program";

    platforms = stdenv.lib.platforms.linux;
    maintainers = [ stdenv.lib.maintainers.astsmtl ];
  };
}
