with import <nixpkgs> {};

stdenv.mkDerivation {
  pname = "bemenu";
  version = "master";

  src = fetchFromGitHub {
    owner = "Cloudef";
    repo = "bemenu";
    rev = "c9d9bcdaf5f4454ea521b1c15452676df1414005";
    sha256 = "1mih2vd2lb8ix3bvb1vxqc13qhck5y3wgz2r4c8lc7gsi9all6sh";
  };

  nativeBuildInputs = [ cmake pkgconfig pcre ];

  buildInputs = with stdenv.lib; [
    cairo
    fribidi
    harfbuzz
    libxkbcommon
    pango
    wayland
  ];

  cmakeFlags = [ "-DBEMENU_CURSES_RENDERER=OFF" "-DBEMENU_X11_RENDERER=OFF" "-DBEMENU_WAYLAND_RENDERER=ON" ];
  
  meta = with stdenv.lib; {
    homepage = "https://github.com/Cloudef/bemenu";
    description = "Dynamic menu library and client program inspired by dmenu";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ thiagokokada ];
    platforms = with platforms; linux;
  };
}
