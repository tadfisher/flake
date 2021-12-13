{ stdenv
, lib
, fetchFromGitHub
, pkg-config
, meson
, ninja
, gdk-pixbuf
, libwebp
}:

stdenv.mkDerivation rec {
  pname = "webp-pixbuf-loader";
  version = "0.0.3";

  src = fetchFromGitHub {
    owner = "aruiz";
    repo = pname;
    rev = version;
    sha256 = "sha256-IdhrE75IGRbR4LG1kUlUk5ppgWc7HNSe6Jtc8Vvu5LY=";
  };

  nativeBuildInputs = [ gdk-pixbuf.dev meson ninja pkg-config ];

  buildInputs = [ gdk-pixbuf libwebp ];

  mesonFlags = [
    "-Dgdk_pixbuf_moduledir=${gdk-pixbuf.moduleDir}"
    "-Dgdk_pixbuf_query_loaders_path=${gdk-pixbuf.dev}/bin/gdk-pixbuf-query-loaders"
  ];

  meta = with lib; {
    description = " WebP Image format GdkPixbuf loader";
    homepage = "https://github.com/aruiz/webp-pixbuf-loader";
    license = [ licenses.gpl2Plus ];
    maintainers = [ maintainers.tadfisher ];
  };
}
