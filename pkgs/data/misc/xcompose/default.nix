{ stdenv, fetchFromGitHub, perl }:
let
  composeFiles = [
    "emoji"
    "modletters"
    "tags"
    "maths"
  ];

in
stdenv.mkDerivation {
  pname = "xcompose-unstable";
  version = "2020-08-18";

  src = fetchFromGitHub {
    owner = "kragen";
    repo = "xcompose";
    rev = "03c8d7761fcfb0e3c754eec8b8cbbb0814ad7a4b";
    sha256 = "sha256-CIWks2rmKNYgS3bBD9gTHMbwIrPzsq6btQ8bg2CJTQw=";
  };

  nativeBuildInputs = [ perl ];

  buildPhase = ''
    for f in ${toString composeFiles}; do
      ${perl}/bin/perl emojitrans2.pl < $f-base > $f.compose
    done
  '';

  installPhase = ''
    mkdir -p $out/share
    cp dotXCompose $out/share
    for f in ${toString composeFiles}; do
      cp $f.compose $out/share
    done
  '';

  meta = with stdenv.lib; {
    description = "Shared .XCompose keybindings";
    homepage = "https://github.com/kragen/xcompose";
    platforms = platforms.all;
    license = licenses.unfree;
    maintainers = with maintainers; [ tadfisher ];
  };
}
