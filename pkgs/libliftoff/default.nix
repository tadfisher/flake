{ lib
, stdenv
, fetchFromGitHub
, libdrm
, meson
, ninja
, pkg-config
}:

let
  version = builtins.readFile ./version;
  source = builtins.fromJSON (builtins.readFile ./source.json);

in
stdenv.mkDerivation {
  name = "libliftoff-unstable";
  inherit version;

  src = fetchFromGitHub source;

  nativeBuildInputs = [ meson ninja pkg-config ];

  buildInputs = [ libdrm ];

  meta = with lib; {
    description = "Lightweight KMS plane library";
    homepage = "https://github.com/emersion/libliftoff";
    license = licenses.mit;
    maintainers = [ maintainers.tadfisher ];
    platforms = platforms.linux;
  };
}
