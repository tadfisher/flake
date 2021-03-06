{ lib, stdenv, fetchFromGitHub, fetchpatch }:

let
  source = builtins.fromJSON (builtins.readFile ./source.json);
  version = builtins.readFile ./version;

in
stdenv.mkDerivation rec {
  pname = "paperwm-unstable";
  inherit version;

  src = fetchFromGitHub source;

  patches = [
    (fetchpatch {
      url = "https://github.com/paperwm/PaperWM/commit/d53746025f45b3a3847bae3d29c32f75c394ef0d.patch";
      sha256 = "sha256-ad2ZaKY8FP/yjVXPAfJS7Z7BT2jonE0qXtR5Xq6p8Tk=";
    })
  ];


  uuid = "paperwm@hedning:matrix.org";

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp -r . $out/share/gnome-shell/extensions/${uuid}
    runHook postInstall
  '';

  meta = with lib; {
    description = "Tiled scrollable window management for Gnome Shell";
    homepage = "https://github.com/paperwm/PaperWM";
    license = licenses.gpl3;
    maintainers = with maintainers; [ hedning ];
  };
}
