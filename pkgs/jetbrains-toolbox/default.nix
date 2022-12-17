{ lib
, stdenv
, appimageTools
, fetchurl
, runCommand
, fuse
}:

let
  pname = "jetbrains-toolbox";
  version = builtins.readFile ./version;
  name = "${pname}-${version}";

  sources = builtins.fromJSON (builtins.readFile ./sources.json);
  source = sources."${stdenv.system}" or (throw "Unsupported system");

  tarball = fetchurl {
    inherit (source) url hash;
  };

  unwrapped = runCommand "${name}-unwrapped" {} ''
    tar -xvf ${tarball}
    ls -la
    echo $out
    echo ${name}
    set -x
    mv "${name}/jetbrains-toolbox" $out
    set +x
  '';

in
appimageTools.wrapType2 rec {
  inherit name;

  src = appimageTools.extract {
    inherit name;
    src = unwrapped;
  };

  extraPkgs = pkgs: with pkgs; [
    fuse
  ];

  passthru = {
    inherit unwrapped;
  };

  meta = with lib; {
    description = "A control panel for your tools and projects";
    homepage = "https://www.jetbrains.com/toolbox/app";
    maintainers = with maintainers; [ tadfisher ];
    platforms = [ "x86_64-linux" ];
  };
}
