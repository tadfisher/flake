{ config, lib, ... }:

with lib;
let
  collectPaths = pathFun:
    let
      matches = builtins.filter (p: builtins.pathExists (pathFun p)) config.home.packages;
    in
    map pathFun matches;

  girPaths = collectPaths (p: "${p}/lib/girepository-1.0");
  ldPaths = map (removeSuffix "/girepository-1.0") girPaths;

in
{
  systemd.user.sessionVariables = {
    GI_TYPELIB_PATH = "${concatStringsSep ":" girPaths}\${GI_TYPELIB_PATH:+:$GI_TYPELIB_PATH}";
    LD_LIBRARY_PATH = "${concatStringsSep ":" ldPaths}\${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}";
  };

  xdg.systemDirs.data =
    collectPaths (p: "${p}/share/gsettings-schemas/${p.name}");
}
