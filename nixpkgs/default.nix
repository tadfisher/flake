{ profiles }:
let
  inherit (builtins) filter listToAttrs map pathExists;

  profile = name: {
    inherit name;
    value =
      if pathExists (./. + "/${name}.nix")
      then import (./. + "/${name}.nix")
      else null;
  };

in
listToAttrs (filter (x: x.value != null) (map profile profiles))
