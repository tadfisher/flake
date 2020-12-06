{ config, lib, pkgs, ... }:

with lib;
let
  editorScript = pkgs.writeScriptBin "emacseditor" ''
    #!${pkgs.runtimeShell}
    ${config.programs.emacs.package}/bin/emacsclient ${
      concatStringsSep " " config.services.emacs.client.arguments
    } "$@"
  '';

in
{
  home = {
    packages = [ editorScript ];

    sessionVariables = {
      EDITOR = "${editorScript}/bin/emacseditor";
      VISUAL = "${editorScript}/bin/emacseditor";
    };
  };

  services.emacs = {
    enable = true;
    client.enable = true;
    socketActivation.enable = true;
  };
}
