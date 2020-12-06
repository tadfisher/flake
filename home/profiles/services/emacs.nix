{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.emacs;

  emacs = config.programs.emacs.finalPackage;

  editorScript = pkgs.writeScriptBin "emacseditor" ''
    #!${pkgs.runtimeShell}
    ${emacs}/bin/emacsclient ${concatStringsSep " " cfg.client.arguments} "$@"
  '';

in
{
  home.packages = [ editorScript ];

  home.sessionVariables = optionalAttrs cfg.defaultEditor {
    EDITOR = "${editorScript}/bin/emacseditor";
    VISUAL = "${editorScript}/bin/emacseditor";
  };

  services.emacs = {
    enable = true;
    client.enable = true;
    socketActivation.enable = true;
  };
}
