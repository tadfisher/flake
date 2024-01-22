{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.inkscape;

in
{
  options.programs.inkscape = {
    enable = mkEnableOption "Inkscape vector graphics editor";

    package = mkOption {
      type = types.package;
      default = pkgs.inkscape;
      example = literalExpression ''pkgs.inkscape-with-extensions'';
      description = ''
        Inkscape package to use.
      '';
    };
  };

  config.home.packages = mkIf cfg.enable [ cfg.package ];
}
