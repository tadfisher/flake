{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.git.passGitHelper;

  mappingFormat = pkgs.formats.ini { };

  mappingModule = types.submodule {
    freeformType = mappingFormat.type;
  };

  mappingFile = mappingFormat.generate "git-pass-mapping.ini" cfg.mapping;

in
{
  options = {
    programs.git.passGitHelper = {
      enable = mkEnableOption "password-store credential helper";

      package = mkOption {
        type = types.package;
        default = pkgs.pass-git-helper;
        defaultText = literalExpression "pkgs.pass-git-helper";
        description = ''
          pass-git-helper package to install.
        '';
      };

      mapping = mkOption {
        type = mappingModule;
        default = { };
        example = literalExpression ''
          {
            "github.com*".target = "dev/github";
            "gitlab.*" = {
              target = "dev/gitlab/user@example.com";
              username_extractor = "entry_name";
              skip_password = 10;
            };
          }
        '';
        description = ''
          Mapping of host pattern to a target entry in the
          password store.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.all (m: m ? target) (builtins.attrValues cfg.mapping);
        message = "'programs.git.passGitHelper.mapping' requires 'target' attribute.";
      }
    ];

    programs.git.extraConfig.credential.helper =
      "${cfg.package}/bin/pass-git-helper -m ${mappingFile}";
  };
}
