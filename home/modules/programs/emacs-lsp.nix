{ config, lib, ... }:

with lib;
let
  cfg = config.programs.emacs.init.lsp;
  pkgCfg = config.programs.emacs.init.usePackage;

  lspModule = types.submodule ({ name, config, ... }: {
    options = {
      enable = mkEnableOption "the LSP client";

      require = mkOption {
        type = types.str;
        default = "lsp-${name}";
        description = ''
          Emacs package or feature which provides the LSP client.
        '';
      };

      modes = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = [ "c-mode" "c++-mode" ];
        description = ''
          Emacs modes for which to start the LSP client.
        '';
      };

      executables = mkOption {
        type = types.attrsOf types.str;
        default = { };
        example = literalExpression ''
          { typescript-language-server = "''${pkgs.nodePackages.typescript-language-server}/bin/typescript-language-server";
            typescript = "''${pkgs.nodePackages.typescript}/bin/tsserver";
          }
        '';
        description = ''
          Set of paths to binaries required by the LSP client.
        '';
      };

      init = mkOption {
        type = types.lines;
        default = "";
        description = ''
          Extra code for this client's <option>:init</option>.
        '';
      };

      config = mkOption {
        type = types.lines;
        default = "";
        example = literalExpression ''
          (setq lsp-clients-clangd-executable "''${pkgs.clang-tools}/bin/clangd")
        '';
        description = ''
          Extra code for this client's <option>:config</option>.
        '';
      };

      packages = mkOption {
        type = types.listOf types.package;
        default = [ ];
        example = literalExpression ''
          [ pkgs.nodePackages.bash-language-server ];
        '';
        description = ''
          Packages to make available in the home profile.
        '';
      };
    };
  });

  flatMap = f: flatten (map f (attrValues cfg.clients));

  requirePackages =
    concatStringsSep " " (map (c: c.require) (attrValues cfg.clients));

  mkModes = c: map (m: "(${m} . lsp)") c.modes;

  mkUsePackages =
    let
      mkDeps =
        mapAttrsToList (n: v: ''(lsp-dependency '${n} '(:nix "${v}"))'');
    in
    mapAttrsToList
      (n: c: {
        "${c.require}" = {
          enable = true;
          defer = true;
          init = c.init;
          config = ''
            ${concatStringsSep "\n" (mkDeps c.executables)}
            ${c.config}
          '';
        };
      })
      (filterAttrs (n: v: v.enable) cfg.clients);

in
{
  options = {
    programs.emacs.init.lsp = {
      enable = mkEnableOption "emacs-lsp clients";

      clients = mkOption {
        type = types.attrsOf lspModule;
        default = { };
        example = literalExpression ''
          {
            "bash" = {
              modes = [ "sh-mode" ];
              packages = [ pkgs.nodePackages.bash-language-server ];
            };
          }
        '';
        description = ''
          Language Server Protocol clients to configure.
        '';
      };

      init = mkOption {
        type = types.lines;
        default = "";
        description = ''
          Extra code for lsp-mode's <option>:init</option>.
        '';
      };

      config = mkOption {
        type = types.lines;
        default = "";
        description = ''
          Extra code for lsp-mode's <option>:config</option>.
        '';
      };

      recommendedPerformanceSettings = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Apply recommended performance settings for emacs-lsp.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = flatMap (c: c.packages);

    programs.emacs.init.prelude =
      optionalString cfg.recommendedPerformanceSettings ''
        ;; Recommended performance settings for emacs-lsp.
        (setq read-process-output-max (* 1024 1024)) ;; 1mb
      '';

    programs.emacs.init = {
      lsp.config = ''
        (setq lsp-client-packages '(${requirePackages}))

        (defun lsp--nix-path (path)
          (or (executable-find (file-name-nondirectory path))
              (lsp-resolve-value path)))

        (setq lsp-deps-providers
          (list :nix (list :path #'lsp--nix-path)))
      '';
      usePackage = mkMerge ([{
        lsp-mode = {
          enable = true;
          command = [ "lsp" "lsp-dependency" ];
          hook = (optional pkgCfg.which-key.enable
            "(lsp-mode . lsp-enable-which-key-integration)") ++ flatMap mkModes;
          inherit (cfg) init config;
        };
      }] ++ mkUsePackages);
    };
  };
}
