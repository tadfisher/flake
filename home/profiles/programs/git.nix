{ config, lib, pkgs, ... }:

with lib;
let secrets = import ../../../secrets;

in
{
  programs = {
    delta = {
      enable = true;
      enableGitIntegration = true;
      options = { features = "decorations"; };
    };
    git = {
      enable = true;
      package = pkgs.gitFull;
      ignores = [ "*~" "#*#" ];
      signing = {
        key = "tadfisher@gmail.com";
        signByDefault = true;
      };
      passGitHelper.enable = true;
      lfs.enable = true;
      settings = {
        branch = {
          autoSetupMerge = "true";
          autoSetupRebase = "remote";
        };
        github.user = "tadfisher";
        http.cookiefile = "${config.xdg.configHome}/git/cookies";
        init.defaultBranch = "main";
        pull.rebase = "true";
        user = {
          name = "Tad Fisher";
          email = "tadfisher@gmail.com";
        };
      };
    };
  };

  xdg.configFile."git/cookies".text =
    let
      username = secrets.googlesource.username;
      password = secrets.googlesource.password;
    in
    ''
      .googlesource.com	TRUE	/	TRUE	2147483647	o	${username}=${password}
    '';
}
