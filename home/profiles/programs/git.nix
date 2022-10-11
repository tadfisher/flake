{ config, lib, pkgs, ... }:

with lib;
let secrets = import ../../../secrets;

in
{
  programs = {
    git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;
      userName = "Tad Fisher";
      userEmail = "tadfisher@gmail.com";
      ignores = [ "*~" "#*#" ];
      signing = {
        key = "tadfisher@gmail.com";
        signByDefault = true;
      };
      delta = {
        enable = true;
        options = { features = "decorations"; };
      };
      extraConfig = {
        branch = {
          autoSetupMerge = "true";
          autoSetupRebase = "remote";
        };
        pull.rebase = "true";
        github.user = "tadfisher";
        http.cookiefile = "${config.xdg.configHome}/git/cookies";
        init.defaultBranch = "main";
      };
      passGitHelper.enable = true;
    };
  };

  xdg.configFile."git/cookies".text =
    let
      username = secrets.googlesource.username;
      password = secrets.googlesource.password;
    in
    ''
      android.googlesource.com	FALSE	/	TRUE	2147483647	o	${username}=${password}
      android-review.googlesource.com	FALSE	/	TRUE	2147483647	o	${username}=${password}
    '';
}
