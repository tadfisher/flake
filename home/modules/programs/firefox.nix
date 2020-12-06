{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.programs.firefox;

  profilesPath =
    if isDarwin then "${firefoxConfigPath}/Profiles" else firefoxConfigPath;

  desktopFile =
    let
      sanitize = name:
        let
          good = upperChars ++ lowerChars ++ stringToCharacters "0123456789-";
          subst = c: if any (x: x == c) good then c else "-";
        in
        stringAsChars subst name;

      profileActions = mapAttrsToList
        (name: profile: {
          name = "Profile ${name}";
          key = "profile-${sanitize name}";
          exec = "firefox -profile '${profilesPath}/${profile.path}'";
        })
        (attrValues cfg.profiles);

      actions = [
        {
          name = "New Tab";
          key = "new-tab";
          exec = "firefox -new-tab %U";
        }
        {
          name = "New Private Window";
          key = "new-private-window";
          exec = "firefox -private-window %U";
        }
      ] ++ profileActions;
    in
    ''
          ${fileContents "${cfg.package}/share/applications/firefox.desktop"}
          Actions=${concatMapStrings (a: a.key + ";") (catAttrs "key" actions)}

          ${concatMapStringsSep "\n"
      (a: ''
            [Desktop Action ${a.key}]
            Name=${a.name}
            Exec=${a.exec}
          '')
      actions}
    '';

  commonProfileOpts = { name, config, ... }: {
    options = {
      settings = mkOption {
        type = with types; attrsOf (either bool (either int str));
        default = { };
        example = literalExample ''
          {
            "browser.startup.homepage" = "https://nixos.org";
            "browser.search.region" = "GB";
            "browser.search.isUS" = false;
            "distribution.searchplugins.defaultLocale" = "en-GB";
            "general.useragent.locale" = "en-GB";
            "browser.bookmarks.showMobileBookmarks" = true;
          }
        '';
        description = "Attribute set of Firefox preferences.";
      };

      extraConfig = mkOption {
        type = types.lines;
        default = "";
        description = ''
          Extra preferences to add to <filename>user.js</filename>.
        '';
      };

      userChrome = mkOption {
        type = types.lines;
        default = "";
        description = "Custom Firefox CSS.";
        example = ''
          /* Hide tab bar in FF Quantum */
          @-moz-document url("chrome://browser/content/browser.xul") {
            #TabsToolbar {
              visibility: collapse !important;
              margin-bottom: 21px !important;
          }
            #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
              visibility: collapse !important;
            }
          }
        '';
      };
    };
  };

in
{
  options.programs.firefox.commonProfileConfig = mkOption {
    type = types.submodule commonProfileOpts;
    default = { };
    description = "Configuration to apply to all Firefox profiles.";
  };

  config = mkIf (cfg.enable) {
    xdg.dataFile."applications/firefox.desktop".text = desktopFile;
  };
}
