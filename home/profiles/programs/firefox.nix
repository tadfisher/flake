{
  programs.firefox = {
    enable = true;

    commonProfileConfig = {
      settings = {
        "browser.tabs.drawInTitlebar" = true;
        "browser.uidensity" = 0;
        "extensions.pocket.enabled" = false;
        "gfx.webrender.all" = true;
        "gfx.webrender.compositor" = true;
        "svg.context-properties.content.enabled" = true;
        "toolkit.cosmeticAnimations.enabled" = false;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "ui.key.menuAccessKey" = 0; # Hide access key underlining
      };

      # TODO package this
      userChrome = ''
        @import "/home/tad/proj/firefox-plata-theme/result/plata-theme.css";
        @import "/home/tad/proj/firefox-plata-theme/result/hide-single-tab.css";
        @import "/home/tad/proj/firefox-plata-theme/result/system-icons.css";
        @import "/home/tad/proj/firefox-plata-theme/result/drag-window-headerbar-buttons.css";
      '';
    };
  };
}
