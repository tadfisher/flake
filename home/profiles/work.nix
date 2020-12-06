{ config, lib, pkgs, ... }:

with lib;
let
  simple-vpn =
    let
      script = pkgs.writeShellScriptBin "simple-vpn" ''
        set -e
        vpn="simple-vpn"
        nmcli con down "$vpn" &> /dev/null || true
        ${pkgs.networkmanager}/bin/nmcli --ask con up "$vpn" &> /dev/null
      '';
      desktopItem = pkgs.makeDesktopItem {
        name = "simple-vpn";
        exec = "${script}/bin/simple-vpn";
        comment = "Connect to the Simple VPN";
        desktopName = "Simple VPN";
        genericName = "Simple VPN";
        icon = "network-vpn-symbolic.symbolic";
        categories = "System;Network;";
        extraEntries = ''
          Keywords=VPN;
        '';
      };
    in
    pkgs.buildEnv {
      name = "simple-vpn";
      paths = [ script desktopItem ];
    };

in
{
  accounts.email.accounts."tad@simple.com" = {
    address = "tad@simple.com";
    flavor = "gmail.com";
    gpg = {
      key = "tad@simple.com";
      signByDefault = true;
    };
    lieer = {
      enable = true;
      dropNonExistingLabels = true;
      sync.enable = true;
    };
    msmtp.enable = true;
    notmuch.enable = true;
    passwordCommand = "pass show mail.google.com/tad@simple.com | head -n 1";
    realName = "Tad Fisher";
    userName = "tad@simple.com";
  };

  home.packages = with pkgs; [ simple-vpn zoom-us ];

  programs.emacs.init.usePackage = {
    forge = {
      config = ''
        (add-to-list 'forge-alist
                     '("github.banksimple.com"
                       "github.banksimple.com/api"
                       "github.banksimple.com"
                       forge-github-repository))
      '';
    };

    org-jira = {
      enable = true;
      after = [ "org" "auth-source-pass" ];
      bind = {
        "M-SPC ojp" = "org-jira-get-projects";
        "M-SPC ojb" = "org-jira-get-boards";
        "M-SPC oji" = "org-jira-get-issues";
      };
      extraConfig = ''
        :bind (:map org-mode-map
               :prefix-map tad/org-jira-prefix-map
               :prefix-docstring "Jira"
               :prefix "M-SPC M-SPC j"
               ("pg" . "org-jira-get-projects")
               ("bg" . "org-jira-get-boards")
               ("iv" . "org-jira-get-issues-by-board")
               ("ib" . "org-jira-browse-issue")
               ("ig" . "org-jira-get-issues")
               ("ij" . "org-jira-get-isues-from-custom-jql")
               ("ih" . "org-jira-get-issues-headonly")
               ("iu" . "org-jira-update-issue")
               ("iw" . "org-jira-progress-issue")
               ("in" . "org-jira-progress-issue-next")
               ("ia" . "org-jira-assign-issue")
               ("ir" . "org-jira-refresh-issue")
               ("iR" . "org-jira-refresh-issues-in-buffer")
               ("ic" . "org-jira-create-issue")
               ("ik" . "org-jira-copy-current-issue-key")
               ("sc" . "org-jira-create-subtask")
               ("sg" . "org-jira-get-subtasks")
               ("cc" . "org-jira-add-comment")
               ("cu" . "org-jira-update-comment")
               ("wu" . "org-jira-update-worklogs-from-org-clocks")
               ("tj" . "org-jira-todo-to-jira")
               ("if" . "org-jira-get-issues-by-fixversion"))
      '';
      config = ''
        (setq jiralib-url "https://banksimple.atlassian.net"
              jiralib-user-login-name "tad@simple.com"
              jiralib-token `("Authorization" . ,(format "Basic %s" (base64-encode-string (concat "tad@simple.com" ":" (auth-source-pass-get "api" "banksimple.atlassian.net/tad@simple.com")) t)))
              org-jira-custom-jqls '(
                (:jql " assignee = currentUser() AND createdDate >= '2020-01-01' AND createdDate < '2021-01-01' ORDER BY status, priority DESC, created"
                 :filename "jira-2020")
              )
              org-jira-done-states '("Closed" "Resolved" "Ready to Deploy" "Done")
              org-jira-jira-status-to-org-keyword-alist '(
                ("In Triage" . "TODO")
                ("Backlog" . "TODO")
                ("Delivery Selected" . "TODO")
                ("Ready for Eng" . "TODO")
                ("In Progress" . "NEXT")
                ("Waiting for Third Party" . "WAITING")
                ("Peer Review" . "WAITING")
                ("Test" . "WAITING")
                ("Needs Review" . "WAITING")
                ("Design QA" . "WAITING")
                ("Ready to Deploy" . "DONE")
                ("Done" . "DONE")
              )
              org-jira-verbosity nil
              org-jira-working-dir "${config.home.homeDirectory}/doc/org/jira")
      '';
    };
  };

  programs.git = {
    includes = [{
      path = "${config.xdg.configHome}/git/config-work";
      condition = "gitdir:~/simple/";
    }];
  };

  programs.firefox = {
    profiles = {
      work = {
        name = "work";
        path = "work";
        id = 0;
        isDefault = true;
      } // config.programs.firefox.commonProfileConfig;
      personal = {
        name = "personal";
        path = "personal";
        id = 1;
      } // config.programs.firefox.commonProfileConfig;
    };
  };

  programs.slack = {
    enable = true;
    arguments = [ "--silent" ];
  };

  programs.pass.stores.".local/share/pass/work" = {
    primary = true;
    alias = "pw";
  };

  xdg.configFile."git/config-work".text = ''
    [user]
        email = tad@simple.com
        signingKey = tad@simple.com

    [credential]
        helper = ${pkgs.pass-git-helper}/bin/pass-git-helper

    [github]
        host = github.banksimple.com/api

    [github "github.banksimple.com/api"]
        user = tad
  '';

  xdg.configFile."pass-git-helper/git-pass-mapping.ini".text =
    generators.toINI { } {
      "github.com*" = {
        target = "github.com/dirac";
        line_username = 1;
        skip_username = 6;
      };
    };
}
