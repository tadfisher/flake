{ config, lib, pkgs, ... }:

with lib;
let
  cfgNotmuch = config.programs.notmuch;

  cfgEmail = config.accounts.email;

  sendmail = pkgs.writeShellScript "lieer-sendmail" ''
    args=
    for arg in "$@"; do
        case "$arg" in
            -o*) continue ;;
            *) args="$args $arg" ;;
        esac
    done
    ${pkgs.lieer}/bin/gmi send ''${args[@]}
  '';

  notmuchAccounts = filter (a: a.notmuch.enable) (attrValues cfgEmail.accounts);

  primaryAccount = builtins.head (filter (a: a.primary) notmuchAccounts);

  # Enable prompting for sender if multiple accounts are defined.
  notmuchPromptFrom = if length notmuchAccounts > 1 then "'t" else "nil";

  elispMaildirPaths =
    let
      mkCons = account: ''("${account.address}" . "${account.maildir.absPath}")'';
    in
    "'(${concatMapStringsSep " " mkCons notmuchAccounts})";

  elispFromAddress =
    ''(nth 1 (mail-extract-address-components (message-field-value "From")))'';

  # The host part of the `from` variable.
  elispFromHost = ''(nth 1 (split-string from "@"))'';

  mkAccountEntries = basePath:
    let
      mkAddressEntry = maildir: folders: address: ''
        ((string-equal from "${address}") "${basePath}/${maildir.path}/${folders.drafts}")
      '';
    in
    account:
    concatMapStrings (mkAddressEntry account.maildir account.folders)
      ([ account.address ] ++ account.aliases);

in
{
  options.accounts.email.primaryAccount = mkOption {
    default = primaryAccount;
    internal = true;
    readOnly = true;
    description = ''
      The primary email address.
    '';
  };

  config = {
    accounts.email = {
      maildirBasePath = "${config.home.homeDirectory}/mail";

      accounts."tadfisher@gmail.com" = {
        address = "tadfisher@gmail.com";
        flavor = "gmail.com";
        gpg = {
          key = "tadfisher@gmail.com";
          signByDefault = true;
        };
        folders.drafts = "drafts";
        msmtp.enable = true;
        notmuch.enable = true;
        passwordCommand =
          "${config.programs.password-store.package}/bin/pass show mail.google.com/tadfisher@gmail.com";
        realName = "Tad Fisher";
        userName = "tadfisher@gmail.com";
      };
    };

    programs = {
      notmuch = {
        enable = true;
        new = { ignore = [ ".*.json" ]; };
      };

      emacs.init.usePackage = {
        message = {
          enable = true;
          package = "";
          defer = true;
          config = ''
            (setq hm--message-maildir-paths ${elispMaildirPaths})

            (defun hm--message-draft-folder (from)
              (cond ${concatMapStrings (mkAccountEntries "${cfgEmail.maildirBasePath}") notmuchAccounts}))

            (defun hm--message-send ()
              (make-local-variable 'message-user-fqdn)
              (make-local-variable 'message-sendmail-extra-arguments)
              (when-let* ((from ${elispFromAddress})
                     (path (cdr (assoc from hm--message-maildir-paths))))
                ; Set the host part of the Message-ID to the email address host.
                (setq message-user-fqdn ${elispFromHost})
                (setq message-sendmail-extra-arguments `("--quiet" "-t" "-C" ,path))))
            (add-hook 'message-send-mail-hook 'hm--message-send)

            (defun hm--notmuch-message-set-auto-save-file-name ()
              (let* ((from ${elispFromAddress})
                     (message-auto-save-directory (hm--message-draft-folder from)))
                (message-set-auto-save-file-name)))
            (add-hook 'message-setup-hook 'hm--notmuch-message-set-auto-save-file-name)

            (setq message-auto-save-directory nil
                  message-directory "${cfgEmail.maildirBasePath}"
                  message-sendmail-extra-arguments '("--quiet" "-t" "-C" "${primaryAccount.maildir.absPath}"))
          '';
        };

        mml = {
          enable = true;
          package = "";
          defer = true;
          config = ''
            (setq mml-secure-openpgp-sign-with-sender t)
          '';
        };

        notmuch = {
          after = [ "message" ];
          config = ''
            ;; See https://github.com/Schnouki/dotfiles/blob/0d6716a041e1db95a27fc393baa8f38b850c5a25/emacs/init-50-mail.el#L243
            (defun hm--notmuch-draft-folder (from)
              (cond ${concatMapStrings (mkAccountEntries "") notmuchAccounts}))

            (defun hm--notmuch-draft-save (orig-fun &rest args)
              (let* ((from ${elispFromAddress})
                     (notmuch-draft-folder (hm--notmuch-draft-folder from))
                     (message-user-fqdn ${elispFromHost}))
                (apply orig-fun args)))
            (advice-add 'notmuch-draft-save :around #'hm--notmuch-draft-save)

            ;; gmi send automatially persists sent messages
            (setq notmuch-fcc-dirs nil
                  notmuch-always-prompt-for-sender ${notmuchPromptFrom})
          '';
        };

        sendmail = {
          enable = true;
          package = ""; # built-in
          defer = true;
          config = ''
            (setq send-mail-function #'sendmail-send-it
                  sendmail-program "${sendmail}")
          '';
        };
      };
    };
  };
}
