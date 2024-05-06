{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.emacs;

  tree-sitter-grammars =
    let
      libName = drv: removeSuffix "-grammar" drv.pname;
      libSuffix = if pkgs.stdenv.isDarwin then "dylib" else "so";
      lib = drv: ''lib${libName drv}.${libSuffix}'';
      linkCmd = drv:
        if pkgs.stdenv.isDarwin then ''
          cp ${drv}/parser .
          chmod +w ./parser
          install_name_tool -id $out/lib/${lib drv} ./parser
          cp ./parser $out/lib/${lib drv}
          /usr/bin/codesign -s - -f $out/lib/${lib drv}
        '' else ''
          ln -s ${drv}/parser $out/lib/${lib drv}
        '';
      plugins = with pkgs.tree-sitter-grammars; [
        tree-sitter-bash
        # tree-sitter-blueprint
        tree-sitter-c
        tree-sitter-c-sharp
        tree-sitter-cmake
        tree-sitter-cpp
        tree-sitter-css
        tree-sitter-dockerfile
        tree-sitter-go
        tree-sitter-gomod
        tree-sitter-java
        tree-sitter-javascript
        tree-sitter-json
        tree-sitter-python
        tree-sitter-ruby
        tree-sitter-rust
        tree-sitter-toml
        tree-sitter-tsx
        tree-sitter-typescript
        tree-sitter-yaml
      ];
    in
    pkgs.runCommandCC "tree-sitter-grammars" { }
      (concatStringsSep "\n" ([ "mkdir -p $out/lib" ] ++ (map linkCmd plugins)));

in
{
  home.packages = with pkgs; [
    ditaa
    emacs-all-the-icons-fonts
    freefont_ttf
    graphviz
    (hunspellWithDicts [ hunspellDicts.en-us ])
    jre
    nerdfonts
    plantuml
    silver-searcher
    sqlite
    zbar
  ];

  programs = {

    # TODO enable only for bash-in-emacs.
    bash.initExtra =
      let vterm = (pkgs.emacsPackagesFor cfg.package).vterm;
      in
      ''
        . ${vterm}/share/emacs/site-lisp/elpa/${vterm.ename}-${vterm.version}/etc/emacs-vterm-bash.sh
      '';

    emacs = {
      enable = true;

      package = mkDefault pkgs.emacs-nox;

      # TODO Not sure if still needed.
      overrides = self: super: rec {
        seq = cfg.package;
        magit-delta = super.magit-delta.overrideAttrs (attrs: {
          nativeBuildInputs = (attrs.nativeBuildInputs or [ ])
            ++ [ config.programs.git.package ];
        });
        treemacs = super.treemacs.overrideAttrs (attrs: {
          nativeBuildInputs = (attrs.nativeBuildInputs or [ ])
            ++ [ pkgs.python3 ];
        });
      };

      init = {
        enable = true;
        packageQuickstart = true;
        recommendedGcSettings = true;
        usePackageVerbose = false;

        earlyInit = ''
          ;; Disable some GUI distractions. We set these manually to avoid starting
          ;; the corresponding minor modes.
          (push '(menu-bar-lines . 0) default-frame-alist)
          (push '(tool-bar-lines . nil) default-frame-alist)
          (push '(vertical-scroll-bars . nil) default-frame-alist)
          (push '(width . 120) default-frame-alist)
          (push '(fullscreen . fullheight) default-frame-alist)

          ;; Set up fonts early.
          (set-face-attribute 'default
                              nil
                              :family "JetBrains Mono")
          (set-face-attribute 'variable-pitch
                              nil
                              :family "Roboto"
                              :height 88
                              :weight 'regular)
        '';
        prelude = ''
          ;; Disable startup message.
          (setq inhibit-startup-message t
                inhibit-startup-echo-area-message (user-login-name))

          (setq initial-major-mode 'fundamental-mode
                initial-scratch-message nil)

          ;; Customize cursor.
          (setq blink-cursor-mode nil)
          (setq-default cursor-type 'bar)

          ;; Set frame title.
          (setq frame-title-format
                '("" invocation-name ": "(:eval
                                          (if (buffer-file-name)
                                              (abbreviate-file-name (buffer-file-name))
                                            "%b"))))

          ;; Resize frames per-pixel.
          (setq frame-resize-pixelwise t)

          ;; Scroll per-pixel.
          (pixel-scroll-precision-mode 1)

          ;; Customize tab bar.
          (setq tab-bar-mode t)

          ;; Accept 'y' and 'n' rather than 'yes' and 'no'.
          (defalias 'yes-or-no-p 'y-or-n-p)

          ;; Don't want to move based on visual line.
          (setq line-move-visual nil)

          ;; Stop creating backup and autosave files.
          (setq make-backup-files nil
                auto-save-default nil)

          ;; Always show line and column number in the mode line.
          (line-number-mode)
          (column-number-mode)

          ;; Enable some features that are disabled by default.
          (put 'narrow-to-region 'disabled nil)

          ;; Typically, I only want spaces when pressing the TAB key. I also
          ;; want 4 of them.
          (setq-default indent-tabs-mode nil
                        tab-width 4
                        c-basic-offset 4)

          (defvar tad/read-only-modes '(read-only-mode special-mode comint-mode vterm-mode)
            "Modes to treat as read-only for UI purposes.")

          ;; Use one space to end sentences.
          (setq sentence-end-double-space nil)

          ;; I typically want to use UTF-8.
          (prefer-coding-system 'utf-8)

          ;; Nicer handling of regions.
          (transient-mark-mode 1)

          ;; Make moving cursor past bottom only scroll a single line rather
          ;; than half a page.
          (setq scroll-step 1
                scroll-conservatively 5)

          ;; Enable highlighting of current line.
          (global-hl-line-mode 1)

          ;; Set a reasonable default fill-column.
          (setq-default fill-column 100)

          ;; <TAB> completes.
          (setq tab-always-indent 'complete)

          ;; Improved handling of clipboard in GNU/Linux and otherwise.
          (setq select-enable-clipboard t
                select-enable-primary t
                save-interprogram-paste-before-kill t)

          ;; Pasting with middle click should insert at point, not where the
          ;; click happened.
          (setq mouse-yank-at-point t)

          ;; Enable a few useful commands that are initially disabled.
          (put 'upcase-region 'disabled nil)
          (put 'downcase-region 'disabled nil)

          (setq custom-file (locate-user-emacs-file "custom.el"))
          (when (file-exists-p custom-file)
                (load custom-file))

          ;; When finding file in non-existing directory, offer to create the
          ;; parent directory.
          (defun with-buffer-name-prompt-and-make-subdirs ()
            (let ((parent-directory (file-name-directory buffer-file-name)))
              (when (and (not (file-exists-p parent-directory))
                         (y-or-n-p (format "Directory `%s' does not exist! Create it? " parent-directory)))
                (make-directory parent-directory t))))

          (add-to-list 'find-file-not-found-functions #'with-buffer-name-prompt-and-make-subdirs)

          ;; Shouldn't highlight trailing spaces in terminal mode.
          (add-hook 'term-mode (lambda () (setq show-trailing-whitespace nil)))
          (add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))

          ;; Handle urls from the command line (also via emacsclient).
          (url-handler-mode 1)

          ;; Just use bash, don't ask.
          (setq explicit-shell-file-name "${pkgs.bashInteractive}/bin/bash")

          ;; Unbind M-SPC, which I use as a prefix key.
          (global-unset-key (kbd "M-SPC"))

          ;; Unbind C-z and C-x C-z, because I never want to leave Emacs.
          (global-unset-key (kbd "C-z"))
          (global-unset-key (kbd "C-x C-z"))

          ;; Set user info.
          (setq user-mail-address "${config.accounts.email.primaryAccount.address}")

          ;; Add tree-sitter grammars.
          (setq treesit-extra-load-path '("${tree-sitter-grammars}/lib"))
        '';

        lsp = {
          enable = false;
          clients = {
            bash = {
              enable = true;
              modes = [ "sh-mode" ];
              executables.bash-language-server =
                "${pkgs.nodePackages.bash-language-server}/bin/bash-language-server";
            };
            clangd = {
              enable = true;
              modes = [ "c-mode" "c++mode" "objc-mode" ];
              config = ''
                (setq lsp-clients-clangd-executable "${pkgs.clang-tools}/bin/clangd")
              '';
            };
            clojure = {
              enable = false;
              modes = [ "clojure-mode" "clojurec-mode" "clojurescript-mode" ];
              # config = ''
              #   (setq lsp-clojure-server-command '("${pkgs.clojure-lsp}/bin/clojure-lsp"))
              # '';
            };
            css = {
              enable = true;
              modes = [ "css-mode" "less-css-mode" "sass-mode" "scss-mode" ];
              executables.css-languageserver =
                "${pkgs.nodePackages.vscode-css-languageserver-bin}/bin/css-languageserver";
            };
            # "dhall" = {
            #   modes = [ "dhall-mode" ];
            #   packages = [ pkgs.haskellPackages.dhall-lsp-server ];
            # };
            go = {
              enable = true;
              modes = [ "go-mode" ];
              config = ''
                (setq lsp-gopls-server-path "${pkgs.gotools}/bin/gopls")
              '';
            };
            haskell = {
              enable = true;
              modes = [ "haskell-mode" "haskell-literate-mode" ];
              config = ''
                (setq lsp-haskell-server-path "${pkgs.haskell-language-server}/bin/haskell-language-server-wrapper")
              '';
            };
            html = {
              enable = true;
              modes = [ "html-mode" "sgml-mode" "mhtml-mode" "web-mode" ];
              executables.html-language-server =
                "${pkgs.nodePackages.vscode-html-languageserver-bin}/bin/html-languageserver";
            };
            javascript-typescript = {
              enable = true;
              require = "lsp-javascript";
              modes = [ "typescript-mode" ];
              executables.typescript-language-server =
                "${pkgs.nodePackages.typescript-language-server}/bin/typescript-language-server";
            };
            metals = {
              enable = false;
              modes = [ "scala-mode" ];
              config = ''
                (setq lsp-metals-server-command "${pkgs.metals}/bin/metals-emacs"
                      lsp-metals-java-home "${pkgs.jdk.home}"
                      lsp-metals-sbt-script "${pkgs.sbt}/bin/sbt"
                      lsp-metals-gradle-script "${pkgs.gradle}/bin/gradle"
                      lsp-metals-maven-script "${pkgs.maven}/bin/mvn"
                      lsp-metals-mill-script "${pkgs.mill}/bin/mill")
              '';
            };
            rust = {
              enable = true;
              modes = [ "rust-mode" ];
              config = ''
                (setq lsp-rust-analyzer-server-command '("${pkgs.rust-analyzer}/bin/rust-analyzer"))
                (setf (lsp--client-environment-fn (gethash 'rust-analyzer lsp-clients))
                      (lambda () `(("PATH" . ,(concat "${pkgs.cargo}/bin:${pkgs.rustfmt}/bin:" (getenv "PATH"))))))
              '';
            };
          };
          config = ''
            (setq lsp-eldoc-render-all nil
                  lsp-lens-enable nil
                  lsp-keymap-prefix "M-SPC l")
          '';
        };

        usePackage = {
          abbrev = {
            enable = true;
            diminish = [ "abbrev-mode" ];
            command = [ "abbrev-mode" ];
          };

          ace-window = {
            enable = true;
            extraConfig = ''
              :bind* (("C-c w" . ace-window)
                      ("M-o" . ace-window))
            '';
          };

          adaptive-wrap = {
            enable = true;
            command = [ "adaptive-wrap-prefix-mode" ];
          };

          adoc-mode = {
            enable = true;
            mode = [ ''"\\.adoc\\'"'' ];
            hook = [
              ''
                (adoc-mode . (lambda ()
                              (visual-line-mode)
                              (buffer-face-mode)))
              ''
            ];
            config = ''
              (set-face-background 'markup-verbatim-face nil)
            '';
          };

          adw-dark-theme = {
            enable = true;
            package = "adw-themes";
            config = ''
              (if (daemonp)
                  (progn
                    (require 'server)
                    (add-hook 'server-after-make-frame-hook
                              (lambda ()
                                (if (member 'adw-dark custom-known-themes)
                                    (enable-theme 'adw-dark)
                                  (load-theme 'adw-dark t)))))
                (load-theme 'adw-dark t))
            '';
          };

          all-the-icons = {
            enable = true;
            defer = true;
            config = ''
              (dolist
                  (entry '((forge-issue-list-mode       all-the-icons-alltheicon "git"                :face all-the-icons-lred)
                           (forge-notifications-mode    all-the-icons-alltheicon "git"                :face all-the-icons-lred)
                           (forge-post-mode             all-the-icons-alltheicon "git"                :face all-the-icons-lred)
                           (forge-pullreq-list-mode     all-the-icons-alltheicon "git"                :face all-the-icons-lred)
                           (forge-repository-list-mode  all-the-icons-alltheicon "git"                :face all-the-icons-lred)
                           (forge-topic-list-mode       all-the-icons-alltheicon "git"                :face all-the-icons-lred)
                           (forge-topic-mode            all-the-icons-alltheicon "git"                :face all-the-icons-lred)
                           (notmuch-hello-mode          all-the-icons-material   "mail" :v-adjust 0.0 :face all-the-icons-red)
                           (notmuch-message-mode        all-the-icons-material   "mail" :v-adjust 0.0 :face all-the-icons-red)
                           (notmuch-search-mode         all-the-icons-material   "mail" :v-adjust 0.0 :face all-the-icons-red)
                           (notmuch-show-mode           all-the-icons-material   "mail" :v-adjust 0.0 :face all-the-icons-red)
                           (notmuch-tree-mode           all-the-icons-material   "mail" :v-adjust 0.0 :face all-the-icons-red)))
                (add-to-list 'all-the-icons-mode-icon-alist entry))
            '';
          };

          all-the-icons-dired = {
            enable = true;
            hook = [ "(dired-mode . all-the-icons-dired-mode)" ];
          };

          ansi-color = {
            enable = true;
            command = [ "ansi-color-apply-on-region" ];
          };

          auth-source-pass = {
            enable = true;
            config = ''
              (setq auth-source-pass-filename "${config.programs.password-store.settings.PASSWORD_STORE_DIR}")
              (auth-source-pass-enable)
            '';
          };

          auto-rename-tag = {
            enable = true;
            command = [ "auto-rename-tag-mode" ];
            hook = [ "((nxml-mode sgml-mode) . auto-rename-tag-mode)" ];
          };

          autorevert = {
            enable = true;
            diminish = [ "auto-revert-mode" ];
            command = [ "auto-revert-mode" ];
          };

          avy = {
            enable = true;
            extraConfig = ''
              :bind* ("C-c SPC" . avy-goto-word-or-subword-1)
            '';
          };

          arc-mode = {
            enable = true;
            package = "";
            defer = true;
            config = ''
              (setq archive-lzh-extract '("${pkgs.lhasa}/bin/lha" "pq")
                    archive-lzh-expunge '("${pkgs.lhasa}/bin/lha" "d")
                    archive-lzh-write-file-member '("${pkgs.lhasa}/bin/lha" "a")
                    archive-zip-extract '("${pkgs.unzip}/bin/unzip" "-qq" "-c")
                    archive-zip-expunge '("${pkgs.zip}/bin/zip" "-d" "-q")
                    archive-zip-update '("${pkgs.zip}/bin/zip" "-q")
                    archive-zip-update-case '("${pkgs.zip}/bin/zip" "-q" "-k")
                    archive-7z-program '("${pkgs._7zz}/bin/7zz")
                    archive-squashfs-extract '("${pkgs.squashfs-tools-ng}/bin/rdsquashfs" "-c"))
            '';
          };

          back-button = {
            enable = true;
            defer = 2;
            command = [ "back-button-mode" ];
            config = ''
              (back-button-mode 1)

              ;; Make mark ring larger.
              (setq global-mark-ring-max 50)
            '';
          };

          base16-theme.enable = true;

          base16-plata-noir-theme = {
            enable = false;
            package = "base16-plata-theme";
            after = [ "base16-theme" ];
            config = ''
              (when-let* ((dir (file-name-directory
                                (locate-file "base16-plata-noir-theme"
                                             load-path
                                             (get-load-suffixes)))))
                (add-to-list 'custom-theme-load-path dir)
            '';
          };

          bindings = {
            enable = true;
            package = "";
            bind = {
              "M-SPC g g" = "goto-line";
            };
          };

          blueprint-ts-mode.enable = true;

          browse-at-remote = { command = [ "browse-at-remote" ]; };

          buffer-move = {
            enable = true;
            bind = {
              "C-S-<up>" = "buf-move-up";
              "C-S-<down>" = "buf-move-down";
              "C-S-<left>" = "buf-move-left";
              "C-S-<right>" = "buf-move-right";
            };
          };

          cape = {
            enable = true;
            init = ''
              (add-to-list 'completion-at-point-functions #'cape-dabbrev)
              (add-to-list 'completion-at-point-functions #'cape-file)
              (add-to-list 'completion-at-point-functions #'cape-elisp-block)
              (add-to-list 'completion-at-point-functions #'cape-history)
              (add-to-list 'completion-at-point-functions #'cape-keyword)
              (add-to-list 'completion-at-point-functions #'cape-abbrev)
              (add-to-list 'completion-at-point-functions #'cape-sgml)
              (add-to-list 'completion-at-point-functions #'cape-dict)
              (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
              (add-to-list 'completion-at-point-functions #'cape-line)
            '';
          };

          cc-mode = {
            enable = true;
            defer = true;
            hook = [
              ''
                (c-mode-common . (lambda ()
                                  (subword-mode)
                                  (c-set-offset 'arglist-intro '++)))
              ''
            ];
          };

          cmake-mode = {
            enable = true;
            config = ''
              (setq cmake-mode-cmake-executable "${pkgs.cmake}/bin/cmake")
            '';
          };

          company = {
            enable = false;
            command = [ "company-mode" "company-doc-buffer" "global-company-mode" ];
            defer = 1;
            extraConfig = ''
              :bind (:map company-mode-map
                          ([remap completion-at-point] . company-complete-common)
                          ([remap complete-symbol] . company-complete-common))
            '';
            config = ''
              (setq company-idle-delay 0.3
                    company-show-quick-access t
                    company-tooltip-maximum-width 100
                    company-tooltip-minimum-width 20
                    ; Allow me to keep typing even if company disapproves.
                    company-require-match nil)

              (global-company-mode)
            '';
          };

          company-dabbrev = {
            enable = cfg.init.usePackage.company.enable;
            after = [ "company" ];
            command = [ "company-dabbrev" ];
            config = ''
              (setq company-dabbrev-downcase nil
                    company-dabbrev-ignore-case t)
            '';
          };

          company-cabal = {
            enable = cfg.init.usePackage.company.enable;
            after = [ "company" ];
            command = [ "company-cabal" ];
            config = ''
              (add-to-list 'company-backends 'company-cabal)
            '';
          };

          company-posframe = {
            enable = cfg.init.usePackage.company.enable;
            hook = [ "(company-mode . company-posframe-mode)" ];
          };

          company-quickhelp = {
            enable = cfg.init.usePackage.company.enable;
            after = [ "company" ];
            command = [ "company-quickhelp-mode" ];
            config = ''
              (company-quickhelp-mode 1)
            '';
          };

          company-restclient = {
            enable = cfg.init.usePackage.company.enable;
            after = [ "company" "restclient" ];
            command = [ "company-restclient" ];
            config = ''
              (add-to-list 'company-backends 'company-restclient)
            '';
          };

          company-yasnippet = {
            enable = cfg.init.usePackage.company.enable;
            after = [ "company" "yasnippet" ];
            bind = { "M-/" = "company-yasnippet"; };
          };

          # From https://github.com/mlb-/emacs.d/blob/a818e80f7790dffa4f6a775987c88691c4113d11/init.el#L472-L482
          compile = {
            enable = true;
            package = ""; # built-in
            defer = true;
            after = [ "ansi-color" ];
            hook = [
              ''
                (compilation-filter . (lambda ()
                                        (when (eq major-mode 'compilation-mode)
                                          (ansi-color-apply-on-region compilation-filter-start (point-max)))))
              ''
              ''
                (compilation-start . (lambda (process)
                                       (bury-buffer)
                                       (delete-windows-on (get-buffer-create "*compilation*"))))
              ''
            ];
            config = ''
              (add-hook 'compilation-finish-functions
                        (lambda (buf status)
                          (when (not (and (equal status "finished\n")
                                          (zerop compilation-num-errors-found)))
                            (display-buffer buf '(nil (allow-no-window . t))))))
            '';
          };

          consult = {
            enable = true;
            bind = {
              "C-s" = "consult-line";
              "C-x b" = "consult-buffer";
              "M-g M-g" = "consult-goto-line";
              "M-g g" = "consult-goto-line";
              "M-s f" = "consult-find";
              "M-s r" = "consult-ripgrep";
              "M-y" = "consult-yank-pop";
            };
            config = ''
              (defvar tad/consult-line-map
                (let ((map (make-sparse-keymap)))
                  (define-key map "\C-s" #'vertico-next)
                  map))

              (consult-customize
                consult-line
                  :history t ;; disable history
                  :keymap tad/consult-line-map
                consult-ripgrep consult-git-grep consult-grep
                consult-bookmark consult-recent-file consult-xref
                consult--source-bookmark consult--source-file-register
                consult--source-recent-file consult--source-project-recent-file
                  :preview-key '(:debounce 0.4 any)
                consult-theme
                  :preview-key '(:debounce 0.2 any)
              )
            '';
          };

          consult-eglot = {
            enable = true;
            bindLocal.eglot-mode-map = {
              "M-s s" = "consult-eglot-symbols";
            };
            after = [ "consult" "eglot" ];
          };

          consult-xref = {
            enable = true;
            after = [ "consult" "xref" ];
            command = [ "consult-xref" ];
            init = ''
              (setq xref-show-definitions-function #'consult-xref
                    xref-show-xrefs-function #'consult-xref)
            '';
          };

          consult-yasnippet = {
            enable = true;
            command = [ "consult-yasnippet" ];
          };

          copy-as-format = {
            enable = true;
            command = [
              "copy-as-format"
              "copy-as-format-asciidoc"
              "copy-as-format-bitbucket"
              "copy-as-format-disqus"
              "copy-as-format-github"
              "copy-as-format-gitlab"
              "copy-as-format-hipchat"
              "copy-as-format-html"
              "copy-as-format-jira"
              "copy-as-format-markdown"
              "copy-as-format-mediawiki"
              "copy-as-format-org-mode"
              "copy-as-format-pod"
              "copy-as-format-rst"
              "copy-as-format-slack"
            ];
          };

          corfu = {
            enable = true;
            bindLocal.corfu-map = {
              "RET" = "nil";
            };
            init = ''
              (setq corfu-auto t)
              (global-corfu-mode)
            '';
            config = ''
              (setq corfu-quit-no-match t)
            '';
          };

          css-mode = {
            enable = true;
            package = ""; # built-in
            defer = true;
            config = ''
              (setq css-fontify-colors nil)
            '';
          };

          csv-mode.enable = true;

          ct.enable = true;

          dap-mode = {
            # FIXME fails with (void-function "dap-ui-mode")
            enable = false;
            hook = [
              "(dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))"
            ];
          };

          dap-lldb = {
            # FIXME fails with (void-function "dap-ui-mode")
            enable = false;
            package = "dap-mode";
            config = ''
              (setq dap-lldb-debug-program "${pkgs.vscode-extensions.llvm-org.lldb-vscode}/bin/lldb-vscode")
            '';
          };

          dhall-mode = {
            enable = true;
            mode = [ ''"\\.dhall\\'"'' ];
          };

          dired = {
            enable = true;
            package = "";       # built-in
            command = [ "dired" "dired-jump" ];
            config = ''
              (put 'dired-find-alternate-file 'disabled nil)
              (setq delete-by-moving-to-trash t
                    dired-dwim-target t
                    dired-listing-switches "-alvh --group-directories-first")
            '';
          };

          dired-aux = {
            enable = true;
            package = "";       # built-in
            config = ''
              (setq dired-compress-files-alist
                    '(("\\.tar\\.gz\\'" . "${pkgs.gnutar}/bin/tar -cf - %i | ${pkgs.gzip}/bin/gzip -c9 > %o")
                     ("\\.tar\\.bz2\\'" . "${pkgs.gnutar}/bin/tar -cf - %i | ${pkgs.bzip2}/bin/bzip2 -c9 > %o")
                     ("\\.tar\\.xz\\'" . "${pkgs.gnutar}/bin/tar -cf - %i | ${pkgs.xz}/bin/xz -c9 > %o")
                     ("\\.tar\\.zst\\'" . "${pkgs.gnutar}/bin/tar -cf - %i | ${pkgs.zstd}/bin/zstd -19 -o %o")
                     ("\\.tar\\.lz\\'" . "${pkgs.gnutar}/bin/tar -cf - %i | ${pkgs.lzip}/bin/lzip -c9 > %o")
                     ("\\.tar\\.lzo\\'" . "${pkgs.gnutar}/bin/tar -cf - %i | ${pkgs.lzop}/bin/lzop -c9 > %o")
                     ("\\.zip\\'" . "${pkgs.zip}/bin/zip %o -r --filesync %i")
                     ("\\.pax\\'" . "${pkgs.pax}/bin/pax -wf %o %i")))
            '';
          };

          dired-x = {
            enable = true;
            package = "";       # built-in
            hook = [ "(dired-mode . dired-omit-mode)" ];
            bindLocal.dired-mode-map = { "." = "dired-omit-mode"; };
            config = ''
              (setq dired-omit-verbose nil
                    dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
            '';
          };

          display-fill-column-indicator = {
            enable = true;
            package = ""; # built-in
            config = ''
              (define-globalized-minor-mode tad/global-display-fill-column-indicator-mode
                display-fill-column-indicator-mode display-fill-column-indicator--turn-on
                :predicate `((not ,@tad/read-only-modes) t))

              (tad/global-display-fill-column-indicator-mode)
            '';
          };

          dockerfile-mode = {
            enable = true;
            mode = [ ''"Dockerfile\\'"'' ];
          };

          doom-modeline = {
            enable = true;
            extraConfig = ''
              :functions doom-modeline-mode
            '';
            config = ''
              (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
              (if (daemonp)
                  (add-hook 'server-after-make-frame-hook
                            (lambda ()
                              (doom-modeline-mode)
                              (custom-reevaluate-setting 'doom-modeline-icon)))
                (doom-modeline-mode))
            '';
          };

          drag-stuff = {
            enable = true;
            bind = {
              "M-<up>" = "drag-stuff-up";
              "M-<down>" = "drag-stuff-down";
            };
          };

          dts-mode = {
            enable = true;
          };

          # Setup ebib, my chosen bibliography manager.
          ebib = {
            enable = false;
            command = [ "ebib" ];
            hook = [
              # Highlighting of trailing whitespace is a bit annoying in ebib.
              ''
                (ebib-index-mode-hook
                 . (lambda ()
                     (setq show-trailing-whitespace nil)))
              ''

              ''
                (ebib-entry-mode-hook
                 . (lambda ()
                     (setq show-trailing-whitespace nil)))
              ''
            ];
            config = ''
              (setq ebib-latex-preamble '("\\usepackage{a4}"
                                          "\\bibliographystyle{amsplain}")
                    ebib-print-preamble '("\\usepackage{a4}")
                    ebib-print-tempfile "/tmp/ebib.tex"
                    ebib-extra-fields '(crossref
                                        url
                                        annote
                                        abstract
                                        keywords
                                        file
                                        timestamp
                                        doi))
            '';
          };

          ediff = {
            enable = true;
            defer = true;
            config = ''
              (setq ediff-window-setup-function 'ediff-setup-windows-plain)
            '';
          };

          eglot = {
            enable = true;
            package = (epkgs: if versionAtLeast (getVersion cfg.package) "29" then "" else epkgs.eglot);
            after = optional cfg.init.usePackage.cape.enable "corfu";
            hook = [
              ''
                ((c-mode c++-mode c-ts-base-mode
                  css-mode less-css-mode sass-mode scss-mode
                  go-mode
                  kotlin-mode
                  haskell-mode
                  html-mode sgml-mode mhtml-mode web-mode
                  js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode
                  nix-mode
                  rust-mode
                  sh-mode) . eglot-ensure)
              ''
            ];
            config = ''
              (setq eglot-autoshutdown t
                    eglot-confirm-server-initiated-edits nil
                    eglot-server-programs
                    `(((c-mode c++-mode c-ts-base-mode)
                        . ,(eglot-alternatives '("clangd" "${pkgs.clang-tools}/bin/clangd")))
                      ((css-mode less-css-mode sass-mode scss-mode)
                        . ,(eglot-alternatives
                            '(("css-languageserver" "--stdio")
                              ("${pkgs.nodePackages.vscode-css-languageserver-bin}/bin/css-languageserver" "--stdio"))))
                      (go-mode
                       . ,(eglot-alternatives '("gopls" "${pkgs.gotools}/bin/gopls")))
                      (kotlin-mode
                       . ,(eglot-alternatives '("kotlin-language-server" "${pkgs.kotlin-language-server}/bin/kotlin-language-server")))
                      (haskell-mode
                       . ,(eglot-alternatives
                           '(("haskell-language-server-wrapper" "--lsp")
                             ("${pkgs.haskell-language-server}/bin/haskell-language-server-wrapper" "--lsp"))))
                      ((html-mode sgml-mode mhtml-mode web-mode)
                        . ,(eglot-alternatives
                            '(("html-languageserver" "--stdio")
                              ("${pkgs.nodePackages.vscode-html-languageserver-bin}/bin/html-languageserver" "--stdio"))))
                      ((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)
                        . ,(eglot-alternatives
                            '(("typescript-language-server" "--stdio")
                              ("${pkgs.nodePackages.typescript-language-server}/bin/typescript-language-server" "--stdio"))))
                      (nix-mode
                       . ("${pkgs.nil}/bin/nil" :initializationOptions
                                                (:formatting (:command ["${pkgs.alejandra}/bin/alejandra"]))))
                      (rust-mode
                       . ,(eglot-alternatives '("rust-analyzer" "${pkgs.rust-analyzer}/bin/rust-analyzer")))
                      (sh-mode
                       . ,(eglot-alternatives
                           '(("bash-language-server" "start")
                             ("${pkgs.nodePackages.bash-language-server}/bin/bash-language-server" "start"))))))
              ${optionalString cfg.init.usePackage.cape.enable ''
                (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
              ''}
            '';
          };

          eglot-booster = {
            enable = true;
            after = [ "eglot" ];
            config = ''
              (add-to-list 'exec-path "${pkgs.emacs-lsp-booster}/bin")
              (eglot-booster-mode)
            '';
          };

          eldoc = {
            enable = true;
            diminish = [ "eldoc-mode" ];
            command = [ "eldoc-mode" ];
          };

          # Enable Electric Indent mode to do automatic indentation on RET.
          electric = {
            enable = true;
            command = [ "electric-indent-local-mode" ];
            hook = [ "(prog-mode . electric-indent-mode)" ];
          };

          embark = {
            enable = true;
            bind = {
              "C-." = "embark-act";
              "M-." = "embark-dwim";
              "C-h B" = "embark-bindings";
            };
            extraConfig = ''
              :ensure t
            '';
            init = ''
              (setq prefix-help-command #'embark-prefix-help-command)
            '';
            config = ''
              ;; Hide the mode line of the Embark live/completions buffers
              (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
            '';
          };

          embark-consult = {
            enable = true;
            extraConfig = ''
              :ensure t
            '';
            hook = [ "(embark-collect-mode . consult-preview-at-point-mode)" ];
          };

          envrc = {
            enable = true;
            demand = true;
            bindKeyMap = { "M-SPC E" = "envrc-command-map"; };
            config = ''
              (envrc-global-mode)
            '';
          };

          etags = {
            enable = true;
            defer = true;
            # Avoid spamming reload requests of TAGS files.
            config = "(setq tags-revert-without-query t)";
          };

          fish-mode.enable = true;

          flymake = {
            enable = true;
            package = "";
            hook = [ "(prog-mode . flymake-mode)" ];
          };

          flymake-shellcheck = {
            enable = true;
            command = [ "flymake-shellcheck-load" ];
            hook = [ "(sh-mode . flymake-shellcheck-load)" ];
            config = ''
              (setq flymake-shellcheck-path "${pkgs.shellcheck}/bin/shellcheck")
            '';
          };

          flyspell = {
            enable = true;
            diminish = [ "flyspell-mode" ];
            command = [ "flyspell-mode" "flyspell-prog-mode" ];
            bindLocal = {
              flyspell-mode-map = { "C-;" = "flyspell-auto-correct-word"; };
            };
            hook = [
              # Spell check in text and programming mode.
              "(text-mode . flyspell-mode)"
              "(prog-mode . flyspell-prog-mode)"
            ];
            init = ''
              ;; Completely override flyspell's own keymap.
              (setq flyspell-mode-map (make-sparse-keymap))
            '';
            config = ''
              ;; In flyspell I typically do not want meta-tab expansion
              ;; since it often conflicts with the major mode. Also,
              ;; make it a bit less verbose.
              (setq flyspell-issue-message-flag nil
                    flyspell-issue-welcome-flag nil
                    flyspell-use-meta-tab nil)
            '';
          };

          forge = {
            enable = true;
            after = [ "magit" ];
            config = ''
              (transient-append-suffix 'forge-dispatch '(0)
               ["Edit"
                ("e a" "assignees" forge-edit-topic-assignees)
                ("e r" "reviewers" forge-edit-topic-review-requests)])
            '';
          };

          gcmh = {
            enable = true;
            defer = 1;
            command = [ "gcmh-mode" ];
            config = ''
              (setq gcmh-idle-delay 'auto)
              (gcmh-mode)
            '';
          };

          ggtags = {
            enable = true;
            diminish = [ "ggtags-mode" ];
            command = [ "ggtags-mode" ];
            hook = [
              ''
                (c-mode-common-hook
                 . (lambda ()
                     (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                       (ggtags-mode 1))))
              ''
            ];
          };

          git-messenger = {
            enable = true;
            bind = { "C-x v p" = "git-messenger:popup-message"; };
          };

          gnome-shell-mode = {
            enable = true;
            command = [ "gnome-shell-mode" ];
          };

          go-mode.enable = true;

          groovy-mode = {
            enable = true;
            mode =
              [ ''"\\.gradle\\'"'' ''"\\.groovy\\'"'' ''"Jenkinsfile\\'"'' ];
          };

          haskell-mode = {
            enable = true;
            command = [
              "haskell-decl-scan-mode"
              "haskell-doc-mode"
              "haskell-indentation-mode"
              "interactive-haskell-mode"
            ];
            mode = [
              ''("\\.hs\\'" . haskell-mode)''
              ''("\\.hsc\\'" . haskell-mode)''
              ''("\\.c2hs\\'" . haskell-mode)''
              ''("\\.cpphs\\'" . haskell-mode)''
              ''("\\.lhs\\'" . haskell-literate-mode)''
            ];
            hook = [
              ''
                (haskell-mode . subword-mode)
              ''
            ];
            bindLocal = {
              haskell-mode-map = {
                "C-c C-l" = "haskell-interactive-bring";
              };
            };
            config = ''
              (setq tab-width 2)

              (setq haskell-process-log t
                    haskell-notify-p t)

              (setq haskell-process-args-cabal-repl
                    '("--ghc-options=+RTS -M500m -RTS -ferror-spans -fshow-loaded-modules"))
            '';
          };

          haskell-cabal = {
            enable = true;
            mode = [ ''("\\.cabal\\'" . haskell-cabal-mode)'' ];
            bindLocal = {
              haskell-cabal-mode-map = {
                "C-c C-c" = "haskell-process-cabal-build";
                "C-c c" = "haskell-process-cabal";
                "C-c C-b" = "haskell-interactive-bring";
              };
            };
          };

          hide-mode-line.enable = true;

          hideshow = {
            enable = true;
            package = "";
            hook = [
              "((nxml-mode sgml-mode) . hs-minor-mode)"
            ];
            config = ''
              ;; XML folding
              (let ((tag-start "<!--\\|<[^/>]*[^/]>")
                    (tag-end "-->\\|</[^/>]*[^/]>")
                    (comment-start "<!--"))
                (add-to-list 'hs-special-modes-alist
                             `(nxml-mode ,tag-start ,tag-end ,comment-start sgml-skip-tag-forward nil))
                (add-to-list 'hs-special-modes-alist
                             `(sgml-mode ,tag-start ,tag-end ,comment-start sgml-skip-tag-forward nil)))
            '';
          };

          image-dired = {
            enable = true;
            package = "";
            defer = true;
            config = ''
              (setq image-dired-thumbnail-storage 'standard-large
                    image-dired-cmd-create-thumbnail-program "${pkgs.imagemagick}/bin/convert"
                    image-dired-cmd-create-temp-image-program "${pkgs.imagemagick}/bin/convert"
                    image-dired-cmd-pngnq-program "${pkgs.pngnq}/bin/pngnq"
                    image-dired-cmd-pngcrush-program "${pkgs.pngcrush}/bin/pngcrush"
                    image-dired-cmd-optipng-program "${pkgs.optipng}/bin/optipng"
                    image-dired-cmd-rotate-thumbnail-program "${pkgs.imagemagick}/bin/mogrify"
                    image-dired-cmd-rotate-original-program "${pkgs.mozjpeg}/bin/jpegtran"
                    image-dired-cmd-write-exif-data-program "${pkgs.exiftool}/bin/exiftool"
                    image-dired-cmd-read-exif-data-program "${pkgs.exiftool}/bin/exiftool"
                    image-dired-thumb-relief 0
                    image-dired-external-viewer "${pkgs.xdg-utils}/bin/xdg-open"
                    image-dired-main-image-directory "${config.xdg.userDirs.pictures}/")
            '';
          };

          ispell = {
            enable = true;
            defer = 1;
          };

          janet-mode = {
            enable = true;
            mode = [ ''"\\.janet\\'"'' ];
          };

          journalctl-mode.enable = true;

          jq-mode = {
            enable = true;
            mode = [ ''"\\.jq\\'"'' ];
          };

          js = {
            enable = true;
            mode = [ ''("\\.js\\'" . js-mode)'' ''("\\.json\\'" . js-mode)'' ];
            config = ''
              (setq js-indent-level 2)
            '';
          };

          json-mode = {
            enable = true;
            bindLocal = {
              json-mode-map = {
                "M-SPC m q" = "jq-interactively";
              };
            };
          };

          kind-icon = {
            enable = true;
            after = [ "corfu" ];
            config = ''
              (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
            '';
          };

          kotlin-mode = {
            enable = true;
          };

          # Configure AUCTeX.
          latex = {
            enable = true;
            package = epkgs: epkgs.auctex;
            mode = [ ''("\\.tex\\'" . latex-mode)'' ];
            hook = [
              ''
                (LaTeX-mode
                 . (lambda ()
                     (turn-on-reftex)       ; Hook up AUCTeX with RefTeX.
                     (auto-fill-mode)
                     (define-key LaTeX-mode-map [adiaeresis] "\\\"a")))
              ''
            ];
            config = ''
              (setq TeX-PDF-mode t
                    TeX-auto-save t
                    TeX-parse-self t
                    TeX-output-view-style '(("^pdf$" "." "evince %o")
                                            ( "^ps$" "." "evince %o")
                                            ("^dvi$" "." "evince %o")))

              ;; Add Glossaries command. See
              ;; http://tex.stackexchange.com/a/36914
              (eval-after-load "tex"
                '(add-to-list
                  'TeX-command-list
                  '("Glossaries"
                    "makeglossaries %s"
                    TeX-run-command
                    nil
                    t
                    :help "Create glossaries file")))
            '';
          };

          ligature = {
            enable = true;
            # Specific to JetBrains Mono font
            # See https://www.jetbrains.com/lp/mono/#key-features
            config = ''
              (ligature-set-ligatures 'prog-mode
                                      '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&"
                                        "&&&" "&=" "++" "+++" "***" ";;" "!!" "??" "?:" "?." "?=" "<:" ":<"
                                        ":>" ">:" "<>" "<<<" ">>>" "<<" ">>" "||" "-|" "_|_" "|-" "||-"
                                        "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_("
                                        "#:" "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>"
                                        "</" "</>" "/>" "<!--" "<#--" "-->" "->" "->>" "<<-" "<-" "<=<"
                                        "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>" "=>>" ">=>" ">>="
                                        ">>-" ">-" ">--" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->"
                                        "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]"
                                        "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||" "|||>" "<|||"
                                        "<|>" "..." ".." ".=" ".-" "..<" ".?" "::" ":::" ":=" "::=" ":?"
                                        ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__"))
              (global-ligature-mode t)
            '';
          };

          lsp-modeline = {
            enable = cfg.init.lsp.enable;
            package = "lsp-mode";
          };

          lsp-ui = {
            enable = cfg.init.lsp.enable;
            command = [ "lsp-ui-mode" ];
          };

          lsp-treemacs = {
            enable = cfg.init.lsp.enable;
            command = [ "lsp-treemacs-errors-list" ];
          };

          # Configure magit, a nice mode for the git SCM.
          magit = {
            enable = true;
            command = [ "magit-project-status" ];
            bind = { "C-c g" = "magit-status"; };
            config = ''
              (setq magit-diff-highlight-indentation nil
                    magit-diff-highlight-trailing nil
                    magit-diff-paint-whitespace nil
                    magit-diff-highlight-hunk-body nil
                    magit-diff-refine-hunk nil
                    magit-git-executable "${config.programs.git.package}/bin/git")
              (add-to-list 'git-commit-style-convention-checks
                           'overlong-summary-line)
              (remove-hook 'server-switch-hook 'magit-commit-diff)
            '';
          };

          magit-extras = {
            enable = true;
            package = epkgs: epkgs.magit;
          };

          marginalia = {
            enable = true;
            command = [ "marginalia-mode" ];
            after = [ "vertico" ];
            defer = 1;
            config = "(marginalia-mode)";
          };

          markdown-mode = {
            enable = true;
            command = [ "markdown-mode" "gfm-mode" ];
            mode = [
              ''("README\\.md\\'" . gfm-mode)''
              ''("\\.markdown\\'" . markdown-mode)''
              ''("\\.md\\'" . markdown-mode)''
            ];
            config = ''
              (setq markdown-command "${pkgs.pandoc}/bin/pandoc")
            '';
          };

          meson-mode = {
            enable = true;
            mode = [ ''"/meson\\(\\.build\\|_options\\.txt\\)\\'"'' ];
          };

          minibuffer = {
            enable = true;
            package = "";
            config = ''
              (setq completion-cycle-threshold 3)
            '';
          };

          mml-sec = {
            enable = true;
            defer = true;
            config = ''
              (setq mml-secure-openpgp-encrypt-to-self t
                    mml-secure-openpgp-sign-with-sender t)
            '';
          };

          mpdel = {
            enable = true;
            diminish = [ "mpdel-mode" ];
            init = ''
              (setq mpdel-prefix-key (kbd "C-c z"))
            '';
            config = ''
              (mpdel-mode 1)
            '';
          };

          multiple-cursors = {
            enable = true;
            bind = {
              "C-S-c C-S-c" = "mc/edit-lines";
              "C-c m" = "mc/mark-all-like-this";
              "C->" = "mc/mark-next-like-this";
              "C-<" = "mc/mark-previous-like-this";
            };
          };

          nix-mode = {
            enable = true;
            mode = [ ''"\\.nix\\'"'' ''"\\.nix.in\\'"'' ];
            hook = [ ''(before-save-hook . nix-format-before-save)'' ];
            config = ''
              (setq nix-nixfmt-bin "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt")

              ;; Match nixpkgs-fmt: align exprs with "in" keyword.
              (smie-config-local '((0 :after "in" nil)))
            '';
          };

          nix-build = {
            enable = true;
            command = [ "nix-build" ];
          };

          nix-drv-mode = {
            enable = true;
            mode = [ ''"\\.drv\\'"'' ];
          };

          nix-repl = { enable = true; };

          nix-shell = { enable = true; };

          notmuch = {
            enable = true;
            config = ''
              (setq-default notmuch-search-oldest-first nil)
              (setq notmuch-archive-tags '("-inbox")
                    notmuch-crypto-gpg-program "${pkgs.gnupg}/bin/gpg2"
                    notmuch-command "${pkgs.notmuch}/bin/notmuch"
                    notmuch-address-save-filename "${config.xdg.cacheHome}/notmuch/address-cache"
                    notmuch-mua-cite-function 'message-cite-original-without-signature
                    notmuch-hello-thousands-separator ","
                    notmuch-show-logo nil)
            '';
          };

          nxml-mode = {
            enable = true;
            mode = [ ''"\\.xml\\'"'' ];
            config = ''
              (setq nxml-slash-auto-complete-flag t)
              (add-to-list 'rng-schema-locating-files
                           "~/.emacs.d/nxml-schemas/schemas.xml")
            '';
          };

          ob-http = {
            enable = true;
            after = [ "org" ];
            defer = true;
          };

          ob-plantuml = {
            enable = true;
            after = [ "org" ];
            defer = true;
            config = ''
              (setq org-plantuml-jar-path "${pkgs.plantuml}/lib/plantuml.jar")
            '';
          };

          ol-notmuch = {
            enable = cfg.init.usePackage.org.enable && cfg.init.usePackage.notmuch.enable;
            after = [ "notmuch" "org" ];
          };

          orderless = {
            enable = true;
            extraConfig = ''
              :custom (completion-styles '(orderless basic))
                      (completion-category-overrides '((file (styles basic partial-completion))))
            '';
          };

          org = {
            enable = true;
            bind = {
              "M-SPC o c" = "org-capture";
              "M-SPC o a" = "org-agenda";
              "M-SPC o l" = "org-store-link";
              "M-SPC o b" = "org-switchb";
            };
            bindKeyMap = { "M-SPC M-SPC" = "org-mode-map"; };
            hook = [
              ''
                (org-mode
                 . (lambda ()
                     (add-hook 'completion-at-point-functions
                               'pcomplete-completions-at-point nil t)))
              ''
            ];
            config = ''
              ;; Some general stuff.
              (setq org-reverse-note-order t
                    org-use-fast-todo-selection t)

              ;; (setq org-tag-alist rah-org-tag-alist)

              ;; GTD
              (defun tad/org-archive-done ()
                "Archive all DONE tasks."
                (interactive)
                (org-map-entries 'org-archive-subtree "/DONE" 'file))

              (require 'find-lisp)

              (setq org-directory "${config.home.homeDirectory}/doc/org"
                    tad/org-inbox (expand-file-name "inbox.org" org-directory)
                    tad/org-email (expand-file-name "email.org" org-directory)
                    org-capture-templates
                    `(("i" "inbox" entry (file ,tad/org-inbox)
                       "* TODO %?")
                      ("e" "email" entry (file+headline ,tad/org-email "Emails")
                       "* TODO [#A] Reply: %a" :immediate-finish t)
                      ("l" "link" entry (file ,tad/org-inbox)
                       "* TODO %(org-cliplink-capture)" :immediate-finish t)
                      ("c" "org-protocol-capture" entry (file ,tad/org-inbox)
                       "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t))
                    org-todo-keywords
                    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                      (sequence "WAITING(w@/!)" "HOLD(h@/!)" "CANCELED(c@/!)"))
                    org-log-done 'time
                    org-log-into-drawer t
                    org-log-state-notes-insert-after-drawers nil
                    org-refile-targets '((org-agenda-files :level . 1))
                    org-refile-use-outline-path t
                    org-outline-path-complete-in-steps nil
                    org-refile-allow-creating-parent-nodes 'confirm)

              ;; Active Org-babel languages
              (org-babel-do-load-languages 'org-babel-load-languages
                                           '((plantuml . t)
                                             (http . t)
                                             (shell . t)))

              ;; Unfortunately org-mode tends to take over keybindings that
              ;; start with C-c.
              (unbind-key "C-c SPC" org-mode-map)
              (unbind-key "C-c w" org-mode-map)
            '';
          };

          org-agenda = {
            enable = true;
            after = [ "org" "transient" ];
            defer = true;
            config = ''
              (setq org-agenda-files `(,org-directory)
                    org-agenda-span 5
                    org-deadline-warning-days 14
                    org-agenda-show-all-dates t
                    org-agenda-skip-deadline-if-done t
                    org-agenda-skip-scheduled-if-done t
                    org-agenda-start-on-weekday t)
            '';
          };

          org-bullets = {
            enable = true;
            hook = [ "(org-mode . org-bullets-mode)" ];
          };

          org-capture = {
            enable = true;
            after = [ "org" ];
            config = ''
              ;; (setq org-capture-templates rah-org-capture-templates)
            '';
          };

          org-clock = {
            enable = true;
            after = [ "org" ];
            config = ''
              (setq org-clock-rounding-minutes 5
                    org-clock-out-remove-zero-time-clocks t)
            '';
          };

          org-duration = {
            enable = true;
            after = [ "org" ];
            config = ''
              ;; I always want clock tables and such to be in hours, not days.
              (setq org-duration-format (quote h:mm))
            '';
          };

          org-protocol = {
            enable = true;
            demand = true;
          };

          org-ql = { enable = true; };

          org-table = {
            enable = true;
            after = [ "org" ];
            command = [ "orgtbl-to-generic" ];
            hook = [
              # For orgtbl mode, add a radio table translator function for
              # taking a table to a psql internal variable.
              ''
                (orgtbl-mode
                 . (lambda ()
                     (defun rah-orgtbl-to-psqlvar (table params)
                       "Converts an org table to an SQL list inside a psql internal variable"
                       (let* ((params2
                               (list
                                :tstart (concat "\\set " (plist-get params :var-name) " '(")
                                :tend ")'"
                                :lstart "("
                                :lend "),"
                                :sep ","
                                :hline ""))
                              (res (orgtbl-to-generic table (org-combine-plists params2 params))))
                         (replace-regexp-in-string ",)'$"
                                                   ")'"
                                                   (replace-regexp-in-string "\n" "" res))))))
              ''
            ];
            config = ''
              (unbind-key "C-c SPC" orgtbl-mode-map)
              (unbind-key "C-c w" orgtbl-mode-map)
            '';
            extraConfig = ''
              :functions org-combine-plists
            '';
          };

          org-tree-slide = {
            enable = true;
            command = [ "org-tree-slide-mode" ];
          };

          pass = {
            enable = true;
            after = [ "password-store" "password-store-otp" ];
          };

          password-store = {
            enable = true;
            config = ''
              (setq password-store-executable "${config.programs.password-store.package}/bin/pass")
            '';
          };

          password-store-otp = {
            enable = true;
            config = ''
              (setq password-store-otp-qrencode-executable "${pkgs.qrencode}/bin/qrencode"
                    password-store-otp-screenshot-command "${pkgs.gnome.gnome-screenshot}/bin/gnome-screenshot -a -f"
                    password-store-otp-zbarimg-executable "${pkgs.zbar}/bin/zbarimg")
            '';
          };

          plantuml-mode = {
            enable = true;
            mode = [ ''"\\.puml\\'"'' ];
            config = ''
              (setq plantuml-jar-path "${pkgs.plantuml}/lib/plantuml.jar")
            '';
          };

          posframe.enable = true;

          pretty-tabs = {
            enable = true;
            after = [ "tab-bar" "all-the-icons" ];
            extraConfig = ''
              :functions pretty-tabs-mode
            '';
            config = ''
              (if (daemonp)
                  (progn
                    (require 'server)
                    (add-hook 'server-after-make-frame-hook
                              'pretty-tabs-mode))
                (pretty-tabs-mode))
            '';
          };

          prog-mode = {
            enable = true;
            package = "";
            defer = true;
          };

          project = {
            enable = true;
            package = "";
            bindKeyMap = {
              "M-SPC p" = "project-prefix-map";
            };
          };

          protobuf-mode = {
            enable = true;
            mode = [ ''"'\\.proto\\'"'' ];
          };

          python = {
            enable = true;
            mode = [ ''("\\.py\\'" . python-mode)'' ];
            hook = [ "ggtags-mode" ];
          };

          rainbow-mode = {
            enable = true;
            hook = [ "css-mode" "scss-mode" ];
          };

          recentf = {
            enable = true;
            defer = 1;
            config = ''
              (setq recentf-save-file (locate-user-emacs-file "recentf")
                    recentf-max-menu-items 20
                    recentf-max-saved-items 500
                    recentf-exclude '("COMMIT_MSG"
                                      "COMMIT_EDITMSG"
                                      "^/\\(?:ssh\\|su\\|sudo\\)?:"))
              (recentf-mode 1)
            '';
          };

          #  Setup RefTeX.
          reftex = {
            enable = true;
            defer = true;
            config = ''
              (setq reftex-default-bibliography '("~/research/bibliographies/main.bib")
                    reftex-cite-format 'natbib
                    reftex-plug-into-AUCTeX t)
            '';
          };

          restclient = {
            enable = true;
            mode = [ ''("\\.http\\'" . restclient-mode)'' ];
          };

          # Use ripgrep for fast text search in projects. I usually use
          # this through Projectile.
          ripgrep = {
            enable = true;
            command = [ "ripgrep-regexp" ];
            config = ''
              (setq ripgrep-executable "${pkgs.ripgrep}/bin/rg")
            '';
          };

          rust-mode = {
            enable = true;
            mode = [ ''"\\.rs\\'"'' ];
          };

          sass-mode.enable = true;

          # Remember where we where in a previously visited file. Built-in.
          saveplace = {
            enable = true;
            defer = 1;
            config = ''
              (setq-default save-place t)
              (setq save-place-file (locate-user-emacs-file "places"))
            '';
          };

          scala-mode.enable = true;

          shr = {
            enable = true;
            defer = true;
            config = ''
              (setq shr-bullet "• "
                    shr-use-colors nil)
            '';
          };

          simple = {
            enable = true;
            package = "";
            defer = true;
            hook = [
              "(before-save . delete-trailing-whitespace)"
              "(prog-mode . (lambda () (toggle-truncate-lines 1)))"
            ];
            config = ''
              (defun save-buffer-preserve-whitespace (&optional arg)
                "Save the current buffer, preserving trailing whitespace."
                (interactive "p")
                (let ((before-save-hook (remove 'delete-trailing-whitespace before-save-hook)))
                  (save-buffer arg)))

              (setq read-extended-command-predicate #'command-completion-default-include-p)
            '';
          };

          smartparens = {
            enable = true;
            defer = 3;
            diminish = [ "smartparens-mode" ];
            command =
              [ "smartparens-global-mode" "show-smartparens-global-mode" ];
            bindLocal = {
              smartparens-mode-map = {
                "C-M-f" = "sp-forward-sexp";
                "C-M-b" = "sp-backward-sexp";
              };
            };
            config = ''
              (require 'smartparens-config)
              (smartparens-global-mode t)
              (show-smartparens-global-mode t)
            '';
          };

          solaire-mode = {
            enable = true;
            config = ''
              (solaire-global-mode +1)
            '';
          };

          string-inflection = {
            enable = true;
            bind = { "C-c C-u" = "string-inflection-all-cycle"; };
          };

          # Auto-save buffers
          super-save = {
            enable = true;
            config = ''
              (super-save-mode 1)
              (setq super-save-auto-save-when-idle t)
            '';
          };

          swift-mode = {
            enable = true;
            mode = [ ''("\\.swift\\'" . swift-mode)'' ];
          };

          systemd = {
            enable = true;
            defer = true;
          };

          tab-bar = {
            enable = true;
            after = [ "all-the-icons" ];
            extraConfig = ''
              :functions all-the-icons-material
            '';
            config = ''
              (setq tab-bar-auto-width t
                    tab-bar-auto-width-max nil
                    tab-bar-show 1
                    tab-bar-close-button
                    (propertize (all-the-icons-material "close" :face 'tab-bar-tab)
                                'close-tab t
                                :help "Close tab")
                    tab-bar-new-button
                    (all-the-icons-material "add" :face 'tab-bar))
            '';
          };

          tramp = {
            enable = true;
            package = ""; # Preferring built-in package for now
            # init = ''
            #   (autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
            # '';
            config = ''
              (setq tramp-auto-save-directory "~/.cache/emacs/tramp"
                    tramp-shell-prompt-pattern
                    "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>].* *\\(\\[[[:digit:];]*[[:alpha:]] *\\)*")
            '';
          };

          transpose-frame = {
            enable = true;
            bind = { "C-c f t" = "transpose-frame"; };
          };

          treemacs = {
            enable = true;
            bind = {
              "C-c t f" = "treemacs-find-file";
              "C-c t t" = "treemacs";
            };
            init = ''
              (setq treemacs-python-executable "${pkgs.python3}/bin/python")
            '';
          };

          tt-mode = {
            enable = true;
            mode = [ ''"\\.tt\\'"'' ];
          };

          typescript-ts-mode = {
            enable = true;
            package = ""; # built-in
            mode = [
              ''("\\.ts\\'" . typescript-ts-mode)''
              ''("\\.tsx\\'" . tsx-ts-mode)''
            ];
          };

          undo-tree = {
            enable = true;
            defer = 1;
            diminish = [ "undo-tree-mode" ];
            command = [ "global-undo-tree-mode" ];
            config = ''
              (setq undo-tree-history-directory-alist `(("." . ,(expand-file-name ".cache/undo-tree" user-emacs-directory)))
                    undo-tree-enable-undo-in-region t
                    undo-tree-visualizer-relative-timestamps t
                    undo-tree-visualizer-timestamps t)
              (global-undo-tree-mode)
            '';
          };

          # More helpful buffer names. Built-in.
          uniquify = {
            enable = true;
            defer = 5;
            config = ''
              (setq uniquify-buffer-name-style 'post-forward)
            '';
          };

          vertico = {
            enable = true;
            command = [ "vertico-mode" "vertico-next" ];
            init = "(vertico-mode)";
          };

          visual-fill-column = {
            enable = true;
            hook = [ "(visual-line-mode . visual-fill-column-mode)" ];
          };

          vterm = {
            enable = true;
            command = [ "vterm" ];
            bindLocal = {
              project-prefix-map = {
                "t" = "project-vterm";
              };
            };
            hook = [
              ''
                (vterm . (lambda ()
                           (blink-cursor-mode -1)
                           (setq-local confirm-kill-processes nil
                                       global-hl-line-mode nil)
                           (hl-line-mode -1)))
              ''
            ];
            init = ''
              (add-to-list 'project-switch-commands '(project-vterm "Vterm") t)
              (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
            '';
            config = ''
              (setq vterm-buffer-name-string "vterm %s"
                    vterm-copy-exclude-prompt t
                    vterm-kill-buffer-on-exit t
                    vterm-max-scrollback 100000)
              (add-to-list 'vterm-keymap-exceptions "M-:")
            '';
            extraConfig = ''
              :preface
              (defun project-vterm (&optional arg)
                "Create an interactive Vterm buffer.
              Start a new Vterm session for the current project root, or switch
              to an already active session. Return the buffer selected (or created).

              With a nonnumeric prefix ARG, create a new session.

              With a string prefix arg, create a new session with arg as buffer name.

              With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
              to the session with that number, or create it if it doesn't
              already exist.

              The buffer name used for Vterm sessions is determined by the
              value of `vterm-buffer-name'."
                (interactive "P")
                (defvar vterm-buffer-name)
                (when-let ((project (project-current t)))
                  (let* ((default-directory (project-root project))
                         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
                         (vterm-buffer-name-string nil)
                         (vterm-buffer (vterm arg)))
                    (setq-local vterm-buffer-name-string nil)
                    vterm-buffer)))
            '';
          };

          wc-mode = {
            enable = true;
            command = [ "wc-mode" ];
          };

          wdired = {
            enable = true;
            bindLocal = {
              dired-mode-map = { "C-c C-w" = "wdired-change-to-wdired-mode"; };
            };
            config = ''
              ;; I use wdired quite often and this setting allows editing file
              ;; permissions as well.
              (setq wdired-allow-to-change-permissions t)
            '';
          };

          web-mode = {
            enable = true;
            mode = [
              ''"\\.html\\'"''
              ''"\\.jsx?\\'"''
            ];
            config = ''
              (setq web-mode-attr-indent-offset 4
                    web-mode-code-indent-offset 2
                    web-mode-markup-indent-offset 2)

              (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))
            '';
          };

          wgrep = {
            enable = true;
          };

          which-key = {
            enable = true;
            command = [ "which-key-mode" ];
            diminish = [ "which-key-mode" ];
            defer = 2;
            config =
              let
                descriptions = { "M-SPC p" = "project"; };
                replacements = optionalString (descriptions != { }) ''
                  (which-key-add-key-based-replacements
                    ${
                      concatStringsSep "\n  "
                      (mapAttrsToList (k: d: ''"${k}" "${d}"'') descriptions)
                    })
                '';

              in
              ''
                ${replacements}

                (which-key-mode)
              '';
          };

          # Enable winner mode. This global minor mode allows you to
          # undo/redo changes to the window configuration. Uses the
          # commands C-c <left> and C-c <right>.
          winner = {
            enable = true;
            defer = 2;
            config = "(winner-mode 1)";
          };

          writeroom-mode = {
            enable = true;
            command = [ "writeroom-mode" ];
            bindLocal = {
              writeroom-mode-map = {
                "M-[" = "writeroom-decrease-width";
                "M-]" = "writeroom-increase-width";
                "M-'" = "writeroom-toggle-mode-line";
              };
            };
            hook = [ "(writeroom-mode . visual-line-mode)" ];
            config = ''
              (setq writeroom-bottom-divider-width 0)
            '';
          };

          xref = {
            enable = true;
            package = "";
            bindLocal = {
              prog-mode-map = {
                "M-SPC g a" = "xref-find-apropos";
                "M-SPC g d" = "xref-find-definitions";
                "M-SPC g r" = "xref-find-references";
              };
            };
            extraConfig = ''
              :custom (xref-search-program . ripgrep)
            '';
          };

          yaml-mode = {
            enable = true;
            mode = [ ''"\\.yaml\\'"'' ];
          };

          # Set up yasnippet. Defer it for a while since I don't generally
          # need it immediately.
          yasnippet = {
            enable = false;
            defer = 1;
            diminish = [ "yas-minor-mode" ];
            command = [ "yas-global-mode" "yas-minor-mode" ];
            hook = [
              # Yasnippet interferes with tab completion in ansi-term.
              "(term-mode . (lambda () (yas-minor-mode -1)))"
            ];
            config = "(yas-global-mode 1)";
          };

          yasnippet-snippets = {
            enable = false;
            after = [ "yasnippet" ];
          };
        };
      };
    };
  };
}
