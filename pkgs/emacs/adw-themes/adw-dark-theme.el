;;; adwaita-dark-theme.el --- inspired by Adwaita Dark -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Commentary:
;;
;; Dark theme based on the Adwaita design language.
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup adw-dark-theme nil
  "Options for the `adw-dark' theme."
  :group 'doom-themes)

(defcustom adw-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'adw-dark-theme
  :type 'boolean)

(defcustom adw-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'adw-dark-theme
  :type 'boolean)

(defcustom adw-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'adw-dark-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme adw-dark
  "A dark theme inspired by the Adwaita design language."

  ;; name        default   256           16
  ((bg         '("#1e1e1e" "#262626"     "black"  ))
   (fg         '("#c0bfbc" "#c0c0c0"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#242424" "#303030"     "black"        ))
   (fg-alt     '("#c0bfbc" "#c0c0c0"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#000000" "#000000"     "black"        ))
   (base1      '("#241f31" "#262626"     "brightblack"  ))
   (base2      '("#3d3846" "#3a3a3a"     "brightblack"  ))
   (base3      '("#504e55" "#4e4e4e"     "brightblack" ))
   (base4      '("#5e5c64" "#5f5f5f"     "brightblack"  ))
   (base5      '("#77767b" "#767676"     "brightblack"  ))
   (base6      '("#9a9996" "#9e9e9e"     "brightblack"  ))
   (base7      '("#c0bfbc" "#c0c0c0"     "brightblack"  ))
   (base8      '("#deddda" "#dadada"     "white"        ))

   (grey       base2)
   (red        '("#ed333b" "#ff5f5f" "red"          ))
   (orange     '("#ffa348" "#ffaf5f" "brightred"    ))
   (green      '("#8ff0a4" "#87af5f" "green"        ))
   (teal       '("#5BC8AF" "#00afaf" "brightgreen"  ))
   (yellow     '("#f6d32d" "#ffd700" "yellow"       ))
   (blue       '("#62a0ea" "#5fafff" "brightblue"   ))
   (dark-blue  '("#1B497E" "#005faf" "blue"         ))
   (magenta    '("#c678dd" "#d75fd7" "brightmagenta"))
   (violet     '("#7d8ac7" "#8787d7" "magenta"      ))
   (cyan       '("#99C1F1" "#00d7ff" "brightcyan"   ))
   (dark-cyan  '("#33b2a4" "#00afaf" "cyan"         ))

   ;; Adwaita named colors; used only by this theme.
   (blue3      '("#3584e4" "#0087ff" "brightblue" ))
   (chameleon3 '("#4e9a06" "#5faf00" "green"      ))
   (light1     '("#ffffff" "#ffffff" "white"      ))
   (orange3    '("#ff7800" "#ff8700" "brightred"  ))
   (orange4    '("#e66100" "#d75f00" "brightred"  ))
   (red1       '("#f66151" "#ff5f5f" "red" ))
   (teal3      '("#33b2a4" "#00afaf" "brightgreen"))
   (yellow4    '("#f5c211" "#ffd700" "yellow"     ))
   (yellow5    '("#e5a50a" "#ffaf00" "yellow"     ))

   ;; these are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      teal3)
   (vertical-bar   '("#454545" "#444444" "brightblack"))
   (selection      dark-blue)
   (builtin        "#62a0ea")
   (comments       (if adw-dark-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if adw-dark-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      blue)
   (keywords       orange)
   (methods        orange)
   (operators      blue)
   (type           teal)
   (strings        teal)
   (variables      chameleon3)
   (numbers        violet)
   (region         (doom-blend blue3 bg 0.3))
   (error          red)
   (warning        yellow4)
   (success        green)
   (vc-modified    orange3)
   (vc-added       teal3)
   (vc-deleted     red1)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (vertical-bar-alt   '("#333333" "#303030" "brightblack"))

   (search                  yellow)
   (search-inactive         (doom-blend bg highlight 0.5))

   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if adw-dark-brighter-modeline
                                 (doom-darken blue 0.45)
                               bg-alt))
   (modeline-bg-alt          (if adw-dark-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when adw-dark-padded-modeline
      (if (integerp adw-dark-padded-modeline) adw-dark-padded-modeline 4))))

  ;;;; Base theme face overrides
  ((cursor :background base6)
   (lazy-highlight :background search-inactive :foreground base1 :distant-foreground base8)
   (isearch :background search :foreground base1 :distant-foreground base8)
   ((line-number &override) :foreground base4 :slant 'normal)
   ((line-number-current-line &override) :foreground base6 :slant 'normal)
   ((font-lock-comment-face &override)
    :background (if adw-dark-brighter-comments (doom-lighten bg 0.05)))
   ((font-lock-doc-markup-face &override) :foreground base6)
   ((font-lock-number-face &override) :foreground violet)
   ((font-lock-preprocessor-face &override) :foreground orange4)
   ((font-lock-type-face &override) :weight 'bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if adw-dark-brighter-modeline base8 highlight))
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground yellow5)
   (css-property             :foreground orange3)
   (css-selector             :foreground teal3)
   ;;;; diff-mode <built-in>
   ((diff-added &override) :foreground teal3)
   ((diff-changed &override) :foreground orange3)
   ((diff-file-header &override) :foreground violet)
   ((diff-hunk-header &override) :foreground yellow4)
   ((diff-removed &override) :foreground red1 :background base2)
   ;;;; display-fill-column-indicator
   ((fill-column-indicator &override) :foreground vertical-bar-alt)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if adw-dark-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   ((markdown-bold-face &override) :foreground 'unspecified)
   ((markdown-code-face &override) :background 'unspecified)
   (markdown-hr-face :foreground teal3)
   (markdown-html-tag-delimiter-face :foreground teal3)
   (markdown-html-tag-name-face :foreground teal3)
   ((markdown-italic-face &override) :foreground 'unspecified)
   (markdown-link-face :foreground red)
   (markdown-list-face :foreground orange4 :weight 'bold)
   (markdown-markup-face :foreground 'unspecified)
   (markdown-pre-face :foreground violet)
   (markdown-url-face :foreground blue :slant 'italic)
   ;;;; magit
   ((magit-diff-removed &override) :background (doom-blend vc-deleted bg 0.1))
   ((magit-diff-removed-highlight &override) :background (doom-blend vc-deleted bg 0.2))
   ;; ;; mic-paren
   (paren-face-match :foreground base8 :weight 'ultra-bold)
   ;;;; nxml-mode
   (nxml-attribute-local-name :foreground orange3)
   (nxml-attribute-value :foreground violet)
   (nxml-delimiter :foreground teal3)
   (nxml-element-local-name :foreground teal3)
   (nxml-processing-instruction-delimiter :foreground yellow4 :weight 'bold)
   (nxml-processing-instruction-target :foreground yellow4 :weight 'bold)
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))
   ;;;; tab-line/tab-bar (Emacs 27+)
   ((tab-line &override) :inherit 'variable-pitch :foreground vertical-bar)
   ((tab-line-tab &override) :foreground light1)
   ;;;; whitespace <built-in>
   (whitespace-space :foreground base3)
   (whitespace-newline :foreground base3)
   (whitespace-tab :foreground base3)
   (whitespace-indentation :foreground base3))

  ;;;; Base theme variable overrides-
  ())

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path (file-name-directory load-file-name)))

(provide 'adw-dark-theme)

;;; adw-dark-theme.el ends here
