;;; pretty-tabs.el --- Prettier tab-bar-mode         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tad Fisher

;; Author: Tad Fisher <tadfisher@gmail.com>
;; URL: https://github.com/tadfisher/pretty-tabs.el
;; Version 0.1
;; Package-Requires: ((emacs "27"))
;; Keywords: frames

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

(require 'all-the-icons)
(require 'seq)
(require 'subr-x)
(require 'tab-bar)

;;; Code:

(defconst pretty-tabs--close-button (concat tab-bar-close-button))
(defconst pretty-tabs--close-button-inactive (concat tab-bar-close-button))
(defconst pretty-tabs--new-button (concat tab-bar-new-button))
(defconst pretty-tabs--tab-space " ")
(defconst pretty-tabs--image-space " ")

(defun pretty-tabs-default-close-tab-image (active &optional ascent)
  (require 'svg)
  (let ((svg (svg-create 16 16)))
    (svg-path svg
              '((moveto ((5 . 4)))
                (elliptical-arc ((1 1 -1 1) (1 1 0.29297 0.70703)))
                (lineto ((2.29297 . 2.29297) (-2.29297 . 2.29297)))
                (elliptical-arc ((1 1 4 11)) :relative nil)
                (elliptical-arc ((1 1 1 1) (1 1 0.70703 -0.29297)))
                (lineto ((2.29297 . -2.29297) (2.2832 . 2.2832)))
                (elliptical-arc ((1 1 0.7168 0.30273) (1 1 1 -1) (1 1 -0.29297 -0.70703)))
                (lineto ((9.41426 . 8)) :relative nil)
                (lineto ((2.2832 . -2.2832)))
                (elliptical-arc ((1 1 12 5)) :relative nil)
                (elliptical-arc ((1 1 -1 -1) (1 1 -0.70703 0.29297)))
                (lineto ((-2.29297 . 2.29297) (-2.2832 . -2.2832)))
                (elliptical-arc ((1 1 0 0)))
                (elliptical-arc ((1 1 5 4)) :relative nil)
                (closepath))
              :relative t
              :fill-color (face-foreground
                           (if active 'tab-bar-tab 'tab-bar-tab-inactive)
                           nil t))
    (svg-image svg :ascent (or ascent 75))))

(defun pretty-tabs-default-add-tab-image (&optional ascent)
  (require 'svg)
  (let ((svg (svg-create 16 16)))
    (svg-path svg
              '((moveto ((7 . 2)))
                (vertical-lineto    (4))
                (horizontal-lineto (-4))
                (vertical-lineto    (2))
                (horizontal-lineto  (4))
                (vertical-lineto    (4))
                (horizontal-lineto  (2))
                (vertical-lineto   (-4))
                (horizontal-lineto  (4))
                (vertical-lineto   (-2))
                (horizontal-lineto (-4))
                (vertical-lineto   (-4))
                (closepath))
              :relative t
              :fill-color (face-foreground 'tab-bar nil t))
    (svg-image svg :ascent (or ascent 70))))

(defun pretty-tabs--update-button (button image &rest props)
  "Update BUTTON with IMAGE.
Ensure text properties PROPS exist on BUTTON."
  (cond
   ((null image) nil)
   ((stringp image)
    (let ((props (text-properties-at 0 image))
          (dp (plist-get props 'display)))
      (set-text-properties 0 (length button) props button)
      (put-text-property 0 (length button)
                         'display `(,image ,@dp)
                         button)))
   (t (put-text-property 0 (length button)
                         'display image
                         button)))
  (let ((p props))
    (while p
      (put-text-property 0 (length button)
                       (pop p) (pop p)
                       button))))

(defun pretty-tabs--update-spaces (height margin image-margin)
  "Update space characters with HEIGHT, MARGIN, and IMAGE-MARGIN."
  (let* ((font (font-info (face-font 'tab-bar-tab)))
         (fh (aref font 3))
         (fa (aref font 8))
         (ascent (+ fa (/ (- height fh) 2))))
    (put-text-property 0 1
                       'display `(space :height (,height)
                                        :width (,margin)
                                        :ascent (,ascent))
                       pretty-tabs--tab-space)
    (put-text-property 0 1
                       'display `(space :height (,height)
                                        :width (,image-margin)
                                        :ascent (,ascent))
                       pretty-tabs--image-space)))

(defun pretty-tabs--refresh-spaces ()
  "Refresh space characters."
  (pretty-tabs--update-spaces pretty-tabs-tab-height
                              pretty-tabs-tab-margin
                              pretty-tabs-image-margin))

(defun pretty-tabs-default-tab-icon (&optional f)
  "Return the tab icon for the active buffer.
If F is specified, restrict to a font family."
  (let* ((buffer (window-buffer (minibuffer-selected-window)))
         (base-f (concat "all-the-icons-icon" (when f (format "-%s" f))))
         (file-f (intern (concat base-f "-for-file")))
         (mode-f (intern (concat base-f "-for-mode")))
         (icon (if-let* ((file (buffer-file-name buffer))
                         ((all-the-icons-auto-mode-match? file)))
                   (funcall file-f (file-name-nondirectory file))
                 (funcall mode-f (buffer-local-value 'major-mode buffer)))))
     (propertize (if (symbolp icon)
                     (all-the-icons-faicon "file-o"
                                           :face 'all-the-icons-dsilver
                                           :v-adjust 0.0)
                   icon)
                 'display '(raise -0.2))))1

(defgroup pretty-tabs nil
  "Pretty ‘tab-bar-mode' display."
  :group 'tab-bar
  :prefix "pretty-tabs-")

(defcustom pretty-tabs-close-tab-image
  (pretty-tabs-default-close-tab-image t)
  "Image spec to use for the tab close button."
  :type `(choice (const :tag "None" nil)
                 (plist :tag "Image spec")
                 (sexp :tag "Other"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (pretty-tabs--update-button pretty-tabs--close-button val
                                     'close-tab t
                                     :help "Close tab")
         (force-mode-line-update))
  :group 'pretty-tabs)

(defcustom pretty-tabs-close-tab-image-inactive
  (pretty-tabs-default-close-tab-image nil)
  "Image spec to use for the tab close button in inactive tabs."
  :type `(choice (const :tag "None" nil)
                 (plist :tag "Image spec")
                 (sexp :tag "Other"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (pretty-tabs--update-button pretty-tabs--close-button-inactive val
                                     'close-tab t
                                     :help "Close tab")
         (force-mode-line-update))
  :group 'pretty-tabs)

(defcustom pretty-tabs-add-tab-image
  (pretty-tabs-default-add-tab-image)
  "Image spec to use for the new tab button."
  :type `(choice (const :tag "None" nil)
                 (plist :tag "Image spec")
                 (sexp :tag "Other"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (pretty-tabs--update-button pretty-tabs--new-button val)
         (force-mode-line-update))
  :group 'pretty-tabs)

(defcustom pretty-tabs-tab-height 36
  "Preferred tab height in pixels."
  :type '(integer :tag "Height (px)")
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (pretty-tabs--refresh-spaces)
         (force-mode-line-update))
  :group 'pretty-tabs)

(defcustom pretty-tabs-tab-margin 8
  "Horizontal tab margin in pixels."
  :type '(integer :tag "Width (px)")
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (pretty-tabs--refresh-spaces)
         (force-mode-line-update))
  :group 'pretty-tabs)

(defcustom pretty-tabs-image-margin 8
  "Margin between images or buttons and the tab text."
  :type '(integer :tag "Width (px)")
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (pretty-tabs--refresh-spaces)
         (force-mode-line-update))
  :group 'pretty-tabs)

(defcustom pretty-tabs-buffer-icon t
  "Show an icon for the current buffer using ‘pretty-tabs-tab-icon-function'."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
  :group 'pretty-tabs)

(defcustom pretty-tabs-tab-icon-function #'pretty-tabs-default-tab-icon
  "Function to get a tab icon for the current buffer."
  :type 'function
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
  :group 'pretty-tabs)

(defvar pretty-tabs--original-tabs-function nil)
(defvar pretty-tabs--original-tab-name-format-function nil)

(defun pretty-tabs--initialize ()
  "Initialize 'pretty-tabs-mode'."
  (pretty-tabs--update-button pretty-tabs--close-button
                              pretty-tabs-close-tab-image
                              'close-tab t
                              :help "Close tab")
  (pretty-tabs--update-button pretty-tabs--close-button-inactive
                              pretty-tabs-close-tab-image-inactive
                              'close-tab t
                              :help "Close tab")
  (pretty-tabs--update-button pretty-tabs--new-button
                              pretty-tabs-add-tab-image)
  (pretty-tabs--refresh-spaces))

(defun pretty-tabs--separator (width)
  (if window-system
      (propertize " " 'display `(space :width ,width))
    (tab-bar-separator)))

(defun pretty-tabs-tab-name-format (tab i)
  (let* ((current-p (eq (car tab) 'current-tab))
         (face (funcall tab-bar-tab-face-function tab))
         (fg (face-foreground face nil t))
         (bg (face-background face nil t))
         (space (propertize pretty-tabs--tab-space 'face face))
         (image-space (propertize pretty-tabs--image-space 'face face)))
    (concat
     space
     (if-let* ((pretty-tabs-buffer-icon)
               (icon (alist-get 'icon tab))
               (iface (get-text-property 0 'face icon)))
         (concat
          (propertize icon 'face (append iface `(:background ,bg)))
          image-space)
       "")
     (if tab-bar-tab-hints (format "%d " pos) "")
     (propertize (alist-get 'name tab) 'face face)
     (or (and tab-bar-close-button-show
              (not (eq tab-bar-close-button-show
                       (if current-p 'non-selected 'selected)))
              (let* ((icon (if current-p pretty-tabs--close-button pretty-tabs--close-button-inactive))
                     (iface (get-text-property 0 'face icon)))
                (concat image-space
                        (propertize icon 'face (append iface `(:foreground ,fg :background ,bg))))))
         "")
     space)))

(defun pretty-tabs--format-tab (tab i)
  (append
   `((,(intern (format "sep-%i" i)) menu-item
      ,(pcase i
         (1 (pretty-tabs--separator 'left-fringe))
         (_ (tab-bar-separator)))
      ignore))
   (cond
    ((eq (car tab) 'current-tab)
     `((current-tab
        menu-item
        ,(funcall tab-bar-tab-name-format-function tab i)
        ignore
        :help "Current tab")))
    (t
     `((,(intern (format "tab-%i" i))
        menu-item
        ,(funcall tab-bar-tab-name-format-function tab i)
        ,(or
          (alist-get 'binding tab)
          `(lambda ()
             (interactive)
             (tab-bar-select-tab ,i)))
        :help "Click to visit tab"))))
   `((,(if (eq (car tab) 'current-tab) 'C-current-tab (intern (format "C-tab-%i" i)))
      menu-item ""
      ,(or
        (alist-get 'close-binding tab)
        `(lambda ()
           (interactive)
           (tab-bar-close-tab ,i)))))))

(defun pretty-tabs--tab (tab-fun &optional frame)
  "Advise TAB-FUN to retain the ’icon’ entry.
Pass FRAME to TAB-FUN."
  (let* ((tab (assq 'current-tab (frame-parameter frame 'tabs)))
         (icon (alist-get 'icon tab))
         (res (funcall tab-fun frame)))
    (append res `((icon . ,icon)))))

(defun pretty-tabs--current-tab (current-tab-fun &optional tab frame)
  "Advise CURRENT-TAB-FUN to retain the ’icon’ entry.
Pass TAB and FRAME to CURRENT-TAB-FUN."
  (let* ((tab (or tab (tab-bar--current-tab-find nil frame)))
         (icon (or (alist-get 'icon tab)
                   (funcall pretty-tabs-tab-icon-function)))
         (res (funcall current-tab-fun tab frame)))
    (append res `((icon . ,icon)))))

(defun pretty-tabs--current-tab-make (current-tab-make-fun &optional tab)
  "Advise CURRENT-TAB-MAKE-FUN to add an ’icon’ entry.
Pass TAB to CURRENT-TAB-MAKE-FUN."
  (let ((tab (funcall current-tab-make-fun tab))
        (icon (funcall pretty-tabs-tab-icon-function)))
    (append tab `((icon . ,icon)))))

(defun pretty-tabs--format-add-tab ()
  (when (and tab-bar-new-button-show tab-bar-new-button)
    `((add-tab menu-item ,pretty-tabs--new-button tab-bar-new-tab
               :help "New tab"))))

(defun pretty-tabs-tabs (&optional frame)
  "Return a list of tabs belonging to the FRAME.
Ensure the frame parameter `tabs' is pre-populated.
Update the current tab name when it exists.
Return its existing value or a new value."
  (let ((tabs (frame-parameter frame 'tabs)))
    (if tabs
        (let* ((current-tab (tab-bar--current-tab-find tabs))
               (current-tab-name (assq 'name current-tab))
               (current-tab-explicit-name (assq 'explicit-name current-tab))
               (current-tab-icon (assq 'icon current-tab)))
          (when (and current-tab-name
                     current-tab-explicit-name
                     (not (cdr current-tab-explicit-name)))
            (setf (cdr current-tab-name)
                  (funcall tab-bar-tab-name-function)))
          (when (and current-tab-icon
                     (not (cdr current-tab-icon)))
            (setf (cdr current-tab-icon)
                  (funcall pretty-tabs-tab-icon-function))))
      ;; Create default tabs
      (setq tabs (list (tab-bar--current-tab-make)))
      (tab-bar-tabs-set tabs frame))
    tabs))

(define-minor-mode pretty-tabs-mode
  "Make 'tab-bar-mode' prettier."
  :global t
  :lighter nil

  (if pretty-tabs-mode
      (progn
        (pretty-tabs--initialize)

        (advice-add 'tab-bar--tab
                    :around #'pretty-tabs--tab)
        (advice-add 'tab-bar--current-tab
                    :around #'pretty-tabs--current-tab)
        (advice-add 'tab-bar--current-tab-make
                    :around #'pretty-tabs--current-tab-make)
        (advice-add 'tab-bar--format-tab
                    :override #'pretty-tabs--format-tab)
        (advice-add 'tab-bar-format-add-tab
                    :override #'pretty-tabs--format-add-tab)

        (setq pretty-tabs--original-tabs-function
              (or pretty-tabs--original-tabs-function
                  tab-bar-tabs-function)
              pretty-tabs--original-tab-name-format-function
              (or pretty-tabs--original-tab-name-format-function
                  tab-bar-tab-name-format-function)
              tab-bar-tabs-function #'pretty-tabs-tabs
              tab-bar-tab-name-format-function #'pretty-tabs-tab-name-format))

    (setq tab-bar-tabs-function (or pretty-tabs--original-tabs-function
                                    #'tab-bar-tabs)
          tab-bar-tab-name-format-function (or pretty-tabs--original-tab-name-format-function
                                               #'tab-bar-tab-name-format-default)
          pretty-tabs--original-tabs-function nil
          pretty-tabs--original-tab-name-format-function nil)
    (advice-remove 'tab-bar--tab #'pretty-tabs--tab)
    (advice-remove 'tab-bar--current-tab-make #'pretty-tabs--current-tab-make)
    (advice-remove 'tab-bar--current-tab #'pretty-tabs--current-tab)
    (advice-remove 'tab-bar--format-tab #'pretty-tabs--format-tab)))

(provide 'pretty-tabs)
;;; pretty-tabs.el ends here
