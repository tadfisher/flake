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

(defgroup pretty-tabs nil
  "Pretty ‘tab-bar-mode' display."
  :group 'tab-bar
  :prefix "pretty-tabs-")

(defcustom pretty-tabs-close-tab-image nil
  "Image spec to use for the tab close button."
  :type `(choice (const :tag "None" nil)
                 (plist :tag "Image spec")
                 (sexp :tag "Other"))
  :group 'pretty-tabs)

(defcustom pretty-tabs-close-tab-image-inactive nil
  "Image spec to use for the tab close button in inactive tabs."
  :type `(choice (const :tag "None" nil)
                 (plist :tag "Image spec")
                 (sexp :tag "Other"))
  :group 'pretty-tabs)

(defcustom pretty-tabs-new-tab-image nil
  "Image spec to use for the new tab button."
  :type `(choice (const :tag "None" nil)
                 (plist :tag "Image spec")
                 (sexp :tag "Other"))
  :group 'pretty-tabs)

(defcustom pretty-tabs-tab-height 36
  "Preferred tab height in pixels."
  :type '(integer :tag "Height (px)")
  :group 'pretty-tabs)

(defcustom pretty-tabs-tab-margin 8
  "Horizontal tab margin in pixels."
  :type '(integer :tag "Width (px)")
  :group 'pretty-tabs)

(defcustom pretty-tabs-image-margin 8
  "Margin between images or buttons and the tab text."
  :type '(integer :tag "Width (px)")
  :group 'pretty-tabs)

(defcustom pretty-tabs-buffer-icon t
  "Show an icon for the current buffer using ‘all-the-icons-icon-for-buffer'."
  :type 'boolean
  :group 'pretty-tabs)

(defconst pretty-tabs-default-binding
  `(menu-item ,(purecopy "tab bar") ignore
              :filter tab-bar-make-keymap))

(defconst pretty-tabs-pretty-binding
  `(menu-item ,(purecopy "tab bar") ignore
              :filter pretty-tabs-make-keymap))

(defconst pretty-tabs--close-button (concat tab-bar-close-button))
(defconst pretty-tabs--close-button-inactive (concat tab-bar-close-button))
(defconst pretty-tabs--new-button (concat tab-bar-new-button))
(defconst pretty-tabs--tab-space " ")
(defconst pretty-tabs--image-space " ")

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

(defun pretty-tabs--watch-close-tab-image (_sym val op _where)
  (when (eq op 'set)
    (pretty-tabs--update-button pretty-tabs--close-button val
                                'close-tab t
                                :help "Close tab")))

(defun pretty-tabs--watch-close-tab-image-inactive (_sym val op _where)
  (when (eq op 'set)
    (pretty-tabs--update-button pretty-tabs--close-button-inactive val
                                'close-tab t
                                :help "Close tab")))

(defun pretty-tabs--watch-new-tab-image (_sym val op _where)
  (when (eq op 'set)
    (pretty-tabs--update-button pretty-tabs--new-button val)))

(defun pretty-tabs--watch-tab-height (_sym val op _where)
  (when (eq op 'set)
    (pretty-tabs--update-spaces val
                                pretty-tabs-tab-margin
                                pretty-tabs-image-margin)))

(defun pretty-tabs--watch-tab-margin (_sym val op _where)
  (when (eq op 'set)
    (pretty-tabs--update-spaces pretty-tabs-tab-height
                                val
                                pretty-tabs-image-margin)))

(defun pretty-tabs--watch-image-margin (_sym val op _where)
  (when (eq op 'set)
    (pretty-tabs--update-spaces pretty-tabs-tab-height
                                pretty-tabs-tab-margin
                                val)))

(defun pretty-tabs--add-watchers ()
  "Setup variable watchers."
  (add-variable-watcher 'pretty-tabs-close-tab-image #'pretty-tabs--watch-close-tab-image)
  (add-variable-watcher 'pretty-tabs-close-tab-image-inactive
                        #'pretty-tabs--watch-close-tab-image-inactive)
  (add-variable-watcher 'pretty-tabs-new-tab-image #'pretty-tabs--watch-new-tab-image)
  (add-variable-watcher 'pretty-tabs-tab-height #'pretty-tabs--watch-tab-height)
  (add-variable-watcher 'pretty-tabs-tab-margin #'pretty-tabs--watch-tab-margin)
  (add-variable-watcher 'pretty-tabs-image-margin #'pretty-tabs--watch-image-margin))

(defun pretty-tabs--remove-watchers ()
  "Remove variable watchers."
  (remove-variable-watcher 'pretty-tabs-close-tab-image #'pretty-tabs--watch-close-tab-image)
  (remove-variable-watcher 'pretty-tabs-close-tab-image-inactive
                        #'pretty-tabs--watch-close-tab-image-inactive)
  (remove-variable-watcher 'pretty-tabs-new-tab-image #'pretty-tabs--watch-new-tab-image)
  (remove-variable-watcher 'pretty-tabs-tab-height #'pretty-tabs--watch-tab-height)
  (remove-variable-watcher 'pretty-tabs-tab-margin #'pretty-tabs--watch-tab-margin)
  (remove-variable-watcher 'pretty-tabs-image-margin #'pretty-tabs--watch-image-margin))

(defun pretty-tabs-tab-icon (&optional f)
  "Return the tab icon for the active buffer in frame F."
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
                 'display '(raise -0.2))))

(defun pretty-tabs-make-tabs (&optional frame)
  "Return a list of tabs belonging to the selected FRAME.
Ensure the frame parameter `tabs' is pre-populated.
Update the current tab name when it exists.
Return its existing value or a new value."
  (let ((tabs (frame-parameter frame 'tabs)))
    (if tabs
        (let* ((current-tab (assq 'current-tab tabs))
               (current-tab-name (assq 'name current-tab))
               (current-tab-explicit-name (assq 'explicit-name current-tab))
               (current-tab-icon (assq 'icon current-tab)))
          (when (and current-tab-name
                     current-tab-explicit-name
                     (not (cdr current-tab-explicit-name)))
            (setf (cdr current-tab-name)
                  (funcall tab-bar-tab-name-function)))
          (when current-tab-icon
            (setf (cdr current-tab-icon)
                  (pretty-tabs-tab-icon))))
      ;; Create default tabs
      (setq tabs (list (tab-bar--current-tab)))
      (set-frame-parameter frame 'tabs tabs))
    tabs))

(defun pretty-tabs-make-keymap (&optional _ignore)
  "Generate an actual keymap from `tab-bar-map'.
Its main job is to show tabs in the tab bar."
  (if (= 1 (length tab-bar-map))
      (pretty-tabs-make-keymap-1)
    (let ((key (cons (frame-terminal) tab-bar-map)))
      (or (gethash key tab-bar-keymap-cache)
          (puthash key tab-bar-map tab-bar-keymap-cache)))))

(defun pretty-tabs-render-tab (tab active pos show-close)
  "Render TAB to a propertized string.
If ACTIVE is non-nil, render the tab as active.
POS is the index of the tab starting with 1.
MARGIN is the propertized margin string.
IMAGE-MARGIN is the propertized image margin string.
If SHOW-CLOSE is non-nil, render a close button in the tab."
  (let* ((face (if active 'tab-bar-tab 'tab-bar-tab-inactive))
         (fg (face-foreground face nil t))
         (bg (face-background face nil t)))
    (concat
     (propertize pretty-tabs--tab-space 'face face)
     (if-let* ((pretty-tabs-buffer-icon)
               (icon (alist-get 'icon tab))
               (iface (get-text-property 0 'face icon)))
         (concat (propertize icon
                             'face (append iface `(:background ,bg)))
                 (propertize pretty-tabs--image-space 'face face))
       "")
       (if tab-bar-tab-hints
           (propertize (format "%d " pos) 'face face)
         "")
    (propertize (alist-get 'name tab) 'face face)
    (if-let* ((show-close)
              (icon (if active pretty-tabs--close-button pretty-tabs--close-button-inactive))
              (iface (get-text-property 0 'face icon)))
        (concat (propertize pretty-tabs--image-space 'face face)
                (propertize icon
                            'face (append iface `(:foreground ,fg :background ,bg))))
      "")
    (propertize pretty-tabs--tab-space 'face face))))

(defun pretty-tabs-make-keymap-1 ()
  "Generate an actual keymap from `tab-bar-map', without caching."
  (let* ((separator (or tab-bar-separator (if window-system " " "|")))
         (left-separator
          (if window-system
              (propertize " " 'display '(space :width left-fringe))
            "|"))
         (right-separator
          (if window-system
              (propertize " " 'display '(space :width right-fringe))
            "|"))
         (i 0)
         (tabs (funcall tab-bar-tabs-function)))
    (append
     '(keymap (mouse-1 . tab-bar-handle-mouse))
     (when tab-bar-history-mode
       `((sep-history-back menu-item ,separator ignore)
         (history-back
          menu-item ,tab-bar-back-button tab-bar-history-back
          :help "Click to go back in tab history")
         (sep-history-forward menu-item ,separator ignore)
         (history-forward
          menu-item ,tab-bar-forward-button tab-bar-history-forward
          :help "Click to go forward in tab history")))
     (mapcan
      (lambda (tab)
        (setq i (1+ i))
        (append
         `((,(intern (format "sep-%i" i)) menu-item
            ,(pcase i
              (1 left-separator)
              (_ separator))
            ignore))
         (cond
          ((eq (car tab) 'current-tab)
           `((current-tab
              menu-item
              ,(pretty-tabs-render-tab tab t i
                                       (and tab-bar-close-button-show
                                            (not (eq tab-bar-close-button-show
                                                     'non-selected))))
              ignore
              :help "Current tab")))
          (t
           `((,(intern (format "tab-%i" i))
              menu-item
              ,(pretty-tabs-render-tab tab nil i
                                       (and tab-bar-close-button-show
                                            (not (eq tab-bar-close-button-show
                                                     'selected))))
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
      tabs)
     (when tab-bar-new-button
       `((sep-add-tab menu-item ,separator ignore)
         (add-tab menu-item ,pretty-tabs--new-button tab-bar-new-tab
                  :help "New tab"))))))

(defun pretty-tabs--tab (tab-fun &optional frame)
  "Advise TAB-FUN to retain the ’icon’ entry.
Pass FRAME to TAB-FUN."
  (let* ((tab (assq 'current-tab (frame-parameter frame 'tabs)))
         (icon (alist-get 'icon tab))
         (res (funcall tab-fun frame)))
    (append res `((icon . ,icon)))))

(defun pretty-tabs--current-tab (current-tab-fun &optional tab frame)
  "Advise CURRENT-TAB-FUN to retain the ’icon’ entry.
Pass TAB and FRAME to TAB-FUN."
  (let* ((tab (or tab (assq 'current-tab (frame-parameter frame 'tabs))))
         (icon (alist-get 'icon tab))
         (res (funcall current-tab-fun tab frame)))
    (append res `((icon . ,icon)))))

(define-minor-mode pretty-tabs-mode
  "Make tab-bar-mode prettier."
  :global t
  :lighter nil
  (if pretty-tabs-mode
      (progn
        (advice-add 'tab-bar--tab
                    :around #'pretty-tabs--tab)
        (advice-add 'tab-bar--current-tab
                    :around #'pretty-tabs--current-tab)
        (pretty-tabs--add-watchers)
        (pretty-tabs--update-button pretty-tabs--close-button
                                    pretty-tabs-close-tab-image)
        (pretty-tabs--update-button pretty-tabs--close-button-inactive
                                    (or pretty-tabs-close-tab-image-inactive
                                        pretty-tabs-close-tab-image))
        (pretty-tabs--update-button pretty-tabs--new-button
                                    pretty-tabs-new-tab-image)
        (pretty-tabs--refresh-spaces)
        (global-set-key [tab-bar] pretty-tabs-pretty-binding)
        (setq tab-bar-tabs-function #'pretty-tabs-make-tabs)
        (tab-bar-mode -1)
        (tab-bar-mode 1))
    (global-set-key [tab-bar] pretty-tabs-default-binding)
    (setq tab-bar-tabs-function #'pretty-tabs-make-tabs)
    (advice-remove 'tab-bar--tab #'pretty-tabs--tab)
    (advice-remove 'tab-bar--current-tab #'pretty-tabs--current-tab)
    (pretty-tabs--remove-watchers)))

(provide 'pretty-tabs)
;;; pretty-tabs.el ends here
