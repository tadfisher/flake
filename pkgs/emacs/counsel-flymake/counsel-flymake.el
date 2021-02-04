;;; counsel-flymake.el --- Flymake integration for Counsel/Ivy  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Tad Fisher

;; Author: Tad Fisher <tad@dirac>
;; Keywords: convenience, tools
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (flymake "1.0.1") (counsel "0.13.0"))

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

;;; Code:

(require 'counsel)
(require 'flymake)

;;** `counsel-flymake'
(defun counsel-flymake-errors-cands ()
  "Generate candidates for Flymake errors."
  (let* ((flymake--diagnostics-buffer-source (current-buffer))
         (file (let ((file (buffer-file-name flymake--diagnostics-buffer-source)))
                 (when file (file-name-base (file-name-sans-extension file)))))
         (errors (flymake--diagnostics-buffer-entries)))
    (cl-loop with diag     = nil
             with vec      = nil ; a vector containing already propertized attributes
             with line     = nil
             with message  = nil
             for err in errors ; for structure of err see:`flymake--diagnostics-buffer-entries'
             do (setq diag    (plist-get (car err) :diagnostic)
                      vec     (nth 1 err)
                      line    (aref vec 0)
                      message (concat (aref vec 2) " " (car (aref vec 3))))
             collect (propertize (concat (when file (concat file ":"))
                                         line ":"
                                         message)
                                 'point (flymake--diag-beg diag)))))

;;;###autoload
(defun counsel-flymake ()
  "Flymake errors."
  (interactive)
  (require 'flymake)
  (counsel-mark--ivy-read "flymake errors: " (counsel-flymake-errors-cands) 'counsel-flymake))

(provide 'counsel-flymake)
;;; counsel-flymake.el ends here
