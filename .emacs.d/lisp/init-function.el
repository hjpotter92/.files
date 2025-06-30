;;; init-function.el --- User defined functions, used across the project  -*- lexical-binding: t; -*-

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: internal


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; User defined functions to be used across project

;;; Code:

(eval-when-compile
  (require 'init-const))

(require 'all-the-icons)
(require 's)

(defun my/pretty-symbols ()
  "Add custom symbols in pretty-symbol mode."
  (push '("!=" . ?≠) prettify-symbols-alist)
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("==" . ?≡) prettify-symbols-alist)
  (push '("=>" . ?⇒) prettify-symbols-alist)
  (push '("NOTE" . ?¤) prettify-symbols-alist)
  (push '("TODO" . ?§) prettify-symbols-alist))

(defun with-faicon (icon str &optional height v-adjust)
  "ICON STR HEIGHT V-ADJUST."
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-fileicon (icon str &optional height v-adjust)
  "ICON STR HEIGHT V-ADJUST."
  (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-octicon (icon str &optional height v-adjust)
  "ICON STR HEIGHT V-ADJUST."
  (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-material (icon str &optional height v-adjust)
  "ICON STR HEIGHT V-ADJUST."
  (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-mode-icon (mode str &optional height nospace face)
  "MODE STR HEIGHT NOSPACE FACE."
  (let* ((v-adjust (if (eq major-mode 'emacs-lisp-mode) 0.0 0.05))
         (args     `(:height ,(or height 1) :v-adjust ,v-adjust))
         (_         (when face
                      (lax-plist-put args :face face)))
         (icon     (apply #'all-the-icons-icon-for-mode mode args))
         (icon     (if (symbolp icon)
                       (apply #'all-the-icons-octicon "file-text" args)
                     icon)))
    (s-concat icon (if nospace "" " ") str)))

(provide 'init-function)

;;; init-function.el ends here
