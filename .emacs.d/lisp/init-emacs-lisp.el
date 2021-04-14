;;; init-emacs-lisp.el --- Emacs lisp configuration

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools languages
;; Separator: /


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

;; Emacs-lisp and similar languages configuration

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-function))

(use-package emacs-lisp
  :ensure nil
  :preface
  (defun my/pretty-symbols-lisp ()
    "Lisp symbols."
    (my/pretty-symbols)
    (push '("defun"    . ?ƒ) prettify-symbols-alist)
    (push '("defmacro" . ?μ) prettify-symbols-alist)
    (push '("defvar"   . ?ν) prettify-symbols-alist))
  :hook
  (emacs-lisp . my/pretty-symbols-lisp))

(use-package symbol-overlay
  :bind-keymap
  ("C-c s" . symbol-overlay-map)
  :hook
  (emacs-lisp-mode . symbol-overlay-mode))

(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(provide 'init-emacs-lisp)

;;; init-emacs-lisp.el ends here
