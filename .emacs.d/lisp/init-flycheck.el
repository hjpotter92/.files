;;; init-flycheck.el --- Flycheck initialisations

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

;; Configure flycheck and related packages.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :pretty-hydra
  ((:quit-key "q" :title " Flycheck")
   ("Move"
   (("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("^" flycheck-first-error "first")
    ("g" flycheck-error-goto-error "goto"))
   "Actions"
   (("l" flycheck-list-errors "list" :exit t)
    ("h" flycheck-manual "manual" :exit t)
    ("v" flycheck-verify-setup "verify setup")
    ("c" flycheck-buffer "check buffer"))
   "Checkers"
   (("f" flycheck-verify-checker "verify checker")
    ("?" flycheck-describe-checker "describe")
    ("x" flycheck-disable-checker "disable")
    ("s" flycheck-select-checker "select"))
   "Miscellaneous"
   (("C" flycheck-clear "clear")
    ("V" flycheck-version "version")
    ("q" nil "quit hydra"))))
  :config
  (progn
    (flycheck-add-mode 'javascript-eslint 'web-mode)))

(use-package flycheck-pycheckers
  :hook
  (flycheck-mode . flycheck-pycheckers-setup))

(use-package flycheck-package
  :commands flycheck-package-setup
  :init (flycheck-package-setup))

(use-package flycheck-popup-tip
  :if (display-graphic-p)
  :after (flycheck)
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(use-package flycheck-indicator
  :after (flycheck)
  :hook (flycheck-mode . flycheck-indicator-mode))

(use-package flycheck-elixir)

(provide 'init-flycheck)

;;; init-flycheck.el ends here
