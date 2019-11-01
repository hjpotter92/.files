;;; init-go.el --- Programming global modes fo golang

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools languages


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

;; General programming language settings

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-function))

(setq-default python-environment-directory "~/.virtualenvs")

(defun init-go-pretty-symbols ()
  "Python specific pretty symbols."
  (my/pretty-symbols)
  (push '("def"    . ?ƒ) prettify-symbols-alist)
  (push '("sum"    . ?Σ) prettify-symbols-alist)
  (push '("**2"    . ?²) prettify-symbols-alist)
  (push '("**3"    . ?³) prettify-symbols-alist)
  (push '("None"   . ?∅) prettify-symbols-alist)
  (push '("True"   . ?⥾) prettify-symbols-alist)
  (push '("False"  . ?⥿) prettify-symbols-alist)
  (push '("is"     . ?≣) prettify-symbols-alist)
  (push '("is not" . ?≢) prettify-symbols-alist)
  (push '("in"     . ?∈) prettify-symbols-alist)
  (push '("not in" . ?∉) prettify-symbols-alist)
  (push '("return" . ?⟾) prettify-symbols-alist))

(use-package go-mode
  :mode "\\.go"
  :hook
  (go-mode . (lambda ()
               (init-go-pretty-symbols)))
  :custom
  ((gofmt-command "goimports"))
  :mode-hydra
  ("imenu"
   (("m" lsp-ui-imenu "toggle imenu"))
   "actions"
   (("!" flycheck-hydra/body "flycheck hydra"))
   "misc"
   (("q" nil "quit hydra")))
  :config
  (progn
    (use-package go-guru)
    (use-package company-go)
    (use-package golint)
    (use-package go-errcheck)))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(provide 'init-go)

;;; init-go.el ends here
