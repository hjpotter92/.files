;;; init-emacs-lisp.el --- Emacs lisp configuration

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

;; Emacs-lisp and similar languages configuration

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-function))

(use-package emacs-lisp-mode
  :ensure nil
  :after (major-mode-hydra)
  :preface
  (defun my/pretty-symbols-lisp ()
    "Lisp symbols."
    (my/pretty-symbols)
    (push '("defun"    . ?ƒ) prettify-symbols-alist)
    (push '("defmacro" . ?μ) prettify-symbols-alist)
    (push '("defvar"   . ?ν) prettify-symbols-alist))
  :hook
  (emacs-lisp . my/pretty-symbols-lisp)
  :mode-hydra
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" counsel-describe-function "function")
    ("v" counsel-describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))
   "Tools"
   (("!" flycheck-hydra/body "flycheck")
    ("i" flyspell-hydra/body "flyspell")
    ("y" yasnippet-hydra/body "yasnippet"))
   "Quit"
   (("q" nil "quit hydra"))))

(use-package eros
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(provide 'init-emacs-lisp)

;;; init-emacs-lisp.el ends here
