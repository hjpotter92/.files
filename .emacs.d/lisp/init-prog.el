;;; init-prog.el --- Programming global modes

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools


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
  (require 'init-const))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :config
  (progn
    (use-package highlight-numbers
      :hook (prog-mode . highlight-numbers-mode))))

(use-package editorconfig
  :delight
  :hook (after-init . editorconfig-mode))

(use-package realgud
  :init
  (load-library "realgud"))

(use-package emacs-lisp-mode
  :ensure nil
  :after (major-mode-hydra)
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

(provide 'init-prog)

;;; init-prog.el ends here
