;;; init-python.el --- Programming global modes

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools languages
;; Prefix: my/python
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

;; General programming language settings

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-function))

(setq-default python-environment-directory "~/.virtualenvs")

(defun init-python-pretty-symbols ()
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

(use-package python-docstring
  :hook (python-mode . python-docstring-mode))

(use-package python-mode
  :mode "\\.py"
  :hook
  (python-mode . (lambda ()
                   (init-python-pretty-symbols)))
  :custom
  ((py-split-window-on-execute nil)
   (python-indent-guess-indent-offset nil))
  :mode-hydra
  ("imenu"
   (("m" imenu-list-smart-toggle "toggle imenu"))
   "actions"
   (("!" flycheck-hydra/body "flycheck hydra")
    ("v" pyvenv-hydra/body "pyvenv hydra"))
   "misc"
   (("q" nil "quit hydra"))))

(use-package pyvenv
  :pretty-hydra
  ((:quit-key "q" :title "pyvenv hydra")
   ("Actions"
    (("w" pyvenv-workon "workon")
     ("a" pyvenv-activate "activate")
     ("d" pyvenv-deactivate "deactivate")))))

(use-package pipenv
  :disabled t
  :hook (python-mode . pipenv-mode)
  :delight)

(use-package elpy
  :after (flycheck)
  :disabled
  :delight
  :hook
  (elpy-mode . flycheck-mode)
  :custom
  ((elpy-rpc-backend "jedi")
   (elpy-autodoc-delay 0.400)
   (elpy-rpc-ignored-buffer-size 204800))
  :init
  (progn
    (elpy-enable)
    (mapc (lambda (module) (setq elpy-modules (delq module elpy-modules)))
          '(elpy-module-flymake elpy-module-highlight-indentation elpy-module-django elpy-module-pyvenv))))

(use-package auto-virtualenvwrapper
  :disabled t
  :hook
  (python-mode . auto-virtualenvwrapper-activate))

(provide 'init-python)

;;; init-python.el ends here
