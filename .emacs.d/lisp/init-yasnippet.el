;;; init-yasnippet.el --- Setup yasnippet package and its snippets

;; Author: hjpotter92
;; Maintainer: hjpotter92
;; Version: 0.0.1
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: abbrev


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

;; Yasnippet package configuration.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package yasnippet
  :init (yas-global-mode t)
  :bind
  ("C-c y" . yasnippet-hydra/body)
  :pretty-hydra
  ((:quit-key "q" :color amaranth)
   ("Modes"
    (("g" yas/global-mode "Global")
     ("m" yas/minor-mode "Minor")
     ("e" yas-activate-extra-mode "Extra"))
    "Load/Visit"
    (("d" yas-load-directory "Directory")
     ("f" yas-visit-snippet-file "File" :color blue)
     ("l" yas-describe-tables "List")
     ("a" yas-reload-all "All"))
    "Actions"
    (("i" yas-insert-snippet "Insert")
     ("n" yas-new-snippet "New")
     ("t" yas-tryout-snippet "Tryout")
     ("q" nil "Quit hydra"))))
  :config
  (use-package yasnippet-snippets)
  (use-package auto-yasnippet)
  (use-package ivy-yasnippet))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
