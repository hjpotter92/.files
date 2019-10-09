;;; init-flyspell.el --- Flyspell initialisation

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: internal convenience


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

;; Configure flyspell and its dictionaries.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package flyspell
  :hook
  ((prog-mode . flyspell-prog-mode)
   ((text-mode log-edit-mode) . flyspell-mode))
  :custom
  ((flyspell-issue-message-flag nil)
   (ispell-list-command "--list"))
  :pretty-hydra
  ((:quit-key "q" :title "flyspell hydra")
   ("Spell check"
    (("b" flyspell-buffer "buffer")
     ("r" flyspell-region "region")
     ("w" flyspell-word "word"))
    "Movements"
    (("n" flyspell-goto-next-error "next"))
    "Corrections"
    (("N" flyspell-correct-next "fix next")
     ("P" flyspell-correct-previous "fix previous")
     ("W" flyspell-correct-word "fix word")
     ("T" flyspell-correct-at-point "fix at point"))
    "Actions"
    (("f" flyspell-mode "spell check mode")
     ("F" flyspell-prog-mode "prog-spell check mode")
     ("q" nil "quit hydra")))))

(use-package flyspell-correct-ivy
  :bind
  ("C-M-;" . flyspell-correct-wrapper)
  :custom
  (flyspell-correct-interface #'flyspell-correct-ivy))

(use-package flyspell-lazy
  :init
  (flyspell-lazy-mode t))

(provide 'init-flyspell)

;;; init-flyspell.el ends here
