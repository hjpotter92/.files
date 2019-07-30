;;; init-buffer.el --- Buffer handling setup

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools convenience


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

;; Configuration for buffer handling in Emacs.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package ibuffer
  :ensure nil
  :commands
  (ibuffer-auto-mode
   ibuffer-switch-to-saved-filter-groups)
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-auto-mode t)
                    (ibuffer-switch-to-saved-filter-groups "default")))
  :custom
  ((ibuffer-expert t)
   (ibuffer-show-empty-filter-groups nil)
   (ibuffer-saved-filter-groups
    (quote
     (("default"
       (" dired" (mode . dired-mode))
       (" perl" (mode . cperl-mode))
       (" planner"
        (or
         (name . "^\\*Calendar\\*$")
         (name . "^diary$")
         (mode . muse-mode)))
       ("emacs"
        (or
         (name . "^\\*scratch\\*$")
         (name . "^\\*Messages\\*$")))
       ("org" (name . "^.*org$"))
       (" magit" (mode . magit-mode))
       (" gnus"
        (or
         (mode . message-mode)
         (mode . bbdb-mode)
         (mode . mail-mode)
         (mode . gnus-group-mode)
         (mode . gnus-summary-mode)
         (mode . gnus-article-mode)
         (name . "^\\.bbdb$")
         (name . "^\\.newsrc-dribble")))
       ("爵 web" (or (mode . web-mode) (mode . js2-mode)))
       (" shell" (or (mode . eshell-mode) (mode . shell-mode)))))))))

(use-package ibuffer-vc
  :after (ibuffer)
  :commands
  (ibuffer-do-sort-by-alphabetic)
  :hook (ibuffer-mode . (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(provide 'init-buffer)

;;; init-buffer.el ends here
