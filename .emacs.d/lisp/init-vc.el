;;; init-vc.el --- Magit and other VC settings

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: internal tools vc


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

;; VC support in Emacs

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package magit
  :after (projectile)
  :commands
  (magit-get-top-dir projectile-add-known-project magit-list-repos)
  :pretty-hydra
  ((:quit-key "q" :title "Magit bindings" :color teal)
   ("Blame"
    (("B" magit-blame "blame"))
    "Log"
    (("l" magit-log-all-branches "log"))
    "Action"
    (("$" magit-process "process"))
    "Popup"
    (("b" magit-branch-popup "branch")
     ("F" magit-pull-popup "pull")
     ("r" magit-rebase-popup "rebase")
     ("f" magit-fetch-popup "fetch")
     ("P" magit-push-popup "push popup"))
    "Quit"
    (("q" nil "quit hydra"))))
  :bind
  (("C-c g" . magit-status)
   ("C-c C-g" . magit-hydra/body))
  :init
  (progn
    ;; we no longer need vc-git
    (delete 'Git vc-handled-backends))
  :custom
  ((magit-repository-directories '(("~/Documents/src" . 3)
                                   ("~/Documents/projects/" . 2)
                                   ("~/Documents/" . 1)))
   (magit-completing-read-function 'ivy-completing-read)
   (magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
   (magit-diff-refine-hunk t))
  :config
  (progn
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))))

(use-package magit-patch-changelog
  :after (magit))

(use-package git-gutter+
  :delight
  :init
  (progn
    (global-git-gutter+-mode t)))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(provide 'init-vc)

;;; init-vc.el ends here
