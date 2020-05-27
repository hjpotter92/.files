;;; init-projectile.el --- Projectile and friends

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: internal tools


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

;; Projectile and related packages configuration

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package projectile
  :after (ivy)
  :delight '(:eval (concat " P[" (projectile-project-name) "]"))
  :commands
  (projectile-save-known-projects)
  :custom
  ((projectile-enable-caching nil)
   (projectile-completion-system 'ivy)
   (projectile-require-project-root nil)
   (projectile-create-missing-test-files t)
   (projectile-project-root-files-bottom-up (delete ".git" projectile-project-root-files-bottom-up))
   (projectile-tags-backend "ggtags"))
  :init
  (progn
    (projectile-mode t))
  :bind-keymap
  ("s-p" . projectile-command-map)
  :config
  (progn
    (dolist (item '("GTAGS" "GRTAGS" "GPATH"))
      (add-to-list 'projectile-globally-ignored-files item))
    ;; Git projects should be marked as projects in top-down fashion,
    ;; so that each git submodule can be a projectile project.
    (add-to-list 'projectile-project-root-files ".git")
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)))

(use-package activity-watch-mode
  :after (projectile)
  :delight
  :init
  (global-activity-watch-mode t))

(provide 'init-projectile)

;;; init-projectile.el ends here
