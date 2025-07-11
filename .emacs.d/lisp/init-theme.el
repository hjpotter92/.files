;;; init-theme.el --- Set up theme and related UI items  -*- lexical-binding: t; -*-

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: convenience faces


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

;; Theme, icons, font faces, mode line etc.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package monokai-pro-theme
  :if (display-graphic-p)
  :config
  (progn
    (load-theme 'monokai-pro t)))

(use-package idle-highlight-in-visible-buffers-mode
  :disabled t
  :hook
  (prog-mode . idle-highlight-in-visible-buffers-mode))

(use-package all-the-icons
  :if (display-graphic-p)
  :custom
  ((all-the-icons-default-adjust 0)
   (all-the-icons-scale-factor 1)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :init
  (all-the-icons-ibuffer-mode t))

(use-package all-the-icons-ivy
  :custom
  (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window ivy-switch-buffer))
  :init
  (progn
    (use-package all-the-icons-ivy-rich
      :init
      (all-the-icons-ivy-rich-mode t))
    (use-package all-the-icons-completion
      :init
      (all-the-icons-completion-mode t)))
  :config
  (progn
    (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
    (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
    (all-the-icons-ivy-setup)))

(use-package all-the-icons-nerd-fonts
  :after all-the-icons
  :config
  (progn
    (all-the-icons-nerd-fonts-prefer)))

(use-package smart-mode-line
  :custom
  ((sml/no-confirm-load-theme t)
   (sml/mode-width 'full)
   (sml/name-width 64))
  :preface
  (setq-local h-mode-line-patterns
              '(("^:Doc:.files/" . ":.f:")
                ("^:Doc:work/\\([^/]+\\)/" . ":W:\\1:")
                ("^:W:livepeer-\\([^/]+\\):" . ":LP-\\1:")
                ("^:.f:.emacs.d/" . ":ED:")
                ("^:Doc:projects/\\([^/]+\\)/" . ":\\1:")))
  :init
  (sml/setup)
  :config
  (progn
    (mapc (lambda (it)
            (add-to-list 'sml/replacer-regexp-list (list (car it) (cdr it)) t))
          h-mode-line-patterns)))

(use-package mode-icons
  :config
  (mode-icons-mode t))

(use-package major-mode-icons
  :commands (major-mode-icons-mode)
  :config
  (major-mode-icons-mode 1))

(use-package dashboard
  :custom
  ((dashboard-center-content nil)
   (dashboard-items '((projects . 20)
		      (recents . 15)
		      (registers . 5)
		      (bookmarks . 5)))
   (dashboard-projects-backend 'projectile)
   (dashboard-projects-switch-function 'counsel-projectile-switch-project-action)
   (dashboard-set-file-icons t)
   (dashboard-set-heading-icons t)
   (dashboard-set-init-info t)
   (dashboard-set-navigator t)
   (dashboard-startup-banner 'logo))
  :config
  (dashboard-setup-startup-hook))

(provide 'init-theme)

;;; init-theme.el ends here
