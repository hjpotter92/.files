;;; init-theme.el --- Set up theme and related UI items

;; Author: hjpotter92
;; Maintainer: hjpotter92
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: convenience


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

(use-package focus
  :hook
  ((lisp-mode emacs-lisp-mode ruby-mode) . focus-mode)
  :bind ("C-c f" . focus-mode))

(use-package rainbow-mode
  :diminish
  :hook
  (prog-mode . rainbow-mode))

(use-package highlight-parentheses
  :ensure t
  :delight
  (highlight-parentheses-mode " ❪❫")
  (global-highlight-parentheses-mode " ❪❫")
  :config
  (progn
    (global-highlight-parentheses-mode t)))

(use-package beacon
  :diminish
  :init
  (beacon-mode t))

(use-package idle-highlight-in-visible-buffers-mode
  :ensure t
  :hook
  (prog-mode . idle-highlight-in-visible-buffers-mode))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :after (ivy all-the-icons counsel)
  :custom
  (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window ivy-switch-buffer))
  :config
  (progn
    (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
    (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
    (all-the-icons-ivy-setup)))

(use-package smart-cursor-color
  :diminish smart-cursor-color-mode
  :config
  (global-hl-line-mode t)
  (smart-cursor-color-mode t))

(use-package centaur-tabs
  :if (display-graphic-p)
  :after (smart-mode-line)
  :commands
  (centaur-tabs-group-by-projectile-project)
  :custom
  ((centaur-tabs-style "slant")
   (centaur-tabs-set-icons t)
   (centaur-tabs-set-modified-marker t)
   (centaur-tabs-set-bar 'over))
  :init
  (progn
    (centaur-tabs-mode t)
    (centaur-tabs-group-by-projectile-project))
  :bind
  (("s-<prior>" . centaur-tabs-backward)
   ("s-<next>" . centaur-tabs-forward)
   ("C-c t" . centaur-tabs-counsel-switch-group)))

(use-package smart-mode-line
  :after (monokai-pro-theme)
  :custom
  ((sml/no-confirm-load-theme t)
   (sml/mode-width 'full))
  :preface
  (setq-local h-mode-line-patterns
    '(("^:Doc:.files/" . ":.f:")
      ("^:Doc:przemek/app/" . ":PRZK:")
      ("^:Doc:\\(projects|src\\)/\\([^/]+\\)/" . ":\\2:")))
  :init
  (sml/setup)
  :config
  (progn
    (mapc (lambda (it)
            (add-to-list 'sml/replacer-regexp-list (list (car it) (cdr it)) t))
          h-mode-line-patterns)))

(use-package mode-icons
  :after (smart-mode-line)
  :config (mode-icons-mode))

(provide 'init-theme)

;;; init-theme.el ends here
