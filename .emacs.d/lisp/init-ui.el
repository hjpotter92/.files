;;; init-ui.el --- Set up UI items

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
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

;; UI elements

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package focus
  :hook
  ((lisp-mode emacs-lisp-mode ruby-mode) . focus-mode)
  :bind ("C-c f" . focus-mode))

(use-package modern-fringes
  :init
  (progn
    (modern-fringes-invert-arrows)))

(use-package rainbow-mode
  :delight
  :hook
  (prog-mode . rainbow-mode))

(use-package highlight-parentheses
  :delight
  (highlight-parentheses-mode " ❪❫")
  (global-highlight-parentheses-mode " ❪❫")
  :config
  (progn
    (global-highlight-parentheses-mode t)))

(use-package beacon
  :delight
  :init
  (beacon-mode t))

(use-package idle-highlight-in-visible-buffers-mode
  :disabled
  :hook
  (prog-mode . idle-highlight-in-visible-buffers-mode))

(use-package minibuffer-line
  :if (display-graphic-p)
  :disabled t
  :defer 1
  :init
  (progn
    (setq-default display-time-format "")
    (setq minibuffer-line-format (format-time-string "%l:%M %b %d %a")))
  :config
  (minibuffer-line-mode))

(use-package smart-cursor-color
  :delight
  :config
  (global-hl-line-mode t)
  (smart-cursor-color-mode t))

(use-package centaur-tabs
  :disabled
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

(use-package sublimity
  :hook
  (prog-mode . sublimity-mode))

(use-package modern-fringes
  :init
  (progn
    (modern-fringes-mode t)
    (modern-fringes-invert-arrows)))

(provide 'init-ui)

;;; init-ui.el ends here
