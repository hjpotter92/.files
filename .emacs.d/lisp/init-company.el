;;; init-company.el --- Initial setup for company mode

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: internal


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

;; Setting up company mode, and its related sub-packages

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package company-statistics)

(use-package company
  :requires (company-statistics)
  :bind
  (("<C-tab>" . company-complete)
   (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ([tab] . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous)))
  :init
  (progn
    (global-company-mode t)
    (company-statistics-mode t))
  :custom
  ((company-dabbrev-downcase nil)
   (company-idle-delay 0.4)
   (company-tooltip-align-annotations t)
   (company-tooltip-limit 10)
   (company-require-match nil)
   (company-show-numbers t)
   (company-minimum-prefix-length 2))
  :config
  (progn
    (use-package robe
      :after ruby-mode
      :hook
      (ruby-mode . robe-mode)
      :config
      (add-to-list 'company-backends 'company-robe))
    (use-package company-jedi
      :disabled t
      :config
      (progn
        (add-to-list 'company-backends 'company-jedi)))
    (use-package company-lua
      :mode "\\.lua'")
    (use-package company-flx
      :defer t
      :config
      (company-flx-mode t))))

(use-package company-box
  :delight
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :if (window-system)
  :custom
  ((company-quickhelp-delay 0.25))
  :init
  (progn
    (company-quickhelp-mode t)))

(use-package company-fuzzy
  :after (company)
  :delight
  :init
  (global-company-fuzzy-mode t))

(provide 'init-company)

;;; init-company.el ends here
