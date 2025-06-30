;;; init-company.el --- Initial setup for company mode  -*- lexical-binding: t; -*-

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
  :hook
  ((after-init . global-company-mode)
   ;; (after-init . company-tng-mode)
   )
  :delight
  :requires (company-statistics)
  :bind
  (("<C-tab>" . company-complete-common)
   (:map company-mode-map
         ([remap completion-at-point] . company-complete-common)
         ([remap indent-for-tab-command] . company-indent-or-complete-common))
   (:map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("M-/" . company-other-backend)
         ("M-." . company-show-location)
         ("C-d" . company-show-doc-buffer)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ([tab] . company-complete-common-or-cycle)
         ("<backtab>" . company-select-previous)))
  :config
  (progn
    (company-statistics-mode t))
  :custom
  ((company-backends
    '((company-capf :with company-yasnippet :with company-dabbrev-code :with company-dabbrev :with company-keywords)))
   (company-dabbrev-code-modes t)
   (company-dabbrev-downcase nil)
   (company-dabbrev-minimum-length 3)
   (company-dabbrev-other-buffers t)
   (company-eclim-autosave nil)
   (company-files-exclusions '(".git/"))
   (company-format-margin-function 'company-detect-icons-margin)
   (company-global-modes '(not erc-mode eshell-mode message-mode))
   (company-idle-delay 0)
   (company-minimum-prefix-length 1)
   (company-require-match nil)
   (company-selection-wrap-around t)
   (company-show-numbers t)
   (company-tooltip-align-annotations t)
   (company-tooltip-limit 10)
   (company-tooltip-flip-when-above t)))

(use-package company-fuzzy
  :after (company)
  :disabled t
  :config
  (company-fuzzy-mode t)
  :delight)

(use-package company-lua
  :mode "\\.lua'")

(use-package company-flx
  :after (company)
  :delight
  :disabled t
  :hook
  (company-mode . company-flx-mode))

(use-package company-box
  :disabled
  :delight
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  :hook
  (company-mode . company-box-mode))

(use-package company-quickhelp
  :if (display-graphic-p)
  :after (company)
  :custom
  ((company-quickhelp-delay 0.5))
  :hook
  (company-mode . company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :unless (display-graphic-p)
  :hook
  (company-mode . company-quickhelp-terminal-mode))

(provide 'init-company)

;;; init-company.el ends here
