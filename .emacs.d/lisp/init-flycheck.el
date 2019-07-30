;;; init-flycheck.el --- Flycheck initialisations

;; Author: hjpotter92
;; Maintainer: hjpotter92
;; Version: 0.0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: keywords


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

;; Configure flycheck and related packages.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :custom
  ((flycheck-emacs-lisp-load-path 'inherit)
   (flycheck-check-syntax-automatically '(save mode-enabled))))

(use-package flycheck-package
  :commands flycheck-package-setup
  :after (flycheck)
  :init (flycheck-package-setup))

(use-package flycheck-popup-tip
  :if (display-graphic-p)
  :after (flycheck)
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
