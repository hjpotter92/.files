;;; init-lsp.el --- LSP mode settings

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: convenience tools internal


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

;; commentary

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  ((lsp-prefer-flymake nil)
   (lsp-auto-guess-root t)
   (lsp-auto-configure t)
   (lsp-before-save-edits nil))
  :hook
  ((web-mode js2-mode css-mode dockerfile-mode python-mode ruby-mode c++-mode c-mode) . lsp))

(use-package lsp-python-ms
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-python-ms)
                   (lsp))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  ((lsp-ui-sideline-ignore-duplicates t)
   (lsp-ui-flycheck t)
   (lsp-ui-doc-include-signature t)
   (lsp-ui-doc-use-webkit t)
   (lsp-ui-sideline-ignore-duplicate t)
   (lsp-ui-sideline-show-code-actions nil))
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (progn
    (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
      (setq mode-line-format nil))))

(use-package company-lsp
  :config
  (push 'company-lsp company-backends)
  :custom
  ((company-transformers nil)
   (company-lsp-async t)
   (company-lsp-cache-candidates nil)))

(provide 'init-lsp)

;;; init-lsp.el ends here
