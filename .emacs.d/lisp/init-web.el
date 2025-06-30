;;; init-web.el --- Web mode and related packages  -*- lexical-binding: t; -*-

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

;; Configure web mode and its related packages

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package css-mode
  :custom
  (css-indent-offset 2)
  :mode
  ("\\.css\\'"))

(use-package web-mode
  :after company
  :mode
  (("\\.[pm]?html?$" . web-mode)
   ("\\.jsx?$" . web-mode)
   ("\\.tmpl" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))
  :custom
  ((web-mode-markup-indent-offset 2)
   (web-mode-css-indent-offset 2)
   (web-mode-code-indent-offset 2)
   (web-mode-enable-current-element-highlight t)
   (web-mode-enable-current-column-highlight t)
   (web-mode-enable-auto-pairing nil)
   (web-mode-content-types-alist
    '(("jsx" . "\\.jsx?\\'")
      ("json" . "\\.json\\'")
      ("xml" . "\\.xml\\'")))
   (web-mode-engines-alist
    '(("php" . "\\.phtml?\\'")
      ("reactjs" . "\\.jsx\\'")
      ("blade" . "\\.blade\\."))))
  :config
  (progn
    (use-package company-web
      :ensure nil
      :bind
      (("C-c w" . company-web-html))
      :init
      (setq js-indent-level 2)
      :config
      (add-to-list 'company-backends 'company-web-html))))

(use-package emmet-mode
  :hook ((sgml-mode html-mode css-mode web-mode) . emmet-mode))

(provide 'init-web)

;;; init-web.el ends here
