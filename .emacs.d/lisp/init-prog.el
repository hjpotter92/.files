;;; init-prog.el --- Programming global modes

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools


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

;; General programming language settings

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :config
  (progn
    (use-package highlight-numbers
      :hook (prog-mode . highlight-numbers-mode))))

(use-package editorconfig
  :delight
  :hook (after-init . editorconfig-mode))

(use-package hl-indent-scope
  :disabled
  :hook (prog-mode . hl-indent-scope-mode))

(use-package symbol-overlay
  :bind-keymap
  ("C-c s" . symbol-overlay-map)
  :hook
  (prog-mode . symbol-overlay-mode))

(use-package virtual-comment
  :delight
  :config
  (add-hook 'find-file-hook 'virtual-comment-mode))

(provide 'init-prog)

;;; init-prog.el ends here
