;;; init-ivy.el --- Setup ivy, swiper and counsel

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: convenience files tools


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

;; Ivy, swiper and counsel settings

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package amx
  :custom
  ((amx-backend 'ivy)
   (amx-history-length 20)
   (amx-show-key-bindings t))
  :bind
  ("M-S-X" . amx-major-mode-commands))

(use-package ivy
  :delight
  :custom
  ((enable-recursive-minibuffers t)
   (ivy-display-style 'fancy)
   (ivy-extra-directories nil)
   (ivy-initial-inputs-alist nil)
   (ivy-on-del-error-function nil)
   (ivy-use-selectable-prompt t)
   (ivy-use-virtual-buffers t))
  :config (ivy-mode 1)
  :bind
  (("C-s" . swiper-isearch)
   ("C-S-s" . swiper-all)
   ("C-r" . ivy-resume)
   ([f6] . ivy-resume)
   :map isearch-mode-map
   ("M-s" . swiper-isearch-toggle)))

(use-package ivy-rich
  :after (ivy counsel)
  :custom
  (ivy-rich-parse-remote-buffer nil)
  :config (ivy-rich-mode t))

(use-package ivy-xref
  :after (ivy counsel)
  :custom
  ((xref-show-xrefs-function #'ivy-xref-show-xrefs)
   (xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package counsel
  :delight
  :custom
  ((counsel-find-file-at-point t)
   (counsel-yank-pop-separator "\n--------\n"))
  :config (counsel-mode t)
  :bind
  (("C-x C-f" . counsel-find-file)
   ("C-x M-f" . counsel-recentf)
   ("C-x b" . counsel-ibuffer)
   ("C-c c w" . counsel-colors-web)
   ([remap execute-extended-command] . counsel-M-x)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f1> b" . counsel-descbinds)
   ("<f2> u" . counsel-unicode-char)
   ("M-y" . counsel-yank-pop)
   :map swiper-map
   ("M-%" . swiper-query-replace)
   ("M-s" . swiper-isearch-toggle)
   :map counsel-mode-map
   ([remap swiper] . counsel-grep-or-swiper)
   ([remap dired] . counsel-dired)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package counsel-projectile
  :after (counsel projectile)
  :custom
  (counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  :config
  (counsel-projectile-mode t))

(provide 'init-ivy)

;;; init-ivy.el ends here
