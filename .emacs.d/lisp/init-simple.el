;;; init-simple.el --- Simple mode configuration

;; Author: hjpotter92
;; Maintainer: hjpotter92
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

;; Configuring functions defined by simple.el and simple+.el

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package simple
  :ensure nil
  :hook (window-setup . size-indication-mode)
  :custom
  ((display-line-numbers-mode t)
   (display-line-numbers t))
  :init
  (setq column-number-mode t
        line-number-mode t
        kill-whole-line t
        global-visual-line-mode t))

(provide 'init-simple)

;;; init-simple.el ends here
