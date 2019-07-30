;;; init-yasnippet.el --- Setup yasnippet package and its snippets

;; Author: hjpotter92
;; Maintainer: hjpotter92
;; Version: 0.0.1
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: abbrev


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

;; Yasnippet package configuration.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package yasnippet
  :init (yas-global-mode t)
  :config
  (use-package yasnippet-snippets))

(use-package auto-yasnippet)

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
