;;; init-dired.el --- Dired buffer configurations

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools files


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

;; Dired and directory settings

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package dired
  :ensure nil
  :custom
  ((dired-recursive-deletes 'always)
   (dired-recursive-copies 'always)
   (dired-dwim-target t))
  :commands
  (dired
   find-name-dired
   find-dired)
  :config
  (toggle-diredp-find-file-reuse-dir 1))

(use-package dired+
  :ensure nil
  :custom
  (diredp-hide-details-initially-flag nil)
  :commands toggle-diredp-find-file-reuse-dir)

(use-package image-dired+
  :config (image-diredx-async-mode 1))


(provide 'init-dired)

;;; init-dired.el ends here
