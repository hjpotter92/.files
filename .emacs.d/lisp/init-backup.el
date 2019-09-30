;;; init-backup.el --- Backup configuration

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
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

;; Backup rules for the whole environment.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package files
  :ensure nil
  :custom
  ((vc-make-backup-files t)
   (version-control :make-numbered-backups)
   (kept-new-versions 5)
   (kept-old-versions 2)
   (delete-old-versions t)
   (backup-by-copying t)))

(provide 'init-backup)

;;; init-backup.el ends here
