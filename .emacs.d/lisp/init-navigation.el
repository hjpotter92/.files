;;; init-navigation.el --- Navigation related things

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

;; Keybindings and defaults for navigation

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package home-end
  :bind
  (([home] . home-end-home)
   ([end] . home-end-end)))

(use-package switch-window
  :bind
  (("C-x o" . switch-window)
   ("<s-prior>" . switch-to-prev-buffer)
   ("<s-next>" . switch-to-next-buffer)))

(provide 'init-navigation)

;;; init-navigation.el ends here
