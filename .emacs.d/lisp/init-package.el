;;; init-package.el --- Setting up packages

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools internal convenience


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

;; Setting up packages.  Based on centaur-emacs.

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `el-get'
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(eval-and-compile
  (add-to-list 'el-get-recipe-path (expand-file-name "el-get-user/recipes" user-emacs-directory))
  (el-get 'sync))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-minimum-reported-time 0)
  (setq use-package-compute-statistics t)
  (setq use-package-always-ensure t)
  ;; (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (require 'use-package))

(use-package bind-key)
(use-package delight
  :config
  (progn
    (diminish 'abbrev-mode "Ab")
    (diminish 'outline-mode)))
(use-package diminish)

(use-package paradox
  :custom
  ((paradox-lines-per-entry 2)
   (paradox-execute-asynchronously t)
   (paradox-automatically-star nil))
  :init
  (defalias 'upgrade-packages #'paradox-upgrade-packages)
  :config
  (paradox-enable))

(provide 'init-package)

;;; init-package.el ends here
