;;; init-package.el --- Setting up emacs packages -*- lexical-binding: t -*-

;; Author: hjpotter92
;; Maintainer:
;; Version: 0.0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: emacs package init


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
(unless (package-installed-p 'el-get)
  (package-refresh-contents)
  (package-install 'el-get))

(eval-and-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get"))
  (require 'el-get)
  (el-get 'sync)
  (defvar el-get-recipe-path)
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes"))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-minimum-reported-time 0)
  (setq use-package-compute-statistics t)
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (require 'use-package))

(use-package diminish)
(use-package bind-key)

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
