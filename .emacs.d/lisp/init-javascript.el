;;; init-javascript.el --- Programming global modes  -*- lexical-binding: t; -*-

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools languages
;; Prefix: my/js
;; Separator: /


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
  (require 'init-const)
  (require 'init-function))

(use-package js2-mode
  :custom
  ((js2-idle-timer-delay 0)
   (js2-mode-show-parse-errors nil)
   (js2-mode-show-strict-warnings nil)))

(use-package typescript-mode
  :mode "\\.tsx?")

(provide 'init-javascript)

;;; init-javascript.el ends here
