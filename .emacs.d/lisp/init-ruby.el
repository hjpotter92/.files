;;; init-ruby.el --- Programming setup for ruby

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools languages


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

(defun init-ruby-pretty-symbols ()
  "Python specific pretty symbols."
  (my/pretty-symbols)
  (push '("def"   . ?ƒ) prettify-symbols-alist)
  (push '("->"    . ?→) prettify-symbols-alist)
  (push '("=>"    . ?⟹) prettify-symbols-alist)
  (push '("true"  . ?⥾) prettify-symbols-alist)
  (push '("false" . ?⥿) prettify-symbols-alist)
  (push '("nil"   . ?∅) prettify-symbols-alist))

(use-package ruby-mode
  :hook
  (ruby-mode . (lambda ()
             (init-ruby-pretty-symbols)))
  :init
  (progn
    (use-package ruby-tools)
    (use-package inf-ruby
      :hook
      (ruby-mode . inf-ruby-minor-mode)))
  :config
  (progn
    (use-package rbenv
      :hook
      (ruby-mode . rbenv-use-corresponding)
      :init
      (global-rbenv-mode t))
    (use-package ruby-extra-highlight
      :hook
      (ruby-mode . ruby-extra-highlight-mode))))

(provide 'init-ruby)

;;; init-ruby.el ends here
