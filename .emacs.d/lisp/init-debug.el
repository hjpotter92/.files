;;; init-debug.el --- Attaching and defining debuggers.

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: internal


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

;; Attaching and defining debuggers.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package dap-mode
  :commands dap-mode
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (progn
    (dap-mode t)
    (use-package dap-ui
      :ensure nil
      :config
      (dap-ui-mode t))
    (use-package dap-tooltip
      :ensure nil
      :config
      (progn
        (dap-tooltip-mode t)
        (tooltip-mode t)))
    (use-package dap-ui-controls
      :ensure nil
      :config
      (dap-ui-controls-mode t))
    (require 'dap-python)
    (require 'dap-node)
    (require 'dap-pwsh))))

(use-package realgud
  :disabled
  :init
  (load-library "realgud"))

(provide 'init-debug)

;;; init-debug.el ends here
