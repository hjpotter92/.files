;;; init-base.el --- Init defaults

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: bib internal


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

;; Define saner default values for the Emacs environment.

;;; Code:

(eval-when-compile
  (require 'init-const))

(setq user-full-name "hjpotter92")
(setq user-mail-address "hjpotter92+github@gmail.com")

(use-package benchmark-init
  :hook
  (after-init . benchmark-init/deactivate)
  :init
  (benchmark-init/activate))

(use-package use-package-hydra
  :disabled)

(use-package pretty-hydra)

(use-package major-mode-hydra
  :bind
  ("M-SPC" . major-mode-hydra))

(use-package server
  :ensure nil
  :functions (server-running-p)
  :if (display-graphic-p)
  :config
  (progn
    (unless (or (daemonp) (server-running-p))
      (server-start))))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package time
  :ensure nil
  :custom
  ((display-time-default-load-average nil)
   (display-time-day-and-date t)
   (display-time-format "%I:%M %p %b %d %a")
   (display-time-use-mail-icon t))
  :hook (after-init . display-time-mode))

(provide 'init-base)

;;; init-base.el ends here
