;;; init-docker.el --- Docker and docker-compose settings  -*- lexical-binding: t; -*-

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
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

;; Docker related settings and packages.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package dockerfile-mode
  :mode "Dockerfile[.a-zA-Z-]*\\'")

(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")

(use-package docker
  :bind ("C-c d" . docker)
  :delight
  :init
  (use-package docker-image
    :ensure nil
    :commands docker-images)
  (use-package docker-container
    :ensure nil
    :commands docker-containers)
  (use-package docker-volume
    :ensure nil
    :commands docker-volumes)
  (use-package docker-network
    :ensure nil
    :commands docker-containers)
  (use-package docker-machine
    :ensure nil
    :commands docker-machines))

(provide 'init-docker)

;;; init-docker.el ends here
