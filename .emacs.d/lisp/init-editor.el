;;; init-editor.el --- Editor configuration

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: convenience tools internal


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

;; Setup Emacs as the editor.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package abbrev
  :ensure nil
  :hook (after-init . abbrev-mode))

(use-package avy
  :bind
  (("C-." . avy-goto-word-1)
   ("M-g g" . avy-goto-line)))

(use-package better-shell
  :after (projectile)
  :bind
  (("C-c `" . better-shell-shell)
   :map projectile-mode-map
   ("`" . better-shell-for-projectile-root)))

(use-package helpful
  :ensure t
  :delight
  :bind
  (:map help-map
   ("f" . helpful-callable)
   ("v" . helpful-variable)
   ("F" . helpful-function)
   ("m" . helpful-macro)
   ("C" . helpful-command)
   ("k" . helpful-key)
   ("c" . helpful-key)
   ("C-d" . helpful-at-point)))

(use-package home-end
  :bind
  (([home] . home-end-home)
   ([end] . home-end-end)))

(use-package with-editor
  :config (shell-command-with-editor-mode t))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer-other-window)
  :custom
  ((ibuffer-show-empty-filter-groups nil)
   (ibuffer-expert t)
   (ibuffer-saved-filter-groups
    (quote
     (("default"
       ("dired" (mode . dired-mode))
       ("org" (name . "\\.org$"))
       ("magit" (mode . magit-mode))
       ("IRC" (or (mode . circe-channel-mode) (mode . circe-server-mode)))
       ("web" (or (mode . web-mode) (mode . js2-mode)))
       ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
       ("mu4e"
        (or
         (mode . mu4e-compose-mode)
         (name . "\*mu4e\*")))
       ("programming"
        (or
         (mode . emacs-lisp-mode)
         (mode . lua-mode)
         (mode . python-mode)
         (mode . c++-mode)))
       ("emacs"
        (or
         (name . "^\\*scratch\\*$")
         (name . "^\\*Messages\\*$"))))))))
  :functions (ibuffer-auto-mode ibuffer-switch-to-saved-filter-groups)
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-auto-mode 1)
                    (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package hideshow
  :diminish
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  (:map prog-mode-map
        ("C-c h" . hs-toggle-hiding)))

(use-package which-key
  :diminish
  :config
  (progn
    (which-key-mode t)
    (which-key-setup-minibuffer)
    (setq which-key-idle-delay 0.400)))

(use-package omni-scratch
  :custom
  ((omni-scratch-pale-background nil))
  :bind
  (("M-s $ DEL" . omni-scratch)
   ("M-s $ -" . omni-scratch-major)
   ("M-s $ _" . omni-scratch-buffer)
   ("M-s $ $" . omni-scratch-goto-latest)
   ("M-s $ b" . omni-scratch-buffers)))

(use-package smartparens
  :commands
  (sp-local-pair
   sp-local-tag)
  :init
  (progn
    (require 'smartparens-config)
    (require 'smartparens-ruby)
    (require 'smartparens-html)
    (require 'smartparens-python)
    (require 'smartparens-elixir)
    (require 'smartparens-markdown)
    (require 'smartparens-text)
    (require 'smartparens-latex)
    (require 'smartparens-lua)
    (require 'smartparens-javascript)
    (smartparens-global-mode 1))
  :config
  (progn
    (sp-with-modes '(web-mode)
      (sp-local-pair "%" "%" :unless '(sp-in-string-p)
                     :post-handlers '(((lambda (&rest _ignored)
                                         (just-one-space)
                                         (save-excursion (insert " ")))
                                       "SPC" "=" "#")))
      (sp-local-tag "%" "<% "  " %>")
      (sp-local-tag "=" "<%= " " %>")
      (sp-local-tag "#" "<%# " " %>"))
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))
  :bind
  (("C-M-k" . sp-kill-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ;; ("M-s" . sp-splice-sexp)
   ;; ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-DEL" . sp-unwrap-sexp)
   ("<M-backspace>" . sp-backward-unwrap-sexp)
   ;; ("M-S" . sp-split-sexp)
   ;; ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

(use-package smart-window)

(use-package region-bindings-mode
  :diminish
  :commands
  (region-bindings-mode-enable)
  :custom
  (region-bindings-mode-disabled-modes '(dired-mode ibuffer-mode))
  :init
  (region-bindings-mode-enable))

(use-package multiple-cursors
  :after region-bindings-mode
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)
  :map mc/keymap
  ("C-|" . mc/vertical-align-with-space)
  :map region-bindings-mode-map
  ("a" . mc/mark-all-like-this)
  ("p" . mc/mark-previous-like-this)
  ("n" . mc/mark-next-like-this)
  ("P" . mc/unmark-previous-like-this)
  ("N" . mc/unmark-next-like-this)
  ("j" . mc/cycle-backward)
  ("k" . mc/cycle-forward)
  ("m" . mc/mark-more-like-this-extended)
  ("h" . mc-hide-unmatched-lines-mode)
  ("\\" . mc/vertical-align-with-space)
  ("#" . mc/insert-numbers)             ; use num prefix to set the starting number
  ("^" . mc/edit-beginnings-of-lines)
  ("$" . mc/edit-ends-of-lines)))

(use-package subword
  :ensure nil
  :hook
  ((prog-mode minibuffer-setup) . subword-mode))

(provide 'init-editor)

;;; init-editor.el ends here
