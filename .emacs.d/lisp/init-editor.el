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
   ("s-`" . better-shell-for-projectile-root)))

(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark))
  :config
  (progn
    (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
    (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
    (add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
    (add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
    (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
    (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward ""))))

(use-package helpful
  :delight
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Helpful"
    (("f" helpful-callable "callable")
     ("v" helpful-variable "variable")
     ("k" helpful-key "key")
     ("F" helpful-function "function")
     ("m" helpful-macro "macro")
     ("c" helpful-command "command")
     ("d" helpful-at-point "thing at point"))
    "Quit"
    (("q" nil "Quit hydra"))))
  :bind
  ("C-h" . helpful-hydra/body))

(use-package home-end
  :bind
  (([home] . home-end-home)
   ([end] . home-end-end)))

(use-package with-editor
  :config (shell-command-with-editor-mode t))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package hideshow
  :diminish
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  ("C-c h" . hideshow-hydra/body)
  :pretty-hydra
  ((:quit-key "q" :title "Code folding")
   ("Hide"
    (("h" hs-hide-all "All")
     ("d" hs-hide-block "Block")
     ("l" hs-hide-level "Level"))
    "Show"
    (("s" hs-show-all "All")
     ("a" hs-show-block "Block"))
    "Toggle"
    (("t" hs-toggle-hiding "Toggle"))
    "Navigation"
    (("n" forward-line "Next line")
     ("p" (forward-line -1) "Previous line"))
    "Quit"
    (("q" nil "Quit hydra")))))

(use-package which-key
  :diminish
  :config
  (progn
    (which-key-mode t)
    (which-key-setup-minibuffer)
    (setq which-key-idle-delay 0.400)))

(use-package omni-scratch
  :custom
  (omni-scratch-pale-background nil)
  :pretty-hydra
  ((:color amaranth :quit-key "q" :title "Omni scratch buffer management")
   ("Omni Scratch"
    (;; ("DEL" omni-scratch "omni-scratch")
     ("-" omni-scratch-major "major mode")
     ("_" omni-scratch-buffer "buffer")
     ("$" omni-scratch-goto-latest "goto latest")
     ("b" omni-scratch-buffers "buffers"))
    "Quit"
    (("q" nil "Quit hydra"))))
  :bind
  ("C-c $" . omni-scratch-hydra/body))

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
