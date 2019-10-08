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

(use-package anzu
  :hook
  (after-init . global-anzu-mode)
  :custom
  ((anzu-cons-mode-line-p nil))
  :bind
  ([remap query-replace] . anzu-query-replace-regexp))

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
     ("M" helpful-macro "macro")
     ("c" helpful-command "command")
     ("d" helpful-at-point "thing at point"))
    "Describe"
    (("m" describe-mode "mode")
     ("b" describe-bindings "bindings"))
    "Quit"
    (("q" nil "Quit hydra"))))
  :bind
  ("C-h" . helpful-hydra/body))

(use-package with-editor
  :config (shell-command-with-editor-mode t))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package hideshow
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
  :delight
  :config
  (progn
    (which-key-mode t)
    (which-key-setup-minibuffer)
    (setq which-key-idle-delay 0.400)))

(use-package which-key-posframe
  :custom
  (which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-left-corner)
  :config
  (which-key-posframe-mode t))

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
  :pretty-hydra
  ((:quit-key "q" :title "Smartparens hydra menu" :color teal)
   ("Moving"
    (("a" sp-beginning-of-sexp "beginning")
     ("e" sp-end-of-sexp "end")
     ("f" sp-forward-sexp "forward")
     ("b" sp-backward-sexp "backward")
     ("n" sp-down-sexp "down")
     ("N" sp-backward-down-sexp "bw down")
     ("p" sp-up-sexp "up")
     ("P" sp-backward-up-sexp "bw up"))
    "Slurping & barfing"
    (("h" sp-backward-slurp-sexp "bw slurp")
     ("H" sp-backward-barf-sexp "bw barf")
     ("l" sp-forward-slurp-sexp "slurp")
     ("L" sp-forward-barf-sexp "barf"))
    "Wrapping"
    (("R" sp-rewrap-sexp "rewrap")
     ("u" sp-unwrap-sexp "unwrap")
     ("U" sp-backward-unwrap-sexp "bw unwrap")
     ("(" sp-wrap-round "wrap (")
     ("{" sp-wrap-curly "wrap {")
     ("[" sp-wrap-square "wrap ["))
    "Sexp juggling"
    (("S" sp-split-sexp "split")
     ("s" sp-splice-sexp "splice")
     ("r" sp-raise-sexp "raose")
     ("j" sp-join-sexp "join")
     ("t" sp-transpose-sexp "transpose")
     ("A" sp-absorb-sexp "absorb")
     ("E" sp-emit-sexp "emit")
     ("o" sp-convolute-sexp "convolute"))
    "Destructive editing"
    (("c" sp-change-inner "change inner" :exit t)
     ("C" sp-change-enclosing "change outer" :exit t)
     ("k" sp-kill-sexp "kill")
     ("K" sp-backward-kill-sexp "bw kill")
     ("w" sp-copy-sexp "copy"))
    "Quit hydra"
    (("q" nil "quit hydra"))))
  :bind
  ("C-c p" . smartparens-hydra/body))

(use-package smart-window)

(use-package imenu-list
  :custom
  ((imenu-list-auto-resize t))
  :hook
  ((python-mode js2-mode c-mode c++-mode java-mode ruby-mode yaml-mode) . imenu-list-smart-toggle))

(use-package region-bindings-mode
  :delight
  :commands
  (region-bindings-mode-enable)
  :custom
  (region-bindings-mode-disabled-modes '(dired-mode ibuffer-mode))
  :init
  (region-bindings-mode-enable))

(use-package multiple-cursors
  :after region-bindings-mode
  :pretty-hydra
  ((:color amaranth :quit-key "q")
   ("Up"
    (("p" mc/mark-previous-like-this "next")
     ("P" mc/skip-to-previous-like-this "skip")
     ("M-p" mc/unmark-previous-like-this "unmark")
     ("j" mc/cycle-backward "visit previous cursor"))
    "Down"
    (("n" mc/mark-next-like-this "next")
     ("N" mc/skip-to-next-like-this "skip")
     ("M-n" mc/unmark-next-like-this "unmark")
     ("k" mc/cycle-forward "visit next cursor"))
    "Actions"
    (("l" mc/edit-lines "edit lines" :exit t)
     ("^" mc/edit-beginnings-of-lines "edit beginnings of lines" :exit t)
     ("$" mc/edit-ends-of-lines "edit ends of lines" :exit t))
    "Miscellaneous"
    (("a" mc/mark-all-like-this "mark all" :exit t)
     ("s" mc/mark-all-in-region-regexp "search" :exit t)
     ("SPC" mc/mark-pop "pop mark")
     ("h" mc-hide-unmatched-line-mode "toggle view")
     ("0" mc/insert-numbers "insert numbers" :exit t)
     ("A" mc/insert-letters "insert letters" :exit t))
    "Align"
    (("|" mc/vertical-align-with-space "align with space")
     ("M-|" mc/vertical-align "align vertically"))
    "Mouse"
    (("<mouse-1>" mc/add-cursor-on-click "cursor at point")
     ;; Help with click recognition in this hydra
     ("<down-mouse-1>" ignore)
     ("<drag-mouse-1>" ignore))
    "Quit hydra"
    (("q" nil "quit hydra"))))
  :bind
  ("C-c u" . multiple-cursors-hydra/body))

(use-package rg
  :defer t
  :custom
  (rg-keymap-prefix "\M-ss")
  :init
  (rg-enable-default-bindings))

(use-package subword
  :ensure nil
  :hook
  ((prog-mode minibuffer-setup) . subword-mode))

(provide 'init-editor)

;;; init-editor.el ends here
