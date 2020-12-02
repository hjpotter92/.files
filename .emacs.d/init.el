;;; init --- Summary
;;; Commentary:

;;; Code:
;; You might already have this line
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Constants
(require 'init-const)

;; Packages
(require 'init-package)

;; Base config
(require 'init-base)
(require 'init-simple)
(require 'init-backup)
(require 'init-editor)
(require 'init-navigation)
(require 'init-theme)
(require 'init-ui)

;; Tools
(require 'init-ivy)
(require 'init-docker)
(require 'init-dired)
(require 'init-company)
(require 'init-lsp)
(require 'init-buffer)
(require 'init-projectile)
(require 'init-vc)
(require 'init-flycheck)
(require 'init-flyspell)
(require 'init-yasnippet)
(require 'init-debug)

;; Prgramming languages specific
(require 'init-prog)
(require 'init-web)
(require 'init-python)
(require 'init-ruby)

(setq-default auto-revert-mode t)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default custom-file "~/.emacs.d/custom.el")

;; (when (file-exists-p custom-file)
;;   (load custom-file :noerror :nomessage))

(setq highlight-blocks-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my/pretty-symbols-lisp ()
  "Lisp symbols."
  (my/pretty-symbols)
  (push '("defun"    . ?ƒ) prettify-symbols-alist)
  (push '("defmacro" . ?μ) prettify-symbols-alist)
  (push '("defvar"   . ?ν) prettify-symbols-alist))

(add-hook 'emacs-lisp-mode-hook 'my/pretty-symbols-lisp)

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ggtags
  :diminish
  :hook (prog-mode . ggtags-mode))

(use-package counsel-gtags
  :delight counsel-gtags-mode
  :hook (ggtags-mode . counsel-gtags-mode)
  :custom
  ((counsel-gtags-auto-update t)))

(use-package lua-mode
  :custom
  (lua-indent-level 2))

(use-package gxref
  :init
  (progn
    (require 'xref))
  :config
  (progn
    (add-to-list 'xref-backend-functions 'gxref-xref-backend)))

(use-package emacs
  :bind
  (("C-c m" . menu-bar-mode)
   ("C-c s" . scroll-bar-mode)
   ("C-x k" . kill-this-buffer)
   ([f5] . revert-buffer)
   ("RET" . newline-and-indent))
  :custom
  ((package-archive-priorities
    '(("melpa" . 20)
      ("marmalade" . 15)
      ("gnu" . 10)))
   (frame-title-format
    '("["
      (:eval (if (buffer-file-name)
                 (abbreviate-file-name (buffer-file-name))
               "%b"))
      " (%*%+%z) %F@" emacs-version "]"))
   (x-stretch-cursor t)
   (use-dialog-box nil)
   (scroll-error-top-bottom t)
   (visual-line-fringe-indicators (quote (left-curly-arrow nil)))
   (require-final-newline t)
   (uniquify-buffer-name-style 'forward))
  :custom-face
  (default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "SauceCodePro Nerd Font"))))
  :init
  (progn
    (setq inhibit-compacting-font-caches t)
    (setq prettify-symbols-unprettify-at-point t)
    (setq-default inhibit-startup-screen t)
    (setq-default initial-scratch-message nil)
    (setq-default indent-tabs-mode nil))
  :config
  (progn
    (my/pretty-symbols)
    (global-prettify-symbols-mode t)
    (display-battery-mode -1)
    (display-line-numbers-mode t)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (defalias 'yes-or-no-p 'y-or-n-p)))

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
