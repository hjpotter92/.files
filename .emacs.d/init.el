;;; init --- Summary
;;; Commentary:
;; You might already have this line
(require 'package)

;;; Code:
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
;; You might already have this line
(setq package-enable-at-startup nil)
(package-initialize)

(prefer-coding-system 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(debug-on-error t)
 '(display-time-format "%a %d %b, %I:%M %p")
 '(display-time-mode t)
 '(dynamic-completion-mode t)
 '(global-git-commit-mode t)
 '(global-highlight-parentheses-mode t)
 '(global-hl-line-mode t)
 '(global-visual-line-mode t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(line-number-mode t)
 '(save-place t nil (saveplace))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-smartparens-global-mode t)
 '(smart-cursor-color-mode t nil (smart-cursor-color))
 '(smartparens-global-mode t)
 '(smtpmail-smtp-server "email-smtp.us-west-2.amazonaws.com")
 '(smtpmail-smtp-service 587)
 '(starttls-gnutls-program "gnutls-cli")
 '(starttls-use-gnutls t)
 '(tool-bar-mode nil)
 '(visual-line-fringe-indicators (quote (left-curly-arrow nil))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "Ricty Diminished")))))

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(defvar highlight-blocks-mode)
(defvar sml/replacer-regexp-list)

(setq-default abbrev-mode t)
(setq-default auto-revert-mode t)
(setq-default auto-complete-mode t)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq x-stretch-cursor t)
(setq require-final-newline t)
(setq frame-title-format '("[ %b ] @ Emacs " emacs-version))
(setq highlight-blocks-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(load-theme 'monokai t)
(mode-icons-mode)

(require 'dired+)
(use-package image-dired+)
(require 'emmet-mode)

(require 'smart-window)
(require 'switch-window)
(require 'web-mode)
(global-set-key (kbd "C-x o") 'switch-window)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(sml/setup nil)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Niki\.ai/" ":niki:") t)
(add-to-list 'sml/replacer-regexp-list '("^:niki:\\(.*\\)/src/main/java/code/niki/" ":N/\\1/SMJCN:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:homeyantra/" ":hy:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:homeyantra/homeyantra/app/" ":hya:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Ivycamp/ivycamp.in/app/" ":ivy:") t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package virtualenvwrapper
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs/"))

(provide 'init)
;;; init.el ends here
