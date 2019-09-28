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
(require 'init-theme)

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
(require 'init-yasnippet)

;; Prgramming languages specific
(require 'init-prog)

(setq-default auto-revert-mode t)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default python-environment-directory "~/.virtualenvs")
(setq-default custom-file "~/.emacs.d/custom.el")

;; (when (file-exists-p custom-file)
;;   (load custom-file :noerror :nomessage))

(setq highlight-blocks-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my/pretty-symbols ()
  "Add custom symbols in pretty-symbol mode."
  (push '("!=" . ?≠) prettify-symbols-alist)
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("==" . ?≡) prettify-symbols-alist)
  (push '("=>" . ?⇒) prettify-symbols-alist)
  (push '("NOTE" . ?¤) prettify-symbols-alist)
  (push '("TODO" . ?§) prettify-symbols-alist))

(defun my/pretty-symbols-python ()
  "Python specific pretty symbols."
  (my/pretty-symbols)
  (push '("def"    . ?ƒ) prettify-symbols-alist)
  (push '("sum"    . ?Σ) prettify-symbols-alist)
  (push '("**2"    . ?²) prettify-symbols-alist)
  (push '("**3"    . ?³) prettify-symbols-alist)
  (push '("None"   . ?∅) prettify-symbols-alist)
  (push '("True"   . ?⥾) prettify-symbols-alist)
  (push '("False"  . ?⥿) prettify-symbols-alist)
  (push '("is"     . ?≣) prettify-symbols-alist)
  (push '("is not" . ?≢) prettify-symbols-alist)
  (push '("in"     . ?∈) prettify-symbols-alist)
  (push '("not in" . ?∉) prettify-symbols-alist)
  (push '("return" . ?⟾) prettify-symbols-alist))

(defun my/pretty-symbols-ruby ()
  "Python specific pretty symbols."
  (my/pretty-symbols)
  (push '("def"   . ?ƒ) prettify-symbols-alist)
  (push '("->"    . ?→) prettify-symbols-alist)
  (push '("=>"    . ?⟹) prettify-symbols-alist)
  (push '("true"  . ?⥾) prettify-symbols-alist)
  (push '("false" . ?⥿) prettify-symbols-alist)
  (push '("nil"   . ?∅) prettify-symbols-alist))

(defun my/pretty-symbols-lisp ()
  "Lisp symbols."
  (my/pretty-symbols)
  (push '("defun"    . ?ƒ) prettify-symbols-alist)
  (push '("defmacro" . ?μ) prettify-symbols-alist)
  (push '("defvar"   . ?ν) prettify-symbols-alist))

(add-hook 'python-mode-hook 'my/pretty-symbols-python)
(add-hook 'emacs-lisp-mode-hook 'my/pretty-symbols-lisp)
(add-hook 'ruby-mode-hook 'my/pretty-symbols-ruby)

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package python-docstring
  :hook (python-mode . python-docstring-mode))

(use-package ggtags
  :diminish
  :hook (prog-mode . ggtags-mode))

(use-package counsel-gtags
  :delight counsel-gtags-mode
  :diminish
  :after (ggtags)
  :hook (ggtags-mode . counsel-gtags-mode)
  :custom
  ((counsel-gtags-auto-update t)))

(use-package emmet
  :ensure emmet-mode
  :hook ((sgml-mode html-mode css-mode web-mode) . emmet-mode)
  :after (web-mode))

(use-package web-mode
  :mode
  (("\\.[pm]?html?$" . web-mode)
   ("\\.jsx?$" . web-mode)
   ("\\.tmpl" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))
  :custom
  ((web-mode-markup-indent-offset 2)
   (web-mode-css-indent-offset 2)
   (web-mode-code-indent-offset 2)
   (web-mode-enable-current-element-highlight t)
   (web-mode-enable-current-column-highlight t)
   (web-mode-enable-auto-pairing nil)
   (web-mode-content-types-alist
    '(("jsx" . "\\.jsx?\\'")
      ("json" . "\\.json\\'")
      ("xml" . "\\.xml\\'")))
   (web-mode-engines-alist
    '(("php" . "\\.phtml?\\'")
      ("reactjs" . "\\.jsx\\'")
      ("blade" . "\\.blade\\."))))
  :config
  (progn
    (use-package company-web
      :ensure nil
      :bind
      (("C-c w" . company-web-html))
      :config
      (add-to-list 'company-backends 'company-web-html))))

(use-package switch-window
  :bind
  ("C-x o" . switch-window))

(use-package minibuffer-line
  :if (display-graphic-p)
  :disabled t
  :defer 1
  :init
  (progn
    (setq-default display-time-format "")
    (setq minibuffer-line-format (format-time-string "%l:%M %b %d %a")))
  :config
  (minibuffer-line-mode))

(use-package ruby-mode
  :init
  (progn
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
      (ruby-mode . ruby-extra-highlight-mode))
    (use-package ruby-tools)))

(use-package python-mode
  :disabled
  :defer t
  :mode "\\.py"
  :custom
  ((py-split-window-on-execute nil)
   (python-indent-guess-indent-offset nil)))

(use-package pipenv
  :disabled
  :hook (python-mode . pipenv-mode)
  :diminish)

(use-package elpy
  :after (flycheck)
  :diminish
  :hook
  (elpy-mode . flycheck-mode)
  :custom
  ((elpy-rpc-backend "jedi")
   (elpy-autodoc-delay 0.400)
   (elpy-rpc-ignored-buffer-size 204800))
  :init
  (progn
    (elpy-enable)
    (mapc (lambda (module) (setq elpy-modules (delq module elpy-modules)))
          '(elpy-module-flymake elpy-module-highlight-indentation elpy-module-django elpy-module-pyvenv))))

(use-package auto-virtualenvwrapper
  :hook
  (python-mode . auto-virtualenvwrapper-activate))

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

(use-package css-mode
  :custom
  (css-indent-offset 2)
  :hook
  (css-mode . emmet-mode)
  :mode
  ("\\.css\\'"))

(use-package git-gutter+
  :diminish
  :init
  (progn
    (global-git-gutter+-mode t)))

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
      ("elpa" . 10)))
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
