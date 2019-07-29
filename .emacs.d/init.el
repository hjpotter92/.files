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

;; Tools
(require 'init-ivy)
(require 'init-docker)

(defvar highlight-blocks-mode)
(defvar sml/replacer-regexp-list)
(defvar use-package-compute-statistics)
(defvar company-dabbrev-downcase)
(defvar region-bindings-mode-map)

(setq-default abbrev-mode t)
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

(use-package benchmark-init
  :ensure t
  :hook
  (after-init . benchmark-init/deactivate)
  :init
  (benchmark-init/activate))

(use-package rainbow-mode
  :ensure t
  :diminish t
  :hook
  (prog-mode . rainbow-mode))

(use-package beacon
  :ensure t
  :init
  (beacon-mode t))

(use-package smart-window
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package realgud
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package python-docstring
  :ensure t
  :hook (python-mode . python-docstring-mode))

(use-package magit
  :ensure t
  :after (projectile)
  :commands magit-get-top-dir
  :bind
  (("C-c g" . magit-status)
   ("C-c C-g l" . magit-file-log))
  :init
  (progn
    ;; we no longer need vc-git
    (delete 'Git vc-handled-backends))
  :custom
  ((magit-repository-directories '(("~/Documents/Loktra/" . 2)
                                   ("~/Documents/" . 1)))
   (magit-completing-read-function 'ivy-completing-read))
  :config
  (progn
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    (global-magit-file-mode t)))

(use-package focus
  :ensure t
  :hook
  ((lisp-mode emacs-lisp-mode ruby-mode) . focus-mode)
  :bind ("C-c f" . focus-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-package
  :commands flycheck-package-setup
  :after (flycheck)
  :init (flycheck-package-setup))

(use-package yasnippet
  :ensure t
  :init
  (progn
    (yas-global-mode t))
  :config
  (progn
    (use-package yasnippet-snippets
      :ensure t)))

(use-package all-the-icons-ivy
  :after (ivy)
  :config
  (all-the-icons-ivy-setup))

(use-package projectile
  :ensure t
  :after (ivy)
  :delight '(:eval (concat " P[" (projectile-project-name) "]"))
  :custom
  ((projectile-enable-caching nil)
   (projectile-completion-system 'ivy)
   (projectile-require-project-root nil)
   (projectile-create-missing-test-files t)
   (projectile-tags-backend "ggtags"))
  :init
  (progn
    (projectile-mode t)
    (use-package projectile-rails
      :ensure t
      :init
      (projectile-rails-global-mode t)))
  :config
  (progn
    (setq projectile-project-root-files-bottom-up (delete ".git" projectile-project-root-files-bottom-up))
    (dolist (item '("GTAGS" "GRTAGS" "GPATH"))
      (add-to-list 'projectile-globally-ignored-files item))
    ;; Git projects should be marked as projects in top-down fashion,
    ;; so that each git submodule can be a projectile project.
    (add-to-list 'projectile-project-root-files ".git")
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)))

(use-package persp-mode
  :no-require t
  :disabled
  :init
  (progn
    (persp-mode t))
  :config
  (progn
    (use-package persp-mode-projectile-bridge
      :ensure t
      :config
      (progn
        (with-eval-after-load "persp-mode-projectile-bridge-autoloads"
          (add-hook 'persp-mode-projectile-bridge-mode-hook
                    #'(lambda ()
                        (if persp-mode-projectile-bridge-mode
                            (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                          (persp-mode-projectile-bridge-kill-perspectives))))
          (add-hook 'after-init-hook
                    #'(lambda ()
                        (persp-mode-projectile-bridge-mode 1))
                    t))))))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package better-shell
  :ensure t
  :bind
  (("C-c `" . better-shell-shell)
   ("s-p `" . better-shell-for-projectile-root)))

;; (use-package perspective
;;   :ensure t
;;   :commands persp-mode
;;   :config
;;   (progn
;;     (use-package persp-projectile
;;       :ensure t
;;       :commands persp-projectile)
;;     (persp-mode)))

(use-package ggtags
  :ensure t
  :diminish
  :hook
  ((python-mode ruby-mode js2-mode emacs-lisp-mode web-mode) . ggtags-mode))

(use-package counsel-gtags
  :ensure t
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
  :ensure t
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
      :ensure t
      :bind
      (("C-c w" . company-web-html))
      :config
      (add-to-list 'company-backends 'company-web-html))))

(use-package smart-cursor-color
  :ensure t
  :diminish smart-cursor-color-mode
  :config
  (global-hl-line-mode t)
  (smart-cursor-color-mode t))

(use-package multiple-cursors
  :ensure t
  :after region-bindings-mode
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)
  :bind
  (:map region-bindings-mode-map
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

(use-package region-bindings-mode
  :ensure t
  :diminish
  :config
  (region-bindings-mode-enable))

(use-package switch-window
  :ensure t
  :bind
  ("C-x o" . switch-window))

(use-package company-statistics
  :ensure t)

(use-package company-quickhelp
  :ensure t
  :if (window-system)
  :custom
  ((company-quickhelp-delay 0.25))
  :init
  (progn
    (company-quickhelp-mode t)))

(use-package company
  :ensure t
  :requires (company-statistics)
  :bind
  (("<C-tab>" . company-complete)
   (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ([tab] . company-complete-common-or-cycle)
        ("S-TAB" . company-select-previous)
        ("<backtab>" . company-select-previous)))
  :init
  (progn
    (global-company-mode t)
    (company-statistics-mode t))
  :custom
  ((company-dabbrev-downcase nil)
   (company-idle-delay 0)
   (company-require-match nil)
   (company-show-numbers t)
   (company-minimum-prefix-length 2))
  :config
  (progn
    (use-package robe
      :ensure t
      :after ruby-mode
      :hook
      (ruby-mode . robe-mode)
      :config
      (add-to-list 'company-backends 'company-robe))
    (use-package company-jedi
      :ensure t
      :config
      (progn
        (add-to-list 'company-backends 'company-jedi)))
    (use-package company-flx
      :defer t
      :config
      (company-flx-mode t))))

(use-package avy
  :ensure t
  :bind
  (("C-." . avy-goto-word-1)
   ("M-g g" . avy-goto-line)))

(use-package monokai-pro-theme
  :ensure t
  :if (display-graphic-p)
  :config
  (progn
    (load-theme 'monokai-pro t)))

(use-package centaur-tabs
  :if (display-graphic-p)
  :after (smart-mode-line)
  :custom
  ((centaur-tabs-style "slant")
   (centaur-tabs-set-icons t)
   (centaur-tabs-set-modified-marker t)
   (centaur-tabs-set-bar 'over))
  :init
  (progn
    (centaur-tabs-mode t)
    (centaur-tabs-group-by-projectile-project))
  :bind
  (("s-<prior>" . centaur-tabs-backward)
   ("s-<next>" . centaur-tabs-forward)
   ("C-c t" . centaur-tabs-counsel-switch-group)))

(use-package smart-mode-line
  :ensure t
  :after (monokai-pro-theme)
  :init
  (setq sml/no-confirm-load-theme t)
  :config
  (progn
    (setq sml/mode-width 'full)
    (sml/setup)
    ;; (sml/apply-theme 'dark)
    (add-to-list 'sml/replacer-regexp-list '("^:Doc:przemek/app/" ":PRZK:") t)
    (add-to-list 'sml/replacer-regexp-list '("^:lk:backend/odyssey/v\\([12]\\)/" ":lbOD\\1:") t)))

(use-package mode-icons
  :ensure t
  :after (smart-mode-line)
  :config (mode-icons-mode))

(use-package minibuffer-line
  :if (display-graphic-p)
  :ensure t
  :disabled t
  :defer 1
  :init
  (progn
    (setq-default display-time-format "")
    (setq minibuffer-line-format (format-time-string "%l:%M %b %d %a")))
  :config
  (minibuffer-line-mode))

(use-package smartparens
  :ensure t
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

(use-package ruby-mode
  :ensure t
  :init
  (progn
    (use-package inf-ruby
      :ensure t
      :hook
      (ruby-mode . inf-ruby-minor-mode)))
  :config
  (progn
    (use-package rbenv
      :ensure t
      :hook
      (ruby-mode . rbenv-use-corresponding)
      :init
      (global-rbenv-mode t))
    (use-package ruby-extra-highlight
      :hook
      (ruby-mode . ruby-extra-highlight-mode))
    (use-package ruby-tools
      :ensure t)))

(use-package python-mode
  :ensure t
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
  :ensure t
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
  :ensure t
  :hook
  (python-mode . auto-virtualenvwrapper-activate))

(use-package fill-column-indicator
  :ensure t
  ;; Disabled due to issue with company-popup
  :disabled
  :commands (fci-mode)
  :hook
  ((lua-mode python-mode emacs-lisp-mode) . fci-mode)
  :custom
  ((fci-rule-column 80)))

(use-package home-end
  :ensure t
  :bind
  (([home] . home-end-home)
   ([end] . home-end-end)))

(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 2))

(use-package with-editor
  :config (shell-command-with-editor-mode t))

(use-package ibuffer
  :ensure t
  :bind
  ("C-x C-b" . ibuffer-other-window))

(use-package which-key
  :diminish
  :config
  (progn
    (which-key-mode t)
    (which-key-setup-minibuffer)
    (setq which-key-idle-delay 0.400)))

(use-package highlight-parentheses
  :ensure t
  :delight
  (highlight-parentheses-mode " ❪❫")
  (global-highlight-parentheses-mode " ❪❫")
  :config
  (progn
    (global-highlight-parentheses-mode t)))

(use-package omni-scratch
  :ensure t
  :custom
  ((omni-scratch-pale-background nil))
  :bind
  (("M-s $ DEL" . omni-scratch)
   ("M-s $ -" . omni-scratch-major)
   ("M-s $ _" . omni-scratch-buffer)
   ("M-s $ $" . omni-scratch-goto-latest)
   ("M-s $ b" . omni-scratch-buffers)))

(use-package helpful
  :ensure t
  :delight
  :bind
  (:map help-map
   ("f" . helpful-callable)
   ("v" . helpful-variable)
   ("F" . helpful-function)
   ("k" . helpful-key)
   ("c" . helpful-key)
   ("C-d" . helpful-at-point)))

(use-package gxref
  :ensure t
  :init
  (progn
    (require 'xref))
  :config
  (progn
    (add-to-list 'xref-backend-functions 'gxref-xref-backend)))

(use-package css-mode
  :ensure t
  :custom
  (css-indent-offset 2)
  :hook
  (css-mode . emmet-mode)
  :mode
  ("\\.css\\'"))

(use-package git-gutter+
  :ensure t
  :diminish
  :init
  (progn
    (global-git-gutter+-mode t)))

(use-package idle-highlight-in-visible-buffers-mode
  :ensure t
  :hook
  (prog-mode . idle-highlight-in-visible-buffers-mode))

(use-package emacs
  :ensure t
  :diminish
  (visual-line-mode abbrev-mode)
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
   (display-line-numbers 'relative)
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
    (global-subword-mode t)
    (global-visual-line-mode t)
    (display-battery-mode -1)
    (line-number-mode t)
    (display-line-numbers-mode t)
    (column-number-mode t)
    (save-place-mode t)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (defalias 'yes-or-no-p 'y-or-n-p)))

(provide 'init)
;;; init.el ends here
