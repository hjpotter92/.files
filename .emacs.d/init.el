;;; init --- Summary
;;; Commentary:
;; You might already have this line
(require 'package)
(setq package-enable-at-startup nil)

;;; Code:
(mapc (lambda(p) (add-to-list 'package-archives p t))
      '(("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(package-initialize)

(el-get 'sync)
(defvar el-get-recipe-path)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

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
 '(global-visual-line-mode t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(line-number-mode t)
 '(package-selected-packages
   (quote
    (all-the-icons-ivy all-the-icons-dired magit diminish company-jedi ac-html-bootstrap company-ngram company-quickhelp company-shell company-statistics unicode-fonts counsel-etags counsel-gtags ctags-update region-bindings-mode multiple-cursors web git-commit git-gutter git-gutter+ git-gutter-fringe git-ps1-mode gitattributes-mode gitconfig gitconfig-mode company-emoji company-erlang company-go company-lua company-math company-rtags company-web editorconfig font-utils fontawesome format-all format-sql nose pipenv pydoc python-mode pythonic flycheck-color-mode-line flycheck-css-colorguard flycheck-elixir flycheck-mix flycheck-pycheckers flycheck-tcl xpm web-mode use-package-ensure-system-package use-package-el-get switch-window smartparens smart-window smart-mode-line smart-cursor-color monokai-theme mode-icons markdown-preview-mode markdown-mode+ image-dired+ highlight-parentheses flycheck emmet-mode counsel company auto-minor-mode auto-indent-mode auto-auto-indent)))
 '(save-place t nil (saveplace))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-smartparens-global-mode t)
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

(defvar highlight-blocks-mode)
(defvar sml/replacer-regexp-list)

(setq-default abbrev-mode t)
(setq-default auto-revert-mode t)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq x-stretch-cursor t)
(setq require-final-newline t)
(setq frame-title-format '("[ %b ] @ Emacs " emacs-version))
(setq highlight-blocks-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'dired+)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package smart-window)

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package image-dired+
  :ensure t
  :config (image-diredx-async-mode 1))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
  ("\C-s" . swiper)
  ("C-c C-r" . ivy-resume)
  ([f6] . ivy-resume))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :config (counsel-mode t)
  :bind
  ("C-x C-f" . counsel-find-file)
  ("M-x" . counsel-M-x)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> b" . counsel-descbinds)
  ("C-c k" . counsel-rg)
  ("C-c g" . counsel-git)
  ("M-y" . counsel-yank-pop))

(use-package all-the-icons-ivy
  :after (ivy)
  :config
  (all-the-icons-ivy-setup))

(use-package emmet
  :ensure emmet-mode
  :hook ((sgml-mode html-mode css-mode web-mode) . emmet-mode)
  :after (web-mode))

(use-package web-mode
  :ensure t
  :mode
  (("\\.[pm]?html?$" . web-mode)
   ("\\.tmpl" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))
  :config
  (setq 'web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-engines-alist
        '(("php" . "\\.phtml?\\'")
          ("blade" . "\\.blade\\."))))

(use-package smart-cursor-color
  :ensure t
  :diminish smart-cursor-color-mode
  :config
  (global-hl-line-mode t)
  (smart-cursor-color-mode t))

(use-package multiple-cursors
  :ensure t
  :diminish multiple-cursors-mode
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
        ("[" . mc/cycle-backward)
        ("]" . mc/cycle-forward)
        ("m" . mc/mark-more-like-this-extended)
        ("h" . mc-hide-unmatched-lines-mode)
        ("\\" . mc/vertical-align-with-space)
        ("#" . mc/insert-numbers)             ; use num prefix to set the starting number
        ("^" . mc/edit-beginnings-of-lines)
        ("$" . mc/edit-ends-of-lines)))

(use-package region-bindings-mode
  :ensure t
  :config
  (region-bindings-mode-enable))

(use-package switch-window
  :ensure t
  :bind
  ("C-x o" . switch-window))

(use-package company-jedi
  :ensure t
  :after (company)
  :config
  (with-eval-after-load 'company
    '(push 'company-jedi company-backends)))

(use-package company-statistics
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :requires company-statistics
  :config
  (defun my/python-mode-hook ()
    "Add jedi backend to company"
    (with-eval-after-load 'company
      '(push 'company-jedi company-backends)))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  :hook
  ((after-init . global-company-mode)
   (after-init . company-statistics-mode)))

(use-package monokai-theme
  :config
  (progn
    (load-theme 'monokai t)))

(use-package mode-icons
  :ensure t
  :after (smart-mode-line)
  :config (mode-icons-mode))

(use-package smart-mode-line
  :ensure t
  :after (monokai-theme)
  :init
  (setq sml/no-confirm-load-theme t)
  :config
  (progn
    (setq sml/mode-width 'full)
    (sml/setup)
    ;; (sml/apply-theme 'dark)
    (add-to-list 'sml/replacer-regexp-list '("^:Doc:Loktra/" ":lk:") t)))

(use-package diminish
  :ensure t
  :config
  (progn
    (diminish 'abbrev-mode "Ab")
    (diminish 'outline-mode)))

(use-package emacs
  :ensure t
  :diminish
  (visual-line-mode abbrev-mode)
  :init
  (progn
    (setq package-archive-priorities
          '(("melpa" . 20)
            ("marmalade" . 15)
            ("gnu" . 10)))))

;; (use-package virtualenvwrapper
;;   :init
;;   (venv-initialize-interactive-shells)
;;   (venv-initialize-eshell)
;;   (setq venv-location "~/.virtualenvs/"))

(provide 'init)
;;; init.el ends here
