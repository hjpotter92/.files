;;; init --- Summary
;;; Commentary:

;;; Code
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

(defvar highlight-blocks-mode)
(defvar sml/replacer-regexp-list)
(defvar use-package-minimum-reported-time)
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
(setq-default custom-file "~/.emacs.d/init-custom.el")

(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

(setq highlight-blocks-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; (require 'dired+)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun hjpotter92/pretty-symbols ()
  "Add custom symbols in pretty-symbol mode."
  (push '("!=" . ?≠) prettify-symbols-alist)
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("+=" . ?⩲) prettify-symbols-alist)
  (push '("==" . ?⩵) prettify-symbols-alist)
  (push '("=>" . ?⇒) prettify-symbols-alist))

(defun hjpotter92/pretty-symbols-python ()
  "Python specific pretty symbols."
  (hjpotter92/pretty-symbols)
  (push '("def"    . ?ƒ) prettify-symbols-alist)
  (push '("sum"    . ?Σ) prettify-symbols-alist)
  (push '("**2"    . ?²) prettify-symbols-alist)
  (push '("**3"    . ?³) prettify-symbols-alist)
  (push '("None"   . ?∅) prettify-symbols-alist)
  (push '("in"     . ?∈) prettify-symbols-alist)
  (push '("not in" . ?∉) prettify-symbols-alist)
  (push '("return" . ?➡) prettify-symbols-alist))

(defun hjpotter92/pretty-symbols-lisp ()
  "Lisp symbols."
  (hjpotter92/pretty-symbols)
  (push '("defun"    . ?ƒ) prettify-symbols-alist)
  (push '("defmacro" . ?μ) prettify-symbols-alist)
  (push '("defvar"   . ?ν) prettify-symbols-alist))

(add-hook 'python-mode-hook 'hjpotter92/pretty-symbols-python)
(add-hook 'emacs-lisp-mode-hook 'hjpotter92/pretty-symbols-lisp)

(progn
  (setq use-package-minimum-reported-time 0)
  (setq use-package-compute-statistics t)
  (require 'use-package))

(use-package dired+
  :commands toggle-diredp-find-file-reuse-dir)

(use-package dired
  :commands (dired
             find-name-dired
             find-dired)
  :config
  (toggle-diredp-find-file-reuse-dir 1))

(use-package smart-window)

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package image-dired+
  :ensure t
  :config (image-diredx-async-mode 1))

(use-package realgud
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode
  (("Dockerfile\\'" . dockerfile-mode)))

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
  ((lisp-mode emacs-lisp-mode) . focus-mode)
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

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :custom
  ((ivy-use-virtual-buffers t)
   (enable-recursive-minibuffers t)
   (ivy-extra-directories nil))
  :config
  (progn
    (ivy-mode 1))
  :bind
  (("\C-s" . swiper)
   ("\C-r" . ivy-resume)
   ([f6] . ivy-resume)))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :config (counsel-mode t)
  :bind
  (("C-x C-f" . counsel-find-file)
   ("C-x M-f" . counsel-recentf)
   ("M-x" . counsel-M-x)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f1> b" . counsel-descbinds)
   ("<f2> u" . counsel-unicode-char)
   ("C-c k" . counsel-rg)
   ("M-y" . counsel-yank-pop)
   (:map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))))

(use-package ivy-xref
  :ensure t
  :after (ivy)
  :init
  (progn
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))

(use-package all-the-icons-ivy
  :after (ivy)
  :config
  (all-the-icons-ivy-setup))

(use-package projectile
  :ensure t
  :after (ivy)
  :delight (projectile-mode '(:eval (concat " P[" (projectile-project-name) "]")))
  :custom
  ((projectile-enable-caching t)
   (projectile-completion-system 'ivy)
   (projectile-require-project-root nil)
   ;; (projectile-mode-line '(:eval
   ;;                         (if (projectile-project-p)
   ;;                             (format "P[%s]" (projectile-project-name))
   ;;                           "")))
   (projectile-tags-backend "ggtags"))
  :init
  (progn
    (projectile-mode t))
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

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :init
  (progn
    (counsel-projectile-mode t)))

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
  :delight ggtags-mode
  :diminish
  :hook
  ((python-mode ruby-mode js3-mode emacs-lisp-mode) . ggtags-mode))

(use-package counsel-gtags
  :ensure t
  :delight counsel-gtags-mode
  :diminish
  :after (ggtags)
  :hook (ggtags-mode . counsel-gtags-mode))

(use-package treemacs
  :ensure t
  :defer t
  :custom
  ((treemacs-collapse-dirs              (if (executable-find "python") 3 0))
   (treemacs-file-event-delay           30000)
   (treemacs-follow-after-init          t)
   (treemacs-follow-recenter-distance   0.1)
   (treemacs-goto-tag-strategy          'refetch-index)
   (treemacs-indentation                2)
   (treemacs-indentation-string         " ")
   (treemacs-is-never-other-window      nil)
   (treemacs-no-png-images              nil)
   (treemacs-project-follow-cleanup     nil)
   (treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
   (treemacs-recenter-after-file-follow nil)
   (treemacs-recenter-after-tag-follow  nil)
   (treemacs-show-hidden-files          t)
   (treemacs-silent-filewatch           t)
   (treemacs-silent-refresh             t)
   (treemacs-sorting                    'alphabetic-desc)
   (treemacs-space-between-root-nodes   t)
   (treemacs-tag-follow-cleanup         t)
   (treemacs-tag-follow-delay           1.5)
   (treemacs-width                      35)
   (imenu-create-index-function #'ggtags-build-imenu-index))
  :config
  (progn
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (treemacs-resize-icons 16)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ([f7]   . treemacs)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

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
  (setq web-mode-markup-indent-offset 2)
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
  :config
  (progn
    (setq company-quickhelp-delay 0.1)
    (company-quickhelp-mode)))

(use-package company
  :ensure t
  :diminish
  :requires company-statistics
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
  :custom
  ((company-dabbrev-downcase nil)
   (company-idle-delay 0)
   (company-require-match nil)
   (company-show-numbers t)
   (company-minimum-prefix-length 2))
  :config
  (progn
    (use-package company-web
      :ensure t
      :bind
      (("C-c w" . company-web-html))
      :config
      (add-to-list 'company-backends 'company-web-html))
    (use-package company-jedi
      :defer t)
    (use-package company-flx
      :defer t
      :config
      (company-flx-mode t)))
  :hook
  ((after-init . global-company-mode)
   (after-init . company-statistics-mode)
   (python-mode . (lambda ()
                    (add-to-list 'company-backends 'company-jedi)))))

(use-package ace-jump-mode
  :bind
  (("C-." . ace-jump-mode)))

(use-package monokai-theme
  :if (display-graphic-p)
  :config
  (progn
    (load-theme 'monokai t)))

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
    (add-to-list 'sml/replacer-regexp-list '("^:Doc:Loktra/" ":lk:") t)
    (add-to-list 'sml/replacer-regexp-list '("^:lk:hypertrack-webhook/" ":lkHT:") t)
    (add-to-list 'sml/replacer-regexp-list '("^:lkHT:vered/v1/" ":lkHT1:") t)
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
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1))
  :config
  (progn
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
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

(use-package diminish
  :ensure t
  :config
  (progn
    (diminish 'abbrev-mode "Ab")
    (diminish 'outline-mode)))

(use-package python-mode
  :ensure t
  :defer t
  :mode "\\.py"
  :custom
  ((py-split-window-on-execute nil)))

(use-package pipenv
  :disabled
  :hook (python-mode . pipenv-mode)
  :diminish)

(use-package fill-column-indicator
  :ensure t
  :commands (fci-mode)
  :hook
  ((lua-mode python-mode elisp-mode) . fci-mode)
  :custom
  ((fci-rule-column 80)))

(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 2))

(use-package server
  :if (display-graphic-p)
  :config
  (progn
    (unless (or (daemonp) (server-running-p))
        (server-start))))

(use-package with-editor
  :config (shell-command-with-editor-mode t))

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
  (('highlight-parentheses-mode " ❪❫" 'highlight-parentheses)
   ('global-highlight-parentheses-mode " ❪❫" 'highlight-parentheses))
  :config
  (progn
    (global-highlight-parentheses-mode t)))

(use-package omni-scratch
  :ensure t
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

(use-package emacs
  :ensure t
  :diminish
  (visual-line-mode abbrev-mode)
  :bind
  (("C-c m" . menu-bar-mode)
   ("C-c s" . scroll-bar-mode)
   ("C-x k" . kill-this-buffer)
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
    (display-time-default-load-average nil)
    (display-time-format "%a %d %b, %I:%M %p")
    (visual-line-fringe-indicators (quote (left-curly-arrow nil)))
    (require-final-newline t))
  :init
  (progn
    (setq inhibit-compacting-font-caches t)
    (setq prettify-symbols-unprettify-at-point t)
    (setq-default inhibit-startup-screen t)
    (setq-default initial-scratch-message nil)
    (setq-default indent-tabs-mode nil))
  :config
  (progn
    (hjpotter92/pretty-symbols)
    (global-prettify-symbols-mode t)
    (global-subword-mode t)
    (global-visual-line-mode t)
    (line-number-mode t)
    (column-number-mode t)
    (save-place-mode t)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (defalias 'yes-or-no-p 'y-or-n-p)
    (display-time-mode t)))

(provide 'init)
;;; init.el ends here
