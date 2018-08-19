(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(company-require-match nil)
 '(company-show-numbers t)
 '(debug-on-error t)
 '(display-time-default-load-average nil)
 '(display-time-format "%a %d %b, %I:%M %p")
 '(display-time-mode t)
 '(enable-recursive-minibuffers t)
 '(frame-title-format
   (quote
    ("["
     (:eval
      (if
          (buffer-file-name)
          (abbreviate-file-name
           (buffer-file-name))
        "%b"))
     " (%*%+%z) %F@" emacs-version "]")) t)
 '(imenu-create-index-function (quote ggtags-build-imenu-index) t)
 '(ivy-extra-directories nil)
 '(ivy-use-virtual-buffers t)
 '(lua-indent-level 2)
 '(magit-completing-read-function (quote ivy-completing-read) t)
 '(magit-repository-directories (quote (("~/Documents/Loktra/" . 2) ("~/Documents/" . 1))) t)
 '(package-archive-priorities (quote (("melpa" . 20) ("marmalade" . 15) ("gnu" . 10))))
 '(package-selected-packages
   (quote
    (ivy-yasnippet fill-column-indicator zenburn-theme hl-todo unicode-fonts pipenv smart-window which-key monokai-theme helpful omni-scratch highlight-parentheses switch-window region-bindings-mode multiple-cursors smart-cursor-color web-mode treemacs-projectile treemacs ivy-xref magit python-docstring markdown-mode realgud image-dired+ use-package use-package-el-get persp-mode persp-mode-projectile-bridge yasnippet-snippets yasnippets-mode yasnippets pyvenv amx lua elixir-yasnippets xpm smartparens smart-mode-line python-mode mode-icons minibuffer-line gxref graphviz-dot-mode gitconfig-mode gitconfig gitattributes-mode git-ps1-mode git-gutter-fringe git-gutter+ ggtags format-sql format-all fontawesome focus flycheck-yamllint flycheck-tcl flycheck-pycheckers flycheck-package flycheck-mix flycheck-elixir flycheck-demjsonlint flycheck-css-colorguard flycheck-color-mode-line emmet-mode editorconfig dotenv-mode dockerfile-mode diminish counsel-gtags company-web company-statistics company-shell company-rtags company-quickhelp company-ngram company-math company-lua company-jedi company-go company-flx company-erlang company-emoji auto-minor-mode auto-indent-mode auto-auto-indent all-the-icons-ivy all-the-icons-dired ace-jump-mode ac-html-bootstrap)))
 '(projectile-completion-system (quote ivy))
 '(projectile-enable-caching t)
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line
   (quote
    (:eval
     (if
         (projectile-project-p)
         (format "P[%s]"
                 (projectile-project-name))
       ""))))
 '(projectile-require-project-root nil)
 '(projectile-tags-backend "ggtags")
 '(py-split-window-on-execute nil t)
 '(require-final-newline t)
 '(scroll-error-top-bottom t)
 '(send-mail-function (quote smtpmail-send-it))
 '(tool-bar-mode nil)
 '(treemacs-collapse-dirs 3 t)
 '(treemacs-file-event-delay 30000 t)
 '(treemacs-follow-after-init t t)
 '(treemacs-follow-recenter-distance 0.1 t)
 '(treemacs-goto-tag-strategy (quote refetch-index) t)
 '(treemacs-indentation 2 t)
 '(treemacs-indentation-string " " t)
 '(treemacs-is-never-other-window nil t)
 '(treemacs-no-png-images nil t)
 '(treemacs-persist-file "/home/hjpotter92/.emacs.d/.cache/treemacs-persist" t)
 '(treemacs-project-follow-cleanup nil t)
 '(treemacs-recenter-after-file-follow nil t)
 '(treemacs-recenter-after-tag-follow nil t)
 '(treemacs-show-hidden-files t t)
 '(treemacs-silent-filewatch t t)
 '(treemacs-silent-refresh t t)
 '(treemacs-sorting (quote alphabetic-desc) t)
 '(treemacs-space-between-root-nodes t t)
 '(treemacs-tag-follow-cleanup t t)
 '(treemacs-tag-follow-delay 1.5 t)
 '(treemacs-width 35 t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow nil)))
 '(x-stretch-cursor t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "Hack Nerd Font")))))
