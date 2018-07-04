(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(debug-on-error t)
 '(display-time-mode t)
 '(imenu-create-index-function (quote ggtags-build-imenu-index) t)
 '(package-selected-packages
   (quote
    (yaml-mode xpm which-key web-mode web use-package-ensure-system-package use-package-el-get unicode-fonts treemacs-projectile switch-window smartparens smart-window smart-mode-line smart-cursor-color region-bindings-mode python-mode pydoc projectile-ripgrep projectile-rails pipenv nose multiple-cursors monokai-theme mode-icons minibuffer-line markdown-preview-mode markdown-mode+ magit latex-unicode-math-mode latex-math-preview latex-extra json-navigator json-mode js3-mode ivy-xref image-dired+ highlight-parentheses helpful gxref graphviz-dot-mode gitconfig-mode gitconfig gitattributes-mode git-ps1-mode git-gutter-fringe git-gutter+ ggtags format-sql format-all fontawesome focus flycheck-yamllint flycheck-tcl flycheck-pycheckers flycheck-package flycheck-mix flycheck-elixir flycheck-demjsonlint flycheck-css-colorguard flycheck-color-mode-line emmet-mode editorconfig dotenv-mode dockerfile-mode diminish counsel-projectile counsel-gtags company-web company-statistics company-shell company-rtags company-quickhelp company-ngram company-math company-lua company-jedi company-go company-flx company-erlang company-emoji auto-minor-mode auto-indent-mode auto-auto-indent all-the-icons-ivy all-the-icons-dired ace-jump-mode ac-html-bootstrap)))
 '(projectile-enable-caching t t)
 '(projectile-mode-line
   (quote
    (:eval
     (if
         (projectile-project-p)
         (format "P[%s]"
                 (projectile-project-name))
       ""))) t)
 '(projectile-tags-backend "ggtags" t)
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
 '(treemacs-width 35 t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "Hack Nerd Font")))))
