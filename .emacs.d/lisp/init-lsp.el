;;; init-lsp.el --- LSP mode settings  -*- lexical-binding: t; -*-

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

;; commentary

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (progn
    ;; Set gc to 100 MB and process buffer to 5 MB
    (setq gc-cons-threshold (* 100 1024 1024))
    (setq read-process-output-max (* 5 1024 1024)))
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :custom
  ((lsp-auto-configure t)
   (lsp-auto-guess-root t)
   (lsp-before-save-edits t)
   (lsp-completion-show-detail t)
   (lsp-completion-show-kind t)
   (lsp-diagnostic-provider :flycheck)
   (lsp-eldoc-enable-hover t)
   (lsp-enable-completion-at-point t)
   (lsp-enable-file-watchers t)
   (lsp-enable-folding t)
   (lsp-enable-indentation t)
   (lsp-enable-imenu t)
   (lsp-enable-links t)
   (lsp-enable-on-type-formatting t)
   (lsp-enable-text-document-color t)
   (lsp-enable-semantic-highlighting t)
   (lsp-enable-snippet t)
   (lsp-enable-symbol-highlighting t)
   (lsp-enable-xref t)
   (lsp-file-watch-threshold 4096)
   (lsp-headerline-breadcrumb-enable t)
   (lsp-keep-workspace-alive nil)
   (lsp-lens-enable t)
   (lsp-modeline-code-actions-enable t)
   (lsp-modeline-diagnostics-enable t)
   (lsp-prefer-capf t)
   (lsp-prefer-flymake nil)
   (lsp-semantic-tokens-enable t)
   (lsp-signature-auto-activate t)
   (lsp-signature-render-documentation t))
  :hook
  ((web-mode js-mode json-mode js2-mode css-mode dockerfile-mode
             python-mode ruby-mode c++-mode c-mode go-mode LaTeX-mode
             erlang-mode elixir-mode markdown-mode typescript-mode sbt-mode scala-mode)
   . lsp)
  (yaml-mode . (lambda ()
                 (when (eq major-mode 'yaml-mode)
                   (lsp))))
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-pyright
  :disabled
  :if (executable-find "pyright")
  :custom
  ((lsp-pyright-auto-import-completions t)
   (lsp-pyright-auto-search-paths t)
   ;; (lsp-pyright-log-level "Trace")
   (lsp-pyright-multi-root nil)
   (lsp-pyright-venv-path "/home/hjpotter92/.pyenv/versions/"))
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp))))  ; or lsp-deferred

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind
  (([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
   ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  ((lsp-ui-doc-delay 1)
   (lsp-ui-doc-enable t)
   (lsp-ui-doc-include-signature t)
   (lsp-ui-doc-use-webkit nil)
   (lsp-ui-doc-position 'at-point)
   (lsp-ui-flycheck t)
   (lsp-ui-imenu-enable t)
   (lsp-ui-peek-enable t)
   (lsp-ui-sideline-enable t)
   (lsp-ui-sideline-diagnostic-max-lines 5)
   (lsp-ui-sideline-ignore-duplicate t)
   (lsp-ui-sideline-show-code-actions t)
   (lsp-ui-sideline-show-hover nil))
  :hook
  ((lsp-mode . lsp-lens-mode))
  ;; :config
  ;; (progn
  ;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
  ;;     (setq mode-line-format nil)))
  )

(use-package lsp-ivy
  :after lsp-mode)

(provide 'init-lsp)

;;; init-lsp.el ends here
