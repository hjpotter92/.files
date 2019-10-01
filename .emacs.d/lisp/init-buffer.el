;;; init-buffer.el --- Buffer handling setup

;; Author: hjpotter92 <hjpotter92+github@gmail.com>
;; Maintainer: hjpotter92 <hjpotter92+github@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/hjpotter92/.files
;; Keywords: tools convenience vc


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

;; Configuration for buffer handling in Emacs.

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package ibuffer
  :ensure nil
  :bind
  (("C-x C-b" . ibuffer)
   :map ibuffer-mode-map
   ("." . ibuffer-hydra/body))
  :commands
  (ibuffer-auto-mode
   ibuffer-switch-to-saved-filter-groups)
  :config
  (progn
    (defhydra hydra-ibuffer-mark
      (:color teal :columns 5 :after-exit (ibuffer-hydra/body))
      "Mark"
      ("*" ibuffer-unmark-all "unmark all")
      ("M" ibuffer-mark-by-mode "mode")
      ("m" ibuffer-mark-modified-buffers "modified")
      ("u" ibuffer-mark-unsaved-buffers "unsaved")
      ("s" ibuffer-mark-special-buffers "special")
      ("r" ibuffer-mark-read-only-buffers "read-only")
      ("/" ibuffer-mark-dired-buffers "dired")
      ("e" ibuffer-mark-dissociated-buffers "dissociated")
      ("h" ibuffer-mark-help-buffers "help")
      ("z" ibuffer-mark-compressed-file-buffers "compressed")
      ("b" nil "back" :color blue))
    (defhydra hydra-ibuffer-action
      (:color teal :columns 4
              :after-exit
              (if (eq major-mode 'ibuffer-mode)
                  (ibuffer-hydra/body)))
      "Action"
      ("A" ibuffer-do-view "view")
      ("E" ibuffer-do-eval "eval")
      ("F" ibuffer-do-shell-command-file "shell-command-file")
      ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
      ("H" ibuffer-do-view-other-frame "view-other-frame")
      ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
      ("M" ibuffer-do-toggle-modified "toggle-modified")
      ("O" ibuffer-do-occur "occur")
      ("P" ibuffer-do-print "print")
      ("Q" ibuffer-do-query-replace "query-replace")
      ("R" ibuffer-do-rename-uniquely "rename-uniquely")
      ("T" ibuffer-do-toggle-read-only "toggle-read-only")
      ("U" ibuffer-do-replace-regexp "replace-regexp")
      ("V" ibuffer-do-revert "revert")
      ("W" ibuffer-do-view-and-eval "view-and-eval")
      ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
      ("b" nil "back"))
    (defhydra hydra-ibuffer-sort (:color amaranth :columns 3 :after-exit (ibuffer-hydra/body))
      "Sort"
      ("i" ibuffer-invert-sorting "invert")
      ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
      ("v" ibuffer-do-sort-by-recency "recently used")
      ("s" ibuffer-do-sort-by-size "size")
      ("f" ibuffer-do-sort-by-filename/process "filename")
      ("m" ibuffer-do-sort-by-major-mode "mode")
      ("b" nil "back" :color blue))
    (defhydra hydra-ibuffer-filter (:color amaranth :columns 4 :after-exit (ibuffer-hydra/body))
      "Filter"
      ("m" ibuffer-filter-by-used-mode "mode")
      ("M" ibuffer-filter-by-derived-mode "derived mode")
      ("n" ibuffer-filter-by-name "name")
      ("c" ibuffer-filter-by-content "content")
      ("e" ibuffer-filter-by-predicate "predicate")
      ("f" ibuffer-filter-by-filename "filename")
      (">" ibuffer-filter-by-size-gt "size")
      ("<" ibuffer-filter-by-size-lt "size")
      ("/" ibuffer-filter-disable "disable")
      ("b" nil "back" :color blue)))
  :pretty-hydra
  ((:quit-key "." :title "ibuffer hydra")
   ("Navigation"
    (("h" ibuffer-backword-filter-group "up group")
     ("k" ibuffer-backward-line "up")
     ("RET" ibuffer-visit-buffer "visit" :color blue)
     ("j" ibuffer-forward-line "down")
     ("l" ibuffer-forward-filter-group "down group"))
    "Mark"
    (("m" ibuffer-mark-forward "mark")
     ("u" ibuffer-unmark-forward "unmark")
     ("*" hydra-ibuffer-mark/body "specific" :color blue)
     ("t" ibuffer-toggle-marks "toggle"))
    "Actions"
    (("D" ibuffer-do-delete "delete")
     ("S" ibuffer-do-save "save")
     ("." nil "toggle hydra" :color blue)
     ("a" hydra-ibuffer-action/body "all" :color blue))
    "View"
    (("g" ibuffer-update "refresh")
     ("s" hydra-ibuffer-sort/body "sort" :color blue)
     ("H" describe-mode "help")
     ("/" hydra-ibuffer-filter/body "filter" :color blue))
    "Select"
    (("o" ibuffer-visit-buffer-other-window "other window" :color blue)
     ("TAB" ibuffer-toggle-filter-group "toggle")
     ("q" quit-window "quit ibuffer" :color blue))))
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-auto-mode t)
                    (ibuffer-switch-to-saved-filter-groups "default")
                    (ibuffer-hydra/body)))
  :custom
  ((ibuffer-expert t)
   (ibuffer-show-empty-filter-groups nil)
   (ibuffer-saved-filter-groups
    (quote
     (("default"
       (" dired" (mode . dired-mode))
       (" perl" (mode . cperl-mode))
       (" programming"
        (or
         (mode . emacs-lisp-mode)
         (mode . lua-mode)
         (mode . python-mode)
         (mode . c++-mode)))
       (" planner"
        (or
         (name . "^\\*Calendar\\*$")
         (name . "^diary$")
         (mode . muse-mode)))
       ("emacs"
        (or
         (name . "^\\*scratch\\*$")
         (name . "^\\*Messages\\*$")))
       ("org" (name . "^.*org$"))
       (" magit" (mode . magit-mode))
       (" gnus"
        (or
         (mode . message-mode)
         (mode . bbdb-mode)
         (mode . mail-mode)
         (mode . gnus-group-mode)
         (mode . gnus-summary-mode)
         (mode . gnus-article-mode)
         (name . "^\\.bbdb$")
         (name . "^\\.newsrc-dribble")))
       ("爵 web" (or (mode . web-mode) (mode . js2-mode)))
       (" shell" (or (mode . eshell-mode) (mode . shell-mode)))))))))

(use-package ibuffer-vc
  :after (ibuffer)
  :commands
  (ibuffer-do-sort-by-alphabetic)
  :hook (ibuffer-mode . (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(provide 'init-buffer)

;;; init-buffer.el ends here
