;;; early-init --- Summary
;;; Commentary:

;;; Code:
(setq package-user-dir (concat user-emacs-directory "elpa-" emacs-version))
(setq package-check-signature nil)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu-devel" . "https://elpa.gnu.org/devel/")
        ("gelpa" . "https://gelpa.gdritter.com/")))

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
