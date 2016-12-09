;;; Color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/repos/emacs-color-theme-solarized/")
(when window-system
  (load-theme 'monokai t))
(unless window-system
  (load-theme 'monokai t))
;; (defun toggle-background ()
;;   (interactive)
;;   (let ((mode (if (eq frame-background-mode 'dark) 'light 'dark)))
;;     (setq frame-background-mode mode)
;;     (setq terminal-background-mode mode))
;;   (enable-theme 'solarized))
;; (global-set-key "\C-cb" 'toggle-background)
