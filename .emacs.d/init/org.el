;;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-indented t)
(setq org-startup-folded nil)
(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-files '("~/org"))

(add-hook 'org-mode-hook 'auto-fill-mode)

;; Soji
(defun soji-open-logfile ()
  "Open today's Soji engineering log."
  (interactive)
  (find-file (shell-command-to-string "soji note-path log")))

(global-set-key (kbd "C-x M-l") 'soji-open-logfile)
