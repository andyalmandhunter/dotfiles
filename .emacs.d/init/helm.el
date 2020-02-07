;;; helm --- Andy Almand-Hunter
;;; Commentary:

;;; Code:
;; Helm
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)

(global-unset-key (kbd "C-x c"))(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(helm-mode 1)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq helm-ag-insert-at-point 'symbol)

;; helm-company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; helm-xref
(require 'helm-xref)
(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

;;; helm ends here
