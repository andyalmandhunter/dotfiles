;;; linum --- Andy Almand-Hunter
;;; Commentary:

;;; Code:
;;; Shortcut for linum mode
(require 'linum)
(global-set-key "\C-cn" 'linum-mode)

;;; Fix font scaling with linum mode
;; (defun linum-update-window-scale-fix (win)
;;   "Fix linum by scaling text in WIN."
;;   (set-window-margins win
;; 		      (ceiling (* (if (boundp 'text-scale-mode-step)
;; 				      (expt text-scale-mode-step
;; 					    text-scale-mode-amount) 1)
;; 				  (if (car (window-margins))
;; 				      (car (window-margins)) 1)
;; 				  ))))
;; (advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

;;; linum ends here
