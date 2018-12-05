;;; ternimal --- Andy Almand-Hunter
;;; Commentary:

;;; Code:
;;; Emacs Powerline
(unless window-system
  (require 'powerline)
  (powerline-default-theme))


;;; Mouse in iTerm2
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))


(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-format-fmt
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d ")))))
  (defun linum-format-func (line)
    (propertize (format linum-format-fmt line) 'face 'linum))
  (unless window-system
    (setq linum-format 'linum-format-func)))

;;; terminal.el ends here
