;;; font --- Andy Almand-Hunter
;;; Commentary:

;;; Code:
;;; Fonts (For GUI.  In the terminal just use the terminal font)
(when window-system
  (add-to-list 'default-frame-alist '(font . "Anonymous Pro"))
  (setq-default cursor-type 'box)
  (blink-cursor-mode 0))

;;; font.el ends here
