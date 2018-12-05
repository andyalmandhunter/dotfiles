;;; win_switch --- Andy Almand-Hunter
;;; Commentary:

;;; Code:
;;; Make it easier to identify current window at a glance
(menu-bar-mode -1)
(tool-bar-mode -1)
(unless window-system
  (set-face-background 'mode-line-inactive "black"))
(when window-system
  (scroll-bar-mode -1))

;;; Win-switch mode
(require 'win-switch)
(global-set-key (kbd "C-x o") 'win-switch-dispatch)
(win-switch-authors-configuration)

;;; win_switch ends here
