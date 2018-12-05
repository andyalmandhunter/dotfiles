;;; .emacs --- Andy Almand-Hunter
;;; Commentary:

;;; Code:
;;; General config options
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'default-frame-alist '(height . 52))
(add-to-list 'default-frame-alist '(width . 80))

(setq shell-file-name "/bin/bash")
(setq transient-mark-mode t)
(setq global-auto-revert-mode t)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(display-time-mode 1)
(global-set-key (kbd "C-c r") 'revert-buffer)

;;; Set PATH and exec-path
(setenv "PATH" (concat "/Library/Frameworks/Python.framework/Versions/2.7/bin:"
                       "/usr/local/bin:"
                       "/Library/TeX/texbin:"
                       "/Users/ahunter/bin:"
                       "/Users/ahunter/.nvm/versions/node/v6.11.1/bin:"
                       (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
(setq exec-path (append exec-path '("/Library/Frameworks/Python.framework/Versions/2.7/bin")))
(setq exec-path (append exec-path '("/Users/ahunter/bin")))
(setq exec-path (append exec-path '("/Users/ahunter/.nvm/versions/node/v6.11.1/bin")))


;; Package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("elpy" . "http://jorgenschaefer.github.io/packages/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Ensime
(use-package ensime
  :ensure t
  :pin melpa-stable)

;; JS-comint
(package-install 'js-comint)
(require 'js-comint)
(js-do-use-nvm)
(setq inferior-js-program-command "babel-node")

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

;;; Indent and align
(global-set-key (kbd "C-x |") 'align-regexp)
(global-set-key (kbd "C-x \\") 'indent-rigidly)

;; Subword mode makes it easier to work with camel-case and snake-
;; case variables
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;; Highlight-indentation-mode
(require 'highlight-indentation)
;; (unless window-system
;;   (set-face-background 'highlight-indentation-face "#222"))
(global-set-key (kbd "C-c i") 'highlight-indentation-mode)

;;; Highlight-parentheses-mode
(require 'highlight-parentheses)
(show-paren-mode 1)

;;; Multi-term mode
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;;; Tab width
(setq tab-width 4)

;; handle emacs utf-8 input
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")

;; typing 'yes' is too hard
(defalias 'yes-or-no-p 'y-or-n-p)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; Imports
(load-file "~/.emacs.d/init/theme.el")
(load-file "~/.emacs.d/init/font.el")
(load-file "~/.emacs.d/init/snippets.el")
(load-file "~/.emacs.d/init/terminal.el")
(load-file "~/.emacs.d/init/sql.el")
(load-file "~/.emacs.d/init/macos.el")
(load-file "~/.emacs.d/init/languages.el")
(load-file "~/.emacs.d/init/linum.el")
(load-file "~/.emacs.d/init/flycheck.el")
(load-file "~/.emacs.d/init/org.el")
(load-file "~/.emacs.d/init/win_switch.el")
(load-file "~/.emacs.d/init/helm.el")

;; Fancy Org-mode Bullets
(use-package org-bullets
  :commands org-bullets-mode
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; Visual regexp
(use-package visual-regexp
  :config
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace)
  ;; if you use multiple-cursors, this is for you:
  (define-key global-map (kbd "C-c m") 'vr/mc-mark))

;;; Company mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)
(add-to-list 'company-backends 'company-jedi)
