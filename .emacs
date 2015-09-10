;;;; .emacs
;;;   Andy Almand-Hunter

;;; General config options
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq transient-mark-mode t)
(setq global-auto-revert-mode t)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(display-time-mode 1)


;;; Color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'sunburst t)


;;; Keep buffers opened when leaving an emacs client
(setq server-kill-new-buffers nil)


;;; Set PATH and exec-path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))


;;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.8.0")
(require 'yasnippet)
(yas-global-mode 1)


;;; Emacs Powerline
(require 'powerline)
(powerline-default-theme)


;;; Mouse in iTerm2
(require 'mouse)
(xterm-mouse-mode t)
(global-set-key [mouse-4] (lambda ()
                            (interactive)
                            (scroll-down 1)))
(global-set-key [mouse-5] (lambda ()
                            (interactive)
                            (scroll-up 1)))
(defun track-mouse (e))
(setq mouse-sel-mode t)


;; ;;; CSV mode
;; (require 'csv-mode)


;;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.htm[l]?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.htm[l]?\\'")))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)


;;; Shortcut for linum mode
(require 'linum)
(global-set-key "\C-cl" 'linum-mode)


;;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


;;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;; Make it easier to identify current window at a glance
(menu-bar-mode -1)


;;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;;; Indent and align
(global-set-key "\C-x|" 'align-regexp)
(global-set-key "\C-x\\" 'indent-rigidly)


;;; Interact with OSX clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


;;; Win-switch mode
(require 'win-switch)
(global-set-key "\C-xo" 'win-switch-dispatch)
(win-switch-authors-configuration)


;; Subword mode makes it easier to work with camel-case and snake-
;; case variables
(add-hook 'prog-mode-hook 'subword-mode)


;;; ESS Mode
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ess")
(load "ess-site")
(require 'ess-site)
