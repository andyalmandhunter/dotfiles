;;; .emacs --- Andy Almand-Hunter

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
(add-to-list 'custom-theme-load-path "~/repos/emacs-color-theme-solarized/")
(load-theme 'seti t)
(defun toggle-background ()
  (interactive)
  (let ((mode (if (eq frame-background-mode 'dark) 'light 'dark)))
    (setq frame-background-mode mode)
    (setq terminal-background-mode mode))
  (enable-theme 'solarized))
(global-set-key "\C-cb" 'toggle-background)


;;; Keep buffers opened when leaving an emacs client
(setq server-kill-new-buffers nil)


;;; Set PATH and exec-path
(setenv "PATH" (concat "/Library/Frameworks/Python.framework/Versions/2.7/bin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Library/Frameworks/Python.framework/Versions/2.7/bin")))


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


;;; Shortcut for linum mode
(require 'linum)
(global-set-key "\C-cl" 'linum-mode)
(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-format-fmt
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d "))))))
(defun linum-format-func (line)
  (propertize (format linum-format-fmt line) 'face 'linum))
(unless window-system
  (setq linum-format 'linum-format-func))

;;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


;;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;; Make it easier to identify current window at a glance
(menu-bar-mode -1)
(set-face-background 'mode-line-inactive "black")


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
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)


;; SQL
;; http://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client/
(setq sql-postgres-program "psql"
      sql-send-terminator t)

(defun aeh/sql-mode-hook ()
  (abbrev-mode 1)
  ;; Make underscore a word character so that abbrev stops expanding send_count to send_COUNT
  (modify-syntax-entry ?_ "w" sql-mode-syntax-table)
  (when (file-exists-p "~/.sql-abbreviations")
    (load "~/.sql-abbreviations")))

(defun aeh/sql-interactive-mode-hook ()
  (toggle-truncate-lines t))

(defun sql-in-code-context-p ()
  (if (fboundp 'buffer-syntactic-context) ; XEmacs function.
       (null (buffer-syntactic-context))
     ;; Attempt to simulate buffer-syntactic-context
     ;; I don't know how reliable this is.
     (let* ((beg (save-excursion
 		  (beginning-of-line)
 		  (point)))
 	   (list
 	    (parse-partial-sexp beg (point))))
       (and (null (nth 3 list))		; inside string.
 	   (null (nth 4 list))))))	; inside cocmment

(defun sql-pre-abbrev-expand-hook ()
  ;; Allow our abbrevs only in a code context.
  (setq local-abbrev-table
        (if (sql-in-code-context-p)
            sql-mode-abbrev-table)))

(add-hook 'sql-mode-hook 'aeh/sql-mode-hook)
(add-hook 'sql-interactive-mode-hook 'aeh/sql-mode-hook)
(add-hook 'sql-interactive-mode-hook 'aeh/sql-interactive-mode-hook)
(add-hook 'pre-abbrev-expand-hook 'sql-pre-abbrev-expand-hook)

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)


;;; ESS Mode
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ess")
(load "ess-site")
(require 'ess-site)


;;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)


;;; Elpy (Python)
(elpy-enable)
(setq elpy-rpc-python-command
      "/Library/Frameworks/Python.framework/Versions/2.7/bin/python")
(elpy-use-ipython)
(setq elpy-modules
      (delete 'flymake-mode
              (delete 'elpy-module-highlight-indentation elpy-modules)))
(global-set-key "\C-cf" 'flymake-mode)


;;; Highlight-indentation-mode
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#222")
(global-set-key "\C-ch" 'highlight-indentation-mode)


;;; Highlight-parentheses-mode
(require 'highlight-parentheses)
(global-set-key "\C-cp" 'highlight-parentheses-mode)


;;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.htm[l]?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.htm[l]?\\'")))
(add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-html-offset 4)

;; .js and .jsx
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))
(global-set-key "\C-cj" 'flycheck-mode)
