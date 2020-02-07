;;; languages --- Andy Almand-Hunter
;;; Commentary:

;;; Code:
;;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;; ESS Mode
(require 'key-combo)
(key-combo-mode 1)
(add-hook 'ess-mode-hook
          '(lambda()
             (key-combo-mode t)))
(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (key-combo-mode t)))
(defvar key-combo-ess-default
  '((">"  . (" > " " %>% "))
    ("$"  . ("$" " %$% "))
    ("<>" . " %<>% ")
    ("*"  . ("*" " * "))
    ("%" . ("%" "%*%" "%%"))
    ("^"  . ("^" " ^ "))
    ("/"  . ("/" " / "))
    ("~" . " ~ ")
    (":" . (":" "::" ":::"))
    (":="  . " := ") ; data.table
    ("->"  . " -> ")))
(key-combo-define-hook '(ess-mode-hook inferior-ess-mode-hook)
                       'ess-key-combo-load-default
                       key-combo-ess-default)
(setq ess-default-style 'DEFAULT)

;;; Elpy (Python)
(use-package elpy
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  :config
  (setq elpy-rpc-backend "jedi")
  (setq elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules))
  (remove-hook 'elpy-modules 'elpy-module-flymake))

(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4)
  (elpy-enable))

(use-package pyenv-mode
  :init
  (setq exec-path (cons "~/.pyenv/shims" exec-path))
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))

(defvar pyenv-current-version nil nil)

(defun pyenv-init()
  "Initialize pyenv's current version to the global one."
  (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
    (message (concat "Setting pyenv version to " global-pyenv))
    (pyenv-mode-set global-pyenv)
    (setq pyenv-current-version global-pyenv)))

(add-hook 'after-init-hook 'pyenv-init)

;;; Web mode
(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-html-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-attr-value-indent-offset 2)
(setq web-mode-enable-auto-quoting nil)
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))

;; Prettier
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))))

(eval-after-load 'web-mode
  '(progn
     (add-hook 'web-mode-hook #'add-node-modules-path)))

;; CSS and SCSS
(setq css-indent-offset 2)

;; .js and .jsx
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; JSON
(setq json-reformat:indent-width 2)
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))
(add-to-list 'auto-mode-alist '("\\.ddl\\'" . json-mode))

;;; JavaScript
(setq js-indent-level 2)

;;; CoffeeScript
(custom-set-variables '(coffee-tab-width 2))

;;; Ruby
(setq ruby-insert-encoding-magic-comment nil)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(setq enh-ruby-deep-indent-paren nil)
(setq enh-ruby-deep-indent-construct nil)

;;; YAML
(require 'yaml-mode)

;;; Golang
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)))


;;; HTTP requests
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;;; languages.el ends here
