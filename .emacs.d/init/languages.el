;;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;; ESS Mode
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/ess")
(load "ess-site")
(require 'ess-site)

;;; Elpy (Python)
(elpy-enable)
(setq elpy-rpc-python-command
      "/Library/Frameworks/Python.framework/Versions/2.7/bin/python")
(setq elpy-modules
      (delete 'elpy-module-highlight-indentation elpy-modules))
(remove-hook 'elpy-modules 'elpy-module-flymake)

;;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.htm[l]?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.htm[l]?\\'")))
(add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-html-offset 4)
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))

;; .js and .jsx
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; AucTeX
(when window-system
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  (setq TeX-master "master"))

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
